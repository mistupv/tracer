-module(tracer).

-export([trace/2, trace/3]).

trace(InitialCall, PidAnswer) ->
    trace(InitialCall, PidAnswer, []).

trace(InitialCall, PidAnswer, Opts) ->
    NOpts0 =
        case proplists:is_defined(timeout, Opts) of
            true -> Opts;
            false -> [{timeout, 2000} | Opts]
        end,
    NOpts1 =
        case proplists:is_defined(dir, NOpts0) of
            true -> NOpts0;
            false -> [{dir, "."} | NOpts0]
        end,
    NOpts2 =
        case proplists:is_defined(mods, NOpts1) of
            true -> NOpts1;
            false -> [{mods, []} | NOpts1]
        end,
    NOpts3 =
        case proplists:is_defined(log_dir, NOpts2) of
            true -> NOpts2;
            false -> [{log_dir, "trace"} | NOpts2]
        end,
    trace_1(InitialCall, PidAnswer, NOpts3).

trace_1(InitialCall, PidAnswer, Opts) ->
    ModName = get_mod_name(InitialCall),
    {ok, TracingNode} = 
        slave:start(
            list_to_atom(net_adm:localhost()), 
            tracing, 
            "-setcookie cookie"),
    Timeout = proplists:get_value(timeout, Opts),
    Dir     = proplists:get_value(dir,     Opts),
    Mods    = proplists:get_value(mods,    Opts),
    LogDir  = proplists:get_value(log_dir, Opts),
    put(modules_to_instrument, Mods),
    LogHandler = logger:init_log_dir(LogDir),
    put(log_handler, LogHandler),
    logger:append_data(io_lib:fwrite("call ~p~n", [InitialCall])),
    % io:format("~p\n", [SO]),
    % io:format("~p\n~p\n", [ModName, Dir]),
    % OriginalLibCode = 
    %     [code:get_object_code(Mod) || Mod <- [gen_server, supervisor, gen_fsm, proc_lib, gen]],
    instrument_and_reload(ModName, Dir, TracingNode),
    PidMain = self(),
    PidCall = execute_call(InitialCall, self(), Dir, TracingNode),
    logger:append_data(io_lib:fwrite("main_pid ~p~n", [PidCall])),
    RunningProcs = [{PidCall, logger:init_log_file(LogDir, PidCall)}],
    % io:format("PIDCALL: ~p\n", [PidCall]),
    TimeoutServer = Timeout,
    InstMod = 
        get(modules_to_instrument),
    PidTrace = 
        spawn(
            fun() ->
                put(modules_to_instrument, InstMod),
                put(lambda, 0),
                put(log_handler, LogHandler),
                receive_loop(
                    0, 
                    [],
                    [ModName],
                    PidMain, 
                    TimeoutServer, 
                    Dir,
                    LogDir, 
                    TracingNode,
                    RunningProcs)
            end),
    register(tracer, PidTrace),
    PidCall!start,
    InitTime = erlang:monotonic_time(),
    receive 
        all_done ->
            logger:append_data(io_lib:fwrite("tracing success~n", [])),
            receive
                {result,Result} ->
                    logger:append_data(io_lib:fwrite("result ~p~n", [Result]))
            end;
        idle ->
            logger:append_data(io_lib:fwrite("tracing timeout~n", [])),
            receive
                {result,Result} ->
                    logger:append_data(io_lib:fwrite("result ~p~n", [Result]))
                after
                    0 ->
                        logger:append_data(io_lib:fwrite("result none~n", []))
            end
    end,
    EndTime =  erlang:monotonic_time(),
    DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
    logger:append_data(io_lib:fwrite("exec ~p~n", [DiffTime])),
    unregister(tracer),
    PidTrace!stop,
    slave:stop(TracingNode),
    Trace = 
        receive 
            {trace,Trace0} ->
                lists:reverse(Trace0)
        end,
    % Loaded = % Commented to avoid warning
        receive 
            {loaded,Loaded0} ->
                Loaded0
        end,
    PidAnswer!{Trace}.

receive_loop(Current, Trace, Loaded, PidMain, Timeout, Dir, LogDir, TracingNode, RunningProcs) ->
    receive 
        TraceItem = {trace, _, _, _} ->
            NTraceItem =
                case TraceItem of 
                    {trace, send_sent, Pid, _} ->
                        Lambda = get_lambda(),
                        Pid ! {lambda, Lambda},
                        {Pid, send, Lambda};
                    {trace, made_spawn, Pid, {SpawnPid}} ->
                        {Pid, spawn, SpawnPid};
                    {trace,receive_evaluated, Pid, {Lambda}} ->
                        {Pid, 'receive', Lambda};
                    _ -> 
                        TraceItem
                end,
            NTrace =
                case TraceItem of
                    {trace, proc_done, _, _} ->
                        Trace;
                    _ ->
                        [NTraceItem | Trace]
                end,
            NRunningProcs =
                case TraceItem of
                    {trace, made_spawn, _, {SpPid}} ->
                      LogItem = {SpPid, logger:init_log_file(LogDir, SpPid)},
                      [LogItem | RunningProcs];
                    {trace, proc_done, PidDone, _} ->
                      LogHandler = proplists:get_value(PidDone, RunningProcs),
                      logger:append_pid_data(LogHandler, NTrace, PidDone),
                      logger:stop_log_file(LogHandler),
                      lists:delete({PidDone, LogHandler}, RunningProcs);
                    _ ->
                        RunningProcs
                end,
            case NRunningProcs of
                [] -> PidMain ! all_done;
                _ -> continue
            end,

            % io:format("~p~n", [NRunningProcs]),
            receive_loop(
                Current + 1, 
                NTrace,
                Loaded, PidMain, Timeout, Dir, LogDir, TracingNode, NRunningProcs);
        {load_module, Module, PidAnswer} ->
            % io:format("Load module " ++ atom_to_list(Module) ++ "\n"),
            NLoaded = 
                case lists:member(Module, Loaded) of 
                    true ->
                        PidAnswer!loaded,
                        Loaded;
                    false ->
                       instrument_and_reload(Module, Dir, TracingNode),
                       PidAnswer!loaded,
                       [Module | Loaded]
                end, 
            receive_loop(Current, Trace, NLoaded, PidMain, Timeout, Dir, LogDir, TracingNode, RunningProcs);
        stop ->
            PidMain!{trace, Trace},
            PidMain!{loaded, Loaded};
        Other -> 
            io:format("Untracked msg ~p\n", [Other]),
            receive_loop(Current, Trace, Loaded, PidMain, Timeout, Dir, LogDir, TracingNode, RunningProcs)
    after 
        Timeout ->
            IdlePids = [Pid || {Pid, _} <- RunningProcs],
            [
             begin
                LogHandler =
                    proplists:get_value(IdlePid, RunningProcs),
                    logger:append_pid_data(LogHandler, Trace, IdlePid),
                    logger:stop_log_file(LogHandler)
             end || IdlePid <- IdlePids],
            PidMain!idle,
            receive_loop(Current, Trace, Loaded, PidMain, Timeout, Dir, LogDir, TracingNode, RunningProcs)
    end.

send_module(TracingNode, Module, Dir) ->
    CompileOpts = 
        [binary, {i,Dir}, {outdir,Dir}, return],
    File = 
        get_file_path(Module, Dir),
    {ok, Module, Bin , _} = 
        compile:file(File, CompileOpts),
    {_ResL, _BadNodes} = 
        rpc:call(
            TracingNode, code, load_binary, [Module, File, Bin]),
    ok.


execute_call(Call, PidParent, _Dir, TracingNode) ->
    send_module(TracingNode, ?MODULE, filename:absname(filename:dirname(code:which(?MODULE)) ++ "/..") ++ "/src"),
    send_module(TracingNode, smerl, filename:absname(filename:dirname(code:which(?MODULE)) ++ "/..") ++ "/src"),
    MainNodeStr = atom_to_list(node()),
    FUN = 
        fun() -> 
            M1 = smerl:new(foo),
            {ok, M2} = 
                smerl:add_func(M1, "bar() -> try MainRes = " ++ Call ++
                                   ", {tracer, '" ++ MainNodeStr ++ "'} ! {trace, proc_done, self(), {}}," ++
                                   "MainRes catch E1:E2 -> {E1,E2} end."),
            smerl:compile(M2,[nowarn_format]),
            receive 
                start -> ok 
            end,
            Res = foo:bar(), 
            PidParent!{result,Res}
        end,
    spawn(TracingNode, FUN).

get_mod_name(InitialCall) ->
    AExpr = 
        case is_list(InitialCall) of 
            true ->
                hd(parse_expr(InitialCall++"."));
            false ->
                InitialCall
        end,
    {call,_,{remote,_,{atom,_,ModName},_},_} = AExpr,
    ModName.

get_file_path(ModName, Dir) ->
    case Dir of 
        none -> 
            atom_to_list(ModName) ++ ".erl";
        _ ->
            Dir ++ "/" ++ atom_to_list(ModName) ++ ".erl"
    end.

instrument_and_reload(ModName, Dir, TracingNode) ->
    CompileOpts = 
        [{parse_transform, trace_pt}, binary, {i,Dir}, {outdir,Dir}, return, {inst_mod, get(modules_to_instrument)}],
    Msg = 
        "Instrumenting...",
    instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode).

instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode) ->
    % [gen_server, gen_fsm, supervisor, proc_lib, gen]
    case lists:member(ModName, get(modules_to_instrument)) of 
        true -> 
            instrument_and_reload_sticky(ModName, Dir, CompileOpts, Msg, TracingNode);
        false -> 
            FilePath = get_file_path(ModName, Dir),
            io:format("~s~p~n", [Msg, FilePath]),
            % io:format("~p\n", [CompileOpts]),
            InitTime = erlang:monotonic_time(),
            {ok,ModName,Binary,_} = 
                case compile:file(FilePath, CompileOpts) of 
                    {ok,_,_,_} = Res ->
                        Res
                    %     ;
                    % Other ->
                    %     io:format("~p\n", [Other])
                    % _ ->
                    %     io:format("~p\n", [element(1, filename:find_src(ModName))]),
                    %     Res = compile:file(element(1, filename:find_src(ModName)) ++ ".erl", CompileOpts),
                    %     io:format("~p\n", [Res]),
                    %     Res 
                end,
                EndTime =  erlang:monotonic_time(),
                DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
                logger:append_data(io_lib:fwrite("inst ~p ~p~n", [FilePath, DiffTime])),
                % io:format("~p\n", [get_file_path(ModName, Dir)]),
                % io:format("~p\n", [filename:find_src(ModName)]),
                % io:format("~p\n", [ file:get_cwd()]),
                %  = 
                %     compile:file(get_file_path(ModName, Dir),),
            reload_module(ModName, Binary, TracingNode)
            % catch 
            %     _:_ -> ok 
            % end.
            ,ok
    end.

instrument_and_reload_sticky(ModName, _UserDir, CompileOpts, Msg, TracingNode) ->
    LibDir = 
        code:lib_dir(stdlib, src),
    BeamDir = 
        code:lib_dir(stdlib, ebin),
    FilePath = 
        get_file_path(ModName, LibDir),
    io:format("~s~p\n", [Msg, FilePath]),
    InitTime = erlang:monotonic_time(),
    {ok, ModName, Binary,_} = 
        case compile:file(FilePath, CompileOpts) of 
            {ok,_,_,_} = Res ->
                Res;
            Other ->
                io:format("~p\n", [Other])
        end,
    EndTime =  erlang:monotonic_time(),
    DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
    logger:append_data(io_lib:fwrite("inst ~p ~p~n", [FilePath, DiffTime])),
    % ok = 
    %     code:unstick_dir(BeamDir),
    %% TODO: Tracer gets stuck from here
    rpc:call(
        TracingNode, code, unstick_dir, [BeamDir]),
    reload_module(ModName, Binary, TracingNode),
    rpc:call(
        TracingNode, code, stick_dir, [BeamDir]).
    % ok = 
    %     code:stick_dir(BeamDir).

% undo_instrument_and_reload(ModName, Dir) ->
%     CompileOpts = 
%         [binary, {i,Dir}, {outdir,Dir}, return],
%     Msg = 
%         "Restoring...",
%     instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg).   
%     % case lists:member(ModName, [gen_server, gen_fsm, supervisor]) of 
%     %     true -> 
%     % {ok,ModName,Binary} = 
%     %     compile:file(get_file_path(ModName, Dir), [binary, {i,Dir}, {outdir,Dir}]),
%     % reload_module(ModName, Binary).

reload_module(ModName, Binary, TracingNode) ->
    try
        rpc:call(
            TracingNode, erlang, purge_module, [ModName])
    catch 
        _:_ -> ok
    end,
    rpc:call(
        TracingNode, code, load_binary, [ModName, atom_to_list(ModName) ++ ".erl", Binary]).
    % code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary).
    % code:load_abs(atom_to_list(ModName)).

parse_expr(Func) ->
    case erl_scan:string(Func) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, _Term} ->
                    _Term;
                _Err ->
                    {error, parse_error}
            end;
        _Err ->
            {error, parse_error}
    end.

get_lambda() ->
    Lambda = get(lambda),
    put(lambda, Lambda + 1),
    Lambda.
