-module(wrapper).

-export([trace/2, trace/3]).

trace(InitialCall, PidAnswer) ->
    trace(InitialCall, PidAnswer, []).

trace(InitialCall, PidAnswer, Opts) ->
    NOpts0 =
        case proplists:is_defined(dir, Opts) of
            true -> Opts;
            false -> [{dir, "."} | Opts]
        end,
    NOpts1 =
        case proplists:is_defined(log_dir, NOpts0) of
            true -> NOpts0;
            false -> [{log_dir, "trace"} | NOpts0]
        end,
    trace_1(InitialCall, PidAnswer, NOpts1).

trace_1(InitialCall, PidAnswer, Opts) ->
    ModName = get_mod_name(InitialCall),
    {ok, TracingNode} = 
        slave:start(
            list_to_atom(net_adm:localhost()), 
            tracing, 
            "-setcookie cookie"),
    Dir     = proplists:get_value(dir,     Opts),
    LogDir  = proplists:get_value(log_dir, Opts),
    LogHandler = logger:init_log_dir(LogDir),
    put(log_handler, LogHandler),
    logger:append_data(io_lib:fwrite("call ~p~n", [InitialCall])),
    % io:format("~p\n", [SO]),
    % io:format("~p\n~p\n", [ModName, Dir]),
    % OriginalLibCode = 
    %     [code:get_object_code(Mod) || Mod <- [gen_server, supervisor, gen_fsm, proc_lib, gen]],
    instrument_and_reload(ModName, Dir, TracingNode),
    PidCall = execute_call(InitialCall, self(), Dir, TracingNode),
    logger:append_data(io_lib:fwrite("main_pid ~p~n", [PidCall])),
    PidCall!start,
    InitTime = erlang:monotonic_time(),
    receive 
        {result,Result} ->
        logger:append_data(io_lib:fwrite("result ~p~n", [Result]))

    end,
    EndTime =  erlang:monotonic_time(),
    DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
    logger:append_data(io_lib:fwrite("exec ~p~n", [DiffTime])),
    slave:stop(TracingNode),
    PidAnswer!{[]}.

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
    FUN = 
        fun() -> 
            M1 = smerl:new(foo),
            {ok, M2} = 
                smerl:add_func(M1, "bar() -> try " ++ Call ++ "catch E1:E2 -> {E1,E2} end."),
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
        [binary, {i,Dir}, {outdir,Dir}, return],
    Msg = 
        "Compiling...",
    instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode).

instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode) ->
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
        logger:append_data(io_lib:fwrite("comp ~p ~p~n", [FilePath, DiffTime])),
        % io:format("~p\n", [get_file_path(ModName, Dir)]),
        % io:format("~p\n", [filename:find_src(ModName)]),
        % io:format("~p\n", [ file:get_cwd()]),
        %  = 
        %     compile:file(get_file_path(ModName, Dir),),
    reload_module(ModName, Binary, TracingNode),
    ok.

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
