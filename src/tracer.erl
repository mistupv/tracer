-module(tracer).

-export([inst/1, inst/2, stop/0, trace/1, trace/2]).


inst(ModName) ->
    inst(ModName, []).

% invocation of
% tracer:inst(tcp,[{dir,"examples"}]).
% generates the instrumented source of examples/tcp.erl in the current directory

inst(ModName, Opts) ->
    NOpts =
    case proplists:is_defined(dir, Opts) of
        true -> Opts;
        false -> [{dir, "."} | Opts]
    end,
    inst_1(ModName, NOpts).

inst_1(ModName, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    CompileOpts =
        [{parse_transform, trace_pt}, binary, {i,Dir}, {outdir,Dir}, return],
    FilePath = get_file_path(ModName, Dir),
    io:format("~p~n", [FilePath]),
%compiles using trace_pt as transformer
    compile:file(FilePath, CompileOpts).


trace(InitialCall) -> 
      trace(InitialCall, []).

trace(InitialCall, Opts) ->
      NOpts0 =
          case proplists:is_defined(timeout, Opts) of
              true -> Opts;
              false -> [{timeout, 10000} | Opts]
          end,
      NOpts3 =
          case proplists:is_defined(log_dir, NOpts0) of
              true -> NOpts0;
             false -> [{log_dir, "trace"} | NOpts0]
          end,
      trace_1(InitialCall, NOpts3).

trace_1(InitialCall, Opts) ->
    Timeout = proplists:get_value(timeout, Opts),
    LogDir  = proplists:get_value(log_dir, Opts),
    LogHandler = logger:init_log_dir(LogDir),
    put(log_handler, LogHandler),
    logger:append_data(io_lib:fwrite("call ~p~n", [InitialCall])),
    PidMain = self(),
    PidCall = execute_call(InitialCall, PidMain),
    SPidCall = logger:slpid(PidCall),
    logger:append_data(io_lib:fwrite("main_pid ~p~n", [SPidCall])),
    RunningProcs = [{PidCall, logger:init_log_file(LogDir, PidCall)}], 
    PidTrace = 
        spawn(
            fun() ->
                 put(stamp, 0),  % initialize unique identifiers
                receive_loop(
                    0,
                    [],
                    self(),
		  RunningProcs, %added
		  LogDir)
            end),
    register(tracer, PidTrace),
    PidCall!start,
                receive
                  {result,Result} ->
                      logger:append_data(io_lib:fwrite("result ~p~n", [Result]))

     after
          Timeout ->
              PidTrace ! idle,
              logger:append_data(io_lib:fwrite("tracing timeout~n", [])),
              receive
                  {result,Result} ->
                      logger:append_data(io_lib:fwrite("result ~p~n", [Result]))
                  after
                      0 ->
                          logger:append_data(io_lib:fwrite("result none~n", []))
              end
     end,
    
    PidTrace!stop.

stop() ->
    tracer ! stop,
    unregister(tracer).

receive_loop(Current, Trace, PidMain, RunningProcs, LogDir) ->
    receive 
        TraceItem = {trace, _, _, _} ->
            NTraceItem =
                case TraceItem of 
                    {trace, send_sent, Pid, _} ->
                        Stamp = get_stamp(),
                        Pid ! {stamp, Stamp},
                        SPid = logger:slpid(Pid),
                        {SPid, send, Stamp};
                    {trace, made_spawn, Pid, {SpawnPid}} ->
                        SPid = logger:slpid(Pid),
                        SSPid = logger:slpid(SpawnPid),
                        {SPid, spawn, SSPid};
                    {trace,receive_evaluated, Pid, {Stamp}} ->
                        SPid = logger:slpid(Pid),
                        {SPid, 'receive', Stamp};
                    _ -> 
                        Pid = 0, TraceItem
                end,
        NTrace = [NTraceItem|Trace],
        %io:format("~p~n", [NTraceItem]),
        NRunningProcs =
		case TraceItem of
		    {trace, made_spawn, _, {SpPid}} ->
			LogItem = {SpPid, logger:init_log_file(LogDir, SpPid)},
			[LogItem | RunningProcs];
		    _ ->
			RunningProcs
		end,
	    LogHandler = proplists:get_value(Pid, RunningProcs, not_found),
	    case LogHandler of
		not_found ->
		    RunningProcs;
		_ ->
		    logger:append_pid_data(LogHandler, [NTraceItem], Pid)
	    end,
            receive_loop(
                Current + 1,
                NTrace,
                PidMain,
	      NRunningProcs,LogDir);
        stop ->
            PidMain!{trace, Trace};
        Other -> 
            io:format("Untracked msg ~p\n", [Other]),
            receive_loop(Current, Trace, PidMain, RunningProcs, LogDir) %RunningProcs should be NRunningProcs
    end.

execute_call(Call, PidParent) ->
    FUN = 
        fun() -> 
            M1 = smerl:new(foo),
            {ok, M2} = 
                %smerl:add_func(M1, "bar() -> try " ++ Call ++ ". catch E1:E2 -> {E1,E2} end."),
		smerl:add_func(M1, "bar() ->" ++ Call ++ "."), 
            smerl:compile(M2,[nowarn_format]),
            receive 
                start -> ok 
            end,
            Res = foo:bar(), 
            PidParent!{result,Res}
        end,
    spawn(FUN).

get_file_path(ModName, Dir) ->
    case Dir of 
        none -> 
            atom_to_list(ModName) ++ ".erl";
        _ ->
            Dir ++ "/" ++ atom_to_list(ModName) ++ ".erl"
    end.

get_stamp() ->
    Stamp = get(stamp),
    put(stamp, Stamp + 1),
    Stamp.
