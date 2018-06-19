-module(cauder_logger).

-export([init_log_dir/1, init_log_file/2, stop_log_file/1,
         append_data/2, append_pid_data/3]).

init_log_dir(Dir) ->
  file:make_dir(Dir),
  FileName = Dir ++ "/trace_result.log",
  case file:open(FileName, write) of
    {ok, FileHandler} ->
      FileHandler;
    {error, R} ->
      io:format("Error: Couldn't init log dir or result file: ~p.~n", [R]),
      error
  end.

init_log_file(Dir, Pid) ->
  FileName = get_filename(Dir, Pid),
  case file:open(FileName, write) of
    {ok, FileHandler} ->
      FileHandler;
    {error, R} ->
      io:format("Error: Couldn't init log file for pid ~p (reason ~p).~n", [Pid, R]),
      error
  end.

append_data(FileHandler, Data) ->
  file:write(FileHandler, Data).

append_pid_data(FileHandler, Items, Pid) ->
  PidItems = [ Item || {P, _, _} = Item <- Items, P == Pid ],
  RPidItems = lists:reverse(PidItems),
  append_items(FileHandler, RPidItems).

append_items(FileHandler, Items) ->
  [file:write(FileHandler, io_lib:fwrite("~p~n", [Item])) || Item <- Items].

stop_log_file(FileHandler) ->
  file:close(FileHandler).

get_filename(Dir, Pid) ->
  PidStr = pid_to_list(Pid),
  FullName = Dir ++ "/trace_" ++ PidStr ++ ".log",
  io:format("~s~n", [FullName]),
  FullName.