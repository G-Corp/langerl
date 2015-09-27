-module(langerl2).
-export([start/2, stop/1, init/2]).
-export([foo/2, bar/2]).

start(Node, Interpreter) ->
  spawn(?MODULE, init, [Node, Interpreter]).
stop(Node) ->
  Node ! stop.

foo(Node, X) ->
  call_port(Node, {foo, X}).
bar(Node, Y) ->
  call_port(Node, {bar, Y}).

call_port(Node, Msg) ->
  Node ! {call, self(), Msg},
  receive
    {Node, Result} ->
      Result
  end.

init(Node, Interpreter) ->
  register(Node, self()),
  process_flag(trap_exit, true),
  Path = case code:priv_dir(langerl) of
    {error, bad_name} -> "./priv";
    Folder -> Folder
  end,
  Result = case os:find_executable("node." ++ atom_to_list(Interpreter), Path) of
    false ->
      {stop, interpreter_not_found};
    InterpreterCmd ->
      {ok, InterpreterCmd}
  end,
  case Result of
    {ok, Cmd} ->
      Port = open_port({spawn, Cmd}, [{packet, 2}, binary]),
      loop(Port, Node);
    _ ->
      Result
  end.

loop(Port, Node) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {Node, binary_to_term(Data)}
      end,
      loop(Port, Node);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit(port_terminated)
  end.







