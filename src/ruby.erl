-module(ruby).
-export([start/0, stop/0, init/1]).
-export([foo/1]).

-define(NODE, "node.rb").
-define(REGISTER, "node_rb").

start() ->
  spawn(?MODULE, init, [?NODE]).
stop() ->
  ?REGISTER ! stop.

foo(x) ->
  call_port({foo, 1}).

call_port(Msg) ->
  ?REGISTER ! {call, self(), Msg},
  receive
    {?REGISTER, Result} ->
      Result
  end.

init(ExtPrg) ->
  register(?REGISTER, self()),
  process_flag(trap_exit, true),
  Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
  loop(Port).

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {?REGISTER, binary_to_term(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', _Port, _Reason} ->
      exit(port_terminated)
  end.
