-module(langerl).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1, start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([call/3, call/4, load/2, execute/2, test/2]).

-record(state, {
          node,
          port,
          mbox,
          from,
          interpreter
         }).

start_link(Interpreter) ->
  start_link(Interpreter, Interpreter).
start_link(Node, Interpreter) ->
  gen_server:start_link({local, Node}, ?MODULE, [Node, Interpreter], []).

stop(Node) ->
	gen_server:call(Node, stop, infinity).

call(Node, Module, Fun, Args) ->
	gen_server:call(Node, {call, Module, Fun, Args}, infinity).

call(Node, Fun, Args) ->
	gen_server:call(Node, {call, Fun, Args}, infinity).

load(Node, File) ->
	gen_server:call(Node, {load, File}, infinity).

execute(Node, Code) ->
	gen_server:call(Node, {exec, Code}, infinity).

test(Node, Value) ->
  gen_server:call(Node, {test, Value}, infinity).

init([Node, Interpreter]) ->
  process_flag(trap_exit, true),
	{CleanId, Host, NodeName} = mk_node_name(Node),
  Path = case code:priv_dir(langerl) of
    {error, bad_name} -> "./priv";
    Folder -> Folder
  end,
  Result = case os:find_executable("node." ++ atom_to_list(Interpreter), Path) of
    false ->
      {stop, interpreter_not_found};
    Cmd ->
			{ok, mk_cmdline(Cmd, CleanId, Host)}
  end,
  case Result of
    {stop, Reason} ->
      {stop, Reason};
    {ok, InterpreterCmd} ->
			Port = open_port({spawn, InterpreterCmd}, [stream, {line, 100}, stderr_to_stdout, exit_status]),
			wait_for_startup(#state{interpreter = Interpreter, node = Node, port = Port, mbox = {interpreter, NodeName}})
  end.

handle_call(stop, _From, #state{mbox = Mbox, from = undefined} = State) ->
  Mbox ! {stop, self()},
	{stop, normal, ok, State};
handle_call({test, Value}, From, #state{mbox = Mbox, from = undefined} = State) ->
	Mbox ! {test, self(), Value},
	{noreply, State#state{from=From}};
handle_call({call, Module, Fun, Args}, From, #state{mbox = Mbox, from = undefined} = State) ->
	Mbox ! {call, self(), Module, Fun, Args},
	{noreply, State#state{from=From}};
handle_call({call, Fun, Args}, From, #state{mbox = Mbox, from = undefined} = State) ->
	Mbox ! {call, self(), Fun, Args},
	{noreply, State#state{from=From}};
handle_call({load, File}, From, #state{mbox = Mbox, from = undefined} = State) ->
	Mbox ! {load, self(), File},
	{noreply, State#state{from=From}};
handle_call({exec, Code}, From, #state{mbox = Mbox, from = undefined} = State) ->
	Mbox ! {exec, self(), Code},
	{noreply, State#state{from=From}};
handle_call(_Request, _From, #state{from = Id} = State) when Id =/= undefined ->
	{reply, {error, busy}, State};
handle_call(_Request, _From, State) ->
  {reply, {error, wrong_request}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({test, Result}, #state{interpreter = Int, from = From} = State) when From =/= undefined ->
	gen_server:reply(From, {Int, {ok, Result}}),
	{noreply, State#state{from=undefined}};
handle_info({load, Result}, #state{interpreter = Int, from = From} = State) when From =/= undefined ->
	gen_server:reply(From, {Int, {ok, Result}}),
	{noreply, State#state{from=undefined}};
handle_info({exec, Result}, #state{interpreter = Int, from = From} = State) when From =/= undefined ->
	gen_server:reply(From, {Int, Result}),
	{noreply, State#state{from=undefined}};
handle_info({error, _} = Result, #state{interpreter = Int, from = From} = State) when From =/= undefined ->
	gen_server:reply(From, {Int, Result}),
	{noreply, State#state{from = undefined}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

mk_node_name(Id) ->
	This_Id = re:replace(atom_to_list(Id), "[^_0-9a-zA-Z]+", "_", [global, {return, list}]),
	This_Host = string:sub_word(atom_to_list(node()), 2, $@),
	{This_Id, This_Host, list_to_atom(lists:flatten([This_Id, "@", This_Host]))}.

mk_cmdline(Cmd, Id, Host) ->
	lists:flatten([
		Cmd,
		quote(Id),
		quote(Host),
		quote(atom_to_list(node())),
		quote(atom_to_list(erlang:get_cookie()))
	]).

quote(S) ->
	case ostype() of
		win32 -> [" \"", S, "\""];
		unix -> [" '", S, "'"]
	end.

ostype() ->
	case os:type() of
		{Type, _} -> Type;
		Type -> Type
	end.

wait_for_startup(#state{port=Port} = State) ->
	receive
		{Port, {exit_status, N}} ->
			{stop, {exit_status, N}};
		{Port, {data, {eol, "READY"}}} ->
			{ok, State};
		{Port, {data, {eol, "."}}} ->
			wait_for_startup(State);
		{Port, {data, {eol, _}}} ->
			wait_for_startup(State)
	end.
