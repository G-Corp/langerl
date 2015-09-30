-module(langerl_ruby_tests).

-include_lib("eunit/include/eunit.hrl").

langerl_ruby_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_test_erlang_module())
      , ?_test(t_test_loaded_module())
      , ?_test(t_test_undef())
   ]}.

setup() ->
  langerl:start_link(x, ruby).

teardown(_) ->
  langerl:stop(x).

t_test_erlang_module() ->
  ?assertMatch({ruby,{ok,"hello"}}, langerl:call(x, <<"Erlang">>, <<"test">>, ["hello"])),
  ?assertMatch({ruby,{ok,<<"hello">>}}, langerl:call(x, <<"Erlang">>, <<"test">>, [<<"hello">>])),
  ?assertMatch({ruby,{ok,hello}}, langerl:call(x, <<"Erlang">>, <<"test">>, [hello])),
  ?assertMatch({ruby,{ok,true}}, langerl:call(x, <<"Erlang">>, <<"test">>, [true])),
  ?assertMatch({ruby,{ok,false}}, langerl:call(x, <<"Erlang">>, <<"test">>, [false])),
  ?assertMatch({ruby,{ok,[1,2]}}, langerl:call(x, <<"Erlang">>, <<"test">>, [[1,2]])),
  ?assertMatch({ruby,{ok,[1,<<"deux">>,trois]}}, langerl:call(x, <<"Erlang">>, <<"test">>, [[1, <<"deux">>, trois]])),
  ?assertMatch({ruby,{ok,{1,<<"deux">>,trois}}}, langerl:call(x, <<"Erlang">>, <<"test">>, [{1, <<"deux">>, trois}])),
  ?assertMatch({ruby,{ok,#{one := un, 2 := 2, <<"three">> := <<"trois">>, "quatre" := "four"}}}, langerl:call(x, <<"Erlang">>, <<"test">>, [#{one => un, 2 => 2, <<"three">> => <<"trois">>, "quatre" => "four"}])).

t_test_loaded_module() ->
   ?assertMatch({ruby,{ok,_}}, langerl:load(x, <<"./test/ruby/langerl_ruby_test.rb">>)),
 
   ?assertMatch({ruby,{ok,_}}, langerl:call(x, <<"LangerlRubyTest">>, <<"generate">>, [])),
   ?assertMatch({ruby,{ok,49}}, langerl:call(x, <<"LangerlRubyTest">>, <<"calc">>, [7])),
 
   ?assertMatch({ruby,{ok,"hello"}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, ["hello"])),
   ?assertMatch({ruby,{ok,<<"hello">>}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [<<"hello">>])),
   ?assertMatch({ruby,{ok,hello}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [hello])),
   ?assertMatch({ruby,{ok,true}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [true])),
   ?assertMatch({ruby,{ok,false}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [false])),
   ?assertMatch({ruby,{ok,[1,2]}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [[1,2]])),
   ?assertMatch({ruby,{ok,[1,<<"deux">>,trois]}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [[1, <<"deux">>, trois]])),
   ?assertMatch({ruby,{ok,#{one := un, 2 := 2, <<"three">> := <<"trois">>, "quatre" := "four"}}}, langerl:call(x, <<"LangerlRubyTest">>, <<"re">>, [#{one => un, 2 => 2, <<"three">> => <<"trois">>, "quatre" => "four"}])),
 
   ?assertMatch({ruby,{ok,[un,2,<<"trois">>]}}, langerl:call(x, <<"LangerlRubyTest">>, <<"array">>, [])),
   ?assertMatch({ruby,{ok,{un,2,<<"trois">>}}}, langerl:call(x, <<"LangerlRubyTest">>, <<"tuple">>, [])).

t_test_undef() ->
   ?assertMatch({ruby,{error,undefined_function}}, langerl:call(x, <<"UndefinedModule">>, <<"this_function_does_not_exist">>, [])),
   ?assertMatch({ruby,{error,undefined_function}}, langerl:call(x, <<"LangerlRubyTest">>, <<"this_function_does_not_exist">>, [])).

