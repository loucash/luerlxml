-module(luerlxml_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     t_dom_parser,
     t_simple_tree_parser
    ].

-define(XML, <<"<a>1</a>">>).

%% ===================================================================
%% setup and teardown
%% ===================================================================
init_per_suite(Config) ->
    ok = luerlxml:start(),
    Config.

end_per_suite(_Config) ->
    luerlxml:stop(),
    ok.

%% ===================================================================
%% tests
%% ===================================================================
t_dom_parser(_Config) ->
    {ok, [{<<"_children">>,
           [[{<<"_children">>,[[{<<"_text">>,<<"1">>},{<<"_type">>,<<"TEXT">>}]]},
             {<<"_name">>,<<"a">>},
             {<<"_type">>,<<"ELEMENT">>}]]},
          {<<"_type">>,<<"ROOT">>}]} = luerlxml:dom_parser(?XML),
    ok.

t_simple_tree_parser(_Config) ->
    {ok, [{<<"a">>,<<"1">>}]} = luerlxml:simple_tree_parser(?XML),
    ok.
