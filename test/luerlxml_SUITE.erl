-module(luerlxml_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     t_dom_parser
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
    {ok, Dom} = luerlxml:dom_parser(?XML),
    ct:pal("~p~n", [Dom]),
    ok.
