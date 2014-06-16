%%%-------------------------------------------------------------------
%%% @author Łukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(luerlxml).
-author('Łukasz Biedrycki <lukasz.biedrycki@gmail.com>').

-export([start/0, stop/0]).

-export([init_lua_state/0]).

-export([dom_parser/1,
         simple_tree_parser/1]).

-define(LUA_STATE_ETS, lua_state_ets).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    reltool_util:application_start(?MODULE).

stop() ->
    reltool_util:application_stop(?MODULE).

dom_parser(Xml) ->
    {ok, LuaState0} = get_lua_state(),
    call_lua_function(LuaState0, <<"dom_parser">>, [Xml]).

simple_tree_parser(Xml) ->
    {ok, LuaState0} = get_lua_state(),
    call_lua_function(LuaState0, <<"simple_tree_parser">>, [Xml]).

%% ===================================================================
%% Lua State
%% ===================================================================
init_lua_state() ->
    AppPrivPath = code:priv_dir(luerlxml),
    PackagePath = AppPrivPath ++ "/lua/src/?.lua",
    LuaState0 = luerl:init(),
    {[], LuaState1} = luerl:do("package.path = \"" ++ PackagePath ++ "\"\n"
                               "require \"main\"", LuaState0),
    ?LUA_STATE_ETS = ets:new(?LUA_STATE_ETS, [public, set, named_table,
                                              {read_concurrency, true}]),
    true = ets:insert(?LUA_STATE_ETS, {lua_state, LuaState1}),
    ok.

get_lua_state() ->
    [{lua_state, LuaState}] = ets:lookup(?LUA_STATE_ETS, lua_state),
    {ok, LuaState}.

call_lua_function(LuaState0, Function, Args) ->
    {_Res, LuaState1} = luerl:call_function1([Function], Args,
                                             LuaState0),
    {ReturnVal, _LuaState2} = luerl:get_table([<<"return_val">>], LuaState1),

    Status = proplists:get_value(<<"status">>, ReturnVal),
    case Status of
        <<"ok">> ->
            Response = proplists:get_value(<<"response">>, ReturnVal),
            {ok, Response};
        <<"warning">> ->
            Response = proplists:get_value(<<"response">>, ReturnVal),
            Warnings1 = proplists:get_value(<<"warnings">>, ReturnVal),
            Warnings2 = lua_to_erlang_list(Warnings1),
            {ok, {Response, Warnings2}};
        <<"error">> ->
            Errors1 = proplists:get_value(<<"errors">>, ReturnVal),
            Errors2 = float_values_to_int(Errors1),
            Errors3 = lua_to_erlang_list(Errors2),
            {error, Errors3}
    end.

%% ===================================================================
%% Internals
%% ===================================================================
float_values_to_int(Json) ->
    float_values_to_int(Json, []).

float_values_to_int([], Acc) ->
    lists:reverse(Acc);
float_values_to_int([{Key, Value} | Tail], Acc) when is_list(Value) ->
    float_values_to_int(Tail, [{Key, float_values_to_int(Value, [])} | Acc]);
float_values_to_int([{Key, Value} | Tail], Acc) when is_float(Value) ->
    case Value - trunc(Value) of
        0.0 ->
            float_values_to_int(Tail, [{Key, trunc(Value)} | Acc]);
        _ ->
            float_values_to_int(Tail, [{Key, Value} | Acc])
    end;
float_values_to_int([H | T], Acc) ->
    float_values_to_int(T, [H | Acc]).

lua_to_erlang_list(Json) ->
    lua_to_erlang_list(Json, []).

lua_to_erlang_list([], Acc) ->
    lists:reverse(Acc);
lua_to_erlang_list([{K, V} | T], Acc) when is_binary(K), is_list(V) ->
    lua_to_erlang_list(T, [{K, lua_to_erlang_list(V, [])} | Acc]);
lua_to_erlang_list([{K, V} | T], Acc) when is_binary(K) ->
    lua_to_erlang_list(T, [{K, V} | Acc]);
lua_to_erlang_list([{K, V} | T], Acc) when is_integer(K) ->
    lua_to_erlang_list(T, [lua_to_erlang_list(V, []) | Acc]).
