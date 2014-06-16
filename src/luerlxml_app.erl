-module(luerlxml_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ============================================================================
%% Application callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    luerlxml:init_lua_state(),
    luerlxml_sup:start_link().


stop(_State) ->
    ok.
