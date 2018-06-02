-module(my_sup).

-behavior(supervisor).
-behavior(gen_fw).

%% API
-export([start_link/1, start_link/0]).

%% 'supervisor' callback:
-export([init/1]).

%% 'gen_fw' callback:
-export([fw_handle_message/2]).


start_link(Rules) ->
    gen_fw:start_link({?MODULE, start_link, []}, Rules).


start_link() ->
    supervisor:start_link(?MODULE, undefined).


init(_) ->
    {ok, {{one_for_one, 1, 3}, []}}.


%% See behaviour info in src/gen_fw.erl for more info
fw_handle_message(call, {{Pid, Ref}, Req}) ->
    case Req of
        count_children ->
            allow;
        _ ->
            Pid ! {Ref, {error, permission_denied}},
            deny
    end;
fw_handle_message(_, _) ->
    deny.