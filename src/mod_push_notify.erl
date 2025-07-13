-module(mod_push_notify).
-behaviour(gen_mod).

%% Required exports for gen_mod behaviour
-export([start/2, stop/1]).
-export([depends/2, mod_doc/0, mod_options/1, mod_opt_type/1]).

%% Hook callback
-export([offline_message_hook/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%%% ------------------------------------------------------------------
%%% Module init/stop
%%% ------------------------------------------------------------------

start(Host, _Opts) ->
    ?INFO_MSG("Starting push notify module for ~p", [Host]),
    %% Ensure httpc is started
    inets:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message_hook, 50),
    ok.

%%% ------------------------------------------------------------------
%%% Required gen_mod callbacks
%%% ------------------------------------------------------------------

depends(_Host, _Opts) ->
    [].

mod_doc() ->
    #{desc => "Push notification module for offline messages"}.

mod_options(_Host) ->
    [
        {url, <<"">>},
        {token, <<"">>}
    ].

%% Fixed mod_opt_type - return validation functions instead of atoms
mod_opt_type(url) -> 
    fun(V) when is_binary(V) -> V;
       (V) when is_list(V) -> list_to_binary(V);
       (V) -> erlang:error({invalid_option_value, url, V})
    end;
mod_opt_type(token) -> 
    fun(V) when is_binary(V) -> V;
       (V) when is_list(V) -> list_to_binary(V);
       (V) -> erlang:error({invalid_option_value, token, V})
    end;
mod_opt_type(_) -> 
    undefined.

%%% ------------------------------------------------------------------
%%% Hook implementation
%%% ------------------------------------------------------------------

offline_message_hook({offlined, Packet}) ->
    #message{
        type = MessageType,
        to = To,
        from = From,
        body = Body
    } = Packet,

    %% Extract host from the 'to' JID
    Host = jid:nameprep(To#jid.server),

    %% Extract body text
    BodyText = case Body of
        [] -> <<>>;
        [#text{data = Text}] -> Text;
        _ -> <<>>
    end,

    %% Prepare payload map or record
    PayloadToPass = #{
        to => jid:to_string(To),
        from => jid:to_string(From),
        body => BodyText,
        type => MessageType
    },

    case MessageType of
        chat ->
            ?INFO_MSG("Sending push notification for offline chat message to ~p. Body: ~p", [jid:to_string(To), BodyText]),
            call_test_api(PayloadToPass, Host);
        _ ->
            ok
    end,
    {offlined, Packet}.

%%% ------------------------------------------------------------------
%%% Utility
%%% ------------------------------------------------------------------
 

call_test_api(PayloadPassed,Host) ->
    EnvUrl = os:getenv("EJABBERD_URL"),
    EnvToken = os:getenv("PUSH_NOTIFY_TOKEN"),
      %% Read URL and token from ejabberd config
    URL = if EnvUrl =/= false -> list_to_binary(EnvUrl);
             true -> ensure_binary(gen_mod:get_module_opt(Host, ?MODULE, url))
          end,
    Token = if EnvToken =/= false -> list_to_binary(EnvToken);
               true -> ensure_binary(gen_mod:get_module_opt(Host, ?MODULE, token))
            end,

    %% Extract and sanitize values
    ToRaw = maps:get(to, PayloadPassed, <<"">>),
    FromRaw = maps:get(from, PayloadPassed, <<"">>),

    %% Strip anything after '@'
    ToClean = strip_domain(ToRaw),
    FromClean = strip_domain(FromRaw),

    %% Ensure binary types
    ToBin = ensure_binary(ToClean),
    FromBin = ensure_binary(FromClean),
    BodyBin = ensure_binary(maps:get(body, PayloadPassed, <<"">>)),
    TypeBin = ensure_binary(maps:get(type, PayloadPassed, <<"">>)),

    %% Headers
    Headers = [
        {"Content-Type", "application/json"},
        {"accept", "*/*"},
        {"authorization", << "Basic ", Token/binary >>}
    ],

    %% JSON body using jsx
    Payload = jsx:encode([
        {to, ToBin},
        {from, FromBin},
        {body, BodyBin},
        {type, TypeBin}
    ]),

    ?INFO_MSG("Making HTTP request to: ~p", [URL]),
    ?INFO_MSG("Request payload: ~p", [Payload]),

    case httpc:request(post, {URL, Headers, "application/json", Payload}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _RespHeaders, Body}} ->
            ?INFO_MSG("✅ HTTP request successful. Response: ~p", [Body]);
        {ok, {{_Version, StatusCode, ReasonPhrase}, _RespHeaders, Body}} ->
            ?INFO_MSG("❌ HTTP request failed with status ~p: ~p. Response: ~p", [StatusCode, ReasonPhrase, Body]);
        {error, Reason} ->
            ?INFO_MSG("❌ HTTP request error: ~p", [Reason])
    end.

%% Utility to ensure binary
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
ensure_binary(Value) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Value]))).

%% Utility to strip @domain part from JIDs like user@host.com
strip_domain(Value) when is_binary(Value) ->
    list_to_binary(strip_domain(binary_to_list(Value)));
strip_domain(Value) when is_list(Value) ->
    case string:tokens(Value, "@") of
        [UserId | _Rest] -> UserId;
        [] -> Value
    end;
strip_domain(Value) ->
    Value.