%% Copyright (c) 2012-2015, Aetrion LLC
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Application event handler implementation.
-module(erldns_event_handler).

-behavior(gen_event).

-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

-record(state, {servers_running = false, metrics_engine = undefined}).

init(_Args) ->
  Engine = metrics:init(erldns_metrics:mod_metrics()),
  {ok, #state{metrics_engine = Engine}}.

handle_event(start_servers, State) ->
  case State#state.servers_running of
    false ->
      % Start up the UDP and TCP servers
      lager:info("Starting the UDP and TCP supervisor"),
      erldns_server_sup:start_link(),
      erldns_events:notify(servers_started),
      {ok, State#state{servers_running = true}};
    _ ->
      erldns_events:notify(servers_already_started),
      {ok, State}
  end;

handle_event({end_udp, [{host, _Host}]}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, udp_request_meter, 1),
  metrics:increment_counter(Engine, udp_request_counter),
  {ok, State};

handle_event({end_tcp, [{host, _Host}]}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, tcp_request_meter, 1),
  metrics:increment_counter(Engine, tcp_request_counter),
  {ok, State};

handle_event({udp_error, Reason}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, udp_error_meter, 1),
  folsom_metrics:notify({udp_error_history, Reason}),
  {ok, State};

handle_event({tcp_error, Reason}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, tcp_error_meter, 1),
  folsom_metrics:notify({tcp_error_history, Reason}),
  {ok, State};

handle_event({refused_response, Questions}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, refused_response_meter, 1),
  metrics:increment_counter(Engine, refused_response_counter),
  lager:debug("Refused response: ~p", [Questions]),
  {ok, State};

handle_event({empty_response, Message}, State) ->
  Engine = State#state.metrics_engine,
  metrics:update_meter(Engine, empty_response_meter, 1),
  metrics:increment_counter(Engine, empty_response_counter),
  lager:info("Empty response: ~p", [Message]),
  {ok, State};

handle_event({dnssec_request, _Host, _Qname}, State) ->
  Engine = State#state.metrics_engine,
  metrics:increment_counter(Engine, dnssec_request_counter),
  metrics:update_meter(Engine, dnssec_request_meter, 1),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Message, State) ->
  {ok, ok, State}.

handle_info(_Message, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
