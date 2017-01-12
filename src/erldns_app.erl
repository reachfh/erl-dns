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

%% @doc The erldns OTP application.
-module(erldns_app).
-behavior(application).

% Application hooks
-export([start/2, start_phase/3, stop/1]).

start(_Type, _Args) ->
  lager:debug("Starting erldns application"),
  setup_metrics(),
  erldns_sup:start_link().

start_phase(post_start, _StartType, _PhaseArgs) ->
  lager:debug("Post start phase for erldns application"),
  erldns_events:add_handler(erldns_event_handler),

  lager:debug("Loading custom zone parsers"),
  case application:get_env(erldns, custom_zone_parsers) of
    {ok, Parsers} -> erldns_zone_parser:register_parsers(Parsers);
    _ -> ok
  end,

  lager:debug("Loading custom zone encoders"),
  case application:get_env(erldns, custom_zone_encoders) of
    {ok, Encoders} -> erldns_zone_encoder:register_encoders(Encoders);
    _ -> ok
  end,

  lager:info("Loading zones from local file"),
  erldns_zone_loader:load_zones(),

  lager:info("Notifying servers to start"),
  erldns_events:notify(start_servers),

  ok.

stop(_State) ->
  lager:info("Stop erldns application"),
  ok.

setup_metrics() ->
  {ok, _Started} = application:ensure_all_started(metrics2),

  metrics2:new(counter, [udp_request_counter]),
  metrics2:new(counter, [tcp_request_counter]),
  metrics2:new(meter, [udp_request_meter]),
  metrics2:new(meter, [tcp_request_meter]),

  metrics2:new(meter, [udp_error_meter]),
  metrics2:new(meter, [tcp_error_meter]),

  % The metrics module doesn't suport history, so use native folsom
  folsom_metrics:new_history(udp_error_history),
  folsom_metrics:new_history(tcp_error_history),

  metrics2:new(meter, [refused_response_meter]),
  metrics2:new(counter, [refused_response_counter]),

  metrics2:new(meter, [empty_response_meter]),
  metrics2:new(counter, [empty_response_counter]),

  metrics2:new(histogram, [udp_handoff_histogram]),
  metrics2:new(histogram, [tcp_handoff_histogram]),

  metrics2:new(counter, [request_throttled_counter]),
  metrics2:new(meter, [request_throttled_meter]),
  metrics2:new(histogram, [request_handled_histogram]),

  metrics2:new(counter, [packet_dropped_empty_queue_counter]),
  metrics2:new(meter, [packet_dropped_empty_queue_meter]),

  metrics2:new(meter, [cache_hit_meter]),
  metrics2:new(meter, [cache_expired_meter]),
  metrics2:new(meter, [cache_miss_meter]),

  metrics2:new(counter, [dnssec_request_counter]),
  metrics2:new(meter, [dnssec_request_meter]).
