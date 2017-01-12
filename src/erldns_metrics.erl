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

%% Functions related to metrics
-module(erldns_metrics).

-export([mod_metrics/0]).
-export([init/0, init/1]).

-spec mod_metrics() -> atom().
mod_metrics() ->
  case application:get_env(erldns, mod_metrics) of
    {ok, folsom} -> metrics_folsom;
    {ok, exometer} -> metrics_exometer;
    {ok, dummy} -> metrics_dummy;
    {ok, Mod} -> Mod;
    _ -> metrics_dummy
  end.

-spec init() -> metrics:metrics_engine().
init() ->
    init(mod_metrics()).

-spec init(atom()) -> metrics:metrics_engine().
init(EngineName) ->
  Engine = metrics:init(EngineName),

  metrics:new(Engine, counter, udp_request_counter),
  metrics:new(Engine, counter, tcp_request_counter),
  metrics:new(Engine, meter, udp_request_meter),
  metrics:new(Engine, meter, tcp_request_meter),

  metrics:new(Engine, meter, udp_error_meter),
  metrics:new(Engine, meter, tcp_error_meter),

  % The metrics package doesn't support history type, so we use the native folsom one
  folsom_metrics:new_history(udp_error_history),
  folsom_metrics:new_history(tcp_error_history),

  metrics:new(Engine, meter, refused_response_meter),
  metrics:new(Engine, counter, refused_response_counter),

  metrics:new(Engine, meter, empty_response_meter),
  metrics:new(Engine, counter, empty_response_counter),

  metrics:new(Engine, histogram, udp_handoff_histogram),
  metrics:new(Engine, histogram, tcp_handoff_histogram),

  metrics:new(Engine, counter, request_throttled_counter),
  metrics:new(Engine, meter, request_throttled_meter),
  metrics:new(Engine, histogram, request_handled_histogram),

  metrics:new(Engine, counter, packet_dropped_empty_queue_counter),
  metrics:new(Engine, meter, packet_dropped_empty_queue_meter),

  metrics:new(Engine, meter, cache_hit_meter),
  metrics:new(Engine, meter, cache_expired_meter),
  metrics:new(Engine, meter, cache_miss_meter),

  metrics:new(Engine, counter, dnssec_request_counter),
  metrics:new(Engine, meter, dnssec_request_meter),
  Engine.
