-module(geoip).

%%
%% Basic Usage:
%%
%% > Geoip = geoip:new("GeoIP.dat").
%% #Port<0.2755>
%%
%% > geoip:get_country_by_ip(Geoip, "24.24.24.24").
%% "United States"
%%
%% > geoip:get_country_by_ip(Geoip, <<"80.24.24.80">>).
%% "Spain"
%%
%% > geoip:delete(Geoip).
%% ok
%%

%%
%% Advanced usage:
%%
%% > Geoip = geoip:new("GeoIP.dat").
%% #Port<0.2755>
%%
%% > geoip:use_binary(Geoip).
%% []
%%
%% > geoip:get_country_by_ip(Geoip, "24.24.24.24").
%% <<"United States">>
%%
%% > geoip:get_country_by_ip(Geoip, "invalidip").
%% []
%%
%% > geoip:use_string(Geoip).                      
%% []
%%
%% > geoip:get_country_by_ip(Geoip, "24.24.24.24").
%% "United States"
%%
%% > geoip:get_country_by_ip(Geoip, "invalidip").
%% []
%%
%% > geoip:delete(Geoip).
%% ok
%%

%%
%% Erlang benchmark
%%
%% > geoip:benchmark().
%% 100000 lookups for "? GEOIP_STANDARD": (2556 ms) 
%% 100000 lookups for "? GEOIP_CHECK_CACHE": (2435 ms) 
%% 1000000 lookups for "? GEOIP_MEMORY_CACHE": (890 ms) 
%% 1000000 lookups for "? GEOIP_MEMORY_CACHE bor ? GEOIP_CHECK_CACHE": (903 ms) 
%% 

%%
%% C Benchmark
%%
%% >benchmark.exe
%%
%% GeoIP Country
%% 120000 lookups made in 2.769962 seconds
%%
%% GeoIP Country with GEOIP_CHECK_CACHE
%% 120000 lookups made in 2.684795 seconds
%%
%% GeoIP Country with GEOIP_MEMORY_CACHE
%% 1200000 lookups made in 0.409928 seconds
%%
%% GeoIP Country with GEOIP_MEMORY_CACHE and GEOIP_CHECK_CACHE
%% 1200000 lookups made in 0.414053 seconds
%%

%% new/delete functions
-export([new/1, new/2, delete/1]).

%% options
-export([use_string/1,use_binary/1]).

%% public API
-export([get_country_by_ip/2]).

-export([benchmark/0, 
		 benchmark/1, benchmark_overhead/1]).
		
%% Port commands
-define(EVENT_OPEN, 0).
-define(EVENT_USE_BINARY, 1).
-define(EVENT_USE_STRING, 2).
-define(EVENT_COUNTRY_BY_IP, 3).


%% Geoip flags
-define(GEOIP_STANDARD, 0).
-define(GEOIP_MEMORY_CACHE, 1).
-define(GEOIP_CHECK_CACHE, 2).
%% -define(GEOIP_INDEX_CACHE, 4). %% does not work with geoip.dat
%% -define(GEOIP_MMAP_CACHE, 8). mmap doesnt work on windows!

%%
%% Settings
%%
-define(DRIVER_LOCATION,".").
%-define(DRIVER_LOCATION, (code:priv_dir(ee_server))).
%-define(GEOIP_DATABASE, (code:priv_dir(ee_server) ++ "/" ++ "GeoIP.dat")).
-define(GEOIP_DATABASE, "GeoIP.dat").
%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Public API
%%

%% inline this
-define(call(Port, Cmd, Data), port_control(Port, Cmd, Data)).

new(Name) ->
	new(Name, ?GEOIP_MEMORY_CACHE).
	
new(Name, Type) -> 
	ok = erl_ddll:load(?DRIVER_LOCATION, ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	[] = open(Port, Name, Type),
	Port.

delete(Port) -> 
	port_close(Port),
	erl_ddll:unload(?MODULE).

open(Port, Name, Type) when is_integer(Type),is_binary(Name) ->	
	?call(Port, ?EVENT_OPEN, <<Type, Name/binary, 0>>);
open(Port, Name, Type) when is_integer(Type) ->	
	?call(Port, ?EVENT_OPEN, [Type, Name, 0]).

get_country_by_ip(Port, IP) when is_binary(IP) ->
	?call(Port, ?EVENT_COUNTRY_BY_IP, <<IP/binary, 0>>);
get_country_by_ip(Port, IP) ->
	?call(Port, ?EVENT_COUNTRY_BY_IP, [IP, 0]).

%% tell driver to return string instead of binary, this is the default option
use_string(Port) ->
	?call(Port, ?EVENT_USE_STRING, <<>>).
	
%% tell driver to return binary instead of string	
use_binary(Port) ->
	?call(Port, ?EVENT_USE_BINARY, <<>>).
	
%%
%% test and benchmark code...
%%

	
ip_test() ->
	ip_test_internal(?GEOIP_STANDARD),
	ip_test_internal(?GEOIP_MEMORY_CACHE),
	ip_test_internal(?GEOIP_CHECK_CACHE),
	ip_test_internal(?GEOIP_MEMORY_CACHE bor ?GEOIP_CHECK_CACHE).
	
ip_test_internal(Type) ->
	erl_ddll:load(?DRIVER_LOCATION, ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
    link(Port),
	open(Port, ?GEOIP_DATABASE, Type),
	[?assertEqual(Country,get_country_by_ip(Port, IP)) || {IP, Country} <- ip_input()],
	[?assertEqual(Country,get_country_by_ip(Port, IP)) || {IP, Country} <- ip_input2()],
	use_binary(Port), % switch to binary return mode
	[?assertEqual(Country,get_country_by_ip(Port, IP)) || {IP, Country} <- ip_input3()],
	[?assertEqual(Country,get_country_by_ip(Port, IP)) || {IP, Country} <- ip_input4()],
	port_close(Port),
	erl_ddll:unload(?MODULE).

ip_input() ->
	[ %% {Input, Output} pairs
     {"24.24.24.24",  "United States"},
     {"80.24.24.80",  "Spain"},
	 {"200.24.24.40", "Colombia"},
     {"68.24.24.46",  "United States"}
    ].

ip_input2() ->
	[ %% {Input, Output} pairs
     {<<"24.24.24.24">>,  "United States"},
     {<<"80.24.24.80">>,  "Spain"},
	 {<<"200.24.24.40">>, "Colombia"},
     {<<"68.24.24.46">>,  "United States"}
    ].
	
ip_input3() ->
	[ %% {Input, Output} pairs
     {"24.24.24.24",  <<"United States">>},
     {"80.24.24.80",  <<"Spain">>},
	 {"200.24.24.40", <<"Colombia">>},
     {"68.24.24.46",  <<"United States">>}
    ].
ip_input4() ->
	[ %% {Input, Output} pairs
     {<<"24.24.24.24">>,  <<"United States">>},
     {<<"80.24.24.80">>,  <<"Spain">>},
	 {<<"200.24.24.40">>, <<"Colombia">>},
     {<<"68.24.24.46">>,  <<"United States">>}
    ].
	
-define(BENCHMARK(TYPE, N), 
begin
	print_benchmark(??TYPE, N,
					 element(1, timer:tc(?MODULE, benchmark, [{type, TYPE, N}])))
end).
 
benchmark() ->
	?BENCHMARK(?GEOIP_STANDARD, 100000),
	?BENCHMARK(?GEOIP_CHECK_CACHE, 100000),
	?BENCHMARK(?GEOIP_MEMORY_CACHE, 1000000),
	?BENCHMARK(?GEOIP_MEMORY_CACHE bor ?GEOIP_CHECK_CACHE, 1000000).

print_benchmark(Type, N, Res) ->
	io:format("~p lookups for ~p: (~p ms) ~n", 
			 [
				N,
				Type, 
				Res div 1000
			 ]).	

benchmark_overhead({Type, N}) ->
	erl_ddll:load(?DRIVER_LOCATION, ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port,  ?GEOIP_DATABASE, Type),
	benchmark_overhead(Port, N),
	port_close(Port),
	erl_ddll:unload(?MODULE).	
	
benchmark_overhead(Port, N) ->
    IPs = lists:map(fun({IP,_}) -> IP end, ip_input()), 
	benchmark_overhead(Port, N, IPs, []).

benchmark_overhead(_, 0, _, _) -> ok;

benchmark_overhead(Port, N, [], IP ) ->
	benchmark_overhead(Port, N, IP, []);
	
benchmark_overhead(Port, N, [IP|T], IPList ) ->
	catch ?call(Port,1010101, <<IP, 0>>),
	benchmark_overhead(Port, N-1, T, [IP|IPList]).
	
	
benchmark({type, Type, N}) ->
	erl_ddll:load(?DRIVER_LOCATION, ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port, ?GEOIP_DATABASE, Type),
	benchmark(Port, N),
	port_close(Port),
	erl_ddll:unload(?MODULE).	
	
benchmark(Port, N) ->
    IPs = lists:map(fun({IP,_}) -> IP end, ip_input()),
	benchmark(Port, N, IPs, []).

benchmark(_, 0, _, _) -> ok;

benchmark(Port, N, [], IP ) ->
	benchmark(Port, N, IP, []);
	
benchmark(Port, N, [IP|T], IPList ) ->
	get_country_by_ip(Port, IP), 
	benchmark(Port, N-1, T, [IP|IPList]).

