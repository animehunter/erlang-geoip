-module(geoip).

%%
%% Usage:
%%
%% > Geoip = geoip:new("GeoIP.dat").
%% #Port<0.2755>
%%
%% > geoip:get_country_by_ip(Geoip, "24.24.24.24").
%% <<"United States">>
%%
%% > geoip:get_country_by_ip(Geoip, <<"80.24.24.80">>).
%% <<"Spain">>
%%

%%
%% Erlang benchmark
%%
%% > geoip:benchmark().
%% (Slow) Result for "? GEOIP_STANDARD": (2578 ms) (overhead: 61 ms) (C-only: 2516 ms) x100000
%% (Slow) Result for "? GEOIP_CHECK_CACHE": (2390 ms) (overhead: 30 ms) (C-only: 2360 ms) x100000
%% (Fast) Result for "? GEOIP_MEMORY_CACHE": (1188 ms) (overhead: 390 ms) (C-only: 797 ms) x1000000
%% (Fast) Result for "? GEOIP_MEMORY_CACHE bor ? GEOIP_CHECK_CACHE": (1203 ms) (overhead: 390 ms) (C-only: 812 ms) x1000000
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

-export([new/1, new/2, delete/1]).
-export([get_country_by_ip/2]).

-export([test/0, benchmark/0, 
		 benchmark/1, benchmark_overhead/1]).
		
%% Port commands
-define(EVENT_OPEN, 0).
-define(EVENT_COUNTRY_BY_IP, 1).

%% Geoip flags
-define(GEOIP_STANDARD, 0).
-define(GEOIP_MEMORY_CACHE, 1).
-define(GEOIP_CHECK_CACHE, 2).
%% -define(GEOIP_INDEX_CACHE, 4). %% does not work with geoip.dat
%% -define(GEOIP_MMAP_CACHE, 8). mmap doesnt work on windows!


%%
%% Public API
%%

%% inline this
-define(call(Port, Cmd),
begin
	port_command(Port, Cmd),
	receive
		{Port, {data, Bin}} -> 
			%% io:format("Got: ~p~n",[(Bin)]), 
			Bin
		after 0 ->
			<<>>
	end
end).

new(Name) ->
	new(Name, ?GEOIP_MEMORY_CACHE).
	
new(Name, Type) -> 
	erl_ddll:load(".", ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port, Name, Type),
	Port.

delete(Port) -> 
	port_close(Port),
	erl_ddll:unload(?MODULE).
	
open(Port, Name, Type) ->	
	?call(Port,[?EVENT_OPEN, Type, Name, 0]).

get_country_by_ip(Port, IP) ->
	?call(Port,[?EVENT_COUNTRY_BY_IP, IP, 0]).

	
%%
%% test and benchmark code...
%%

-define(GEOIP_DATABASE, (<<"GeoIP.dat">>)).
	
test() ->
	test(?GEOIP_STANDARD),
	test(?GEOIP_MEMORY_CACHE),
	test(?GEOIP_CHECK_CACHE),
	test(?GEOIP_MEMORY_CACHE bor ?GEOIP_CHECK_CACHE).
	
test(Type) ->
	erl_ddll:load(".", ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port, ?GEOIP_DATABASE, Type),
	[io:format("Type: ~p ~s~n", [Type, get_country_by_ip(Port, IP)]) || IP <- test_ip()],
	io:format("~n"),
	port_close(Port),
	erl_ddll:unload(?MODULE).

test_ip() ->
	[<<"24.24.24.24">>,<<"80.24.24.80">>,
	 <<"200.24.24.40">>,<<"68.24.24.46">>].

	 
-define(BENCHMARK(TYPE, N), 
begin
	print_benchmark(??TYPE, N,
					 element(1, timer:tc(?MODULE, benchmark, [{type, TYPE, N}])),
					 element(1, timer:tc(?MODULE, benchmark_overhead, [{TYPE, N}])))
end).
 
benchmark() ->
	?BENCHMARK(?GEOIP_STANDARD, 100000),
	?BENCHMARK(?GEOIP_CHECK_CACHE, 100000),
	?BENCHMARK(?GEOIP_MEMORY_CACHE, 1000000),
	?BENCHMARK(?GEOIP_MEMORY_CACHE bor ?GEOIP_CHECK_CACHE, 1000000).

print_benchmark(Type, N, Res, Overhead) ->
	io:format("Result for ~p: (~p ms) (overhead: ~p ms) (C-only: ~p ms) x~p~n", 
			 [
				Type, 
				Res div 1000,
				Overhead  div 1000,
				(Res - Overhead)  div 1000,
				N
			 ]).	

benchmark_overhead({Type, N}) ->
	erl_ddll:load(".", ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port,  ?GEOIP_DATABASE, Type),
	benchmark_overhead(Port, N),
	port_close(Port),
	erl_ddll:unload(?MODULE).	
	
benchmark_overhead(Port, N) ->
	benchmark_overhead(Port, N, test_ip(), []).

benchmark_overhead(_, 0, _, _) -> ok;

benchmark_overhead(Port, N, [], IP ) ->
	benchmark_overhead(Port, N, IP, []);
	
benchmark_overhead(Port, N, [IP|T], IPList ) ->
	catch ?call(Port,[3, IP, 0]),
	benchmark_overhead(Port, N-1, T, [IP|IPList]).
	
	
benchmark({type, Type, N}) ->
	erl_ddll:load(".", ?MODULE),
	Port = open_port({spawn, ?MODULE},[binary]),
	open(Port, ?GEOIP_DATABASE, Type),
	benchmark(Port, N),
	port_close(Port),
	erl_ddll:unload(?MODULE).	
	
benchmark(Port, N) ->
	benchmark(Port, N, test_ip(), []).

benchmark(_, 0, _, _) -> ok;

benchmark(Port, N, [], IP ) ->
	benchmark(Port, N, IP, []);
	
benchmark(Port, N, [IP|T], IPList ) ->
	get_country_by_ip(Port, IP), 
	benchmark(Port, N-1, T, [IP|IPList]).

