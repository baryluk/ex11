-module(map).

-export([reformat/0, reformat/2]).


%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

%% started 2004-03-10 by joe@sics.se (Joe Armstrong)

-compile(export_all).

-import(lists, [foldl/3, map/2, reverse/1]).

reformat() ->
    reformat(90, 50).

reformat(Rot, Threshold) ->
    {ok, F} = file:open("mapdata.dat", [read]),
    {ok, L} = io:read(F, '>'),
    file:close(F),
    Vectors = parse(L, []),
    V1 = map(fun(X) -> normalise(Rot, X) end, Vectors),
    Lines1 = map(fun({_,L1}) -> L1 end, V1),
    Lines2 = make_lines(Lines1, []),
    %% io:format("Lines2=~p~n",[Lines2]),
    Lines3 = foldl(fun(X,A) ->
			   if
			       length(X) > Threshold ->
				   X2 = map(fun({X1,Y1}) ->
						    {trunc(1000*X1),
						     trunc(1000*Y1)}
					    end, X),
				   [X2|A];
			       true ->
				   A
			   end
		   end, [], Lines2),
    file:write_file("mapdata.bin", [term_to_binary(Lines3)]).

make_lines([L|T], Lns) ->
    Lns1 = get_lines(L, Lns),
    make_lines(T, Lns1);
make_lines([], L) ->
    L.

get_lines([{X,Y}|T], Lns) ->
    {Line, T1} = get_line(X, Y, T, [{X,Y}]),
    get_lines(T1, [Line|Lns]);
get_lines([], Lns) ->
    Lns.

get_line(X, Y, [], L) ->
    {L, []};
get_line(X, Y, [{X1,Y1}=H|T]=All, L) ->
    case near(X,Y,X1,Y1) of
	true ->
	    get_line(X1,Y1, T, [H|L]);
	false ->
	    {L, All}
    end.

near(X1,Y1,X2,Y2) ->
    XX = X1 - X2,
    YY = Y1 - Y2,
    D = XX*XX + YY*YY,
    D < 100.

normalise(Rot,[H,X,Y,Z|T]) ->
    {H,[xyz2ll(Rot,X,Y,Z)|normalise(Rot,X, Y, Z, T)]}.

normalise(Rot,X,Y,Z,[Dx,Dy,Dz|T]) ->
    X1=X+Dx,Y1=Y+Dy,Z1=Z+Dz,
    [xyz2ll(Rot,X1,Y1,Z1)|normalise(Rot,X1,Y1,Z1,T)];
normalise(_,_,_,_,[]) ->
    [].

parse([0|T], L) ->
    reverse(L);
parse([H|T], L) ->
    {Stuff, T1} = fetch(3*H+1, T, []),
    parse(T1, [Stuff|L]).
    
fetch(0, X, L)     -> {reverse(L), X};
fetch(N, [H|T], L) -> fetch(N-1, T, [H|L]).

xyz2ll(Rot, Z, Y, X) ->
    D = math:sqrt(X*X+Y*Y+Z*Z),
    RtoD = 57.2958,
    C = 90 - (RtoD * math:acos(abs(Y)/D)),
    Lat = if 
	      Y < 0 -> -C;
	      true  -> C
	  end,
    Long = rot(Rot, (RtoD*math:atan2(-X, Z))),
    {sane_lat(Lat), sane_long(Long)}.


rot(Alpha, Long) ->
    Long1 = Alpha + Long,
    rot1(Long1).

rot1(Long) when Long > 180 ->
    rot1(Long - 360);
rot1(Long) when Long < -180->
    rot1(Long + 360);
rot1(Long) ->
    Long.

sane_lat(Lat) when Lat > 0, Lat =< 90 ->
    Lat;
sane_lat(Lat) when Lat < 0, Lat >= -90 ->
    Lat;
sane_lat(Lat) ->
    io:format("insane Lat=~p~n",[Lat]),
    Lat.

sane_long(Long) when Long >= 0, Long =< 180 ->
    Long;
sane_long(Long) when Long =< 0, Long >= -180 ->
    Long;
sane_long(Long) ->
    io:format("insane Long=~p~n",[Long]),
    Long.



