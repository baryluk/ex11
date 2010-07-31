-module(ex11_lib_read_jpeg).

%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-export([file/1, test/0]).

%% -compile(export_all).

test() ->
    file(dir() ++ "/joe.jpg").

dir() ->
    filename:dirname(code:which(?MODULE)).

file(File) ->
    Port = open_port({spawn,dir() ++ "/ex11_lib_read_jpeg"},
		     [{packet,2}, binary]),
    case do_command(Port, File ++ [0]) of
	{data, <<1:8,Width:16,Ht:16>>} ->
	    %% io:format("Image width=~p Ht=~p~n",[Width, Ht]),
	    Image = get_image(Port, 1, []),
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    true
	    end,
	    {ok, {Width, Ht, Image}};
	{data, <<0:8>>} ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    true
	    end,
	    {error, eNoJPG}
    end.
    
do_command(Port, Data) ->
    Port ! {self(), {command, Data}},
    receive
        {Port, Any} ->
            Any
    end.
   
get_image(Port, N, L) ->
    receive
	{Port, {data, <<0:8>>}} ->
	    %% io:format("received end of image~n"),
	    lists:reverse(L);
	{Port, {data, <<1:8,B/binary>>}} ->
	    %% io:format("Received line:~p size=~p~n",[N, size(B)]),
	    get_image(Port, N+1, [B|L]);
	Other ->
	    io:format("UNexpected packet:~p~n",[Other])
    end.
