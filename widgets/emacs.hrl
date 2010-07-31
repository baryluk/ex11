
%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.


-record(e, {width,     % width
	    ht,        % height,
	    data,      % lines,
	    exit,      % exit keys
	    mode,      % active | passive
	    cx,        % X pos of cursor
	    cy,        % Y pos of cursor
	    text,      % Pid of the text Widget
	    start,     % start line in lines of first row in buffer
	    kill = [], % kill ring
	    current,   % start of current line containg the cursor
	    col}).     % index before the current line
