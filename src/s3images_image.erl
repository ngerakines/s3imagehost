%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(s3images_image).

-export([find/2, create/3]).

-include("s3images.hrl").
-include_lib("stdlib/include/qlc.hrl").

get_query(name, [Args]) ->
    qlc:q([E || E <- mnesia:table(image), E#image.name == Args]);
get_query(_, _) ->
    qlc:q([E || E <- mnesia:table(image)]).

find(Type, Args) ->
    F = fun() -> qlc:e(get_query(Type, Args)) end,
    mnesia:activity(transaction, F).

create(Name, Object, Owner) ->
    F = fun() ->
        NewImage = #image{
            name = Name,
            %% get bucket
            bucket = s3images:env_key(s3bucket),
            object = Object,
            creator = Owner
        },
        mnesia:write(NewImage)
    end,
    mnesia:activity(transaction, F).
