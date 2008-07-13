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

-export([find/2, find/3, create/4, record_to_json/1]).

-include("s3images.hrl").
-include_lib("stdlib/include/qlc.hrl").

get_query(name, [Args]) ->
    qlc:q([E || E <- mnesia:table(image), E#image.name == Args]);
get_query(object, [Args]) ->
    qlc:q([E || E <- mnesia:table(image), E#image.object == Args]);
get_query(creator, [Args]) ->
    qlc:q([E || E <- mnesia:table(image), E#image.creator == Args]);
get_query(_, _) ->
    qlc:q([E || E <- mnesia:table(image)]).

find(Type, Args) ->
    F = fun() -> qlc:e(get_query(Type, Args)) end,
    mnesia:activity(transaction, F).

find(Type, Args, Count) ->
    F = fun() ->
        QH = qlc:sort(get_query(Type, Args), [{order, descending}]),
        QC = qlc:cursor(QH),
        Res = qlc:next_answers(QC, Count),
        qlc:delete_cursor(QC),
        Res
    end,
    mnesia:activity(transaction, F).


create(X, Y, Z, AA) when is_binary(X) == false ->
    create(list_to_binary(X), Y, Z, AA);
create(X, Y, Z, AA) when is_binary(Y) == false ->
    create(X, list_to_binary(Y), Z, AA);
create(X, Y, Z, AA) when is_binary(Z) == false ->
    create(X, Y, list_to_binary(Z), AA);

create(Name, Object, Owner, UserData) ->
    F = fun() ->
        NewImage = #image{
            name = Name,
            %% get bucket
            bucket = s3images:env_key(s3bucket),
            object = Object,
            creator = Owner,
            create_date = s3images_util:s3now(),
            userdata = UserData
        },
        mnesia:write(NewImage)
    end,
    mnesia:activity(transaction, F).

record_to_json(Records) ->
    [begin
        Name = binary_to_list(Record#image.name),
        Ext = filename:extension(Name),
        Root = filename:rootname(Name),
        {obj, [
            {<<"id">>, list_to_binary(Root)},
            {<<"url">>, list_to_binary(lists:concat(["http://", s3images:env_key(s3bucket), "/", Name]))},
            {<<"thumbnail">>, list_to_binary(lists:concat(["http://", s3images:env_key(s3bucket), "/", Root, "_sq", Ext]))}
        ]}
    end || Record <- Records].
