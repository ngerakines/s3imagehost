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
-module(s3images_demo).

-export([verify_image/2, transform_userdata/1]).

-include_lib("kernel/include/file.hrl").

transform_userdata(OldUserData) ->
    OldUserData.

%% verify_image delegate
%% This function runs through a number of rules to assert that the file can
%% and should be processed.

verify_image(Filename, UserData) ->
    image_rules(Filename, UserData, [password, filesize, imagetest]).

image_rules(_, _, []) -> ok;

image_rules(Filename, UserData, [filesize | Rules]) ->
    case file:read_file_info(["/tmp/", Filename]) of
        {ok, FileInfo} when FileInfo#file_info.size < 5242880 -> image_rules(Filename, UserData, Rules);
        _ -> {error, filesize}
    end;

image_rules(Filename, UserData, [imagetest | Rules]) ->
    case image_detect:image_type(["/tmp/", Filename]) of
        unknown -> {error, filetype};
        {_, Height, _} when Height > 4096 -> {error, height};
        {_, _, Width} when Width > 4096 -> {error, width};
        _ -> image_rules(Filename, UserData, Rules)
    end;

image_rules(Filename, UserData, [password | Rules]) ->
    case lists:keysearch("password", 1, UserData) of
        {value, {"password", "erlang"}} -> image_rules(Filename, UserData, Rules);
        _ -> {error, password}
    end.
