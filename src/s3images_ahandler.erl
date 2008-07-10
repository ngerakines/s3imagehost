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
-module(s3images_ahandler).

-export([out/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("s3images.hrl").

-record(upload, {fd, filename, last}).

out(Arg) ->
    Req = Arg#arg.req,
    handle_request(Req#http_request.method, Arg#arg.server_path, Arg).

wrap_body(Outer, {Inner, Args}) ->
    InnerBody = apply(s3imagestmpl_ahandler, Inner, [Args]),
    OuterBody = apply(s3imagestmpl_ahandler, Outer, [InnerBody]),
    iolist_to_binary(OuterBody).

handle_request('GET', "/", _Arg) ->
    make_response(200, wrap_body(default, {index, ok}));

handle_request('GET', "/upload", _Arg) ->
    RespBody = wrap_body(default, {upload, ok}),
    make_response(200, RespBody);

handle_request('POST', "/upload", Arg) ->
    case multipart(Arg, #upload{}) of
        {upload, _, Filename, _} ->
            case verify_image(Filename) of 
                ok ->
                    ImageData = image_detect:image_type([?DIR, Filename]),
                    S3Filename = filename_with_ext(Filename, ImageData),
                    file:rename([?DIR, Filename], [?DIR, S3Filename]),
                    s3images_util:write_file(S3Filename, ImageData, s3images:env_key(s3bucket)),
                    s3images_image:create(list_to_binary(S3Filename), <<"none">>, <<"none">>),
                    make_response(200, wrap_body(default, {upload, {ok, S3Filename}}));
                _ -> make_response(200, wrap_body(default, {upload, error}))
            end;
        _ -> make_response(200, wrap_body(default, {upload, error}))
    end;

handle_request(_, _, _Arg) ->
    make_response(505, "<p>Request not supported.</p>").

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, Result} -> Result;
                {cont, NewState} -> {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, Result} -> Result;
                {cont, _} -> error
            end
    end.

addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last == true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->
    file:close(State#upload.fd),
    {done, State};

addFileChunk(_A, [], State) when State#upload.last==true ->
    {done, error, State};

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {"file", Opts}}|Res], State ) ->
    case lists:keysearch(filename, 1, Opts) of
        {value, {_, _FnameRaw}} ->
            file:make_dir(?DIR),
            Fname = s3images_util:guid(),
            case file:open([?DIR, Fname] ,[write]) of
                {ok, Fd} ->
                    addFileChunk(A, Res, State#upload{filename = Fname, fd = Fd});
                _ -> {done, State}
            end;
        false -> {done, error, State}
    end;

addFileChunk(A, [{body, Data}|Res], State) when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok -> addFileChunk(A, Res, State);
        _ -> {done, State}
    end.

verify_image(Filename) ->
    case image_rules(Filename, [filesize, imagetest]) of
        {error, Reason} ->
            file:delete(Filename),
            {error, Reason};
        ok -> ok
    end.

image_rules(_, []) -> ok;

image_rules(Filename, [filesize | Rules]) ->
    case file:read_file_info([?DIR, Filename]) of
        {ok, FileInfo} when FileInfo#file_info.size < 5242880 -> image_rules(Filename, Rules);
        _ -> {error, filesize}
    end;

image_rules(Filename, [imagetest | Rules]) ->
    case image_detect:image_type([?DIR, Filename]) of
        unknown -> {error, filetype};
        {_, Height, _} when Height > 4096 -> {error, height};
        {_, _, Width} when Width > 4096 -> {error, width};
        _ -> image_rules(Filename, Rules)
    end.

filename_with_ext(Filename, {ImageType, _, _}) ->
    lists:concat([filename:basename(Filename), ".", atom_to_list(ImageType)]).
