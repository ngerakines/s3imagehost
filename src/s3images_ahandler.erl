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
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @todo Remove all non-yaws code to somewhere else.
%% @doc `s3images_ahandler` handles most of the yaws requests for the web
%% aspect of the application.
%% 
%% Note: This module is yaws_specific.
-module(s3images_ahandler).

-export([out/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("s3images.hrl").

-record(upload, {fd, filename, last, inhead, userdata = []}).

%% @doc The primary point of entry for Yaws as an appmod. This module will
%% attempt to route requests based on the server_path of the argument record
%% given.
%% When a request cannot be processed a 404 is returned.
out(Arg) ->
    Req = Arg#arg.req,
    handle_request(Req#http_request.method, Arg#arg.server_path, Arg).

%% @doc Provide a simple way to wrap request specific body inside of a
%% larger document.
wrap_body(Outer, {Inner, Args}) ->
    Tmpl = s3images:env_key(template, s3imagestmpl_ahandler),
    InnerBody = apply(Tmpl, Inner, [Args]),
    OuterBody = apply(Tmpl, Outer, [InnerBody]),
    iolist_to_binary(OuterBody).

%% @spec handle_request(Request, Path, Arg) -> Response
%% when 
%%      Request = GET | POST
%%      Path = string()
%%      Arg = any()
%%      Response = any()
%% @doc Process a specific request based on the requst type and requested
%% URL. The README file has more general information on the URLs available.
handle_request('GET', "/", _Arg) ->
    Images = s3images_image:find(all, 0, 25),
    make_response(200, wrap_body(default, {index, {image_funs(Images)}}));

handle_request('GET', "/a/recent", _Arg) ->
    Records = s3images_image:find(all, 0, 25),
    CleanRecords = s3images_image:record_to_json(Records),
    RespBody = rfc4627:encode(CleanRecords),
    make_response(200, RespBody);

handle_request('GET', "/a/owner:" ++ OwnerId, _Arg) ->
    Records = s3images_image:find(creator, [list_to_binary(OwnerId)]),
    CleanRecords = s3images_image:record_to_json(Records),
    RespBody = rfc4627:encode(CleanRecords),
    make_response(200, RespBody);

handle_request('GET', "/a/object:" ++ ObjectId, _Arg) ->
    Records = s3images_image:find(object, [list_to_binary(ObjectId)]),
    CleanRecords = s3images_image:record_to_json(Records),
    RespBody = rfc4627:encode(CleanRecords),
    make_response(200, RespBody);

handle_request('GET', "/a/", _Arg) ->
    RespBody = wrap_body(default, {upload, ok}),
    make_response(200, RespBody);

handle_request('GET', "/upload", _Arg) ->
    RespBody = wrap_body(default, {upload, ok}),
    make_response(200, RespBody);

handle_request('POST', "/upload", Arg) ->
    case multipart(Arg, #upload{}) of
        Upload when is_record(Upload, upload) ->
            UserData = collect_userdata(Upload#upload.userdata),
            try verify_image(Upload#upload.filename, UserData) of
                ok ->
                    catch process_image(Upload#upload.filename, UserData),
                    {redirect, "/"};
                _ -> make_response(200, wrap_body(default, {upload, error}))
            catch
                _ -> make_response(200, wrap_body(default, {upload, error}))
            end;
        _ ->
            make_response(200, wrap_body(default, {upload, error}))
    end;

handle_request(_Request, _Path, _Arg) ->
    make_response(505, "<p>Request not supported.</p>").

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

%% @doc Process multipart form submission data. This function will iterate
%% through the chunks of submission data using the addFileChunk/3 function.
%% The #upload record is used to track the status of the submission. When a
%% 'file' part is found it will save the data to a temporary file for post-
%% processing. All other chunks are saved in a large list of key/value
%% pairs.
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
                    addFileChunk(A, Res, State#upload{filename = Fname, fd = Fd, inhead = "file"});
                _ -> {done, State}
            end;
        false -> {done, error, State}
    end;

addFileChunk(A, [{body, Data}|Res], State) when State#upload.filename /= undefined, State#upload.inhead == "file" ->
    case file:write(State#upload.fd, Data) of
        ok -> addFileChunk(A, Res, State#upload{ inhead = "none" });
        _ -> {done, State}
    end;

addFileChunk(A, [X | Tail], State) ->
    case X of
        {head, {Name, _}} -> 
            addFileChunk(A, Tail, State#upload{ userdata = [Name | State#upload.userdata ] });
        {body, Data} ->
            addFileChunk(A, Tail, State#upload{ userdata = [Data | State#upload.userdata ] })
    end.

%% @doc Determine if the file is legitamite and if it is not then delete it.
%% This function will look for the 'delegate' env variable to find a module
%% that responds to the verify_image/2 function and then uses it's response.
verify_image(Filename, UserData) ->
    Resp = case s3images:env_key(delegate) of
        <<"none">> -> ok;
        Module ->
            case erlang:function_exported(Module, verify_image, 2) of
                false -> ok;
                true -> erlang:apply(Module, verify_image, [Filename, UserData])
            end
    end,
    case Resp of
        {error, Reason} ->
            file:delete(Filename),
            {error, Reason};
        ok -> ok
    end.

filename_with_ext(Filename, {ImageType, _, _}) ->
    lists:concat([filename:basename(Filename), ".", atom_to_list(ImageType)]).

thumbnail_with_ext(Filename, {ImageType, _, _}) ->
    lists:concat([filename:basename(Filename), "_sq.", atom_to_list(ImageType)]).

%% @doc Send the image in Filename to Amazon and store a record of the event
%% in mnesia. If the create_sq env variable was set then this function will
%% attempt to create a 100x100 square image thumbnail using the ImageMagick
%% command-line utility "convert". The thumbnail is then sent to Amazon S3
%% as well.
process_image(Filename, UserData) ->
    {ImageType, _, _} = image_detect:image_type([?DIR, Filename]),
    S3Filename = filename_with_ext(Filename, {ImageType, 0, 0}),
    file:rename([?DIR, Filename], [?DIR, S3Filename]),
    s3images_util:write_file(S3Filename, {ImageType, 0, 0}, s3images:env_key(s3bucket)),
    case s3images:env_key(create_sq) of
        true ->
            ThumbnailName = thumbnail_with_ext(Filename, {ImageType, 0, 0}),
            ResizeCmd = lists:concat(["convert /tmp/", S3Filename," -resize 100x100 /tmp/", ThumbnailName]),
            os:cmd(ResizeCmd),
            s3images_util:write_file(ThumbnailName, {ImageType, 0, 0}, s3images:env_key(s3bucket));
        _ -> ok
    end,
    s3images_image:create(list_to_binary(S3Filename), userdata_value(UserData, "object"), userdata_value(UserData, "owner"), UserData),
    ok.

collect_userdata(Data) ->
    UserData = collect_userdata(Data, []),
    case s3images:env_key(delegate) of
        <<"none">> -> UserData;
        Module ->
            case erlang:function_exported(Module, transform_userdata, 1) of
                false -> UserData;
                true -> erlang:apply(Module, transform_userdata, [UserData])
            end
    end.
    
collect_userdata([], Acc) -> Acc;
collect_userdata([Value, Key | Tail], Acc) ->
    collect_userdata(Tail, [{Key, Value} | Acc]).

userdata_value(UserData, Key) ->
    case lists:keysearch(Key, 1, UserData) of
        {value, {Key, Value}} -> Value;
        _ -> <<"none">>
    end.

image_funs(Images) ->
    [image_fun(Image) || Image <- Images].
image_fun(Image) ->
    fun
        (name) -> binary_to_list(Image#image.name);
        (bucket) -> binary_to_list(Image#image.bucket);
        (object) -> binary_to_list(Image#image.object);
        (create_date) -> binary_to_list(Image#image.create_date);
        (userdata) -> binary_to_list(Image#image.userdata);
        (url) ->
            Name = binary_to_list(Image#image.name),
            lists:concat(["http://", Image#image.bucket, "/", Name]);
        (thumbnail) ->
            Name = binary_to_list(Image#image.name),
            Ext = filename:extension(Name),
            Root = filename:rootname(Name),
            lists:concat(["http://", Image#image.bucket, "/", Root, "_sq", Ext])
    end.
