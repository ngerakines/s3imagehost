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
%% @todo Make this application mochiweb compatible.
%% @doc `s3images` is the core application module for the s3images project.
%% This application has 2 start phases that you should be aware of. The
%% first is the "s3" phase. During the "s3" phase the application starts
%% an s3 gen_server using the third-party s3 module. This process provides
%% the core functionality of interacting with the Amazon S3 service.
%% 
%% The second start phase is the "mnesia" whereby the application creates
%% the mnesia tables used by the application. Note that the scheme defs will
%% already have been created by this point because mnesia is a required
%% application via the s3images.app configuration file. Because of this, we
%% change the table type of the schema table to a disc_copy if it is a ram
%% copy.
-module(s3images).
-behaviour(application).

-export([start/2, stop/1, start_phase/3, build_templates/0]).
-export([env_key/1, env_key/2]).

-include("s3images.hrl").
-include("s3.hrl").

start(_Type, Args) -> s3images_sup:start_link(Args).

stop(_State) -> ok.

start_phase(s3, _, _) ->
    s3:start(#aws_credentials{
        accessKeyId = env_key(s3key),
        secretAccessKey = env_key(s3secret)}
    ),
    ok;

start_phase(mnesia, _, _) ->
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> ok
    end,
    ExistingTables = mnesia:system_info(tables) -- [schema],
    Tables = [image],
    [begin
        create_table(Table)
    end || Table <- Tables, not lists:member(Table, ExistingTables)],
    ok.

create_table(image) ->
    mnesia:create_table(image, [{disc_copies, [node()]}, {attributes, record_info(fields, image)}]).

build_templates() ->
    [begin
        erltl:compile(F, [{outdir, "ebin"}, show_errors, show_warnings])
    end || F <- filelib:wildcard("templates/*.et")].

env_key(Key) ->
    env_key(Key, <<"none">>).

env_key(Key, Default) ->
    case application:get_env(s3images, Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.
