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
%% @doc Provides basic image type detection functionality.
-module(image_detect).
-compile(export_all).

%% @spec image_type(Filename) -> error | {atom(), integer(), integer()}
%% where
%%       Filename = term()
%% @doc Determine the image type of Filename by looking at the binary data
%% of the file. Each image type matches a signature and pattern that can be
%% used to determine the type of image it is and the images deminsions.
image_type(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, FileHandle} -> image_type(FileHandle);
        _ -> error
    end;

%% Gif header, width and then height
image_type(<<71,73,70,56,55,97,Width:16/little,Height:16/little,_/binary>>) -> {gif, Height, Width};
image_type(<<71,73,70,56,57,97,Width:16/little,Height:16/little,_/binary>>) -> {gif, Height, Width};

%% PNG header, chunk1, width and height
%% spec: http://www.w3.org/TR/PNG/#
image_type(<<137,80,78,71,13,10,26,10,_:4/signed-integer-unit:8,73,72,68,82,Width:4/signed-integer-unit:8,Height:4/signed-integer-unit:8,_/binary>>) -> {png, Height, Width};

%% JPEG
%% ref: http://www.obrador.com/essentialjpeg/headerinfo.htm
%% thanks http://www.ravenna.com/gifs/isize.pl
image_type(Data = <<255,216,255,224,_/binary>>) ->
    {H, W} = parse_jpeg(Data),
    {jpeg, H, W};

image_type(_) -> unknown.

%% @doc Determine the height and width of a jpeg image. JPEG imges are too
%% complex to just look at the first n bytes so we have to iterate through
%% all of the "chunks" that define the JPEG and when we get to the one that
%% contains the height and width, return it.
parse_jpeg(Data) -> parse_jpeg(Data, {}).
parse_jpeg(<<>>, Results) -> Results;
parse_jpeg(<<255,192,_:16/signed-integer,_:8,Width:16/signed-integer,Height:16/signed-integer,_/binary>>, _) ->
    {Height, Width};
%% parse_jpeg(<<255,195,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos);
%% parse_jpeg(<<255,216,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos);
%% parse_jpeg(<<255,218,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos);
%% parse_jpeg(<<255,217,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos);
%% parse_jpeg(<<255,254,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos);
parse_jpeg(<<_,Rest/binary>>, Pos) -> parse_jpeg(Rest, Pos).

%% @doc From an image type, return the mime type for it.
mime_type(gif) -> "image/gif";
mime_type(png) -> "image/png";
mime_type(jpeg) -> "image/jpeg".

