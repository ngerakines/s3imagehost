-module(image_detect).
-compile(export_all).

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

mime_type(gif) -> "image/gif";
mime_type(png) -> "image/png";
mime_type(jpeg) -> "image/jpeg".

