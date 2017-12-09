-module(wave).
-export([read/1, write/2, write/3]).

% audio_format = integer (see wav docs for formats)
% sampling_rate in kHz
% data is list of lists of integers (one list for each channel)

-record(wave, {audio_format, sampling_rate, data}).

read(FileName) -> %% returns wave record
    {ok, Binary} = file:read_file(FileName),

    <<ChunkID:4/binary,
     ChunkSize:4/little-unsigned-integer-unit:8,
     Format:4/binary,
     SubChunk1ID:4/binary,
     SubChunk1Size:4/little-unsigned-integer-unit:8,
     AudioFormat:2/little-unsigned-integer-unit:8,   % 1 = pcm
     NumChannels:2/little-unsigned-integer-unit:8,   % 1 = mono
     SampleRate:4/little-unsigned-integer-unit:8,    % 16000, etc
     ByteRate:4/little-unsigned-integer-unit:8,      % SampleRate * NumChannels * BitsPerSample/8
     BlockAlign:2/little-unsigned-integer-unit:8,    % NumChannels * BitsPerSample/8
     BitsPerSample:2/little-unsigned-integer-unit:8, % 16, etc
     SubChunk2ID:4/binary,
     SubChunk2Size:4/little-unsigned-integer-unit:8,
     Data/binary>> = Binary,

    % TODO: test file for well-formedness, e.g.:
    % ChunkID == "RIFF"
    % ChunkSize == SubChunk2Size + 36
    % Format == "WAVE"
    % AudioFormat = 1  % no other encodings supported in v1.0
    % SubChunk1ID == "fmt "
    % SubChunk1Size == 16
    % SubChunk2ID == "data"
    % SubChunk2Size == NumSamples * NumChannels * BitsPerSample div 8

    % Channels = data2channels(Data, NumChannels, BitsPerSample),

    io:format("* ~p~n~n- ChunkID: ~p~n- ChunkSize: ~B~n- Format: ~p~n- SubChunk1ID: ~p~n- SubChunk1Size: ~B~n- AudioFormat: ~B~n- NumChannels: ~B~n- SampleRate: ~B~n- ByteRate: ~B~n- BlockAlign: ~B~n- BitsPerSample: ~B~n- SubChunk2ID: ~p~n- SubChunk2Size: ~B~n- Data: [...]~n~n",
    [FileName,
    binary_to_list(ChunkID),
    ChunkSize,
    binary_to_list(Format),
    binary_to_list(SubChunk1ID),
    SubChunk1Size,
    AudioFormat,
    NumChannels,
    SampleRate,
    ByteRate,
    BlockAlign,
    BitsPerSample,
    binary_to_list(SubChunk2ID),
    SubChunk2Size
   ]),

    #wave{sampling_rate=SampleRate,
      audio_format=AudioFormat,
    %   data=Channels
      data=Data
     }.

mono(Data, NBytes) ->
    [[ X || <<X:NBytes/little-signed-integer-unit:8>> <= Data ]].

stereo(Data, NBytes) ->
    LP = [ [A,B] || << A:NBytes/little-signed-integer-unit:8, B:NBytes/little-signed-integer-unit:8 >> <= Data],
    lp2pl(LP, [], []).

% list of pairs to pair of lists
lp2pl([], Left, Right) ->
    [lists:reverse(Left), lists:reverse(Right)];

lp2pl([[L,R]|T], Left, Right) ->
    lp2pl(T, [L|Left], [R|Right]).

data2channels(Data, NumChannels, BitsPerSample) ->
    NBytes = BitsPerSample div 8,
    case NumChannels of
    1  -> mono(Data, NBytes);
    2  -> stereo(Data, NBytes)
    end.

lt2l([], Acc) ->
    lists:reverse(Acc);

lt2l([H|T], Acc) ->
    lt2l(T, [element(2, H), element(1, H) | Acc]).

pl2l(PL) ->
    X = lists:zip(lists:nth(1, PL), lists:nth(2, PL)),
    lt2l(X, []).

channels2dataBin(Channels, BitsPerSample) ->
    case length(Channels) of
    1 -> Data = lists:nth(1, Channels);
    2 -> Data = pl2l(Channels)
    end,
    Size = length(Data) * (BitsPerSample div 8),
    DataBin = list_to_binary([ <<X:2/little-signed-integer-unit:8>> ||  X <- Data]),
    {Size, DataBin}.

write(WavRecord, FileName) ->
    write(WavRecord, FileName, 16).

write(WavRecord, FileName, BitsPerSample) ->
    {SubChunk2Size, Data} = channels2dataBin(WavRecord#wave.data, BitsPerSample),

    ChunkID = list_to_binary("RIFF"),
    Format = list_to_binary("WAVE"),
    SubChunk1ID = list_to_binary("fmt "),
    SubChunk1Size = 16,
    AudioFormat = 1, % only pcm encoding supported
    NumChannels = length(WavRecord#wave.data),
    SampleRate = WavRecord#wave.sampling_rate,
    ByteRate = SampleRate * NumChannels * BitsPerSample div 8,
    BlockAlign = NumChannels * BitsPerSample div 8,
    SubChunk2ID = list_to_binary("data"),
    ChunkSize = SubChunk2Size + 36,

    io:format("* ~p~n~n- ChunkID: ~p~n- ChunkSize: ~B~n- Format: ~p~n- SubChunk1ID: ~p~n- SubChunk1Size: ~B~n- AudioFormat: ~B~n- NumChannels: ~B~n- SampleRate: ~B~n- ByteRate: ~B~n- BlockAlign: ~B~n- BitsPerSample: ~B~n- SubChunk2ID: ~p~n- SubChunk2Size: ~B~n- Data: [...]~n~n",
    [FileName,
     binary_to_list(ChunkID),
     ChunkSize,
     binary_to_list(Format),
     binary_to_list(SubChunk1ID),
     SubChunk1Size,
     AudioFormat,
     NumChannels,
     SampleRate,
     ByteRate,
     BlockAlign,
     BitsPerSample,
     binary_to_list(SubChunk2ID),
     SubChunk2Size
    ]),

    Binary = <<ChunkID:4/binary,
          ChunkSize:4/little-unsigned-integer-unit:8,  %%
          Format:4/binary,
          SubChunk1ID:4/binary,
          SubChunk1Size:4/little-unsigned-integer-unit:8,
          AudioFormat:2/little-unsigned-integer-unit:8,
          NumChannels:2/little-unsigned-integer-unit:8,
          SampleRate:4/little-unsigned-integer-unit:8,
          ByteRate:4/little-unsigned-integer-unit:8,
          BlockAlign:2/little-unsigned-integer-unit:8,
          BitsPerSample:2/little-unsigned-integer-unit:8,
          SubChunk2ID:4/binary,
          SubChunk2Size:4/little-unsigned-integer-unit:8,  %%
          Data/binary
          >>,

    file:write_file(FileName, Binary),
    {ok, FileName}.
