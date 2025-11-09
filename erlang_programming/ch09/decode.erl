-module(decode).
-export([decode/1, seg1/0, seg2/0]).

decode(Segment) ->
    case Segment of
        << _SourcePort:16, _DestinationPort: 16,
            _SequenceNumber:32,
            _AckNumber:32,
            DataOffset:4, _Reserved:4, Flags:8, _WindowSize:16,
            _Checksum:16, _UnrgentPointer:16,
            Payload/binary >> when DataOffset > 4 ->
                OptSize = (DataOffset - 5) * 32,
                << _Options:OptSize, Message/binary>> = Payload,
                <<_CWR:1, _ECE:1, _URG:1, _ACK:1, _PSH:1, _RST:1, _SYN:1, _FIN:1>> = <<Flags:8>>,
                binary_to_list(Message);

       _ ->
           {error, bad_segment}
    end.

seg1() ->
    Seg = <<0:16, 0:16,
      0:32,
      0:32,
      5:4, 0:4, 0:8, 0:16,
      0:16, 0:16,
      "message"
    >>,
    decode(Seg).

seg2() ->
    Seg = <<0:16, 0:16,
        0:32,
        0:32,
        7:4, 0:4, 0:8, 0:16,
        0:16, 0:16,
        0:64,
        "message"
    >>,
    decode(Seg).
