module Day13

type Packet =
  | Numbers of int
  | List of Packet list

type PacketPair = { left: Packet; right: Packet }

type DistressSignal = PacketPair seq

module DistressSignal =
  let parse filename = Seq.empty
