module Day10

type Instruction =
  | AddX of int
  | Noop

type Program = Instruction array

module Program =
  open System.IO

  let parseLine (line: string) : Instruction =
    let split = line.Split(" ")

    match split[0] with
    | "addx" -> AddX(int split[1])
    | _ -> Noop

  let parse filename : Program =
    File.ReadLines filename |> Seq.map parseLine |> Seq.toArray

type Device =
  { cycle: int
    X: int
    delay: int * Instruction
    instruction: int }

module Device =

  let private executeNextInstruction device (program: Program) : Device =
    match program[device.instruction] with
    | AddX n ->
      { device with
          cycle = device.cycle + 1
          delay = 1, AddX n
          instruction = device.instruction + 1 }
    | Noop ->
      { device with
          cycle = device.cycle + 1
          instruction = device.instruction + 1 }

  let private runCycle (device: Device) (program: Program) : Device =
    let delay, instr = device.delay
    let updatedDelay = delay - 1

    if updatedDelay > 0 then
      { device with
          cycle = device.cycle + 1
          delay = updatedDelay, instr }
    else
      let _, delayedInstr = device.delay

      match delayedInstr with
      | AddX n ->
        { device with
            cycle = device.cycle + 1
            X = device.X + n
            delay = 0, Noop }
      | Noop -> executeNextInstruction device program


  let runUntil cycle device program : Device =
    seq {
      let mutable device = device

      for _ in seq { device.cycle .. cycle - 1 } do
        device <- runCycle device program
        yield device
    }
    |> Seq.last

  let init =
    { cycle = 1
      X = 1
      delay = 0, Noop
      instruction = 0 }

  let signalStrength (device: Device) : int = device.cycle * device.X