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
    instruction: int
    pixels: string array }

module Device =
  let private renderPixel device : Device =
    let modCycle = (device.cycle - 1) % 40

    let pixel =
      if modCycle >= device.X - 1 && modCycle <= device.X + 1 then
        "#"
      else
        "."

    let lineIndex = (device.cycle - 1) / 40
    let line = device.pixels[lineIndex] + pixel
    { device with pixels = Array.updateAt lineIndex line device.pixels }

  let private executeNextInstruction (program: Program) (device: Device) : Device =
    let readInstrDevice =
      match program[device.instruction] with
      | AddX _ as inst -> { device with delay = 1, inst }
      | Noop -> device

    { readInstrDevice with instruction = device.instruction + 1 }

  let private runCycle (program: Program) (device: Device) : Device =
    let delay, instr = device.delay
    let updatedDelay = delay - 1

    if updatedDelay > 0 then
      { device with delay = updatedDelay, instr }
    else
      let _, delayedInstr = device.delay

      match delayedInstr with
      | AddX n ->
        { device with
            X = device.X + n
            delay = 0, Noop }
      | Noop -> executeNextInstruction program device

  let private updateCycle device =
    { device with cycle = device.cycle + 1 }

  let init () =
    { cycle = 1
      X = 1
      delay = 0, Noop
      instruction = 0
      pixels = Array.create 6 "" }

  let runUntil cycle program device : Device =
    let runCycle = (runCycle program) >> updateCycle

    seq { device.cycle .. cycle - 1 }
    |> Seq.fold (fun d _ -> d |> runCycle |> renderPixel) (renderPixel device)

  let signalStrength (device: Device) : int = device.cycle * device.X

  let renderPixels device = String.concat "\n" device.pixels
