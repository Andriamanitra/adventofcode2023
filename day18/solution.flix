enum Direction with ToString {
    case Up
    case Down
    case Left
    case Right
}

type alias Instruction = { direction = Direction, steps = Int64 }

def polygonArea(vertices: List[(Int64, Int64)]): Int64 =
    // Calculate area of a polygon using the trapezoid formula
    List.zipWith(
        (p0, p1) -> {
            let (x0, y0) = p0;
            let (x1, y1) = p1;
            (y0+y1) * (x1-x0) / 2i64
        },
        vertices,
        List.drop(1, vertices)
    )
    |> List.foldLeft(Add.add, 0i64)

def parseLine(line: String): (Direction, Int64, Int64) =
    match String.words(line) {
        case d :: n :: s :: Nil => {
            let steps =
                match Int64.fromString(n) {
                    case Some(steps) => steps
                    case None => bug!("Invalid Int64 '${n}'")
                };
            let direction =
                match d {
                    case "D" => Direction.Down
                    case "U" => Direction.Up
                    case "L" => Direction.Left
                    case "R" => Direction.Right
                    case _ => bug!("Invalid direction '${d}'")
                };
            let color =
                match Int64.parse(16, s |> String.dropLeft(2) |> String.dropRight(1)) {
                    case Ok(x) => x
                    case Err(_) => bug!("Invalid color: '${s}'")
                };
            (direction, steps, color)
        }
        case _ => bug!("invalid line ${line}")
    }

def dugLagoonSize(instructions: List[Instruction]): Int64 =
    let vertices =
        instructions
        |> List.scanLeft((acc, instruction) -> {
            let steps = instruction.steps;
            let (x, y) = acc; 
            match instruction.direction {
                case Direction.Up => (x, y+steps)
                case Direction.Down => (x, y-steps)
                case Direction.Left => (x-steps, y)
                case Direction.Right => (x+steps, y)
            }
        }, (0i64, 0i64));
    // we need to account for the width of the path: for each cube excavated
    // along the path 2/4 of it is not counted, except for 4 of the corners
    // which account for the +1
    let extra =
        instructions
        |> List.map(match {steps = steps | _} -> steps)
        |> List.foldLeft(Add.add, 0i64) / 2i64 + 1i64;

    polygonArea(vertices) + extra


def solvePart1(lines: List[String]): Int64 =
    def getInstruction(line: (Direction, Int64, Int64)): Instruction = {
        let (dir, steps, _) = line;
        { direction = dir, steps = steps }
    };
    lines
    |> List.map(parseLine >> getInstruction)
    |> dugLagoonSize


def solvePart2(lines: List[String]): Int64 =
    def getInstruction(line: (Direction, Int64, Int64)): Instruction = {
        let (_, _, num) = line;
        let steps = num / 16i64;
        let direction =
            match Int64.mod(num, 16i64) {
                case 0i64 => Direction.Right
                case 1i64 => Direction.Down
                case 2i64 => Direction.Left
                case 3i64 => Direction.Up
                case _ => bug!("invalid direction")
            };
        { direction = direction, steps = steps }
    };
    lines
    |> List.map(parseLine >> getInstruction)
    |> dugLagoonSize

def main(): Result[IOError, Unit] \ IO = 
    forM (lines <- Files.readLines("input.txt")) yield {
        println("Part 1:");
        solvePart1(lines) |> println;
        println("Part 2:");
        solvePart2(lines) |> println
    }
