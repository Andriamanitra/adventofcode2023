enum Direction {
    case Up
    case Down
    case Left
    case Right
}

def polygonArea(vertices: List[(Int32, Int32)]): Int32 =
    // Calculate area of a polygon using the trapezoid formula
    List.zipWith(
        (p0, p1) -> {
            let (x0, y0) = p0;
            let (x1, y1) = p1;
            (y0+y1) * (x1-x0)
        },
        vertices,
        List.drop(1, vertices)
    )
    |> List.sum / 2

def parseLine(line: String): (Direction, Int32, String) =
    match String.words(line) {
        case d :: n :: s :: Nil => {
            let steps =
                match Int32.fromString(n) {
                    case Some(steps) => steps
                    case None => bug!("Invalid Int32 ${n}")
                };
            let direction =
                match d {
                    case "D" => Direction.Down
                    case "U" => Direction.Up
                    case "L" => Direction.Left
                    case "R" => Direction.Right
                    case _ => bug!("Invalid direction ${d}")
                };
            (direction, steps, s)
        }
        case _ => bug!("invalid line ${line}")
    }

def solvePart1(lines: List[String]): Int32 =
    let parsedLines = List.map(parseLine, lines);
    let vertices =
        parsedLines
        |> List.scanLeft((acc, instruction) -> {
            let (direction, steps, _) = instruction;
            let (x, y) = acc; 
            match direction {
                case Direction.Up => (x, y+steps)
                case Direction.Down => (x, y-steps)
                case Direction.Left => (x-steps, y)
                case Direction.Right => (x+steps, y)
            }
        }, (0, 0));
    // we need to account for the width of the path: for each cube excavated
    // along the path 2/4 of it is not counted, except for 4 of the corners
    // which account for the +1
    let extra =
        parsedLines
        |> List.map(x -> {let (_, v, _) = x; v})
        |> List.sum / 2 + 1;

    polygonArea(vertices) + extra

def main(): Result[IOError, Unit] \ IO = 
    forM (
        lines <- Files.readLines("input.txt")
    ) yield {
        println("Part 1:");
        solvePart1(lines) |> println
    }
