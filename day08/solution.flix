type alias Input = {directions = String, network = Network}
type alias Network = Map[String, (String, String)]

def parseRule(line: String): Result[String, (String, (String, String))] =
    match String.split({regex = "[^A-Z]+"}, line) {
        case node :: left :: right :: Nil => Ok((node, (left, right)))
        case _ => Err("Invalid rule: '${line}'")
    }

def readInput(lines: List[String]): Result[String, Input] =
    match lines {
        case directions :: _ :: rest => {
            forM (lst <- List.map(parseRule, rest) |> List.sequence)
                yield {directions = directions, network = List.toMap(lst)}
        }
        case _ => Err("Invalid input")
    }

def withErrorMsg(res: Result[_, t], errmsg: String): Result[String, t] =
    match res {
        case Ok(v) => Ok(v)
        case Err(_) => Err(errmsg)
    }

def solvePart1(input: Input): Int32 =
    let target = "ZZZ";
    let directionsLength = String.length(input.directions);
    let directions =
        input.directions
        |> String.toList
        |> List.zipWithIndex
        |> List.toMap;
    def walk(node: String, steps: Int32): Int32 = {
        if (node == target)
            steps
        else match Map.get(node, input.network) {
            case Some((left, right)) => {
                let dir = Map.get(Int32.mod(steps, directionsLength), directions);
                let next = if (dir == Some('L')) left else right;
                walk(next, steps + 1)
            }
            case None => -1
        }
    };
    walk("AAA", 0)

def main(): Result[String, Unit] \ IO =
    let filename = "input.txt";
    forM (
        lines <- Files.readLines(filename) `withErrorMsg` "unable to read ${filename}";
        input <- readInput(lines)
    ) yield {
        println("Part 1:");
        solvePart1(input) |> println
    }
