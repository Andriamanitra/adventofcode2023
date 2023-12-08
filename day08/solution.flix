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

/// Greatest common divisor
def gcd(a: Int64, b: Int64): Int64 =
    match (a <=> b) {
        case Comparison.EqualTo => a
        case Comparison.GreaterThan => gcd(a - b, b)
        case Comparison.LessThan => gcd(b - a, a)
    }

/// Lowest common denominator
def lcm(a: Int64, b: Int64): Int64 =
    if (a == 0i64 or b == 0i64)
        0i64
    else
        a * b / gcd(b, a)

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

def solvePart2(input: Input): Int64 =
    let directionsLength = String.length(input.directions);
    let directions =
        input.directions
        |> String.toList
        |> List.zipWithIndex
        |> List.toMap;

    def next(node: String, steps: Int32): String = {
        match Map.get(node, input.network) {
            case Some((left, right)) => {
                let dir = Map.get(Int32.mod(steps, directionsLength), directions);
                if (dir == Some('L')) left else right
            }
            case None => bug!("graph is not connected")
        }
    };

    def walk(node: String, steps: Int32): (String, Int32) = {
        let nxt = next(node, steps);
        if (String.endsWith({suffix = "Z"}, nxt)) {
            (nxt, steps + 1)
        } else {
            walk(nxt, steps + 1)
        }
    };

    let startingPoints =
        Map.keysOf(input.network)
        |> Set.toList
        |> List.filter(String.endsWith({suffix = "A"}));

    // not entirely sure *why* but from looking at the results it turned out that
    // the number of steps from xxA->yyZ is the exact same as
    // the number of steps from yyZ->yyZ for all the paths
    // so we can use that as the cycle length.
    // I lack the math skills required to figure out if this works for all inputs
    // but it worked for mine so ¯\_(ツ)_/¯
    let cycleLengths =
        startingPoints
        |> List.map(a -> {
            let (_z, steps) = walk(a, 0);
            steps
        });

    cycleLengths
    |> List.map(Int32.toInt64)
    |> List.foldLeft(lcm, 1i64)

def main(): Result[String, Unit] \ IO =
    let filename = "input.txt";
    forM (
        lines <- Files.readLines(filename) `withErrorMsg` "unable to read ${filename}";
        input <- readInput(lines)
    ) yield {
        println("Part 1:");
        solvePart1(input) |> println;
        println("Part 2:");
        solvePart2(input) |> println
    }
