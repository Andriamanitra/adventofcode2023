def sample(): List[String] =
    "Time:      7  15   30" ::
    "Distance:  9  40  200" :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        288,
        solvePart1({
            times = List#{7, 15, 30},
            distances = List#{9, 40, 200}
        })
    )

type alias Input = { times = List[Int32], distances = List[Int32] }

def parseLine(header: String, line: String): Result[String, List[Int32]] =
    if (String.startsWith({prefix = header}, line))
        String.split({regex = "\\s+"}, line)
        |> List.drop(1)
        |> List.map(Int32.parse(10))
        |> List.sequence
    else
        bug!("invalid input")

def parseInput(lines: List[String]): Result[String, Input] =
    match lines {
        case firstLine :: secondLine :: Nil => {
            forM (
                times <- parseLine("Time:", firstLine);
                dists <- parseLine("Distance:", secondLine)
            ) yield {times = times, distances = dists}
        }
        case _ => Err("invalid input")
    }

def withErrorMsg(res: Result[_, t], errmsg: String): Result[String, t] =
    match res {
        case Ok(x) => Ok(x)
        case Err(_) => Err(errmsg)
    }

def countWaysToBeatTheRecord(t: Int32, recordDistance: Int32): Int32 =
    List.range(1, t)
    |> List.map(charge -> charge * (t - charge))
    |> List.count(Order.less(recordDistance))

def solvePart1(input: Input): Int32 =
    let {times = times, distances = distances} = input;
    List.zipWith(countWaysToBeatTheRecord, times, distances)
    |> List.foldLeft(Mul.mul, 1)

def main(): Result[String, Unit] \ IO =
    forM (
        lines <- Files.readLines("input.txt") `withErrorMsg` "IOError";
        input <- parseInput(lines)
    ) yield {
        input |> solvePart1 |> ToString.toString |> println
    }
