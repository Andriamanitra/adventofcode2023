def sample(): List[String] =
    "Time:      7  15   30" ::
    "Distance:  9  40  200" :: Nil

@Test
def test_parseInput(): Bool =
    match parseInput(sample()) {
        case Ok({times = times, distances = distances}) => {
            Assert.eq(
                (List#{7, 15, 30}, List#{9, 40, 200}),
                (times, distances)
            )
        }
        case Err(e) => Assert.eq(Ok(), Err(e))
    }

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        288,
        solvePart1({
            times = List#{7, 15, 30},
            distances = List#{9, 40, 200}
        })
    )

@Test
def test_solvePart2(): Bool =
    Assert.eq(
        71503i64,
        solvePart2({
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
        Err("invalid input")

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

def solvePart2(input: Input): Int64 =
    let {times = times, distances = distances} = input;
    let concatNums = List.map(ToString.toString) >> List.join("") >> Int64.fromString >> Option.getWithDefault(0i64);
    let time = concatNums(times);
    let recordDistance = concatNums(distances);

    def recu(acc: Int64, charge: Int64): Int64 = {
        if (charge >= time)
            acc
        else if ((charge * (time - charge)) > recordDistance)
            recu(acc+1i64, charge+1i64)
        else
            recu(acc, charge+1i64)
    };

    recu(0i64, 1i64)

def main(): Result[String, Unit] \ IO =
    forM (
        lines <- Files.readLines("input.txt") `withErrorMsg` "IOError";
        input <- parseInput(lines)
    ) yield {
        println("Part 1:");
        input |> solvePart1 |> ToString.toString |> println;
        println("Part 2:");
        input |> solvePart2 |> ToString.toString |> println
    }
