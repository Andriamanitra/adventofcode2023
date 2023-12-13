def example(): List[ConditionRecord] =
    { row = "???.###", lengths = List#{1,1,3} } ::
    { row = ".??..??...?##.", lengths = List#{1,1,3} } ::
    { row = "?#?#?#?#?#?#?#?", lengths = List#{1,3,1,6} } ::
    { row = "????.#...#...", lengths = List#{4,1,1} } ::
    { row = "????.######..#####.", lengths = List#{1,6,5} } ::
    { row = "?###????????", lengths = List#{3,2,1} } :: Nil

@Test
def test_solveRecord(): Bool =
    Assert.eq(
        List#{1,4,1,1,4,10},
        List.map(solveRecord, example())
    )

type alias ConditionRecord = {
    row = String,
    lengths = List[Int32]
}

pub trait MayFail[m: Type -> Type] {
    pub def withErrorMsg(x: m[t], errmsg: String): Result[String, t]
}

instance MayFail[Result[e]] {
    pub def withErrorMsg(x: Result[e, t], errmsg: String): Result[String, t] =
        match x {
            case Ok(v) => Ok(v)
            case Err(_) => Err(errmsg)
        }
}

instance MayFail[Option] {
    pub def withErrorMsg(x: Option[t], errmsg: String): Result[String, t] =
        match x {
            case Some(v) => Ok(v)
            case None => Err(errmsg)
        }
}

def parseLine(line: String): Result[String, ConditionRecord] =
    match String.words(line) {
        case s :: nums :: Nil => {
            let optLengths =
                nums
                |> String.splitOn({ substr = "," })
                |> List.map(Int32.fromString)
                |> List.sequence;
            match optLengths {
                case Some(lengths) => Ok({ row = s, lengths = lengths })
                case _ => Err("Invalid input: '${line}'")
            }
        }
        case _ => Err("Invalid input: '${line}'")
    }

def canBeSpring(c: Char): Bool =
    c == '#' or c == '?'

def solveRecord(record: ConditionRecord): Int32 = 
    def recur(st: List[Char], lengths: List[Int32]): Int32 = {
        match (st, lengths) {
            case (Nil, Nil) => 1
            case ('.'::cs, xs) => recur(cs, xs)
            case ('?'::cs, Nil) => recur(cs, Nil)
            case (_, Nil) => 0
            case (Nil, _) => 0
            case (cs, xs) if (List.sum(xs) + List.length(xs) - 1) > List.length(cs) => 0
            case ('#'::_, x::xs) => {
                let (spring, rest) = List.splitAt(x, st);
                let validSpring = List.length(spring) == x and List.forAll(canBeSpring, spring);
                match rest {
                    case Nil if validSpring and List.isEmpty(xs) => 1
                    case '.'::ys if validSpring => recur(ys, xs)
                    case '?'::ys if validSpring => recur(ys, xs)
                    case _ => 0
                }
            }
            case ('?'::cs, x::xs) => {
                let (spring, rest) = List.splitAt(x, st);
                let validSpring = List.length(spring) == x and List.forAll(canBeSpring, spring);
                if (validSpring) {
                    match rest {
                        case Nil if List.isEmpty(xs) => 1
                        case '.'::ys => recur(ys, xs)
                        case '?'::ys => recur(ys, xs)
                        case _ => 0
                    } + recur(cs, x::xs)
                } else recur(cs, x::xs)
            }
            case _ => bug!("unhandled case ${st}")
        }
    };
    recur(String.toList(record.row), record.lengths)

def solvePart1(input: List[ConditionRecord]): Int32 =
    input
    |> List.map(solveRecord)
    |> List.sum

def solvePart2(input: List[ConditionRecord]): Int32 =
    input
    |> List.map(match {row = r, lengths = ls} -> {
        row = r ++ "?" ++ r ++ "?" ++ r ++ "?" ++ r ++ "?" ++ r,
        lengths = ls ::: ls ::: ls ::: ls ::: ls
    })
    |> solvePart1

def main(): Result[String, Unit] \ IO =
    use MayFail.withErrorMsg;
    let fname = "input.txt";
    forM (
        lines <- Files.readLines(fname) `withErrorMsg` "unable to read ${fname}";
        input <- List.map(parseLine, lines) |> List.sequence
    ) yield {
        println("Part 1:");
        input |> solvePart1 |> println;
        println("Part 2:");
        println("(is too slow)")
        // input |> solvePart2 |> println
    }
