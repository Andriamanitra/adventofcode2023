@Test
def test_solvePart1(): Bool =
    Assert.eq(
        114,
        solvePart1(
            List#{0, 3, 6, 9, 12, 15} ::
            List#{1, 3, 6, 10, 15, 21} ::
            List#{10, 13, 16, 21, 30, 45} :: Nil
        )
    )

@Test
def test_solvePart2(): Bool =
    Assert.eq(
        2,
        solvePart2(
            List#{0, 3, 6, 9, 12, 15} ::
            List#{1, 3, 6, 10, 15, 21} ::
            List#{10, 13, 16, 21, 30, 45} :: Nil
        )
    )

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

def readInput(lines: List[String]): Option[List[List[Int32]]] =
    lines
    |> List.map(String.words >> List.map(Int32.fromString) >> List.sequence)
    |> List.sequence

def makePyramid(nums: List[Int32]): List[List[Int32]] =
    match nums {
        case x :: xs => nums :: makePyramid(List.zipWith(Sub.sub, xs, x::xs))
        case Nil => Nil
    }

def nextNumInSeq(nums: List[Int32]): Int32 =
    makePyramid(nums)
    |> List.filterMap(List.last)
    |> List.sum

def prevNumInSeq(nums: List[Int32]): Int32 =
    let isEven = Int32.bitwiseAnd(1) >> Eq.eq(0);
    makePyramid(nums)
    |> List.filterMap(List.head)
    |> List.zipWithIndex
    |> List.sumWith(match (i, v) -> if (isEven(i)) v else -v)

def solvePart1(input: List[List[Int32]]): Int32 =
    List.sumWith(nextNumInSeq, input)

def solvePart2(input: List[List[Int32]]): Int32 =
    List.sumWith(prevNumInSeq, input)

def main(): Result[String, Unit] \ IO =
    use MayFail.withErrorMsg;
    let fname = List.head(Environment.getArgs()) |> Option.getWithDefault("input.txt");
    forM (
        lines <- Files.readLines(fname) `withErrorMsg` "Unable to read ${fname}";
        input <- readInput(lines) `withErrorMsg` "Invalid input"
    ) yield {
        println("Part 1:");
        input |> solvePart1 |> println;
        println("Part 2:");
        input |> solvePart2 |> println
    }
