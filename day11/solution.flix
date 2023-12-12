def sample(): List[String] =
    "...#......" ::
    ".......#.." ::
    "#........." ::
    ".........." ::
    "......#..." ::
    ".#........" ::
    ".........#" ::
    ".........." ::
    ".......#.." ::
    "#...#....." :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(374i64, solvePart1(sample()))

@Test
def test_solvePart2(): Bool =
    Assert.eq(
        (1030i64, 8410i64),
        (solveWith({multiplier = 10}, sample()), solveWith({multiplier = 100}, sample()))
    )

def solveWith(opts: {multiplier = Int32}, lines: List[String]): Int64 =
    let multiplier = Int32.toInt64(opts.multiplier);
    let universe = List.map(String.toList, lines);
    let countGalaxies = List.count(Eq.eq('#')) >> Int32.toInt64;
    let galaxiesByRow = universe |> List.map(countGalaxies);
    let galaxiesByCol = universe |> List.transpose |> List.map(countGalaxies);

    def totalConnectionLength(galCounts: List[Int64]): Int64 = {
        List.zipWith3(
            (here, nLeft, nRight) -> {
                if (here == 0i64) nLeft * nRight * multiplier
                else nLeft * nRight
            },
            galCounts,
            List.scanLeft(Add.add, 0i64, galCounts),
            List.scanRight(Add.add, 0i64, galCounts)
        )
        |> List.foldLeft(Add.add, 0i64)
    };

    totalConnectionLength(galaxiesByRow) + totalConnectionLength(galaxiesByCol)

def solvePart1(lines: List[String]): Int64 =
    lines |> solveWith({multiplier = 2})

def solvePart2(lines: List[String]): Int64 =
    lines |> solveWith({multiplier = 1_000_000})

def main(): Result[IOError, Unit] \ IO =
    forM(
        lines <- Files.readLines("input.txt")
    ) yield {
        println("Part 1:");
        lines |> solvePart1 |> println;
        println("Part 2:");
        lines |> solvePart2 |> println
    }
