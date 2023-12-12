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
    Assert.eq(374, solvePart1(sample()))

def solvePart1(lines: List[String]): Int32 =
    let universe = List.map(String.toList, lines);
    let countGalaxies = List.count(Eq.eq('#'));
    let galaxiesByRow = universe |> List.map(countGalaxies);
    let galaxiesByCol = universe |> List.transpose |> List.map(countGalaxies);

    def totalConnectionLength(galCounts: List[Int32]): Int32 = {
        List.zipWith3(
            (here, nLeft, nRight) -> if (here == 0) nLeft*nRight*2 else nLeft*nRight,
            galCounts,
            List.scanLeft(Add.add, 0, galCounts),
            List.scanRight(Add.add, 0, galCounts)
        )
        |> List.sum
    };

    totalConnectionLength(galaxiesByRow) + totalConnectionLength(galaxiesByCol)

def main(): Result[IOError, Unit] \ IO =
    forM(
        lines <- Files.readLines("input.txt")
    ) yield {
        println("Part 1:");
        lines |> solvePart1 |> println
    }
