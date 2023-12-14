
def checkReflection(grid: List[String], idx: Int32): Bool =
    let (left, right) = List.splitAt(idx, grid);
    List.zipWith(Eq.eq, List.reverse(left), right)
    |> List.forAll(identity)

def solvePattern(patt: List[String]): (Int32, Int32) =
    let topBot =
        List.range(1, List.length(patt))
        |> List.filter(i -> checkReflection(patt, i));
    let pattTransposed =
        patt
        |> List.map(String.toList)
        |> List.transpose
        |> List.map(List.join(""));
    let leftRight =
        List.range(1, List.length(pattTransposed))
        |> List.filter(i -> checkReflection(pattTransposed, i));
    match (topBot, leftRight) {
        case (x::_, _) => (x, 0)
        case (_, y::_) => (0, y)
        case _ => bug!("wtf")
    }

def solvePart1(input: List[List[String]]): Int32 =
    let (nRowsAbove, nColsLeft) =
        input
        |> List.map(solvePattern)
        |> List.foldLeft(
            (acc, x) -> {
                let (rTotal, cTotal) = acc;
                let (r, c) = x;
                (rTotal+r, cTotal+c)
            },
            (0,0)
        );
    100 * nRowsAbove + nColsLeft

def main(): Result[IOError, Unit] \ IO =
    forM (
        contents <- Files.read("input.txt")
    ) yield {
        let patterns =
            String.splitOn({ substr = "\n\n" }, contents)
            |> List.map(String.lines);
        solvePart1(patterns) |> println
    }
