use Monad.{>>=};
use Direction.{Up, Down, Left, Right};

def sample(): List[String] =
    "..F7." ::
    ".FJ|." ::
    "SJ.L7" ::
    "|F--J" ::
    "LJ..." :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(8, solvePart1(sample()))

@Test
def test_solvePart2(): Bool =
    let grid =
        "FF7FSF7F7F7F7F7F---7" ::
        "L|LJ||||||||||||F--J" ::
        "FL-7LJLJ||||||LJL-77" ::
        "F--JF--7||LJLJIF7FJ-" ::
        "L---JF-JLJIIIIFJLJJ7" ::
        "|F|F-JF---7IIIL7L|7|" ::
        "|FFJF7L7F-JF7IIL---7" ::
        "7-L-JL7||F7|L7F-7F7|" ::
        "L.L7LFJ|||||FJL7||LJ" ::
        "L7JLJL-JLJLJL--JLJ.L" :: Nil;
    Assert.eq(10, solvePart2(grid))

@Test
def test_solvePart2_2(): Bool =
    let grid =
        "OF----7F7F7F7F-7OOOO" ::
        "O|F--7||||||||FJOOOO" ::
        "O||OFJ||||||||L7OOOO" ::
        "FJL7L7LJLJ||LJIL-7OO" ::
        "L--JOL7IIILJS7F-7L7O" ::
        "OOOOF-JIIF7FJ|L7L7L7" ::
        "OOOOL7IF7||L7|IL7L7|" ::
        "OOOOO|FJLJ|FJ|F7|OLJ" ::
        "OOOOFJL-7O||O||||OOO" ::
        "OOOOL---JOLJOLJLJOOO" :: Nil;

    Assert.eq(8, solvePart2(grid))

enum Direction with ToString {
    case Up
    case Down
    case Left
    case Right
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

def go(ch: Char, dir: Direction): Direction =
    match (ch, dir) {
        case ('|', Up) => Up
        case ('|', Down) => Down
        case ('-', Left) => Left
        case ('-', Right) => Right
        case ('L', Down) => Right
        case ('L', Left) => Up
        case ('J', Down) => Left
        case ('J', Right) => Up
        case ('7', Up) => Left
        case ('7', Right) => Down
        case ('F', Up) => Right
        case ('F', Left) => Down
        case _ => bug!("tried to enter '${ch}' from  ${dir}")
    }

def walk(grid: List[String], startRow: Int32, startCol: Int32): List[(Int32, Int32)] = region rc {
    let arr = List.toArray(rc, grid);
    let path = MutList.new(rc);

    def at(i: Int32, j: Int32): Option[Char] = {
        Array.nth(i, arr) >>= String.nth(j)
    };

    def makePath(r: Int32, c: Int32, d: Direction) = {
        MutList.push!((r, c), path);
        let (nr, nc) = match d {
            case Up => (r-1, c)
            case Down => (r+1, c)
            case Left => (r, c-1)
            case Right => (r, c+1)
        };
        match at(nr, nc) {
            case Some('S') => ()
            case Some(ch) => makePath(nr, nc, go(ch, d))
            case None => bug!("out of bounds")
        }
    };

    let startDirection = match (at(startRow-1, startCol), at(startRow+1, startCol)) {
        case (Some('|'), _) => Up
        case (Some('7'), _) => Up
        case (Some('F'), _) => Up
        case (_, Some('|')) => Down
        case (_, Some('J')) => Down
        case (_, Some('L')) => Down
        case (_, _) => Right
    };

    makePath(startRow, startCol, startDirection);
    MutList.toList(path)
}

def findStart(needle: Char, grid: List[String]): (Int32, Int32) =
    let foundStarts =
        List.zipWithIndex(grid)
        |> List.filterMap(match (r, line) -> {
            match String.findIndexOfLeft(Eq.eq(needle), line) {
                case Some(c) => Some((r, c))
                case None => None
            }
        });
    match foundStarts {
        case (r, c) :: Nil => (r, c)
        case Nil => bug!("no '${needle}' in input")
        case _ => bug!("too many starting points")
    }

def solvePart1(grid: List[String]): Int32 =
    let (startRow, startCol) = findStart('S', grid);
    let pathLength = List.length(walk(grid, startRow, startCol));
    pathLength / 2

def solvePart2(grid: List[String]): Int32 =
    let isOdd = Int32.bitwiseAnd(1) >> Eq.eq(1);
    let (startRow, startCol) = findStart('S', grid);
    let path = walk(grid, startRow, startCol) |> List.toSet;

    let rowAboveS = List.head(List.drop(startRow - 1, grid));
    let charAboveS = rowAboveS >>= String.nth(startCol);

    def connectsUp(ch: Char): Bool = {
        match ch {
            case '|' => true
            case 'L' => true
            case 'J' => true
            case 'S' =>
                match charAboveS {
                    case Some('|') => true
                    case Some('7') => true
                    case Some('F') => true
                    case _ => false
                }
            case _ => false
        }
    };

    def countInside(rowIdx: Int32, row: String): Int32 = {
        String.toList(row)
        |> List.foldLeft2(
            (acc, chIdx, ch) -> {
                let (nLeft, total) = acc;
                if ((rowIdx, chIdx) `Set.memberOf` path)
                    ((if (connectsUp(ch)) nLeft+1 else nLeft), total)
                else
                    (nLeft, (if (isOdd(nLeft)) total+1 else total))
            },
            (0, 0),
            List.range(0, String.length(row))
        )
        |> snd
    };
    grid
    |> List.zipWithIndex
    |> List.sumWith(match (i, row) -> countInside(i, row))

def main(): Result[String, Unit] \ IO =
    use MayFail.withErrorMsg;
    let fname = "input.txt";
    forM (
        lines <- Files.readLines(fname) `withErrorMsg` "Unable to read ${fname}"
    ) yield {
        println("Part 1:");
        lines |> solvePart1 |> println;
        println("Part 2:");
        lines |> solvePart2 |> println
    }
