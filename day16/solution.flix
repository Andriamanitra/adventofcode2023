def sample(): List[String] =
    ".|...\\...." ::
    "|.-.\\....." ::
    ".....|-..." ::
    "........|." ::
    ".........." ::
    ".........\\" ::
    "..../.\\\\.." ::
    ".-.-/..|.." ::
    ".|....-|.\\" ::
    "..//.|...." :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(46, sample() |> solvePart1)

enum Direction with Eq, ToString, Order {
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

type alias Beam = (Direction, Int32, Int32)

def solveGrid(rc: Region[r], grid: Array[Array[Char, r], r], initialBeam: Beam): Int32 \ r = {
    use Direction.{Up,Down,Left,Right};
    use Monad.{>>=};
    let visited = MutSet.new(rc);

    def moveBeam(beam: Beam): List[Beam] = {
        let (dir, x, y) = beam;
        let ch = Array.nth(y, grid) >>= Array.nth(x);
        let nextBeams = match (ch, dir) {
            case _ if MutSet.memberOf(beam, visited) => Nil
            case (None, _) => Nil

            case (Some('-'), Up) => {
                (Left, x-1, y) :: (Right, x+1, y) :: Nil
            }
            case (Some('.'), Up) => (Up, x, y-1) :: Nil
            case (Some('|'), Up) => (Up, x, y-1) :: Nil
            case (Some('/'), Up) => (Right, x+1, y) :: Nil
            case (Some('\\'), Up) => (Left, x-1, y) :: Nil

            case (Some('-'), Down) => {
                (Left, x-1, y) :: (Right, x+1, y) :: Nil
            }
            case (Some('.'), Down) => (Down, x, y+1) :: Nil
            case (Some('|'), Down) => (Down, x, y+1) :: Nil
            case (Some('/'), Down) => (Left, x-1, y) :: Nil
            case (Some('\\'), Down) => (Right, x+1, y) :: Nil

            case (Some('|'), Left) => {
                (Up, x, y-1) :: (Down, x, y+1) :: Nil
            }
            case (Some('.'), Left) => (Left, x-1, y) :: Nil
            case (Some('-'), Left) => (Left, x-1, y) :: Nil
            case (Some('/'), Left) => (Down, x, y+1) :: Nil
            case (Some('\\'), Left) => (Up, x, y-1) :: Nil

            case (Some('|'), Right) => {
                (Up, x, y-1) :: (Down, x, y+1) :: Nil
            }
            case (Some('.'), Right) => (Right, x+1, y) :: Nil
            case (Some('-'), Right) => (Right, x+1, y) :: Nil
            case (Some('/'), Right) => (Up, x, y-1) :: Nil
            case (Some('\\'), Right) => (Down, x, y+1) :: Nil
            case asd => bug!("wtf: ${asd}")
        };
        if (List.isEmpty(nextBeams)) {
            nextBeams
        } else {
            MutSet.add!(beam, visited);
            nextBeams
        }
    };

    def recur(beams: List[Beam]): Unit = {
        match beams {
            case Nil => ()
            case x :: xs => {
                recur(moveBeam(x));
                recur(xs)
            }
        }
    };

    recur(initialBeam :: Nil);
    MutSet.transform!(match (_, x, y) -> (Up, x, y), visited);
    MutSet.size(visited)
}

def solvePart1(lines: List[String]): Int32 = region rc {
    lines
    |> List.map(String.toArray(rc))
    |> List.toArray(rc)
    |> grid -> solveGrid(rc, grid, (Direction.Right, 0, 0))
}

def solvePart2(lines: List[String]): Int32 = region rc {
    let grid =
        lines
        |> List.map(String.toArray(rc))
        |> List.toArray(rc);
    let width = match Array.head(grid) {
        case None => bug!("empty grid")
        case Some(firstRow) => Array.length(firstRow)
    };
    let height = Array.length(grid);
    let topBeams =
        List.range(0, width)
        |> List.map(c -> (Direction.Down, c, 0));
    let leftBeams =
        List.range(0, height)
        |> List.map(r -> (Direction.Right, 0, r));
    let rightBeams =
        List.range(0, height)
        |> List.map(r -> (Direction.Left, width-1, r));
    let bottomBeams =
        List.range(0, width)
        |> List.map(c -> (Direction.Up, c, height-1));
    (topBeams ::: leftBeams ::: rightBeams ::: bottomBeams)
    |> List.map(beam -> solveGrid(rc, grid, beam))
    |> List.foldLeft((a, b) -> Int32.max(a, b), 0)
}

def main(): Result[String, Unit] \ IO =
    use MayFail.withErrorMsg;
    let fname = "input.txt";
    forM (lines <- Files.readLines(fname) `withErrorMsg` "Unable to read '${fname}'")
    yield {
        println("Part 1:");
        lines |> solvePart1 |> println;
        println("Part 2:");
        lines |> solvePart2 |> println
    }
