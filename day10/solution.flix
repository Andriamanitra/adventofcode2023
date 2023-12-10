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
    use Monad.{>>=};
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

def walkFrom(startChar: Char, grid: List[String]): List[(Int32, Int32)] =
    let startPos =
        List.zipWithIndex(grid)
        |> List.filterMap(match (r, line) -> {
            match String.findIndexOfLeft(Eq.eq(startChar), line) {
                case Some(c) => Some((r, c))
                case None => None
            }
        });
    match startPos {
        case (r, c) :: Nil => walk(grid, r, c)
        case Nil => bug!("no S in input")
        case xs => {
            let len = List.length(xs);
            bug!("too many starting points (${len})")
        }
    }

def solvePart1(grid: List[String]): Int32 =
    let pathLength = List.length(walkFrom('S', grid));
    pathLength / 2

def main(): Result[String, Unit] \ IO =
    use MayFail.withErrorMsg;
    let fname = "input.txt";
    forM (
        lines <- Files.readLines(fname) `withErrorMsg` "Unable to read ${fname}"
    ) yield {
        println("Part 1:");
        lines |> solvePart1 |> println
    }
