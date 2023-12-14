def sample(): List[String] =
    "O....#...." ::
    "O.OO#....#" ::
    ".....##..." ::
    "OO.#O....O" ::
    ".O.....O#." ::
    "O.#..O.#.#" ::
    "..O..#O..O" ::
    ".......O.." ::
    "#....###.." ::
    "#OO..#...." :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        136,
        solvePart1(sample())
    )

def solvePart1(lines: List[String]): Int32 = region rc {
    let height = List.length(lines);
    let load = ref 0 @ rc;
    let platform = ref 0 @ rc;
    lines
    |> List.map(String.toArray(rc))
    |> List.toArray(rc)
    |> Array.transpose(rc)
    |> Array.forEach(col -> {
        platform := 0;
        col |> Array.forEachWithIndex((i, ch) -> {
            match ch {
                case 'O' => {
                    let addedLoad = height - deref platform;
                    platform := deref platform + 1;
                    load := deref load + addedLoad
                }
                case '#' => {
                    platform := i + 1
                }
                case '.' => ()
                case _ => bug!("unknown char ${ch}")
            }
        })
    });
    deref load
}

def main(): Result[IOError, Unit] \ IO =
    forM (lines <- Files.readLines("input.txt"))
    yield {
        println("Part 1:");
        lines |> solvePart1 |> println
    }
