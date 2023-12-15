@Test
def test_hash(): Bool =
    Assert.eq(52, hash("HASH"))

def readInput(fname: String): Result[String, List[String]] \ IO =
    match Files.read(fname) {
        case Ok(s) => Ok(s |> String.trim |> String.splitOn({ substr = "," }))
        case Err(_) => Err("Unable to read '${fname}'")
    }

def hash(s: String): Int32 =
    String.toList(s)
    |> List.map(Char.toBmpCodePoint)
    |> List.foldLeft((acc, x) -> ((acc + x) * 17) `Int32.bitwiseAnd` 255, 0)

def solvePart1(input: List[String]): Int32 =
    List.sumWith(hash, input)

def main(): Result[String, Unit] \ IO =
    forM (input <- readInput("input.txt")) yield {
        input |> solvePart1 |> println
    }
