def sample(): String =
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        35i64,
        sample() |> parseInput |> solvePart1
    )

type alias Section = List[{dest = Int64, src = Int64, length = Int64}]

def readSpacedInts(s: String): List[Int64] =
    String.splitOn({substr = " "}, s)
    |> List.map(Int64.parse(10) >> Result.getWithDefault(0i64))

def parseSection(s: String): Section =
    match String.splitOn({substr = " map:\n"}, s) {
        case _header :: body :: Nil => {
            body
            |> String.splitOn({substr = "\n"})
            |> List.map(line -> {
                match readSpacedInts(line) {
                    case dest :: src :: len :: Nil => {
                        {
                            dest = dest,
                            src = src,
                            length = len
                        }
                    }
                    case _ => bug!("invalid section")
                }
            })
        }
        case _ => bug!("invalid section")
    }

def parseInput(s: String): {seeds = List[Int64], sections = List[Section]} =
    match String.splitOn({substr = "\n\n"}, String.trim(s)) {
        case seedStr :: rest => {
            let seeds = readSpacedInts(seedStr) |> List.drop(1);
            let sections = List.map(parseSection, rest);
            {seeds = seeds, sections = sections}
        }
        case _ => bug!("invalid input")
    }

def applySection(seed: Int64, section: Section): Int64 =
    let matchingRule =
        List.find(match {src = src, length = length | _} -> src <= seed and seed < src + length, section);
    match matchingRule {
        case Some({dest = dest, src = src | _}) => seed - src + dest
        case None => seed
    }

def solvePart1(input: {seeds = List[Int64], sections = List[Section]}): Int64 =
    let {seeds = seeds, sections = sections} = input;
    seeds
    |> List.map(seed -> List.foldLeft(applySection, seed, sections))
    |> List.minimum
    |> Option.getWithDefault(0i64)

def main(): Unit \ IO =
    match Files.read("input.txt") {
        case Ok(s) => {
            let input = parseInput(s);
            println("Part 1:");
            input |> solvePart1 |> ToString.toString |> println;
            println("Part 2:");
            println("(is too hard)")
        }
        case Err(_) => System.StdErr.println("unable to read file")
    }
