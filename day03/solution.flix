def sample(): List[String] =
    "467..114.." ::
    "...*......" ::
    "..35..633." ::
    "......#..." ::
    "617*......" ::
    ".....+.58." ::
    "..592....." ::
    "......755." ::
    "...$.*...." ::
    ".664.598.." :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        4361,
        sample() |> solvePart1
    )

@Test
def test_symbolPositions(): Bool =
    Assert.eq(
        Set#{
            Position.Position(0, 3),
            Position.Position(1, 0),
            Position.Position(2, 2),
            Position.Position(2, 5)
        },
        List#{"012*45", "#12..5", "01$34#"} |> findSymbolPositions
    )

@Test
def test_splitWhen(): Bool =
    Assert.eq(
        List#{List#{1, 2}, List#{4, 5}},
        splitWhen((a, b) -> b - a != 1, List#{1, 2, 4, 5})
    )

@Test
def test_findPartNumbers(): Bool =
    use Position.Position;
    let partNumbers = List#{".123..", "4...56", "......"} |> findPartNumbers;
    Assert.eq(
        List#{123, 4, 56},
        List.map(p -> p.value, partNumbers)
    )

enum Position(Int32, Int32) with Eq, Order, ToString

type alias PartNumber = { value = Int32, positions = List[Position] }



def findSymbolPositions(lines: List[String]): Set[Position] = region rc {
    use Position.Position;
    let positions = MutSet.new(rc);
    List.enumerator(rc, lines)
    |> Iterator.forEach(match (r, line) -> {
        String.enumerator(rc, line)
        |> Iterator.forEach(w -> {
            match w {
                case (_, '.') => ()
                case (_, char) if Char.isAsciiDigit(char) => ()
                case (c, _) => {
                    MutSet.add!(Position(r, c), positions)
                }
            }
        })
    });
    positions |> MutSet.toSet
}

def splitWhen(shouldSplitBetween: (a -> (a -> Bool)), list: List[a]): List[List[a]] = region rc {
    let result = MutList.new(rc);
    let accumList = ref MutList.new(rc) @ rc;
    foreach (v <- list) {
        match MutList.last(deref accumList) {
            case Some(prev) if shouldSplitBetween(prev, v) => {
                MutList.push!(MutList.toList(deref accumList), result);
                accumList := MutList.new(rc)
            }
            case _ => ()
        };
        MutList.push!(v, deref accumList)
    };
    MutList.push!(MutList.toList(deref accumList), result);
    MutList.toList(result)
}

mod Line {
    use Position.Position;
    pub def findPartNumbers(rowNumber: Int32, line: String): List[PartNumber] =
        String.findIndices(Char.isAsciiDigit, line)
        |> splitWhen((a, b) -> b - a != 1)
        |> List.filterMap(cols -> {
            let positions = List.map(col -> Position(rowNumber, col), cols);
            match (List.head(positions), List.last(positions)) {
                case (Some(Position(_, left)), Some(Position(_, right))) => {
                    let slice = String.slice({start = left}, {end = right + 1}, line);
                    match Int32.parse(10, slice) {
                        case Ok(val) => Some({value = val, positions = positions})
                        case Err(_) => bug!("unable to parse int32")
                    }
                }
                case _ => None
            }
        })
}

def findPartNumbers(lines: List[String]): List[PartNumber] = region rc {
    List.enumerator(rc, lines)
    |> Iterator.toList
    |> List.flatMap(match (r, line) -> {
        Line.findPartNumbers(r, line)
    })
}

def neighbors(pos: Position): Set[Position] =
    use Position.Position;
    match pos {
        case Position(x, y) => Set#{
            Position(x, y-1),
            Position(x, y+1),
            Position(x-1, y-1),
            Position(x-1, y),
            Position(x-1, y+1),
            Position(x+1, y-1),
            Position(x+1, y),
            Position(x+1, y+1)
        }
    }

def solvePart1(lines: List[String]): Int32 =
    let partNumbers = findPartNumbers(lines);
    let symbolPositions = findSymbolPositions(lines);
    partNumbers
    |> List.filterMap(match { value = value, positions = positions } -> {
        if (List.forAll(
            digitPos -> Set.isEmpty(Set.intersection(neighbors(digitPos), symbolPositions)),
            positions
        ))
            None
        else
            Some(value)
    })
    |> List.sum


def main(): Unit \ IO =
    let args = Environment.getArgs();
    let file = List.head(args) |> Option.getWithDefault("input.txt");
    match Files.readLines(file) {
        case Ok(lines) => {
            lines |> solvePart1 |> ToString.toString |> println
        }
        case Err(e) =>  System.StdErr.println(ToString.toString(e))
    }
