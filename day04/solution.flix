def sample(): List[String] =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" ::
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19" ::
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1" ::
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83" ::
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36" ::
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(13, solvePart1(sample()))

@Test
def test_solvePart2(): Bool =
    Assert.eq(30, solvePart2(sample()))

type alias Card = {
    name = String,
    myNumbers = List[Int32],
    winningNumbers = List[Int32]
}

def toCard(s: String): Card =
    def toInts(words: List[String]): List[Int32] =
        words
        |> List.filter(word -> not String.isWhiteSpace(word))
        |> List.map(word -> match Int32.parse(10, word) {
            case Ok(n) => n,
            case Err(_) => bug!("invalid number '${word}' on line '${s}'")
        });
    match (String.indexOf({ substr = ": " }, s)) {
        case Some(colonIndex) => {
            let name = String.sliceLeft({end = colonIndex}, s);
            let rest = String.sliceRight({start = colonIndex + 2}, s);
            let words = String.split({ regex = "\\s+" }, rest);
            match List.span(Eq.neq("|"), words) {
                case (winningNums, "|" :: myNums) => {
                    {
                        name = name,
                        myNumbers = toInts(myNums),
                        winningNumbers = toInts(winningNums)
                    }
                }
                case _ => bug!("invalid card: '${s}'")
            }
        }
        case None => bug!("invalid card: '${s}'")
    }

def winningNums(card: Card): List[Int32] =
    let winning = List.toSet(card.winningNumbers);
    let mine = List.toSet(card.myNumbers);
    Set.intersection(winning, mine)
    |> Set.toList


def solvePart1(lines: List[String]): Int32 =
    lines
    |> List.map(toCard >> winningNums >> List.length)
    |> List.map(numWinning -> Int32.leftShift(1, numWinning - 1))
    |> List.sum

def solvePart2(lines: List[String]): Int32 = region rc {
    let cards = List.map(toCard, lines);
    let cardCounts = Array.init(rc, constant(1), List.length(cards));

    cards |> List.forEachWithIndex((i, card) -> {
        foreach (j <- List.range(i + 1, i + 1 + List.length(winningNums(card)))) {
            let newValue = Array.get(j, cardCounts) + Array.get(i, cardCounts);
            Array.put(newValue, j, cardCounts)
        }
    });

    Array.sum(cardCounts)
}

def main(): Unit \ IO =
    match Files.readLines("input.txt") {
        case Ok(lines) => {
            println("Part 1:");
            solvePart1(lines) |> ToString.toString |> println;
            println("Part 2:");
            solvePart2(lines) |> ToString.toString |> println
        }
        case Err(_) => System.StdErr.println("unable to read file")
    }
