def sample(): List[String] =
    "32T3K 765" ::
    "T55J5 684" ::
    "KK677 28" ::
    "KTJJT 220" ::
    "QQQJA 483" :: Nil

@Test
def test_solvePart1(): Bool =
    Assert.eq(
        Ok(6440),
        sample() |> parseInput(Hand.parse) |> Result.map(countWinnings)
    )

@Test
def test_solvePart2(): Bool =
    Assert.eq(
        Ok(5905),
        sample() |> parseInput(Hand.parseWithJokers) |> Result.map(countWinnings)
    )


enum Hand with ToString, Eq {
    case FiveOfAKind(String)
    case FourOfAKind(String)
    case FullHouse(String)
    case ThreeOfAKind(String)
    case TwoPair(String)
    case OnePair(String)
    case HighCard(String)
}

mod Hand {
    pub def parse(s: String): Result[String, Hand] =
        let cardCounts =
            String.toList(s)
            |> List.groupBy(Eq.eq)
            |> List.map(List.length)
            |> List.sortBy(Neg.neg);
        Hand.countsToHand(cardCounts, s)

    pub def parseWithJokers(origStr: String): Result[String, Hand] =
        let numJokers = origStr |> String.countSubstring({substr = "J"});
        let cardCountsWithoutJokers =
            String.toList(origStr)
            |> List.filter(Eq.neq('J'))
            |> List.groupBy(Eq.eq)
            |> List.map(List.length)
            |> List.sortBy(Neg.neg);
        let cardCounts = match cardCountsWithoutJokers {
            case Cons(n, rest) => Cons(n + numJokers, rest)
            case Nil => Cons(5, Nil)
        };
        let strWithJokersReplaced = String.replaceChar({from = 'J'}, {to = '*'}, origStr);
        Hand.countsToHand(cardCounts, strWithJokersReplaced)

    pub def cardValues(hand: String): List[Int32] =
        String.toList(hand)
        |> List.map(c -> match c {
            case '*' => 1 // used for jokers in part 2
            case '2' => 2
            case '3' => 3
            case '4' => 4
            case '5' => 5
            case '6' => 6
            case '7' => 7
            case '8' => 8
            case '9' => 9
            case 'T' => 10
            case 'J' => 11
            case 'Q' => 12
            case 'K' => 13
            case 'A' => 14
            case _ => bug!("invalid card ${c}")
        })

    def countsToHand(counts: List[Int32], s: String): Result[String, Hand] =
        match counts {
            case 5::Nil             => Ok(Hand.FiveOfAKind(s))
            case 4::1::Nil          => Ok(Hand.FourOfAKind(s))
            case 3::2::Nil          => Ok(Hand.FullHouse(s))
            case 3::1::1::Nil       => Ok(Hand.ThreeOfAKind(s))
            case 2::2::1::Nil       => Ok(Hand.TwoPair(s))
            case 2::1::1::1::Nil    => Ok(Hand.OnePair(s))
            case 1::1::1::1::1::Nil => Ok(HighCard(s))
            case _ => Err("invalid hand '${s}'")
        }
}

instance Order[Hand] {
    pub def compare(x: Hand, y: Hand): Comparison =
        match (x, y) {
            case (Hand.FiveOfAKind(xh), Hand.FiveOfAKind(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.FiveOfAKind(_), _) => Comparison.GreaterThan
            case (_, Hand.FiveOfAKind(_)) => Comparison.LessThan
            
            case (Hand.FourOfAKind(xh), Hand.FourOfAKind(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.FourOfAKind(_), _) => Comparison.GreaterThan
            case (_, Hand.FourOfAKind(_)) => Comparison.LessThan

            case (Hand.FullHouse(xh), Hand.FullHouse(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.FullHouse(_), _) => Comparison.GreaterThan
            case (_, Hand.FullHouse(_)) => Comparison.LessThan

            case (Hand.ThreeOfAKind(xh), Hand.ThreeOfAKind(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.ThreeOfAKind(_), _) => Comparison.GreaterThan
            case (_, Hand.ThreeOfAKind(_)) => Comparison.LessThan

            case (Hand.TwoPair(xh), Hand.TwoPair(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.TwoPair(_), _) => Comparison.GreaterThan
            case (_, Hand.TwoPair(_)) => Comparison.LessThan

            case (Hand.OnePair(xh), Hand.OnePair(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
            case (Hand.OnePair(_), _) => Comparison.GreaterThan
            case (_, Hand.OnePair(_)) => Comparison.LessThan

            case (Hand.HighCard(xh), Hand.HighCard(yh)) => Order.compare(Hand.cardValues(xh), Hand.cardValues(yh))
        }
}

def parseInput(parseFn: String -> Result[String, Hand], lines: List[String]): Result[String, List[(Hand, Int32)]] =
    lines
    |> List.map(line -> match String.words(line) {
        case handStr :: bidStr :: Nil => {
            forM (
                hand <- parseFn(handStr);
                bid <- Int32.fromString(bidStr) |> Option.toOk("Unable to parse bid '${bidStr}'")
            ) yield (hand, bid)
        }
        case _ => Err("unable to parse line '${line}'")
    })
    |> List.sequence

def countWinnings(hands: List[(Hand, Int32)]): Int32 =
    hands
    |> List.sort
    |> List.map(snd)
    |> List.zipWith(Mul.mul, List.range(1, List.length(hands)+1))
    |> List.sum

def withErrorMsg(res: Result[_, t], errmsg: String): Result[String, t] =
    match res {
        case Ok(x) => Ok(x)
        case Err(_) => Err(errmsg)
    }

def main(): Result[String, Unit] \ IO =
    let filename = "input.txt";
    forM (
        lines <- Files.readLines(filename) `withErrorMsg` "unable to read from file '${filename}'";
        handsPart1 <- parseInput(Hand.parse, lines);
        handsPart2 <- parseInput(Hand.parseWithJokers, lines)
    ) yield {
        println("Part 1:");
        handsPart1 |> countWinnings |> ToString.toString |> println;
        println("Part 2:");
        handsPart2 |> countWinnings |> ToString.toString |> println
    }
