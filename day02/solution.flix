type alias Bag = (Int32, Int32, Int32)
type alias Game = (Int32, List[Bag])

def parseBag(s: String): Bag =
    let colors = s
                 |> String.splitOn({substr = ", "})
                 |> List.map(String.breakOnLeft({substr = " "}))
                 |> List.map(x -> (String.sliceRight({start=1}, snd(x)), Result.getWithDefault(0, Int32.parse(10, fst(x)))))
                 |> List.toMap;
    (
        Map.getWithDefault("red", 0, colors),
        Map.getWithDefault("green", 0, colors),
        Map.getWithDefault("blue", 0, colors)
    )

def parseGame(s: String): Option[Game] =
    match String.splitOn({substr = ": "}, s) {
        case gameTitle :: rest :: Nil => {
            let gameNum = gameTitle
                        |> String.sliceRight({start = 5})
                        |> Int32.parse(10)
                        |> Result.getWithDefault(0);
            let bags = rest
                     |> String.splitOn({substr = "; "})
                     |> List.map(parseBag);
            Some((gameNum, bags))
        }
        case _ => None
    }

def possibleBag(bag: Bag): Bool =
    let (r, g, b) = bag;
    (r <= 12) `Bool.and` lazy (g <= 13) `Bool.and` lazy (b <= 14)

def possibleGame(g: Game): Bool =
    let bags = snd(g);
    List.forAll(possibleBag, bags)

def solvePart1(games: List[Game]): String =
    games
    |> List.filter(possibleGame)
    |> List.map(fst)
    |> List.sum
    |> ToString.toString

def minimumBagPower(game: Game): Int32 =
    let bags = snd(game);
    let (r, g, b) =
        bags
        |> List.foldLeft((acc, bag) -> {
            let (maxR, maxG, maxB) = acc;
            let (r, g, b) = bag;
            (Int32.max(maxR, r), Int32.max(maxG, g), Int32.max(maxB, b))
        }, (0, 0, 0));
    r * g * b
        

def solvePart2(games: List[Game]): String =
    games
    |> List.map(minimumBagPower)
    |> List.sum
    |> ToString.toString

def main(): Unit \ IO =
    let args = Environment.getArgs();
    let file = List.head(args) |> Option.getWithDefault("input.txt");
    match Files.readLines(file) {
        case Ok(lines) => {
            let games = List.filterMap(parseGame, lines);
            println("Part 1:");
            games |> solvePart1 |> println;
            println("Part 2:");
            games |> solvePart2 |> println
        }
        case Err(e) => System.StdErr.println(ToString.toString(e))
    }
