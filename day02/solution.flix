@Test
def test_part1(): Bool = 
    let lines = 
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ::
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" ::
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" ::
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" ::
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" :: Nil;
    let ans = List.filterMap(parseGame, lines) |> solvePart1;
    Assert.eq("8", ans)

@Test
def test_part2(): Bool =
    let lines = 
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ::
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" ::
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" ::
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" ::
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" :: Nil;
    let ans = List.filterMap(parseGame, lines) |> solvePart2;
    Assert.eq("2286", ans)

type alias Bag = { red = Int32, green = Int32, blue = Int32 }
type alias Game = { gameId = Int32, bags = List[Bag] }

def parseBag(s: String): Bag =
    let cubeCounts = s
                    |> String.splitOn({substr = ", "})
                    |> List.map(String.breakOnLeft({substr = " "}))
                    |> List.map(x -> {
                        let cubeColor = String.trimLeft(snd(x));
                        let count = Result.getWithDefault(0, Int32.parse(10, fst(x)));
                        (cubeColor, count)
                    })
                    |> List.toMap;
    {
        red = Map.getWithDefault("red", 0, cubeCounts),
        green = Map.getWithDefault("green", 0, cubeCounts),
        blue = Map.getWithDefault("blue", 0, cubeCounts)
    }

def parseGame(s: String): Option[Game] =
    match String.splitOn({substr = ": "}, s) {
        case gameTitle :: bagsStr :: Nil => {
            let gameNum = gameTitle
                        |> String.sliceRight({start = 5})
                        |> Int32.parse(10)
                        |> Result.getWithDefault(0);
            let bags = bagsStr
                     |> String.splitOn({substr = "; "})
                     |> List.map(parseBag);
            Some({ gameId = gameNum, bags = bags })
        }
        case _ => None
    }

def possibleBag(bag: Bag): Bool =
    let {red = r, green = g, blue = b} = bag;
    (r <= 12) `Bool.and` lazy (g <= 13) `Bool.and` lazy (b <= 14)

def solvePart1(games: List[Game]): String =
    games
    |> List.filter(game -> {
        let {gameId = _, bags = bags} = game;
        List.forAll(possibleBag, bags)
    })
    |> List.map(game -> {
        let { gameId = gameId, bags = _ } = game;
        gameId
    })
    |> List.sum
    |> ToString.toString

def minimumBagPower(game: Game): Int32 =
    let { gameId = _, bags = bags } = game;
    let (r, g, b) = List.foldLeft(
        (acc, bag) -> {
            let (maxR, maxG, maxB) = acc;
            let {red = r, green = g, blue = b} = bag;
            (Int32.max(maxR, r), Int32.max(maxG, g), Int32.max(maxB, b))
        },
        (0, 0, 0),
        bags
    );
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
