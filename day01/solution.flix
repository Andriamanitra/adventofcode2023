def getNum(s : String): Option[Int32] =
    forM (
        tensIdx <- String.findIndexOfLeft(Char.isDigit, s);
        onesIdx <- String.findIndexOfRight(Char.isDigit, s);
        tens <- Char.digit({radix = 10}, String.charAt(tensIdx, s));
        ones <- Char.digit({radix = 10}, String.charAt(onesIdx, s))
    ) yield tens * 10 + ones

def solvePart1(lines : List[String]): String =
    lines
    |> List.filterMap(getNum)
    |> List.sum
    |> ToString.toString

def findFirstDigit(s : String): Int32 =
    if (String.isEmpty(s)) 0
    else if (String.startsWith({ prefix = "one"   }, s)) 1
    else if (String.startsWith({ prefix = "two"   }, s)) 2
    else if (String.startsWith({ prefix = "three" }, s)) 3
    else if (String.startsWith({ prefix = "four"  }, s)) 4
    else if (String.startsWith({ prefix = "five"  }, s)) 5
    else if (String.startsWith({ prefix = "six"   }, s)) 6
    else if (String.startsWith({ prefix = "seven" }, s)) 7
    else if (String.startsWith({ prefix = "eight" }, s)) 8
    else if (String.startsWith({ prefix = "nine"  }, s)) 9
    else
        let c = String.charAt(0, s);
        match Char.digit({ radix = 10 }, c) {
            case Some(x) => x
            case None => findFirstDigit(String.dropLeft(1, s))
        }

def findLastDigit(s : String): Int32 =
    if (String.isEmpty(s)) 0
    else if (String.endsWith({ suffix = "one"   }, s)) 1
    else if (String.endsWith({ suffix = "two"   }, s)) 2
    else if (String.endsWith({ suffix = "three" }, s)) 3
    else if (String.endsWith({ suffix = "four"  }, s)) 4
    else if (String.endsWith({ suffix = "five"  }, s)) 5
    else if (String.endsWith({ suffix = "six"   }, s)) 6
    else if (String.endsWith({ suffix = "seven" }, s)) 7
    else if (String.endsWith({ suffix = "eight" }, s)) 8
    else if (String.endsWith({ suffix = "nine"  }, s)) 9
    else
        let c = String.charAt(String.length(s)-1, s);
        match Char.digit({ radix = 10 }, c) {
            case Some(x) => x
            case None => findLastDigit(String.dropRight(1, s))
        }

def getNumPart2(s : String): Int32 =
    let tens = findFirstDigit(s);
    let ones = findLastDigit(s);
    tens * 10 + ones

def solvePart2(lines : List[String]): String =
    lines
    |> List.map(getNumPart2)
    |> List.sum
    |> ToString.toString

def main(): Unit \ IO =
    match Files.readLines("input.txt") {
        case Ok(lines) => {
            println("Part 1:");
            lines |> solvePart1 |> println;
            println("Part 2:");
            lines |> solvePart2 |> println
        }
        case Err(e) => System.StdErr.println(ToString.toString(e))
    }
