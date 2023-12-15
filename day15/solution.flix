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

enum HashMap[k: Type, v: Type, r: Eff] {
    case HashMap(Region[r], Ref[Array[List[(k,v)], r], r], k -> Int32)
}

mod HashMap {
    pub def new(rc: Region[r], fn: k -> Int32): HashMap[k, v, r] \ r =
        let arr = ref Array.init(rc, constant(Nil), 256) @ rc;
        HashMap.HashMap(rc, arr, fn)

    pub def insert![k: Type, v: Type, r: Eff](newKey: k, newValue: v, hashmap: HashMap[k, v, r]): Unit \ r with Eq[k] =
        let HashMap.HashMap(_rc, arr, fn) = hashmap;
        let idx = fn(newKey);
        let bucket = Array.get(idx, deref arr);
        let newEntry = (newKey, newValue);
        let updatedBucket =
            match bucket |> List.find(fst >> Eq.eq(newKey)) {
                case Some(_) =>
                    bucket |> List.map(
                        match (oldKey, oldValue) ->
                            if (oldKey == newKey) newEntry else (oldKey, oldValue)
                    )
                case None => Cons(newEntry, bucket)
            };
        Array.put(updatedBucket, idx, deref arr)

    pub def delete![k: Type, v: Type, r: Eff](keyToRemove: k, hashmap: HashMap[k, v, r]): Unit \ r with Eq[k] =
        let HashMap.HashMap(_rc, arr, fn) = hashmap;
        let idx = fn(keyToRemove);
        let bucket = Array.get(idx, deref arr);
        let updatedBucket =
            bucket |> List.filter(match (key, _) -> key != keyToRemove);
        Array.put(updatedBucket, idx, deref arr)
}

def solvePart1(input: List[String]): Int32 =
    List.sumWith(hash, input)

def solvePart2(input: List[String]): Int32 = region rc {
    let hm = HashMap.new(rc, hash);
    foreach (line <- input) {
        match String.splitOn({ substr = "=" }, line) {
            case k :: v :: Nil => {
                match Int32.fromString(v) {
                    case Some(value) => hm |> HashMap.insert!(k, value)
                    case None => bug!("invalid Int32 '${v}' on line '${line}'")
                }
            }
            case v :: Nil => {
                let keyToRemove = String.dropRight(1, v);
                hm |> HashMap.delete!(keyToRemove)
            }
            case _ => bug!("invalid line: '${line}'")
        }
    };
    let HashMap.HashMap(_, buckets, _) = hm;
    deref buckets
    |> Array.toList
    |> List.enumerator(rc)
    |> Iterator.sumWith(match (i, bucket) -> {
        let bucketNumber = i + 1;
        bucket
        |> List.reverse
        |> List.enumerator(rc)
        |> Iterator.sumWith(match (j, entry) -> {
            let entryNumber = j + 1;
            bucketNumber * entryNumber * snd(entry)
        })
    })
}

def main(): Result[String, Unit] \ IO =
    forM (input <- readInput("input.txt")) yield {
        println("Part 1:");
        input |> solvePart1 |> println;
        println("Part 2:");
        input |> solvePart2 |> println
    }
