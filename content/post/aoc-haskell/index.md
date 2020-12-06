---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

title: "Advent of Code 2020 with Haskell"
subtitle: "Merry holidays!"
summary: "An ongoing documentation of my implementations of Advent of Code 2020"
authors: []
tags: []
categories: []
date: 2020-12-01T20:51:36+02:00
lastmod: 2020-12-01T20:51:36+02:00
featured: true
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: ""
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---

It's this time of the year again, and that means I will once again undertake my yearly ritual of attempting Advent of Code with a weird language, and inevitably give up somewhere halfway through due to other stuff going on in my life. But hey, this time I have a website, so maybe that will motivate me?

Note that I'm generally taking a pragmatic approach - I might not use epimorphic endomonads to add two numbers, but will opt for simplicity, unless I feel like there's some large value in adding complexity. Also, I will optimize the solutions as far as it is a) necessary, or b) interesting. So if the code runs in a few seconds, that's good enough for me.

I'll be working via [repl.it](https://repl.it) to avoid the process of setting up Haskell/GHC/cargo/stack/nix/ghcup/... locally, so if there's any interest, I can share the link to the running code.


# Day 6

[link](https://adventofcode.com/2020/day/6)

Is this another day where it's mostly just parsing? Sure is! Fortunately, now I can shamelessly reuse my code snippet that deals with the multiline inputs separated by empty lines, so there's that at least. So let's do that:

```haskell
import Control.Monad (join) 

parseInput :: String -> [String]
parseInput s = let 
    lined = lines s
    separated = lines $ join [if null line then "\n" else line ++ " " 
                                    | line <- lined]
    in separated
```

With this, I'll get a list of group answers, where each group answer is space-separated individual answers. Much easier to work with!

Now, let's think about the logic of the problem. For each group separately, we want to check which letters are present in at least one of the individual's answers. This seems like a perfect use case for `Set`s - we don't care about order, or how many times something appears, just whether it does.

Let's get some imports going for that:

```haskell
import Data.Set (Set, fromList, delete, toList, size, intersection)
```

If you're not familiar with haskell `Set`s, they're basically the same thing as their mathematical equivalents, which I'm assuming you know from high school.

Note: Since care about a letter appearing in *any* individual answer, we can just treat all of them as part of one big answer. In other words, the representation that we have right now, of a space-separated string, is perfect.

Anyways, here's the battle plan:

1. Convert each string (`[Char]`) into a `Set`.
2. Remove the space from the resulting sets.
3. Compute the sizes of those sets
4. Sum up the sizes

Points 1-3 can be done with... you guessed it, a list comprehension! What remains is a simple sum, so without further ado, here's the `main`:

```haskell
main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    let sizes = [(size . (delete ' ') . fromList) line | line <- input]

    print $ sum sizes
```

That wasn't so bad!

But wait. Now we need to change it so that rather than finding letters present in *any* individual input, we find those present in all inputs. 

Nothing difficult! Starting from the same representation as earlier, we need to split each group string into `words`, turn each of them into a set, take the intersection, and... the rest is the same. Here's the exact code:

```haskell
main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    let sizes = [(size .(foldl1 intersection) . (map fromList) . words) line |
                     line <- input]

    print $ sum sizes
```

Yay, done!


# Day 5

[link](https://adventofcode.com/2020/day/5)

Hey, no weird parsing this time! All we need to do is read some slightly-concealed binary strings (which I guess is a form of parsing, but shh), convert them into `Int`s and do some simple logic on them. I won't go insane today!

From the structure of the input, we know that the characters `'B'` and `'R'` correspond to binary `1`, and characters `'F'` and `'L'` - to `0`. There's a bunch of ways to convert a binary string to an integer, but I decided to use Haskell's infinite lists and... you guessed it - a list comprehension! If we reverse the input string and zip it with an infinite list `[0, 1, 2, 4, 8, ...]`, we can just sum the entries corresponding to `1` in the input and voilà, we're done. For simplicity, I'll actually zip it with `[0..]` and turn it into an exponent in the expression itself - but tomato tomato.

```haskell
readBinary :: String -> Int
readBinary s = sum [ if c `elem` "BR" then 2^i else 0 | (c, i) <- zip (reverse s) [0..]]
```

Let's have another look at the structure of the input. Each line is an encoded seat ID, and to get the ID we need to:

1. Split it into the row number (from the first 5 characters), and the column number (from the last 3 characters)
2. Compute the ID by taking `8 * row + column`

But... hold on a second. This is exactly the same as if we just parsed the entire string as-is, ignoring the whole column/row thing. Think about it this way, in base 10: is there a difference between saying 1043 and 100 * 10 + 43? Nope! So let's scratch the splitting part and just read the entire string - luckily, our `readBinary` function deals with it perfectly!

With this insight... we're pretty much done. We just need to call `readBinary` on each line, find the maximum, boom.

```haskell
main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let ids = map readBinary input
    print $ maximum ids
```

Now, on to part to - this time we want to find our own number, which will be missing from the input list. But, to make things more complicated - it might be the case that the list of IDs is missing some values at the beginning and at the end. So first, let's just go through all IDs which take values in range `[0..8*128]` until we hit the first one that's actually present in our input.

Then we just need to drop all the value that *are* present in our input, take the first one that's not and... Done. Simple as that.

```haskell
main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let ids = map readBinary input

    let midIds = dropWhile (not . (`elem` ids)) [0..8*128]

    let myId = (head $ dropWhile (`elem` ids) midIds)

    print $ myId
```

I must say, this was much more relaxing. And I got to use a list comprehension!


# Day 4

[link](https://adventofcode.com/2020/day/4)

Did I ever mention how much I like working with strings and parsing stuff? No, I haven't. Because I absolutely hate it. I never bothered to properly learn any parsing libraries, and this day is literally about input validation... But what the hell, let's try.

We want to read a bunch of strings separated by an empty line, but possibly containing a newline somewhere in the middle... Yea, good ol' `lines` will fail. So through some thinking I found the following procedure:

1. Split the input into lines - this will produce an empty string in the space between distinct passports
2. Turn the empty strings into newlines, append a space to all non-empty strings. `join` everything into a single string, and break it again on each newline with `lines`. Now we have a list of strings like "abc:def qwe:rty"
3. Split each of those strings with `words` to get individual passport entries.
4. Break each of the passport entries with a doubly-nested `map`, into parts before and after `':'`. Take only the first part.

Phew, now we have a `[[String]]` where each entry is a passport, and each entry in a passport is the name of a field. The function that produces this is as follows:

```haskell
import Control.Monad (join)

parseInput :: String -> [[String]]
parseInput s = let 
    lined = lines s
    separated = lines $ join [if null line then "\n" else line ++ " " 
                                    | line <- lined]
    passports = map words separated
    field = map (map $ fst . break (==':')) passports
    in field
```

I *could* use more list comprehensions... I mean, each maps can be replaced with it but this time I will resist the temptation. I'll use some later.

Now, we're given a set of required passport fields, so we just need to check that each of them is an element of a passport to consider that passport valid. Then just count up the number of valid passports - classic AoC question. 

In other words, for every passport, go through the predefined list of fields and check that it is, in fact, an element in the passport. If they all come up as `True`, we mark the passport as valid. Then do it for each passport.

...I'm gonna do it. I'll use a double list comprehension. Who's gonna stop me?

```haskell
main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    let valids = [and [e `elem` passport | e <- fields] | passport <- input]
    let result = sum [if x then 1 else 0 | x <- valids]

    print $ result
```

Boom, done. Wasn't *that* bad.

So what do they want now? Ah, now I need to actually look at the values. That's not that bad - I just need a simple tweak to the parsing function to read the values as well. A `Map` seems like a suitable structure, so let's just `import Data.Map (fromList, Map)` at the beginning, and change the parsing function to:

```haskell
parseInput :: String -> [Map String String]
parseInput s = let 
    lined = lines s
    separated = lines $ join [if null line then "\n" else line ++ " " 
                                    | line <- lined]
    passports = map words separated
    field = map (fromList . (map $ break (==':'))) passports
    in field
```

*about an hour later*

Yea, I hate this. But in the end, I created a monstrosity that checks each of the conditions and sums it up. There isn't much interesting going on here, so I'll just dump all the code on you and go to sleep. Hope it helps!

```haskell
import Control.Monad (join)
import Data.Map (fromList, Map, lookup)
import Text.Read (readMaybe)
import Data.Char (isAlpha, isDigit)

import Prelude hiding (lookup)

parseInput :: String -> [Map String String]
parseInput s = let 
    lined = lines s
    separated = lines $ join [if null line then "\n" else line ++ " " 
                                    | line <- lined]
    passports = map words separated
    field = map (fromList . (map $ break (==':'))) passports
    in field


dateValid :: Int -> Int -> String -> Bool
dateValid l h s = let s' = (readMaybe s :: Maybe Int) in 
                    case s' of
                        Nothing -> False
                        (Just x) -> x >= l && x <= h

heightValid :: String -> Bool
heightValid s = if (unit == "cm") then num >= 150 && num <= 193 
                    else if (unit == "in") then num >= 59 && num <= 76
                    else False
    where (num', unit) = span isDigit s
          num = (read num' :: Int)

hairValid :: String -> Bool
hairValid s = hash && len && chars
    where hash = head s == '#'
          color = tail s
          len = length color == 6
          chars = and [c `elem` "1234567890abcdef" | c <- color]

eyeValid :: String -> Bool
eyeValid s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

idValid :: String -> Bool
idValid s = length s == 9 && all isDigit s

validateInput :: String -> Maybe String -> Bool
validateInput _ Nothing = False
validateInput k (Just v) = case k of
    "byr" -> dateValid 1920 2002 v
    "iyr" -> dateValid 2010 2020 v
    "eyr" -> dateValid 2020 2030 v
    "hgt" -> heightValid v
    "hcl" -> hairValid v
    "ecl" -> eyeValid v
    "pid" -> idValid v
    "cid" -> True
    _ -> False

fields :: [String]
fields = [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    ]


main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"

    let valids = [and [validateInput e (tail <$> e `lookup` passport) | e <- fields] | passport <- input]
    let result = sum [if x then 1 else 0 | x <- valids]

    print $ result
```

I hate parsing. Is my hatred stemming from my lack of knowledge? Absolutely. But that seems to be the way to go, looking at the current state of humanity.

# Day 3

[link](https://adventofcode.com/2020/day/3)

There's no way I'm going to do another problem by putting all the logic in a list comprehension, right? ...right?

Wrong! Honestly, I wasn't really planning it, but guess this is my life now. So welcome to Advent of Comprehensions, day 3!

This time we are given what is essentially a 2D binary array, implicitly infinitely repeating horizontally. Starting from `(0, 0)` we want to go 3 right, 1 down, repeat until we hit the bottom (which I'm sure will be somewhere in 2020), and count how many positive values (or "trees") we encounter along the way.

The input parsing is simple enough - what we want ultimately is just a 2D array, so `[[Char]]` works great, and is obtained easily by applying `lines` to the raw input. It's also simple enough to get the number of elements in each row (assuming it's constant across rows, to preserve our sanity) by applying `(length . head)` to that array. (okay, it's not technically an array, but it's good enough for a mind model).

And for the actual logic? Well, we can use the convenient Haskell way of creating integer lists - `[0,3..]` will give us an infinite list of all natural numbers divisible by 3, I'm sure you can see how that will be handy - after all we want to look at points (0, 0), (1, 3), (2, 6) etc.

Then we just have to go through the input, taking the next element of that list for each line, put the column number through a modulo operation (so given that a row has 31 characters, column 33 would correspond to column 2, since we can't index column 33 directly), count how many such lines there are and... done! Really, this was much simpler than I expected looking at the inputs.

```haskell
main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let m = (length . head) input

    let result = sum [if '#' == line !! (k `mod` m) then 1 else 0 | (line, k) <- zip input [0,3..]]

    print result
```

For the second part, we basically just need to change the slope of how far right and how far down we go. And I think it's time to make the code a bit modular - copy-pasting the same code 5 times and changing parameters just feels like a pain, and is terrible coding practice.

So let's try to extract the core logic into a separate function. It feels pretty simple to change the "right" value - just change the hard-coded 3 to whatever else. But the last requested slope actually has a larger "down" increment, and this requires a bit more tweaking.

I was hoping there'd be a built-in Haskell version, but alas, there is none. Fortunately, I found a nice little function for that on [Stack Overflow](https://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell), which I will shamelessly steal here:

```haskell
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []
```

Let's quickly check how it works. A simple test in GHCI gives the following result:

```haskell
>  take 5 $ every 2 [0..]
=> [1,3,5,7,9]
```

Uh-oh, not exactly what we wanted. We want to include the first element, not exclude it. Starting from `-1` would work for this example, but not for the arbitrary list...

But hey, we can just monkey-patch it by adding an extra, trivial element to the beginning of the array, right?

```haskell
>  take 5 $ every 2 (0:[0..])
=> [0,2,4,6,8]
```

Yep, perfect! Let's just make sure it works for the base case...

```haskell
>  take 5 $ every 1 (0:[0..])
=> [0,0,2,4,6]
```

...crap, that's not what we wanted. Going back to the Stack Overflow thread, there's another solution which actually makes an explicit distinction between a version starting at index 0 and at index n. The new function will be 

```haskell
every :: Int -> [a] -> [a]
every _ [] = []
every n as = head as : every n (drop n as)
```

We can do the same tests, this time dropping the ugly `0:` patch and yep, works like a dream!

With that in our inventory, let's write a parametrized function that solves the previous task. There's nothing weird here, just replace the 3 with an argument, and the input lines with a small expression using the function defined above, and we get:

```haskell
solvePuzzle :: Int -> Int -> Int -> [[Char]] -> Int
solvePuzzle r d m inp = sum [if '#' == line !! (k `mod` m) then 1 else 0 | 
                             (line, k) <- zip (every d inp) [0,r..]] 
```

Now we just need to go through each test case, compute the value and get a product of all of them. There's really no need to use list comprehensions for that... but I'm gonna do it anyways. Just out of the principle.

Each test case is described with two integers, so let's just use a tuple:

```haskell
testCases :: [(Int, Int)]
testCases = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
    ]
```

And since we already have our input and row length in the IO context, the final resulting `main` function is really simple:

```haskell
main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let n = length input
    let m = (length . head) input

    let result = product [solvePuzzle r d m input | (r, d) <- testCases]

    print $ result
```

(Quick note - do I need the `$` in the last line there? Nope. But I generally like keeping it after all my `print` statements as a way of saying "take everything on the right and print it", so that I don't have to remember about that later if I make that statement more complex)

Check the output and what do you know - it works!

To be clear, I make no promises I'll stick to list comprehensions. For some parts of this day's problem, my first intuition was something else, for example mapping over the test cases with a partially applied function, but hey, this works too, and at least gives me some sort of a theme. And you know, "comprehension" shares some letters with "christmas", so that can't be a coincidence!


# Day 2

[link](https://adventofcode.com/2020/day/2)

So, this seems to be accidentally turning into "AoC with Haskell list comprehensions"... Might be the mathematician in me, but they're really nice to look at, easy to understand and just super convenient for when you want to do mapping and filtering. And to be fair, most of the time the Python equivalent *is* the preferred way of mapping and filtering, so might be a habit leaking over from that world.

This time we need to go through a list of passwords and the requirements imposed on them. They're presented in a format as follows:

`1-3 a: abcde`

What this means is that we want the character `a` appear in the password `abcde` between 1 and 3 times (inclusive on both ends). The logic seems simple enough, but we also get to have fun with [Haskell's way of dealing with strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings). 

Since I don't really expect AoC to force me to do any heavy computations, I'll just stick with the simple `String` type.

So here's my plan on how to tackle this - first, we read all the inputs, split them into lines, and treat each of them separately. Every line needs to be parsed into a more usable form, so let's make it explicit and define a more usable data structure called Task:

```haskell
data Task = Task {
    low :: Int ,
    high :: Int ,
    char :: Char ,
    password :: String 
} deriving (Show)
```

Not that the record syntax is an overkill in this case and, in fact, won't be explicitly used. Still, I like the clarity it provides - it's immediately pretty clear (I hope) which field holds what.

Next up, we want to parse a line into that Task format with a function of type `String -> Task`. The idea here is: first, we split the line into chunks separated by spaces, which is conveniently available with the `words :: String -> [String]` function. Looking again at the format, this will give us three chunks: `"1-3"`, `"a:"` and "`abcde`".

```haskell
parseLine :: String -> Task
parseLine s = Task l h c pwd
    where parts = words s
```

The first chunk we want to split again to extract the numbers - the same trick with `words` would work great if only we could split on `'-'` rather than a space... alas, that's not available in Prelude. Sure, I could import it from `Data.List.Split`, but importing is for losers (and people who have the patience to deal with stack/cabal/I-don't-even-know). Luckily, there's a slightly more restrictive function which `break`s a list into two chunks - before and after (inclusive) the delimiter.

```haskell
parseLine :: String -> Task
parseLine s = Task l h c pwd
    where parts = words s
          (l', h') = break (=='-') (parts !! 0)
          l = read l'
          h = read $ tail h'
```

Note that we have to apply `tail` to `h'` to get rid of that pesky delimiter.

The rest is significantly easier - the character we're looking for is just the first character in the second chunk, and the password is the entirety of the third chunk. In total, we're left with a small monster like this:

```haskell
parseLine :: String -> Task
parseLine s = Task l h c pwd
    where parts = words s
          (l', h') = break (=='-') (parts !! 0)
          l = read l'
          h = read $ tail h'
          c = head $ parts !! 1
          pwd = parts !! 2
```

Now let's think about processing that task - and let's get to the promised list comprehensions. Since we want to know how many passwords fulfill the conditions, we can just assign a boolean to each of them, and then count up all the `True`s. So we need a function with the signature `Task -> Bool`.

We need to count up all the occurences of a certain character. As mentioned earlier, list comprehensions are super nice for filtering stuff, so let's use one:

```haskell
len = length [x | x <- pwd, x == c]
```

Now, I see your problem with this. Is there *any* advantage of this over using a good old `filter`? I don't think there is, so we might as well replace this line with

```haskell
len = length $ filter (==c) pwd
```

and not include a hidden monad that the reader should understand before trying to use them. But in the end, both approaches work, the first one is more python-ish, the second one is probably better Haskell, and that's alright. I'm sure GHC can handle it just as well.

Finally, we just need to add the simple comparison, and we end up with the following function:

```haskell
solveTask :: Task -> Bool
solveTask (Task l h c pwd) = len >= l && len <= h
    where len = length [x | x <- pwd, x == c]
```

Now we just need the IO glue and a quick conversion from a list of booleans to a sum - this isn't Python where `True + True == 2`, so we'll need to be a bit more explicit, but there's nothing scary - in fact, we can use another list comprehension for that! Neat, right?

```haskell
main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let tasks = map parseLine input
    let result = sum [if solveTask t then 1 else 0 | t <- tasks]

    print result
```

Run the code, yep, it works. Great!

Moving on to task 2, it turns out that the password requirements are slightly different after all - using the same example of `1-3 a: abcde`, we now want to check that the character `a` is on **one** of the positions `1` or `3`. (1-indexed... eww)

We're actually almost done by now. All the parsing, all the glue, that still works, we just need to change the `Task -> Bool` function. And there isn't really much to do there either - a practical way of saying "either x or y is True" is "x /= y" in Haskell, then we also want to fix the horrendous 1-based indexing, and we get a new solution function:

```haskell
solveTask2 :: Task -> Bool
solveTask2 (Task l h c pwd) = (c == pwd !! (l-1) ) /= (c == pwd !! (h-1))
```

Plug this into the same main function as before, replacing `solveTask` with `solveTask2` and boom, another day done!

As an ending comment - my immediate problem with this solution overall is the parsing phase. To be honest I'm not a fan of working with strings and words and stuff like that, so I'm just kinda improvising. It works, but it would crash on an incorrect input, which I don't expect in AoC, but that assumption won't get you hired at Google, so keep that in mind. To make everything neat and safe (we're talking Haskell, after all), you'd probably want to use a Lens or a safe version of (!!) that returns a Maybe, turning `parseLine`'s signature to `String -> Maybe Task`, and `solveTask`'s into `Maybe Task -> Bool`, where `solveTask Nothing = False`. This sounds like a graceful way of doing this.

But this part is left as an exercise to the reader.

Bisous!


# Old solutions


# Day 1

[link](https://adventofcode.com/2020/day/1)

The first day's task is pretty simple, perfect to warm up with the pure joy that is Haskell IO. How I usually start is something like this, to get in the mood of the monadic context.

```haskell
main :: IO ()
main = do
    putStrLn "Hello there"
```

What do we have here? We have a long list of numbers to process, so I went ahead and put them into a `input.txt` file next to the Haskell code. 

We want to go through all the numbers and find any that sum up to 2020. (which I find to be in bad taste - seriously, who likes this year?) The naive solution would be to just go through all pairs of numbers, check if that's the sum and call it a day once we find something. The complexity would be O(n<sup>2</sup>), which would disqualify me from any coding interview, but it's a good thing that's not what we're doing here.

A more clever approach would probably be to sort the numbers (O(n log n)) and then try to go through the array keeping two indices from the edges, choosing the side we want to increment (or decrement) based on whether the current sum is greater or lower than the desired value... But nobody's paying me for that, so let's stat with the simple approach!

Reading a file is, fortunately, pretty simple. We can also apply a function to the result using the `<$>` operator (which really is just `fmap` in disguise), so let's do that to immediately split it in separate lines:

```haskell
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    print inputs
```

The `inputs` symbol now holds a list of strings representing the numbers. Converting them to actual Ints is simple enough... but requires explicit typing, and that always trips me up when using the `<-` operation, so I'm just gonna go ahead and put it in the next line.

```haskell
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let nums = map read inputs :: [Int]
    print nums
```

Time for the actual logic. Haskell happens to have this thing called list comprehensions, which is perfect for the job - you can evaluate an expression over a range of values of its parameters, and then even filter it however we want. It's so beautifully similar to what you'd write in a math class:

```haskell
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let nums = map read inputs :: [Int]
    let result = head [a*b | a <- nums, b <- nums, a + b == 2020]
    print result
```

And that's it. The `a <- nums, b <- nums` part takes care of checking all values, `a + b == 2020` makes sure that we get the right numbers, `a*b` computes the requested product, and `head` makes it so that we get just that one value as output. Wonderful!

But that's not the end - for the second star of the day, we want to search across *triples* of numbers rather than just pairs. Now, don't talk to me about computational complexity, but... can't we try the exact same approach? I mean, in the worst case we'll just get bored waiting and try something more clever.

So let's add just a tiiiny bit of code...

```haskell
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let nums = map read inputs :: [Int]
    let result = head [a*b*c | a <- nums, b <- nums, c <- nums, a + b + c == 2020]
    print result
```

Bingo! Now, I don't know about you, but I personally love how this code looks. Everything is straight-forward, there's no boring details like
```c++
for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
        for (int k = 0; k < N; k++) {
            kill(me); // please
        }
    }
}
```

and it just rolls off the keyboard!

Well, that wraps it up for the day. I hope this will be of use to someone, sometime, and if not - maybe at least it'll motivate me to keep doing AoC. 

Cherry Mristmas!
