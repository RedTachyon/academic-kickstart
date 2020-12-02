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
