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
    let nums = read inputs :: [Int]
    print nums
```

Time for the actual logic. Haskell happens to have this thing called list comprehensions, which is perfect for the job - you can evaluate an expression over a range of values of its parameters, and then even filter it however we want. It's so beautifully similar to what you'd write in a math class:

```haskell
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let nums = read inputs :: [Int]
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
    let nums = read inputs :: [Int]
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