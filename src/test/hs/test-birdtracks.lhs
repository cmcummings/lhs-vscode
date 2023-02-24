 ## Bird tracks example

> module Main where
> main :: IO ()
> main = do
>   putStrLn "test2"

 > This is a quote block, not birdtracks!
 > 
 > Multi paragraph
 >
 > And
 >
 > > nested blockquotes
 >
 > > that aren't bird tracks!

There is some text here too.
We can't have text on the first line after a code line!

> fib :: (Eq t, Num t, Num a) => t -> a
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

**bold**


1. number
2. `codeception`

```
code block in markdown
```

[and this is a link to the other file](test-codeblocks.lhs)
