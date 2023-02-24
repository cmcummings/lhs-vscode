A test file for Literate Haskell

\begin{code}
module Main where
\end{code}

Text here

List of features
- Code blocks
- *Notebooks!*
- Markdown blocks

Quote block

Main function title
=============

\begin{code}
main :: IO ()
main = do
  putStrLn "test"
\end{code}
 > Fibonacci sequence
\begin{code}
fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
\end{code}

