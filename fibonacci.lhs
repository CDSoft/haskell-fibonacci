% Super fast recursive Fibonacci implementation in Haskell
% Christophe Delord
% 11 June 2022

License
=======

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Introduction
============

The Fibonacci sequence is a classical hello-world application for functional
programming. The challenge here is to get a fast implementation. We will first
show two classical implementations: the trivial recursive definition that is
very slow and the iterative version that is slightly faster.

And a third one that is blazing fast.

::: comment
\begin{code}
module Main where
import Control.DeepSeq
import Control.Monad
import Data.List
import Data.List.Split
import System.TimeIt
\end{code}
:::

Definition
==========

::::::::::::::::::: {.columns}

:::::::::: {.column}
The Fibonacci sequence is defined recursively:

$$
    \begin{align}
        F_0 &= 1 \\
        F_1 &= 1 \\
        F_n &= F_{n-1} + F_{n-2}
    \end{align}
$$
::::::::::

:::::::::: {.column}
@@( function fib(n) return n <= 1 and 1 or fib(n-1) + fib(n-2) end
    return F.concat {
        {
            "$n$|$F_n$|$F_n = F_{n-1} + F_{n-2}$",
            "---|-----|-------------------------",
        },
        F.range(0, 1):map(function(i)
            return i.."|"..fib(i)
        end),
        F.range(2, 10):map(function(i)
            return i.."|"..fib(i).."|".."$F_{"..i.."} = F_{"..(i-1).."} + F_{"..(i-2).."}$"
        end)
    }
)
::::::::::

:::::::::::::::::::

Trivial recursive Fibonacci sequence definition
===============================================

A trivial implementation of this sequence in Haskell is:

\begin{code}
recursiveFib :: Int -> Integer
recursiveFib 0 = 1
recursiveFib 1 = 1
recursiveFib n = recursiveFib (n-1) + recursiveFib (n-2)
\end{code}

The objective here is to compare implementations for large values of $n$. For
such values $F_n$ can not be coded by machine integers. $F_n$ can be an
arbitrary large integer (`Integer`{.haskell} type in Haskell).

This function is very simple but also very slow. Its complexity is
$\mathcal{O}(F_n)$ and $F_n$ increases very fast since $F_n \sim
\frac{1}{\sqrt{5}}\left(\frac{1+\sqrt{5}}{2}\right)^n$.

Iterative Fibonacci definition
==============================

$F_n$ can also be constructed iteratively from $F_0$ to $F_n$:

$$
    \begin{align}
        F_0 &= 1 \\
        F_1 &= 1 \\
        F_2 &= F_1 + F_0 \\
        F_3 &= F_2 + F_1 \\
        ... \\
        F_n &= F_{n-1} + F_{n-2} \\
    \end{align}
$$

\begin{code}
iterativeFib :: Int -> Integer
iterativeFib n = loop n 1 1
    where
        loop 0 a b = b
        loop n a b = loop (n-1) (a+b) a
\end{code}

This function is faster since it avoids computing Fibonacci terms several times. Its complexity is $\mathcal{O}(n)$.

Fast Fibonacci implementation
=============================

We can exploit some properties of the Fibonacci sequence to improve its computation.

Let's try to apply the definition several times to compute several steps at once:

$$
    \begin{align}
        F_n &= F_{n-1} &+ F_{n-2} \\
            &= F_{n-2} + F_{n-3} &+ F_{n-2} \\
            &= 2 F_{n-2} &+ F_{n-3} \\
            &= 2 \left( F_{n-3} + F_{n-4} \right) &+ F_{n-3} \\
            &= 3 F_{n-3} &+ 2 F_{n-4} \\
            &= 3 \left( F_{n-4} + F_{n-5} \right) &+ 2 F_{n-4} \\
            &= 5 F_{n-4} &+ 3 F_{n-5} \\
    \end{align}
$$

The coefficients seem to be Fibonacci numbers.

Let's prove by induction (over $k$) that:

$$
        F_n = F_k F_{n-k} + F_{k-1} F_{n-k-1}
$$

The property is obviously true for $k = 1$:

$$
    \begin{align}
        F_n &= F_1 F_{n-1} &+ F_{1-1} F_{n-1-1} \\
            &= F_1 F_{n-1} &+ F_0     F_{n-2  } \\
            &= 1   F_{n-1} &+ 1       F_{n-2  } \\
            &=     F_{n-1} &+         F_{n-2  } \\
    \end{align}
$$

Lets prove that $F_n = F_k F_{n-k} + F_{k-1} F_{n-k-1} \implies F_n = F_{k+1} F_{n-k-1} + F_k F_{n-k-2}$.

$$
    \begin{align}
        F_{k+1} F_{n-k-1} + F_k F_{n-k-2} &= \left( F_k + F_{k-1} \right) F_{n-k-1} + F_k F_{n-k-2} \\
                                          &= F_k \left( F_{n-k-1} + F_{n-k-2} \right) + F_{k-1} F_{n-k-1} \\
                                          &= F_k F_{n-k} + F_{k-1} F_{n-k-1} \\
    \end{align}
$$

The Fibonacci sequence can then be defined by:

$$
    \begin{align}
        F_0 &= 1 \\
        F_1 &= 1 \\
        F_n &= F_k F_{n-k} + F_{k-1} F_{n-k-1}, \forall n \ge 2, \forall k \in [1, n-1]
    \end{align}
$$

The trivial implementation is slow because of the double recursivity. We now
have four recursions but:

1. The call tree is reduced by *jumping* from $n$ to $n-k$ instead of just $n-1$
2. By choosing $k$ wisely some redundant computations can be avoided

Intuitively if $k$ is close to $n-k$ then subtrees will be close too. If $k =
n-k$ (i.e. $k = \frac{n}{2}$) then the first term of $F_n$ is a square ($F_k =
F_{n-k}$).

**First case**: n is even ($n = 2k$)

$$
    \begin{align}
        F_n &= F_k F_{n-k} &+ F_{k-1} F_{n-k-1} \\
            &= F_k F_k     &+ F_{k-1} F_{k-1} &&(n-k-1 = 2k-k-1 = k-1) \\
            &= F_k^2       &+ F_{k-1}^2 \\
    \end{align}
$$

**Second case**: n is odd ($n = 2k+1$)

$$
    \begin{align}
        F_n &= F_k F_{n-k} &+ F_{k-1} F_{n-k-1} \\
            &= F_k F_{k+1} &+ F_{k-1} F_{n-k-1} && (n-k = 2k+1-k = k+1) \\
            &= F_k F_{k+1} &+ F_{k-1} F_{k} && (n-k-1 = k) \\
            &= F_k \left( F_k + F_{k-1} \right) &+ F_{k-1} F_{k} \\
            &= F_k^2 + 2 F_k F_{k-1} \\
            &= F_k \left(F_k + 2 F_{k-1} \right) \\
    \end{align}
$$

This definition can be easily implemented in Haskell:

\begin{code}
fastFib :: Int -> Integer
fastFib 0 = 1
fastFib 1 = 1
fastFib n =
    case r of
        0 -> fk^2 + fk1^2
        1 -> fk * (fk + 2*fk1)
    where
        (k, r) = n `divMod` 2
        fk = fastFib k
        fk1 = fastFib (pred k)

\end{code}

This function is way faster than the previous ones and can deal with very large
numbers. Its complexity is $\mathcal{O}(F_{\log_2 n})$ (I have no proof yet,
just an intuition because the depth of the tree is divided by two every steps
and there is still a double recursion).

Tests
=====

Unit tests
----------

The three functions shall return the same values:

\begin{code}
fibs = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597]

testFib :: String -> (Int -> Integer) -> IO ()
testFib name f = do
    forM_ (zip3 [0..] fibs (map f [0..])) $ \(i, ref, fi) ->
        when (fi /= ref) $
            error ("ERROR: "++name++" "++show i++" = "++show fi++" (should be "++show ref++")")
    putStrLn $ "    - "++name++": OK"

unitTests = do
    putStrLn "* Unit tests:"
    putStrLn ""
    testFib "recursiveFib" recursiveFib
    testFib "iterativeFib" iterativeFib
    testFib "fastFib" fastFib
    putStrLn ""
\end{code}

Performance tests
-----------------

Lets compare the performances of the three function:

\begin{code}
perfTests = do
    putStrLn "* Performance tests:"
    putStrLn ""
    putStrLn "  n   | recursiveFib | iterativeFib | fastFib "
    putStrLn "  ---:|-------------:|-------------:|--------:"
    forM_ [20..40] $ \i -> do
        t1 <- fmtTime <$> profile recursiveFib i
        t2 <- fmtTime <$> profile iterativeFib i
        t3 <- fmtTime <$> profile fastFib i
        putStrLn $ show i++" | "++t1++" | "++t2++" | "++t3
    forM_ [20000, 40000 .. 300000] $ \i -> do
        let t1 = ""
        t2 <- fmtTime <$> profile iterativeFib i
        t3 <- fmtTime <$> profile fastFib i
        putStrLn $ show i++" | "++t1++" | "++t2++" | "++t3
    forM_ [2500000, 5000000 .. 40000000] $ \i -> do
        let t1 = ""
        let t2 = ""
        t3 <- fmtTime <$> profile fastFib i
        putStrLn $ show i++" | "++t1++" | "++t2++" | "++t3
    putStrLn ""

profile :: (Int -> Integer) -> Int -> IO (Double, Integer)
profile f n = timeItT $ do
    let fn = f n
    fn `deepseq` return ()
    return fn

fmtTime :: (Double, Integer) -> String
fmtTime (t, f) | t < 1e-6 = show (round (t*1e9)) ++ " ns"
fmtTime (t, f) | t < 1e-3 = show (round (t*1e6)) ++ " µs"
fmtTime (t, f) | t < 1e-0 = show (round (t*1e3)) ++ " ms"
fmtTime (t, f)            = show (round t) ++ " s"
\end{code}

Large values
------------

\begin{code}
largeFibValue = do
    let n = 100*1000
    let fn = fastFib n
    let fn' = fmt fn
    let nbDigits = length (filter (/=' ') fn')
    putStr "* Large number example: "
    putStrLn $ "fastFib "++fmt (fromIntegral n)++" = "++fn'++" ("++fmt (fromIntegral nbDigits)++" digits)"

fmt :: Integer -> String
fmt = unwords . reverse . map reverse . chunksOf 3 . reverse . show
\end{code}

Tests results
-------------

Tests made on a *@(script.sh [[LANG=C lscpu | grep "Model name" | sed 's/.*://' | sed 's/  */ /g']]:trim())*
powered by *@(script.sh [[. /etc/os-release && echo "$NAME $VERSION_ID"]]:trim())* and *@(script.sh [[stack ghc -- --version]]:trim())*.

::: comment
\begin{code}
main = do
    unitTests
    perfTests
    largeFibValue
\end{code}
:::

@(script.sh ".build/fibonacci")
