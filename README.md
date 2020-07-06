# uncrossed-paths

Finding uncrossed knight's paths on NxM board.  This was my experiment
with Haskell in 2015 to solve a puzzle where a knight (the chess piece)
moves on a rectangular grid.  The objective is to maximize the number
of steps on a path that returns to the starting point and does not cross
over itself at any point.

The script in `Main.hs` writes possible paths from a starting point to
a file.  To compile the script, run

```
$ ghc Main.hs
```

An example run could be

```
$ ./Main steps 10 "(0,1)" "(5,4)"
```

where `steps` is the name of the output file, `10` is the minimum
number of steps, `(0,1)` is the starting point, and `(5,4)` is
the upper right corner.  This writes into `steps` the following:

```
12 steps: [(0,1),(1,3),(3,4),(2,2),(4,1),(3,3),(5,4),(4,2),(5,0),(3,1),(1,2),(2,0),(0,1)]

10 steps: [(0,1),(1,3),(3,4),(5,3),(3,2),(4,0),(2,1),(3,3),(1,2),(2,0),(0,1)]

12 steps: [(0,1),(2,0),(1,2),(3,1),(5,0),(4,2),(5,4),(3,3),(4,1),(2,2),(3,4),(1,3),(0,1)]

10 steps: [(0,1),(2,0),(1,2),(3,3),(2,1),(4,0),(3,2),(5,3),(3,4),(1,3),(0,1)]

10 steps: [(0,1),(2,0),(4,1),(5,3),(3,4),(4,2),(2,1),(3,3),(1,4),(2,2),(0,1)]

10 steps: [(0,1),(2,2),(1,4),(3,3),(2,1),(4,2),(3,4),(5,3),(4,1),(2,0),(0,1)]
```

