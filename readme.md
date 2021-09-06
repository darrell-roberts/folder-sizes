# About
Program to recursively scan a given folder and return total files and total folders.

Written as an exercise in learning Haskell.

  * Concurrency
  * STM
  * Channels
  * MonadReader

# Usage

Ex:

```
$ folder-sizes .
Using 4 worker threads.
Scaning . recursively:
  ./dist-newstyle
  ./.git
  ./src

. -> 183 files 131 sub folders 5,246,161 total bytes
./dist-newstyle -> 34 files 28 sub folders 5,142,059 total bytes
./.git -> 148 files 103 sub folders 95,754 total bytes
./src -> 1 files 0 sub folders 8,348 total bytes
```