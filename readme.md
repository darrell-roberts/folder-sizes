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

. -> 206 files 147 sub folders 5,265,207 total bytes

./dist-newstyle -> 34 files 28 sub folders 5,142,037 total bytes
./.git -> 171 files 119 sub folders 114,840 total bytes
./src -> 1 files 0 sub folders 8,330 total bytes
```