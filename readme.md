# About
Program to recursively scan a given folder and return total files and total folders.

Written as an exercise in learning Haskell.

  * Concurrency
  * Channels
  * MonadState
  * MonadReader

# Usage

Ex:

```
$ folder-sizes .
Using 4 worker threads per worker
Scaning recursively:
./dist-newstyle
./.git
./src

. 97 files, folders 79:
Path ./.git files 62 folders 51
Path ./dist-newstyle files 34 folders 28
Path ./src files 1 folders 0
```