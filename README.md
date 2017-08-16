# TheBlackCurse
Simple game written in Haskell using ncurses.

## Build
TheBlackCurse require UI.NCurses (provides by ncurses package) and Data.ConfigFile (provided by ConfigFile package)
To build, use
```
$cabal build
```

## Use
TheBlackCurse need a map file, that contains evrything for the game.
### Map file
Here is the signification of the different symbols:

* . is a blank cell
* * is a tree
* | is a vertical wall
* + is an angle wall
* - is an horizontal wall
* m is a fire
* K is a man
* ~ is water
