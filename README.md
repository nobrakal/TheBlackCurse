  # TheBlackCurse
Simple game written in Haskell using ncurses.

## Build
TheBlackCurse require UI.NCurses (provides by ncurses package) and Data.ConfigFile (provided by ConfigFile package)
To build, use
```
$cabal build
```

## Use
TheBlackCurse need a map and the rules file associated: they contains evrything for the game.
```
./TheBlackCurse map.txt map.rules [configfile]
```
### Config
You can load a config file by appending its name after the map and the map.rules.

#### KEYBOARD
On the KEYBOARD section, you can specify the following fileds:

* up
* down
* left
* right
* cUp
* cDown
* cLeft
* cRight
* action
* load
* help
* quit

with the following values:

* All characters
* Key{Up,Down,Left,Right}Arrow
* ESC

See config.txt for example

### Map file
Note first that only the first character of a string is diplayed. The rest is used for id in the rules file.
Here is the signification of the different symbols:

* . is a blank cell
* * is a tree
* | is a vertical wall
* + is an angle wall
* - is an horizontal wall
* m is a fire
* K is a man
* ~ is water

### Rules file
#### GAME section
On the [GAME] section, you can specify some attributes:

* cannotgothrough: the list of the char that the player cannot go through

#### ID
You can assign to each id (so to each string on the map file) some actions. Th esyntax is:
[id]
someaction=sometext

Actions aviable are:
* tosay: displayed when the character go through or interract with the object
