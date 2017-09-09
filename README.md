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
dist/build/TheBlackCurse/TheBlackCurse maps/map1.txt maps/map1.rules [configfile]
```

If you press the action key (e by default) near something that provides a dialogue, you will enter the dialogue, and need to escape it by pressing the escape key after.

### Map file
The first character of a string is the only one displayed diplayed. The rest is used for id in the rules file.
Here is the signification of the different symbols in the example. Feel free to  improvise!

* . is a blank cell
* \* is a tree
* | is a vertical wall
* \+ is an angle wall
* \- is an horizontal wall
* m is a fire
* K is a man
* ~ is water

### Rules file
A rules file is a classic config file with some basic parameter. If these are not set correctly, the game may not run.
#### GAME section
On the [GAME] section, you can specify some attributes:

* cannotgothrough: the list of the char that the player cannot go through
* radius: the radius of view of the player. If set to 0 or less, the player can see evrything
* msgwinheight: the height in pixel of the message window. By default to 5
* currul: The actual upper left corner of the camera, by default set to Point {y=0,x=0}
* startdiaogue: Intro text displayed at the starting of the game

#### ID
You can assign to each id (so to each string on the map file) some options. The syntax is:
[id]
someoption=sometext

Monster attributes (hp, dammage, monster, name) and tosay are subject to heritage. If the value is not set, the game will search for it in id that are prefix of the current. Example: if you have many zombies, you can specify all their attributes in [Z], and the program will run.

Options avaible are:
* tosay: Displayed when the character go through or interract with the object
* dialogue: Diplayed when the player interract with the objet. You can specify responses by adding at the end of the line

> | action1 Text Disp | action2 Text | lastoption

where action1 and action2 are option in the same section of the config file and "Text Disp" and "Text" are the texts displayed. lastoption is the parameter that will be used when the user attempt a new dialogue. By default his value is "dialogue". if set to END, the dialogue is definitly ended.

* lastoption: Last option used in the dialogue, it will be diplayed if the player attempt a new dialogue
* hp: Health points of the object.
* name: name of the object
* monster: If an object has this attribute, and the dialogue is ended (lastoption set to END) or if there is no dialogue at all, the player can combat the object. MONSTERS MUST HAVE DIFFERENT IDs

#### Player
[PLAYER] is just a an id for the player, and it behave like a monster (you can specify hp, dammage...). 

### Config file
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
* save
* help
* quit
* view

with the following values:

* All characters
* Key{Up,Down,Left,Right}Arrow
* ESC

See config.txt for example
