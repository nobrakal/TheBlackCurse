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
* save
* help
* quit
* view

with the following values:

* All characters
* Key{Up,Down,Left,Right}Arrow
* ESC

See config.txt for example

### Map file
Note first that only the first character of a string is diplayed. The rest is used for id in the rules file.
Here is the signification of the different symbols:

* . is a blank cell
* \* is a tree
* | is a vertical wall
* \+ is an angle wall
* \- is an horizontal wall
* m is a fire
* K is a man
* ~ is water

### Rules file
#### GAME section
On the [GAME] section, you can specify some attributes:

* cannotgothrough: the list of the char that the player cannot go through
* radius: the radius of view of the player. If set to 0 or less, the player can see evrything
* msgwinheight: the height in pixel of the message window. By default to 5
* currul: The actual upper left corner of the camera, by default set to Point {y=0,x=0}

#### ID
You can assign to each id (so to each string on the map file) some options. The syntax is:
[id]
someoption=sometext

To define a monster, you will have to specify a "hp" option. The combat will be run if and only if the dialogue option is absent of the section.

Options avaible are:
* tosay: Displayed when the character go through or interract with the object
* dialogue: Diplayed when the player interract with the objet. You can specify responses by adding at the end of the line

> | action1 Text Disp | action2 Text | lastoption

where action1 and action2 are option in the same section of the config file and "Text Disp" and "Text" are the texts displayed. lastoption is the parameter that will be used when the user attempt a new dialogue. By default his value is "dialogue". if set to END, the dialogue is definitly ended.

* lastoption: Last option used in the dialogue, it will be diplayed if the player attempt a new dialogue
* hp: Health points of the object. If an object has this attribute, and the dialogue is ended (lastoption set to END) or if there is no dialogue at all, the player can combat the object. MONSTERS MUST HAVE DIFFERENT IDs
* name: name of the object
