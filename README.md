# [Erlbereth] [elbereth]

A [Multi-User Dungeon (MUD)] [mud] game written in Erlang. This game is an internet-enabled, client-server model-based, text-based CLI, dungeon crawling adventure game. Players may explore a dungeon, fighting monsters and discovering treasures.

## Game Design

The dungeon is a collection of rooms, connected by up to four doors in the cardinal directions (North, East, South, and West). Both player characters (PCs) and non-player-characters (NPCs) inhabit the rooms of the dungeon and may traverse the rooms. Users can engage in combat with the NPCs as well as other PCs, as well as use items they find in the dungeon.

The user interface is a command-line prompt interface. Users type commands for their PC to perform.

## How to Play

### Gameplay

Upon joining the dungeon game, you will be notified that you have been placed in a room. Then, you will be prompted to type action commands. You will receive a response to your commands from the server, and you will also receive notifications in real time of events which occur in the room in which you are.

If you specify an incorrect value in your command, or type an invalid command, you will receive an error message. For example, if you try to attack a target which is not in the room, that would be an invalid command.

### Examples of Actions

* `look`: Examine the contents of the room.
* `say <player> <message>`: Whisper a message to a player in the room. Specify the player by typing their name, and write whatever you like in the message, but please be polite.
* `enter <direction> door`: Enter the door (to another room) of the specified direction. Specify the direction by typing one of the following: `north`, `east`, `south`, `west`.
* `attack <target>`: Attack the specified target (i.e. an NPC or another PC). Specify the target by typing its name.
* `pick up <weapon>`: Pick up a weapon in the room to receive an attack bonus. Specify the weapon by typing its name.
* `drink <potion>`: Drink a potion in the room to restore lost health. Specify the potion by typing its name.


## Starting the Server



## Connecting to the Server



## Game Configuration






[elbereth]: http://nethack.wikia.com/wiki/Elbereth
[mud]: http://en.wikipedia.org/wiki/MUD
