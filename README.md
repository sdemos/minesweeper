# Minesweeper

An implementation of the game of minesweeper in Haskell, specifically targeted
toward writing AIs. The implementation is pretty simple, and doesn't have
anything that guarantees mine placement being reasonable, or any convinience
behaviors for playing, such as automatically revealing empty space around a
click, or guaranteeing the first click will be empty space (like the windows
implementation of minesweeper, for instance).

## Future Work

* There should be a nicer, more well defined API for interacting with the
  minesweeper game, or at least one document the one that is there if it turns
  out to be a good API

* Implement flagging squares that the player believes are mines

* Guarantee the first square is a square with `Danger 0`

* Clean up implementation a bit (break out some files, make everything look a
  little better, document functions)
