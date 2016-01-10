# Alluring Allegory

A story driven game.

A game created for http://itch.io/jam/january-2016-lisp-game-jam

# Sample screenshot

![Early Animation](http://ahungry.com/images/alluring-allegory.gif)
![Early Screen](http://ahungry.com/images/alluring-allegory.png)

# Usage

Make sure you have a working copy of SBCL + Quicklisp.

Clone the repository into your local project directory:

```sh
  git clone https://github.com/ahungry/alluring-allegory
```

Hop into the file 'settings.lisp' and make a quick edit to the asset
path variable, to make sure it loads your assets (based on where you
cloned the repository):

```
(setf *asset-path* "/your/path/to/this/game")
```

The default is to look for the assets in the same directory you load
the game from - if you have them in another location, you will want to
update the last part of the setf line to match where.

Alternatively, you can just specify an absolute path name, similar to this:

Start SBCL and load/run it with:

```lisp
(ql:quickload :alluring-allegory)
(alluring-allegory:main)
```

If there is actual interest, I'll add some binaries, but I think
loading the source is the easiest and most likely to work method.

# License

AGPLv3 or later

Portions of the story (in ./src/story.lisp) are under a special
license found under the ./ebooks directory (they are public domain,
but the electronic work itself must be distributed with the full
license and cannot be sold without stipulations, so please note
content in ./ebooks and ./src/story.lisp is not fully AGPLv3, but
Project Guttenberg license).
