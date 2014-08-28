
# What is this?

This is a simple graphical application showing the behavior of
the binary transform in practice.

# What is the binary transform?

The binary transform is a sistematic way of producing a binary
list from another binary list induced a bijection. More information
on the binary transform can be found
[here](https://github.com/plow-technologies/writings/tree/master/binary-transform).

# What is a binary list?

Binary lists are lists whose number of elements is a power of two.
There is a [package in Hackage](http://hackage.haskell.org/package/binary-list)
where they are implemented.

# Compiling the application

## With cabal

A straightforward `cabal build` should just work, except that if you
don't have the required GL libraries in a Linux system, you might want to run

```
sudo apt-get install freeglut3-dev
```
to get them.

If you are under Windows, the executable will need `glut32.dll` to work. Isn't
this distributed with Haskell Platform anyway?

If you are in a Mac Computer... who knows.

## With GHC

However, since this application has very few dependencies, I myself wouldn't
let cabal-install build it for me. I'd rather

```
ghc -O2 -threaded downsampling.hs
```

Although you'll need to install a couple of libraries first, if you don't already
have them.

```
cabal install binary-list
cabal install gloss
```

# Running the application

OK, so you have the application running. You'll see the graph of some function
and two labels displaying the current method that is being used and the current
resolution. You can change both things by pressing the arrow keys. A lower
resolution means that you are seeing the result of applying the inverse binary
transform to a section of the encoded data, thus producing a result with less
points. When you change the method, you change the bijection used in the binary
transform. If the 1/1 resolution doesn't look like the original data, it means
it isn't actually a bijection.
