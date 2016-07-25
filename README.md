## Oscillare

A library for live programming of visual shaders. Inspired by Alex McLean's https://github.com/tidalcycles/Tidal.

Requires https://github.com/ulyssesp/oschader-cinder as a backend.

### Installation

#### Emacs

Add these lines to your configuration. If you're using spacemacs they should go into `user-config`. Replace the load path with where Oscillare is located.

```
(setq haskell-program-name "stack ghci")

(add-to-list 'load-path "c:/Users/Ulysses/Development/oscillare")
(require 'haskell-mode)
(require 'oscillare)
```

#### Other editors

You can probably try the instructions located at http://tidalcycles.org/getting_started.html, but they haven't been tested.


### Usage

First, make sure it's installed correctly:

1. Install oscillare
2. Start [Oschader](`https://github.com/ulyssesp/oschader-cinder`)
3. Run `oscillare-start-haskell`
4. Double check that you see `Prelude Oscillare>` when you run `oscillare-see-output`

Now it's time to get started! Check out `scratch.osc` for some example programs. So long as everything is set up correctly and you have a .osc file open you can run individual lines by pressing `Ctrl-c Ctrl-c`. You can see the haskell buffer by pressing `Ctrl-c Ctrl-s`.

For example, to display a simple sine wave:

`p "s" $ pm (progName Sine) []`

This creates a Sine program and places it in the "s" slot, which is the main slot you'll see displayed.


To get it moving:

`p "s" $ pm (progName Sine) [timeUniform]`

This will send the time to the "time" uniform every frame.


Let's make it scale too:

`p "s" $ pm (progName Sine) [up "scale" $ uf <$> timePattern, timeUniform]`

This will send the time to a "scale" uniform every frame.


We can make it slightly more smooth by using the `Pattern` functor:

`p "s" $ pm (progName Sine) [up "scale" $ uf . sin . (* 3.1415) <$> timePattern, timeUniform]`


Now let's make it react to some audio!

`p "s" $ pm (progName Sine) [up "scale" $ ui <$> pure (Volume, 1), timeUniform]`


Adding an effect is just chaining to another program:

```
p "s" $ pme (progName Sine) "p1" [up "scale" $ ui <$> pure (Volume, 1), timeUniform]

p "p1" $ pm (progName Fade) [up "fade" $ uf <$> pure 0.97]
```

`seqp` sequences a list of patterns to all take place within one cycle

```
p "s" $ pme (progName Sine) "p1" [up "scale" $ ui <$> pure (Volume, 1), timeUniform]

p "p1" $ pm (progName Fade) [up "fade" $ uf <$> (seqp [pure 0.5, pure 0.97])]
```

We can also use pattern to switch between programs but keep them in memory:

```
p "s" $ pt $ seqp [pure "p3", pure "p4"] 

p "s1" $ pme (progName Sine) "p1" [up "scale" $ ui <$> pure (Volume, 1), timeUniform]

p "s1" $ pme (progName AudioData) "p1" [up "tex_audio" $ ui <$> pure (AudioTexture, 1)]
```

Finally let's slow everything down a bit:

```
t 2.3
```


Let's take this apart a bit. `p` puts the following program or effect into a slot. `pm` gives you a program or effect with no effects chained. `pme` chains an effect. 

Next comes the program or effect. Right now programs are `Sine`, `AudioData`, and `Dots`. Effects are `Fade`, `Scale`, and `Repeat`.

Finally there are uniform patterns. These are evaluated every frame and sent to oschader-cinder. You can make a pattern only happen once using `att` along with a time from 0-1. The program messages use this so it's only sent once every cycle. You can make a bunch of patterns happen one after another using `seqp` which will switch between patterns throughout the cycle.

The tempo is set using `t`. The default is 1 second, but any number of seconds will do.

### Tips and Tricks

- Alpha transparent emacs can be found at https://www.emacswiki.org/emacs/TransparentEmacs

### Known Issues

- On OSX you have to start oschader-cinder first. If you quit oschader-cinder you have to restart oscillare.



    Copyright (C) 2016  Ulysses Popple
