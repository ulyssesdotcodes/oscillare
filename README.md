## Oscillare

A library for live programming of visual shaders. Inspired by Alex McLean's https://github.com/tidalcycles/Tidal.

Requires https://github.com/ulyssesp/oschader-cinder as a backend.

### Installation

#### Prerequisites

* TouchDesigner
* Haskell (only tested with [haskellstack](https://docs.haskellstack.org/en/stable/README/))
* An editor (preferably emacs)

#### Editor configuration

##### Emacs

If you're using spacemacs, in `dotspacemacs-configuration-layers` add `(haskell :variables haskell-completion-backend 'intero)`.

Add these lines to your configuration. If you're using spacemacs they should go into `user-config`. Replace the load path with where Oscillare is located.

```
(setq haskell-program-name "stack ghci")

(add-to-list 'load-path "c:/Users/Ulysses/Development/oscillare")
(require 'haskell-mode)
(require 'oscillare)
```

##### Other editors

You can probably try the instructions located at http://tidalcycles.org/getting_started.html, but they haven't been tested.


### Running

#### 1. Start TouchDesigner

You have to start with the correct project loaded. Open `FunctionalDesigner.toe` from the TD directory.

#### 2. Load oscillare

1. Open `scratch.osc`
2. Run the command `intero-mode`
3. Run the command `oscillare-start-haskell` 
4. Switch back to `scratch.osc`
5. Run the command `oscillare-mode`
6. Press C-Enter while the cursor is over the code directly below "Initialization check"


---------------

Note: This is really bad right now. It will be easier soon. Sorry for the hassle.


#### 3. Check that it's working in TouchDesigner

The `lambda` COMP should have a TOP output of a bunch of triangles that move!

### Usage

Now it's time to get started! Check out `scratch.osc` for some example programs. So long as everything is set up correctly and you have a .osc file open you can run individual lines by pressing `Ctrl-c Ctrl-c`. You can see the haskell buffer by pressing `Ctrl-c Ctrl-s`.

For example, to display a simple sine wave:

`r $ sineT (float 0) (float 1) (float 1)` 

This creates a Sine program and places it in the "s" slot, which is the main slot you'll see displayed.


To get it moving:

`r $ sineT seconds (float 1) (float 1)` 

Since the first input of `sineT` is the x position, that's the one we want to edit. Using `seconds` there maps the x position to `absTime.seconds` in TouchDesigner.


Let's make it scale too:

`r $ sineT seconds (float 2 !+ (scycle 1 1 !* float 2)) (float 1)` 

`scycle x y` cycles the value `y` at `seconds * x` speed. Note that we can combine functions with `!+` and `!*`.


But it's still jerky! We can make it slightly more smooth by using `sincycle` instead of `scycle` that oscilates between 0 and 1.

`r $ sineT seconds (float 2 !+ (sincycle 1 1 !* float 2)) (float 1)` 


Now let's make it react to some audio!

`r $ sineT seconds (float 2 !+ (sincycle 1 1 !* float 2)) (volc)` 

We can use float inputs like `volc` in place of any float value or function. 

Adding an effect can be done with the `&` operator

```
r $ sineT seconds (float 2 !+ (sincycle 1 1 !* float 2)) (volc)
  & fade (float 0.95)
```


We can also use `triggerops` to switch between programs but keep them in memory, and apply effects to both programs:

```
r $ triggerops (constC [sincycle 1 1])
  [ sineT seconds (float 2 !+ (sincycle 1 1 !* float 2)) (volc)
  , adata (float 1)
  ]
```

If we want to combine them instead of switching between them, that's easy too.

```
r $ addops
  [ sineT seconds (float 2 !+ (sincycle 1 1 !* float 2)) (volc)
  , adata (float 1)
  ]
```

### Tips and Tricks

- Alpha transparent emacs can be found at https://www.emacswiki.org/emacs/TransparentEmacs

    Copyright (C) 2017  Ulysses Popple
