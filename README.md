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

`p $ pSine "s" 0 1 1`

This creates a Sine program and places it in the "s" slot, which is the main slot you'll see displayed.


To get it moving:

`p $ pSine "s" (* 1) 1 1`

Since the first input of `pSine` is the x position, that's the one we want to edit. If there's a function in that position like `(* 1)` then it will take the base time and apply that function to it.


Let's make it scale too:

`p $ pSine "s" (* 1) ((+ 1) . (* 2)) 1`

Note that we can combine functions with Haskell's `.` operator.


But it's still jerky! We can make it slightly more smooth by using a handy built in `sinMod'` that oscilates between 0 and 1.

`p $ pSine "s" (* 1) ((+ 1) . (* 2) . sinMod') 1`

There's also a `sinMod` operator that goes between -1 and 1, and `cos` versions of both.


Now let's make it react to some audio!

`p $ pSine "s" (* 1) ((+ 1) . (* 2) . sinMod') (VolumeInput, 1)`

We can use float inputs like `VolumeInput` in place of any float value or function. The form for inputs is always `([Input], [float modifier])`


Adding an effect can be done with the `|+|` operator

```
p $ pSine "s" (* 1) ((+ 1) . (* 2) . sinMod') (VolumeInput, 1)
  |+| pFade 0.98
```

If we use an array instead of a plain float value, it goes in sequence over Oscillare's cycle

```
p $ pSine "s" (* 1) ((+ 1) . (* 2) . sinMod') (VolumeInput, 1)
  |+| pFade [0.5, 0.98]
```

We can also use `pt` to switch between programs but keep them in memory, and apply effects to both programs:

```
p "s" $ pt ["a", "b"']
  |+| pFade [0.5, 0.98]

p $ pSine "a" (* 1) ((+ 1) . (* 2) . sinMod') (VolumeInput, 1)

p $ pAudioData "b" 1 (AudioTexInput, 1)
```

If we want to combine them instead of switching between them, that's easy too.

```
p "s" $ pAdd ["a", "b"']
  |+| pFade [0.5, 0.98]

p $ pSine "a" (* 1) ((+ 1) . (* 2) . sinMod') (VolumeInput, 1)

p $ pAudioData "b" 1 (AudioTexInput, 1)
```

Finally let's slow everything down a bit:

```
t 2.3
```

### Inputs

There are three input types: strings, floats, and textures. Each one can be a single value, a list, a pattern, or a backend input with modifier. 

Single values are sent every frame, lists are split evenly over a single cycle, and patterns are evaluated and sent. 

Backend inputs have to be selected from valid input types:

Float inputs are `VolumeInput`, and `KickInput`. The modifier for both multiplies the value.

Texture inputs are `(CameraTexInput, [not used])`, `(EqTexInput, [number of bands])`, and `(AudioTexInput, [not used])`.

There are no string inputs right now.

### Base Programs

Base programs draw directly to an FBO.

```
pAudioData Volume(float) Data(texture)
pDots  Volume(float) Data(texture)
pFlocking  Separation(float) Mult(float) Speed(float) 
pLines  Width(float) Spacing(float)
pImage Image(Image)
pInput  Input(texture)
pShapes  Sides(float) Width(float) Size(float) 
pStringTheory  TimeMod(float) Angle(float) AngleDelta(float) Xoff(float) 
pSine  XPos(float) Scale(float) Amplitude(float) 
pText  Text(string)
```

### Effects

Effects take a base fbo and perfor some operation on it.

```
pBrightness Brightness(float)
pFade Fade(float)
pFilter Filter(float)
pMirror
pOverlay Fade(float)
pRepeat Times(float)
pReverse
pRotate Rotation(float)
pScale XScale(float) YScale(float)
pScale' XYScale(float)
pTranslate XScale(float) YScale(float)
```

### Passthroughs

Passthroughs take a slot name (or list, or pattern) and apply their own effects on top.

```
pt Slot(string)
ptTriggered Slots(string) // Note that since triggers can span multiple cycles, the Slots input here is a space-separated list of slots e.g. "a b c"
```

### Layers

Layers combine multiple slots into one and add effects

```
pAdd Slot(string)
pMult Slot(string)
```

### Tips and Tricks

- Alpha transparent emacs can be found at https://www.emacswiki.org/emacs/TransparentEmacs

### Known Issues

- On OSX you have to start oschader-cinder first. If you quit oschader-cinder you have to restart oscillare.
- Effects on programs using passthrough don't work.
- EqTexInput is buggy
- Stack overflows are still possible - be wary!

    Copyright (C) 2016  Ulysses Popple
