# C64 SID sound editor

C64 SID sound editor written in 6510 assembly.

## How to run:

Compile `startup.asm` and load into VICE C64 emulator.

## How to save and use edited sounds

This program doesn't have a "save" feature.  However, when running in VICE, you can save a snapshot of the C64 state.  This way you can save and load different sounds.

To use the edited sounds in your program, link in soundfx.asm and call soundfx.play from your raster interrupt.

You will also need to grab the edited sound parameters from a saved VICE snapshot.  Use the Haskell GrabSounds.hs module for that.
