# C64 SID sound editor

C64 SID sound editor written in 6510 assembly.

<img width="588" alt="screen shot 2018-04-25 at 11 55 10 pm" src="https://user-images.githubusercontent.com/297823/39272587-fee3461a-48e4-11e8-82d3-c60d1753f9b8.png">

## How to run:

Compile `startup.asm` and load into VICE C64 emulator.

## How to save and use edited sounds

This program doesn't have a "save" feature.  However, when running in VICE, you can save a snapshot of the C64 state.  This way you can save and load different sounds.

To use the edited sounds in your program, link in soundfx.asm and call soundfx.play from your raster interrupt.

You will also need to grab the edited sound parameters from a saved VICE snapshot.  Use the Haskell GrabSounds.hs module for that.
