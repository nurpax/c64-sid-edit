# C64 SID sound editor

C64 SID sound editor written in 6510 assembly.

<img width="588" alt="screen shot 2018-04-25 at 11 55 10 pm" src="https://user-images.githubusercontent.com/297823/39272587-fee3461a-48e4-11e8-82d3-c60d1753f9b8.png">

## How to run:

Prerequisites: Install KickAssembler.

Compile `startup.asm` and load into VICE C64 emulator:

```
# Compile
java cml.kickass.KickAssembler startup.asm -o sidedit.prg
# Run in VICE
x64 sidedit.prg
```

## Keyboard shortcuts

Most keyboard shortcuts (like F for freq, A for attack, etc.) are shown in the UI.

Use F1-F4 to select waveforms.

Press 'X' to load "snapshot saved" sounds from `sounds.bin`.  Note that you need to recompile the editor if you update sounds.bin and want them available in the editor.

## How to save and use edited sounds

This program doesn't have a "save" feature.  However, when running in VICE, you can save a snapshot of the C64 state.  This way you can save and load different sounds.  To use the edited sounds in your own program, you need to extract the sound data from a VICE RAM snapshot.  Here's how to do these steps:

1. Run the editor.  You can load previously edited sound data by loading in a VICE snapshot.
2. Edit sounds.
3. Save a snapshot (on Mac, press F12 to open VICE menu, Select "Snapshot", hit "Quicksave snapshot.svf".)
4. `snapshot.svf` now contains full machine state, including C64 RAM.
5. Extract sounds from snapshot.svf: run "stack runghc GrabSounds.hs".  This will load `snapshot.svf` and store the edited sound parameters into sounds.bin.
6. Import the sounds.bin into your program.  See `load_and_play` function in [startup.asm](https://github.com/nurpax/c64-sid-edit/blob/master/startup.asm) on how to do this.
7. In your app init, call `soundfx.init`.  In your raster IRQ, call `soundfx.play` to play the sounds.

See [Stack Homepage](https://docs.haskellstack.org/en/stable/README/) on how to install the `stack` command used to compile & run `GrabSounds.hs`.
