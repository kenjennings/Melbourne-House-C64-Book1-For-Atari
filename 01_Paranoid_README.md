# Paranoid

This draws kaleidescope-like random patterns on the high res screen screen.

[![AtariParanoidScreen](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/raw/master/01_Paranoid_At8.png)](#features)

Video of the animation on YouTube: https://youtu.be/Fc3VV4xq25s

Modifications in Atari port:

- Some macros were optimized into re-usable functions which reduces final code size.

- Delta handling logic was altered to just increment/decrement values rather than use signed math.

Additionally, the console Keys do the following:

- Option - Clear Screen (Atari800 emulator default F2)

- Select - Increment background color (Atari800 emulator default F3)

- Start - Toggle plotting logic between OR, and EOR. (Atari800 emulator default F4)
 
 
[Back to Home](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/README.md "Home") 
