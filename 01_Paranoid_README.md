# Paranoid

This draws kaleidescope-like random patterns on the high resolution screen.

[![AtariParanoidScreen](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/raw/master/01_Paranoid_At8.png)](#features)

Video of the animation on YouTube: https://youtu.be/Fc3VV4xq25s

---

**Porting BASIC**

These are porting "discussions" and may not actually be working if ENTER'd into Atari BASIC.

[01_Paranoid_At8_commentary.bas](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/01_Paranoid_At8_commentary.bas "01_Paranoid_At8_commentary.bas")  Commented original C64 BASIC plus Atari notes.

[01_Paranoid_At8.bas](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/01_Paranoid_At8.bas "01_Paranoid_At8.bas") Atari BASIC text with comments.

[01_Paranoid_At8_stripped.bas](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/01_Paranoid_At8_stripped.bas "01_Paranoid_At8_stripped.bas") Atari BASIC text with most comments removed.

**Running BASIC**

Working BASIC Code that can be loaded with the ENTER command and then RUN:

[PARAN01.LIS](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/PARAN01.LIS "PARAN01.LIS") Atari BASIC listing from working program.

[PARAN01.LXL](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/PARAN01.LXL "PARAN01.LXL") OSS BASIC XL listing from working program.

**ASSEMBLY**

[01_Paranoid_C64andAt8.asm](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/01_Paranoid_C64andAt8.asm "01_Paranoid_C64andAt8.asm") Assembly program with C64 and Atari code, and commentary.

[01_Paranoid_At8.asm](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/01_Paranoid_At8.asm "01_Paranoid_At8.asm") Atari-specific Assembly program with some optimizations and changes.

---

Modifications in Atari Assembly port:

- Some macros are optimized into re-usable functions which reduces final code size.

- Delta handling logic is altered to just increment/decrement values rather than use signed math.

Additionally, the console keys do the following:

- Option - Clear Screen (Atari800 emulator default F2)

- Select - Increment background color (Atari800 emulator default F3)

- Start - Toggle plotting logic between OR, and EOR. (Atari800 emulator default F4)
 
---

[Back to Home](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/README.md "Home") 
