# Melbourne-House-C64-Book1-For-Atari
Work In Progress -- Porting the C64 code from the Melbourne House C64 Book 1 to the Atari

OldSkoolCoder is running a series of tutorials walking through the very simple games in the book, and converting them to assembly language.

That source is here:  https://github.com/OldSkoolCoder/Melbourne-House-Commodore-64-Games-Book

The OldSkoolCoder Youtube channel is here:  https://www.youtube.com/channel/UCtWfJHX6gZSOizZDbwmOrdg/videos

This repository will be the same programs ported to Atari.  (As I have time and interest).

---

The assembly for the Atari depends on my MADS include library here: https://github.com/kenjennings/Atari-Mads-Includes.  

---

File naming...   

**PORTING**

The **".bas"** text of the BASIC files have several versions.  These are plain text that has not yet been entered into Atari BASIC.  Commands may be present in their abbreviated form (e.g. R. and PL. for REM and PLOT, etc.) These are Work In Progress files to figure out what the code is doing, and determine the Atari equivalents, so it is possible they can't be ENTER'ed into Atari BASIC...

- The **"At8_commentary"** file is the C64 BASIC program with each line commented, and followed by an explanation of what the line is doing, and then followed by the equivalent Atari commands if applicable.

- The **"At8"** file for the Atari is the same as the commentary file with the C64 commands removed.

- The **"At8_stripped"** file is the Atari version above with most of the extra commentary removed, so it resembles the original C64 code, but works for Atari.

**Atari BASIC**

Other files have been run through Atari BASIC and these files are the text listing from BASIC.

- **".LIS"** files are listed from Atari BASIC.  They should be the working equivalent of the "commentary" text files with all the comments.
 
- **".LXL"** files are listed from the nicer OSS BASIC XL which is 100% compatible with Atari BASIC and includes extra commands and other friendly niceties such as upper/lower case formatted keywords.   The contents of the ".LXL" files have the C64-specific code removed, the lines are renumbered, formatting/line lengths may be revised, and logic improvements may be present.

**Atari Assembly**

The **".asm"** assembly files have several versions, too:

- The **"C64andAt8"** file is the original ported version with C64-specific code commented out where it occurs and followed by the equivalent Atari assembly.

- The **"At8"** version is the "C64andAt8" version with the commented C64 code removed leaving only the Atari-specific code.  Logic, behavior improvements, and code modularization not present in the original may be included here. 

---

**[01 Paranoid](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/README01Paranoid.md "Paranoid")**

This draws kaleidescope-like random patterns on the high res screen screen.

https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari/blob/master/README01Paranoid.md

[![AtariHBFITBScreen](https://github.com/kenjennings/Melbourne-House-C64-Book1-For-Atari
/raw/master/01_Paranoid_At8.png)](#features)

Video of the animation on YouTube: https://youtu.be/WLELDFDT3uM

Modifications in Atari port:

- Some macros were optimized into re-usable functions which reduces final code size.

- Delta handling logic was altered to just increment/decrement values rather than use signed math.

Additionally, the console Keys do the following:

- Option - Clear Screen (Atari800 emulator default F2)

- Select - Increment background color (Atari800 emulator default F3)

- Start - Toggle plotting logic between OR, and EOR. (Atari800 emulator default F4)

---

More to come.                          ?

