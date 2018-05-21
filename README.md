# Melbourne-House-C64-Book1-For-Atari
Porting the C64 code from the Melbourne House C64 Book 1 to the Atari

OldSkoolCoder is running a series of tutorials walking through the usually very simple games in the book, and converting some of them to assembly language.

That source is here:  https://github.com/OldSkoolCoder/Melbourne-House-Commodore-64-Games-Book

The OldSkoolCoder Youtube channel is here:  https://www.youtube.com/channel/UCtWfJHX6gZSOizZDbwmOrdg/videos

This repository will be the same programs ported to Atari.  (As I have time/interest).

---

The assembly for the Atari depends on my MADS include library here: https://github.com/kenjennings/Atari-Mads-Includes.  

---

** 01 Paranoid**
This draws a kaleidescope-like random patterns on the screen.

Right now, this is just quick conversion of the BASIC source code from C64 BASIc to Atari BASIC.

There are several striking differences between the Atari BASIC version and the C64 BASIC versions of the same program. Atari BASIC comes with commands to support graphics, which makes the code much more readable, and shorter, not to mention it runs far faster even though Atari BASIC uses floating point numbers for everything.  Atari BASIC's tokenization allows spaces to delimit commands and variables without wasting memory which also contributes to source readability. 

WIP -- At the moment the raw text source is present for Atari BASIC.   This needs to be cleaned up and presented as it would be listed from Atari BASIC.  After that, the next mission is to convert it to assembly. 

---

More to come.
