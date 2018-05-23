# Melbourne-House-C64-Book1-For-Atari
Work In Progress -- Porting the C64 code from the Melbourne House C64 Book 1 to the Atari

OldSkoolCoder is running a series of tutorials walking through the very simple games in the book, and converting them to assembly language.

That source is here:  https://github.com/OldSkoolCoder/Melbourne-House-Commodore-64-Games-Book

The OldSkoolCoder Youtube channel is here:  https://www.youtube.com/channel/UCtWfJHX6gZSOizZDbwmOrdg/videos

This repository will be the same programs ported to Atari.  (As I have time and interest).

---

The assembly for the Atari depends on my MADS include library here: https://github.com/kenjennings/Atari-Mads-Includes.  

---

File naming...   Still deciding what BASIC files to keep for presentation. There is a lot of flotsam in the file lists.  Some of this would go away when I think up a standard.

**PORTING**
The text of the BASIC files have several versions.  These are edited text that have not yet been entered into Atari BASIC.  REMark and several other commands are presented in their abbreviated form (e.g. R. and PL. for REM and PLOT, etc.):

- The "commentary" file is the C64 BASIC program with each line commented, and followed by an explanation of what the line is doing, and then followed by the equivalent Atari commands if applicable.

- The regular file for the Atari is the same as the commentary file with the C64 commands removed.

- The "stripped" file is the Atari version above with most of the extra commentary removed.

**BASIC**

Other files have been run through Atari BASIC and are the text output of listings from BASIC.

- ".LIS" files are listed from Atari BASIC.  
 
- ".LXL" files are listed from the nicer OSS BASIC XL which is 100% compatible with Atari BASIC and includes extra commands and other friendly niceties such as upper/lower case formatted keywords.   

**Assembly**

The assembly files have several versions, too:

- The "C64andAt8" file is the original ported version with C64-specific code commented out where it occurs, and followed by the equivalent Atari assembly.

- The "At8" version is the "C64andAt8" version with the commented C64 code removed.

---

**01 Paranoid**

This draws kaleidescope-like random patterns on the high res screen screen.

---

More to come.                          ? 

