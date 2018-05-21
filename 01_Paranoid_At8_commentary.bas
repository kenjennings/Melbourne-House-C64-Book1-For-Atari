0 R. 0 BACKGROUND=1
1 R. N/A. 
2 R. "BACKGROUND" isn't a C64 BASIC command.
3 R. A variable named "BACKGROUND" is never referenced.

10 R. 5 POKE55,255:POKE56,31
11 R. N/A.
12 R. This changes C64 end of BASIC/start of strings to reserve memory for graphics.
13 R. Atari OS/Atari BASIC GRAPHICS command will reserve space.
14 GRAPHICS 8+16 : R. The +16 eliminates text window.
15 COLOR 1 : R. And set the pixel drawing color.

20 R. 6 DIMP(7):FORI=0TO7:P(I)=2^(7-I):NEXT
21 R. N/A
22 R. Atari OS/Atari BASIC has pixel plotting support built-in.

30 R. 10 V=53248:POKEV+32,0:POKEV+33,0
31 R. Border color and Background color.
32 SETCOLOR 4,0,0: R. Border black.
33 SETCOLOR 2,0,15: R. Background white
34 R. OR it could be done unfriendlier like POKE 712,0:POKE 710,15
35 R. Note that Atari doesn't have color map.  The drawn pixel 
36 R. color is set by color register.  Here, set it to black:
37 SETCOLOR 1,0,0

40 R. 30 POKEV+24,PEEK(V+24)OR8
50 R. 40 POKEV+17,PEEK(V+17)OR32
51 R. N/A.
52 R. Setup registers for graphics display.
53 R. Already done on the Atari by the GRAPHICS command.

60 R. 50 FORI=1024TO2024:POKEI,BA:NEXT
70 R. 60 FORI=8192TO8192+8*1024:POKEI,0:NEXT
71 R. N/A.
72 R. Clearing the screen. (Is BA assigned a value?)
73 R. Already done on the Atari by the GRAPHICS command.

80 R. 100 X=79:Y=49:DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IFDX=0ANDDY=0THEN100
81 R. Initialize Delta variables. Redo if they both end up 0.
82 X=79:Y=49:DX=INT(RND(0)*3-1):DY=INT(RND(0)*3-1): IF DX=0 AND DY=0 THEN GOTO 82

90 R. 105 Y1=Y:X1=X:GOSUB1000:X1=319-X:GOSUB1000:Y1=199-Y:GOSUB1000:X1=X:GOSUB1000
91 R. Plotting pixels.  
92 R. No subroutine needed on Atari.
93 R. One difference is Atari default screen is 192 scan lines, not 200.
94 R. That is a software limit imposed by OS, not a video hardware limit.
95 PL. X,Y:PL. 319-X,Y: PL. 319-X,191-Y: PL. X,191-Y 

100 R. 107 Y1=Y*2:X1=X*2:GOSUB1000:Y1=199-Y1:X1=319-X1:GOSUB1000
101 R. More math and more plotting calls.
102 Y1=Y*2:X1=X*2:PL.X1,Y1:Y1=191-Y1:X1=319-X1:PL.X1,Y1

110 R. 110 X=X+DX:Y=Y+DY:IFX<0ORX>159THENDX=-DX:GOTO110
111 R. Increment deltas and fix horizontal boundary collision.
112 X=X+DX:Y=Y+DY:IF X<0 OR X>159 THEN DX=-DX:GOTO 112

120 R. 115 IFY<0ORY>99THENDY=-DY:GOTO110
121 R. Fix vertical boundary collision 
122 IF Y<0 OR Y>95 THEN DY=-DY:GOTO 112

130 R. 120 IFRND(1)>.9THENDX=INT(RND(1)*3-1)
131 R. X Random direction change.
132 IF RND(0)>.9 THEN DX=INT(RND(0)*3-1)

140 R. 130 IFRND(1)>.9THENDY=INT(RND(1)*3-1)
141 R. Y Random direction change.
142 IF RND(0)>.9 THEN DY=INT(RND(0)*3-1)

150 R. 135 IFDX<>0ORDY<>0THEN105
151 R. If Deltas are non-zero, then go plot again.
152 IF DX<>0 OR DY<>0 THEN GOTO 95

160 R. 140 DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IFDX=0ANDDY=0THEN140
161 R. They were 0, so try to update them again.  It has to work eventually.
162 DX=INT(RND(0)*3-1):DY=INT(RND(0)*3-1):IF DX=0 AND DY=0 THEN GOTO 162

170 R. 150 GOTO105
171 R. Loop back to plot next group of points.
172 GOTO 95

180 R. 1000 YA=INT(Y1/8):YB=Y1-YA*8:XA=INT(X1/8):XB=X1-XA*8
190 R. 1005 P=8*1024+YA*320+XA*8+YB:XC=P(XB)
200 R. 1010 POKEP,PEEK(P)ORXC:RETURN
201 R. N/A all of that.
202 R. Atari OS/Atari BASIC has pixel plotting support built-in.
