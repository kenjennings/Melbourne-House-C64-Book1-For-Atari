14 GRAPHICS 8+16 : R. The +16 eliminates text window.
15 COLOR 1 : R. And set the pixel drawing color.
32 SETCOLOR 4,0,0: R. Border black.
33 SETCOLOR 2,0,15: R. Background white
35 R. Note that Atari doesn't have color map.  The drawn pixel 
36 R. color is set by color register.  Here, set it to black:
37 SETCOLOR 1,0,0
81 R. Initialize Delta variables. Redo if they both end up 0.
82 X=79:Y=49:DX=INT(RND(0)*3-1):DY=INT(RND(0)*3-1): IF DX=0 AND DY=0 THEN GOTO 82
91 R. Plotting pixels.  
95 PL. X,Y:PL. 319-X,Y: PL. 319-X,191-Y: PL. X,191-Y 
101 R. More math and more plotting calls.
102 Y1=Y*2:X1=X*2:PL.X1,Y1:Y1=191-Y1:X1=319-X1:PL.X1,Y1
111 R. Increment deltas and fix horizontal boundary collision.
112 X=X+DX:Y=Y+DY:IF X<0 OR X>159 THEN DX=-DX:GOTO 112
121 R. Fix vertical boundary collision 
122 IF Y<0 OR Y>95 THEN DY=-DY:GOTO 112
131 R. X Random direction change.
132 IF RND(0)>.9 THEN DX=INT(RND(0)*3-1)
141 R. Y Random direction change.
142 IF RND(0)>.9 THEN DY=INT(RND(1)*3-1)
151 R. If Deltas are non-zero, then go plot again.
152 IF DX<>0 OR DY<>0 THEN GOTO 95
161 R. They were 0, so try to update it again.  It has to work eventually.
162 DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IF DX=0 AND DY=0 THEN GOTO 162
171 R. Loop back to plot next group of points.
172 GOTO 95
