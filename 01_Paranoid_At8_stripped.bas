14 GRAPHICS 8+16 : R. The +16 eliminates text window.
15 COLOR 1 : R. And set the pixel drawing color.
32 SETCOLOR 4,0,0: R. Border black.
33 SETCOLOR 2,0,15: R. Background white
37 SETCOLOR 1,0,0 : Drawn pixels black
82 X=79:Y=49:DX=INT(RND(0)*3-1):DY=INT(RND(0)*3-1): IF DX=0 AND DY=0 THEN GOTO 82 
95 PL. X,Y:PL. 319-X,Y: PL. 319-X,191-Y: PL. X,191-Y 
102 Y1=Y*2:X1=X*2:PL.X1,Y1:Y1=191-Y1:X1=319-X1:PL.X1,Y1
112 X=X+DX:Y=Y+DY:IF X<0 OR X>159 THEN DX=-DX:GOTO 112
122 IF Y<0 OR Y>95 THEN DY=-DY:GOTO 112
132 IF RND(0)>.9 THEN DX=INT(RND(0)*3-1)
142 IF RND(0)>.9 THEN DY=INT(RND(0)*3-1)
152 IF DX<>0 OR DY<>0 THEN GOTO 95
162 DX=INT(RND(0)*3-1):DY=INT(RND(0)*3-1):IF DX=0 AND DY=0 THEN GOTO 162
172 GOTO 95
