;*******************************************************************************
;* Tutorial Twenty-Eight Paranoid Coversion                                    *
;*                                                                             *
;* Written By John C. Dale                                                     *
;* Tutorial #28                                                                *
;* Date : 5th Jan, 2018                                                        *
;*                                                                             *
;* Atari port                                                                  *
;* Ken Jennings                                                                *
;*                                                                             *
;*******************************************************************************
;*                                                                             *
;*******************************************************************************

; ==========================================================================
; Atari: System Includes

	icl "ANTIC.asm" ; Graphics display
	icl "GTIA.asm"  ; Color registers
	icl "POKEY.asm" ; Random value register
	icl "OS.asm"    ; Need for internal clock
	icl "DOS.asm"   ; This provides the LOMEM, start, and run addresses.

; 10 SYS (2064)

;*******************************************************************************
;*                                                                             *
;* Debug Watch Variables                                                       *
;*                                                                             *
;*******************************************************************************

;WATCH Prog_X
;WATCH Prog_Y
;WATCH Prog_DX
;WATCH Prog_DY
;WATCH Prog_Y1
;WATCH Prog_X1
;WATCH Prog_Pos
;WATCH Prog_YA
;WATCH Prog_YB
;WATCH Prog_XA
;WATCH Prog_XB
;WATCH Prog_XC

;*******************************************************************************
;*                                                                             *
;* BASIC Auto Run Loader                                                       *
;*                                                                             *
;*******************************************************************************

;*=$0801

;    BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

;*=$0810


; ==========================================================================
; Atari:
; Auto Run works by telling DOS the program's Auto-Run address like this:

		ORG DOS_RUN_ADDR
		.word START

;*******************************************************************************
;*                                                                             *
;* Assembly Reference Variables                                               *
;*                                                                             *
;*******************************************************************************

;VIC         = $D000       ; 53248
; N/A Atari.  No VIC, ANTIC instead.  see "ANTIC.asm"

;SCRNSTART     = $0400        ; 1024
; N/A Atari.  This isn;t applicable to the way Atari graphics works.

; GRAPHICSSRT = $2000        ; 8192

GRAPHICSSRT   = $4000+96     ; 16384+96 ; Tweak actual display start. 
; +96 is playing games with alignment, so the first 100 graphics lines are 
; in the first 4K block, and the next are immediately sequential at the start 
; of the next 4K block.  So, what to do with the 96 bytes we skipped?  Maybe use 
; that for a couple lines of on-screen diagnostics if I'm feeling ambitious.

GRAPHICSSTART = $4000        ; 16384  ; Using this nearest exact page boundary
; value so the C64 screen clearing code will work as-is for this.

X_START     = 79
Y_START     = 49

; POINTADDRESS= $14 ; Atari: Moved to the page 0 declarations.

;*******************************************************************************
;*                                                                             *
;* Program Start                                                               *
;*                                                                             *
;*******************************************************************************

;    jmp START


;*******************************************************************************
;*                                                                             *
;* Code Variables                                                              *
;*                                                                             *
;*******************************************************************************

; Atari:
; Capitalize on the ability to load anything anywhere in memory to put
# all the working variables in page 0 and automagically initize them 
# when the program loads...

	ORG $80  ; Put the working variables in Page 0.
	
	
Prog_BA
    .BYTE 0

Prog_PArray
    .BYTE %10000000,%01000000,%00100000,%00010000
    .BYTE %00001000,%00000100,%00000010,%00000001

Prog_X
    .WORD 0

Prog_Y
    .WORD 0

Prog_DX
    .BYTE 0

Prog_DY
    .BYTE 0

Prog_Y1
    .WORD 0

Prog_X1
    .WORD 0

Prog_Pos
    .WORD 0

Prog_YA
    .BYTE 0

Prog_YB
    .BYTE 0

Prog_XA
    .BYTE 0

Prog_XB
    .BYTE 0

Prog_XC
    .BYTE 0

POINTADDRESS
	.word 0
	
; ==========================================================================
; This is not a complicated program, so lots of RAM is superfluous.  
; Just set code at a convenient place after DOS, DUP, etc.

	ORG LOMEM_DOS_DUP; $3308  From DOS.asm.  First memory after DOS and DUP


;===============================================================================




;*******************************************************************************
;*                                                                             *
;* G^Ray Defender - Randomiser Code from G-Pac Clone Game                      *
;*                                                                             *
;*******************************************************************************

;============================================================
;Init_Random
;    lda #$FF                ; maximum frequency value
;    sta $D40E               ; voice 3 frequency low byte
;    sta $D40F               ; voice 3 frequency high byte
;    lda #$80                ; noise SIRENform, gate bit off
;    sta $D412               ; voice 3 control register
;    rts

;Rand
;    lda $D41B               ; get random value from 0-255
;    rts
;============================================================

; This random management is N/A for Atari.  
; The Atari's POKEY chip provides a register 
; that outputs random 0-255.


;*******************************************************************************
;*                                                                             *
;* Defined Macros                                                              *
;*                                                                             *
;*******************************************************************************

;*******************************************************************************
;*                                                                             *
;* Fill Video Memory Bank With A Value                                         *
;*                                                                             *
;*******************************************************************************
;defm FillVideoMemoryBank    ; StartAddress
;    ; Start Address of Bank
;    ldx #0
;@Looper
;    sta /1,x
;    sta /1 + $0100,x
;    sta /1 + $0200,x
;    sta /1 + $0300,x
;    inx
;    bne @Looper
;    endm

.macro FillVideoMemoryBank StartAddress   
    ; Start Address of Bank
    ldx #0
Looper
    sta :StartAddress,x
    sta :StartAddress + $0100,x
    sta :StartAddress + $0200,x
    sta :StartAddress + $0300,x
    inx
    bne Looper
.endm

;*******************************************************************************
;*                                                                             *
;* Evaluate a Delta Code from a Random Number                                  *
;*                                                                             *
;*******************************************************************************
;defm EvaluateNextDeltaNumber    ; Delta Variable
    
    ;DX=INT(RND(1)*3-1) Results in a number of either -1, 0, +1

;    jsr Rand
;    and #%00000011          ; just give me the 2 least significant bits
;    sec
;    sbc #1
;    sta /1
;    endm

; Actually, that is not right.  The result of the AND could be a 
; value from 0 to 3 -- four values instead of the three needed.
; Therefore the code needs to exclude the undesired fourth value...

.macro EvaluateNextDeltaNumber DeltaVar
    
    ;DX=INT(RND(1)*3-1) Results in a number of either -1, 0, +1

Retry
;    jsr Rand ; N/A Atari
	lda RANDOM
    and #%00000011   ; just give me the 2 least significant bits
	cmp #3           ; but we need only 0, 1, 2, not 3
	beq Retry        ; go try again if it is 3.
    sec
    sbc #1
    sta :DeltaVar
.endm


;*******************************************************************************
;*                                                                             *
;* Copy a Word from One Address to Another Address                             *
;*                                                                             *
;*******************************************************************************
;defm CopyWord ; WordSource, WordTarget

;    lda /1
;    sta /2
;    lda /1 + 1
;    sta /2 + 1
;    endm

.macro CopyWord WordSource, WordTarget

    lda :WordSource
    sta :WordTarget
    lda :WordSource + 1
    sta :WordTarget + 1
.endm
    
;*******************************************************************************
;*                                                                             *
;* Subtract a Number from a Memory Location and Store result in another location
;*                                                                             *
;*******************************************************************************
;defm SubtractNumberWord ; wrdSourceNumber, wrdSubtract, wrdTarget

;    lda #</1
;    sec
;    sbc /2
;    sta /3
;    lda #>/1
;    sbc /2 + 1
;    sta /3 + 1
;    endm

.macro SubtractNumberWord  wrdSourceNumber, wrdSubtract, wrdTarget

    lda #<:wrdSourceNumber
    sec
    sbc :wrdSubtract
    sta :wrdTarget
    lda #>:wrdSourceNumber
    sbc :wrdSubtract + 1
    sta :wrdTarget + 1
.endm
    
;*******************************************************************************
;*                                                                             *
;* Add a twos compliment Byte to an Existing Twos Compliment Word              *
;*                                                                             *
;*******************************************************************************
;defm AddTwosComplimentNumbers ; wrdSource, bytAddition, wrdTarget

;    clc
;    lda /2

;    adc /1
;    sta /3
;    lda /2
;    bpl @JumpCLC
;    clc
;@JumpCLC
;    lda #0
;    adc /1 + 1
;    sta /3 + 1      ; Add the carry over to the HiByte of the wrdTarget
;    endm

.macro AddTwosComplimentNumbers  wrdSource, bytAddition, wrdTarget

    clc
    lda :bytAddition

    adc :wrdSource
    sta :wrdTarget
    lda :bytAddition
    bpl JumpCLC
    clc
JumpCLC
    lda #0
    adc :wrdSource + 1
    sta :wrdTarget + 1      ; Add the carry over to the HiByte of the wrdTarget
.endm


;*******************************************************************************
;*                                                                             *
;* Multiply a word source by 2                                                 *
;*                                                                             *
;*******************************************************************************
;defm MultiplyWordByTwo  ; wrdSource, wrdTarget

;    lda /1
;    asl
;    sta /2
;    lda /1 + 1
;    rol
;    sta /2 + 1
;    endm

.macro MultiplyWordByTwo  wrdSource, wrdTarget

    lda :wrdSource
    asl
    sta :wrdTarget
    lda :wrdSource + 1
    rol
    sta :wrdTarget + 1
.endm
    
;*******************************************************************************
;*                                                                             *
;* Divide the Source Word by eight, and store the result and the remainder     *
;*                                                                             *
;*******************************************************************************
;defm DivideSourceWordByEight ; wrdSource, bytResult, bytRemainder

;    lda /1
;    sta /2
    
;    lda /1 + 1
;    lsr         ; Divide by 2
;    ror /2
;    lsr         ; Divide By 4
;    ror /2
;    lsr         ; Divide By 8
;    ror /2

;    lda /1
;    and #%00000111
;    sta /3
;    endm

.macro DivideSourceWordByEight  wrdSource, bytResult, bytRemainder

    lda :wrdSource
    sta :bytResult
    
    lda :wrdSource + 1
    lsr         ; Divide by 2
    ror :bytResult
    lsr         ; Divide By 4
    ror :bytResult
    lsr         ; Divide By 8
    ror :bytResult

    lda :wrdSource
    and #%00000111
    sta :bytRemainder
.endm



;*******************************************************************************
;*                                                                             *
;* Main Routine                                                                *
;*                                                                             *
;*******************************************************************************
START
;    jsr Init_Random   ; N/A for Atari

Line0               
    ;BACKGROUND=1
;    lda #1
;    sta Prog_BA
; N/A on Atari as this value is only referenced 
; when clearing the 1K screen memory.

Line5               
    ;POKE55,255:POKE56,31
    ; Dont need to convert line 5, as this is related to protecting basic

Line6               
    ;DIMP(7):FORI=0TO7:P(I)=2^(7-I):NEXT

    ;ldx #0
    ;lda #$80
;Line6Loop
    ;sta p,x
    ;lsr
    ;inx
    ;cpx#8
    ;bne Line6Loop

; Lines 10-30 are C64-specific to setup VIC display.

Line10              
    ;V=53248:POKEV+32,0:POKEV+33,0
;    lda #0
;    sta VIC + 32
;    sta VIC + 33

Line30              
    ;POKEV+24,PEEK(V+24)OR8
;    lda VIC + 24
;    ora #8
;    sta VIC + 24

Line40              
    ;POKEV+17,PEEK(V+17)OR32
;    lda VIC + 17
;    ora #32
;    sta VIC + 17

; Below is for Atari to setup the ANTIC display:
	
	lda #0     ; Turn off screen
	sta SDMCTL ; OS Shadow for DMA control

	sta COLOR4 ; Border color  (0 is same as COLOR_BLACK)
	sta COLOR1 ; Drawing color
	lda #COLOR_WHITE
	sta COLOR2 ; Background color

	; Wait for end of frame and screen off before touching display list
	lda RTCLOK60
bLoopWaitFrame
	cmp RTCLOK60
	beq bLoopWaitFrame

	lda #<DisplayList ; point ANTIC to the new display list.
	sta SDLSTL
	lda #>DisplayList
	sta SDLSTH

	lda #ENABLE_DL_DMA|PLAYFIELD_WIDTH_NORMAL ; Turn the display back on.
	sta SDMCTL

Line50              
    ;FORI=1024TO2024:POKEI,BA:NEXT
;    lda Prog_BA
;    FillVideoMemoryBank SCRNSTART
; This is N/A for Atari, as there is no color memory to clear
; and the screen is all graphics mode F (hi res).

Line60              
    ;FORI=8192TO8192+8*1024:POKEI,0:NEXT

    lda #0
    FillVideoMemoryBank GRAPHICSSTART
    FillVideoMemoryBank GRAPHICSSTART+$0400
    FillVideoMemoryBank GRAPHICSSTART+$0800
    FillVideoMemoryBank GRAPHICSSTART+$0C00
    FillVideoMemoryBank GRAPHICSSTART+$1000
    FillVideoMemoryBank GRAPHICSSTART+$1400
    FillVideoMemoryBank GRAPHICSSTART+$1800
    FillVideoMemoryBank GRAPHICSSTART+$1C00

Line100             
    ;X=79:Y=49:DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IFDX=0ANDDY=0THEN100
    lda #0
    
    ldx #X_START
    stx Prog_X
	sta Prog_X + 1
        
    ldy #Y_START
    sty Prog_Y
    sta Prog_Y + 1

    EvaluateNextDeltaNumber Prog_DX
    
    EvaluateNextDeltaNumber Prog_DY

    lda Prog_DX
    bne Line105
    lda Prog_DY
    bne Line105
    jmp Line100

Line105
    ;Y1=Y:X1=X:GOSUB1000:X1=319-X:GOSUB1000:Y1=199-Y:GOSUB1000:X1=X:GOSUB1000
    
    CopyWord Prog_Y, Prog_Y1

    CopyWord Prog_X, Prog_X1

    jsr Line1000
    
    SubtractNumberWord $013F, Prog_X, Prog_X1 ; $13F == 319

    jsr Line1000

    SubtractNumberWord $00C7, Prog_Y, Prog_Y1 ; $C7 = 199

    jsr Line1000

    CopyWord Prog_X, Prog_X1

    jsr Line1000

Line107
    ;Y1=Y*2:X1=X*2:GOSUB1000:Y1=199-Y1:X1=319-X1:GOSUB1000

    MultiplyWordByTwo Prog_Y, Prog_Y1

    MultiplyWordByTwo Prog_X, Prog_X1

    jsr Line1000
    
    SubtractNumberWord $00C7, Prog_Y1, Prog_Y1

    SubtractNumberWord $013F, Prog_X1, Prog_X1

    jsr Line1000

Line110
    ;X=X+DX:Y=Y+DY:IFX<0ORX>159THENDX=-DX:GOTO110
 
    AddTwosComplimentNumbers Prog_X, Prog_DX, Prog_X

    AddTwosComplimentNumbers Prog_Y, Prog_DY, Prog_Y

    lda Prog_X + 1
    bmi bLine110Error
    lda Prog_X
    cmp #159
    bcs bLine110Error
    jmp Line115

bLine110Error
    lda Prog_DX
    eor #$FF
    clc
    adc #01
    sta Prog_DX
    jmp Line110

Line115
    ;IFY<0ORY>99THENDY=-DY:GOTO110

    lda Prog_Y + 1
    bmi bLine115Error
    lda Prog_Y
    cmp #99
    bcs bLine115Error
    jmp Line120
bLine115Error
    lda Prog_DY
    eor #$FF
    clc
    adc #1
    sta Prog_DY
    jmp Line110

Line120
    ;IFRND(1)>.9THENDX=INT(RND(1)*3-1)
;    jsr RandA ; N/A Atari
	lda RANDOM
    cmp #225            ; 90% of 256
    bcc Line130
    
    EvaluateNextDeltaNumber Prog_DX

Line130
    ;IFRND(1)>.9THENDY=INT(RND(1)*3-1)

;    jsr Rand  ; N/A Atari
	lda RANDOM
    cmp #225            ; 90% of 256
    bcc Line135
    
    EvaluateNextDeltaNumber Prog_DY

Line135
    ;IFDX<>0ORDY<>0THEN105
    lda Prog_DX
    bne bLine135
    lda Prog_DY
    bne bLine135
    jmp Line140
bLine135
    jmp Line105

Line140
    ;DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IFDX=0ANDDY=0THEN140

    EvaluateNextDeltaNumber Prog_DX
    
    EvaluateNextDeltaNumber Prog_DY

    lda Prog_DX
    bne Line150
    lda Prog_DY
    bne Line150
    jmp Line140

Line150
	lda #0
	sta ATRACT
    ;GOTO105
    jmp Line105


Line1000
    ;YA=INT(Y1/8):YB=Y1-YA*8:XA=INT(X1/8):XB=X1-XA*8

;    DivideSourceWordByEight Prog_Y1, Prog_YA, Prog_YB

    DivideSourceWordByEight Prog_X1, Prog_XA, Prog_XB

    ; XC=P(XB)
    ldy Prog_XB
    lda Prog_PArray,y
    sta Prog_XC
    
; Atari: 
; Memory is linear on Atari, so the rest of the pixel location 
; determination is considerably easier.
; Byte Address = Y * 40 + GRAPHICSSRT + INT( X / 8 )

; Here do Y * 40, which is the same as (Y * 8) + (Y * 32)

	lda Prog_Y1
	sta POINTADDRESS
	lda #0
;	sta POINTADDRESS+1

	asl POINTADDRESS
	rol  ; * 2
	asl POINTADDRESS
	rol  ; * 4
	asl POINTADDRESS
	rol  ; * 8

	sta POINTADDRESS+1
	pha ; save high byte *8 for later
	
	lda POINTADDRESS ; hold low byte *8 for later
		
	asl POINTADDRESS
	rol POINTADDRESS+1 ; * 16
	asl POINTADDRESS
	rol POINTADDRESS+1 ; * 32

	clc ; Clear carry/borrow. 
	adc POINTADDRESS  ; Add the saved *8 n A to the *32 to get *40.
	sta POINTADDRESS  ; save *40 Result
	pla ; get high byte *8 from stack
	adc POINTADDRESS+1  ; Add to the *32. Now Accumulator is *40
	sta POINTADDRESS+1  ; save *40 Result

; And then add GRAPHICSSRT:

	clc 
	lda #<GRAPHICSSRT
	adc POINTADDRESS
	sta POINTADDRESS
	lda #>GRAPHICSSRT
	adc POINTADDRESS+1
	sta POINTADDRESS+1

; and now, add the INT( X / 8 ) offset on the line.

	clc 
	lda Prog_XA
	adc POINTADDRESS
	sta POINTADDRESS
	bcc Line1005       ; if there was not carry, then skip adding.
	inc POINTADDRESS+1 ; add not needed.  Just inc the high byte

; On the Atari (and Apple... and anything else with linear memory)
; the address determination done above can be sped up and simplified 
; significantly by using a lookup table of 200 words that point to 
; the starting memory address for each Y position.
; Of course, that's throwing 400 bytes of RAM at the implementation 
; where the coded solution is much smaller (but slower).

Line1005
    ;P=8*1024+YA*320+XA*8+YB:XC=P(XB)

    ; 8*1024
;    lda #0
;    sta Prog_Pos
;    lda #$20
;    sta Prog_Pos + 1

    ; YA*320
;    ldy Prog_YA
;@line1005Loop1
;    clc
;    lda #$40
;    adc Prog_Pos
;    sta Prog_Pos
;    lda #1
;    adc Prog_Pos + 1
;    sta Prog_Pos + 1
;    dey
;    bne @Line1005Loop1

    ; XA*8
;    lda Prog_XA
;    asl             ; Multiply By 2
;    asl             ; Multiply By 4
;    asl             ; Multiply By 8
;    pha
    ; Add Carry to Position Hi Byte
;    lda #0
;    adc Prog_Pos + 1
;    sta Prog_Pos + 1
;    pla
;    clc
;    adc Prog_Pos
;    sta Prog_Pos
;    lda Prog_Pos + 1
;    adc #0
;    sta Prog_Pos + 1

    ; YB
;    lda Prog_YB
;    clc
;    adc Prog_Pos
;    sta Prog_Pos
;    lda #0
;    adc Prog_Pos + 1
;    sta Prog_Pos + 1



Line1010
    ; POKEP,PEEK(P)ORXC:RETURN

;    lda Prog_Pos
;    sta POINTADDRESS
;    lda Prog_Pos + 1
;    sta POINTADDRESS + 1


    ldy #0
    lda (POINTADDRESS),y
    ora Prog_XC
    sta (POINTADDRESS),y

    rts
    
;*******************************************************************************
;*                                                                             *
;* Atari                                                                       *
;*                                                                             *
;*******************************************************************************
; Atari graphics are not hardcoded to specific pages in memory by ANTIC, so 
; the program must declare it.   There are a couple options:  
; 1) Use the OS routines for creating the graphics mode and drawing.
; This results in a default 192 scanline display and using generic pixel
; plotting code meant for multiple kinds of graphics modes which is pretty 
; darned slow.
; 2) Set it up ourselves, so we could duplicate the 200 scan lines that the 
; C64 uses (or MORE!) and use a direct plotting solution that will certainly 
; be faster than the OS routines. 
; I vote for number 2.

	ORG GRAPHICSSRT
		.ds 8000      ; That's 200 lines * 40 bytes per line.

	.align $0400 ; Make sure display list does not cross 1K boundary.

DisplayList
	.byte DL_BLANK_8  ; Need some blank lines to center display
	.byte DL_BLANK_8
	.byte DL_BLANK_4 ; total 20 blank scan lines before display starts

	mDL_LMS DL_MAP_F, GRAPHICSSRT ; mode F graphics and init the memory scan address
	.rept 99
	.byte DL_MAP_F    ; 99 more lines of mode F graphics (memory scan is automatic)
	.endr
    ; Reached the end of a 4K page.  Need to reset memory scan.
	mDL_LMS DL_MAP_F, GRAPHICSSRT+4000 ; mode F graphics and init the memory scan address
	.rept 99
	.byte DL_MAP_F    ; 99 more lines of mode F graphics (memory scan is automatic)
	.endr

	.byte DL_JUMP_VB  ; End.  Wait for Vertical Blank.
	.word DisplayList ; Restart the Display List


	END

	
