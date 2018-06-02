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

;*******************************************************************************
; Atari: System Includes

	icl "ANTIC.asm" ; Graphics display
	icl "GTIA.asm"  ; Color registers
	icl "POKEY.asm" ; Random value register
	icl "OS.asm"    ; Need for internal clock
	icl "DOS.asm"   ; This provides the LOMEM, start, and run addresses.


;*******************************************************************************
;*                                                                             *
;* Program Startup
;*
;* Auto Run works by telling DOS the program's Auto-Run address like this:
;*                                                                             *
;*******************************************************************************

	ORG DOS_RUN_ADDR
	.word START


;*******************************************************************************
;*                                                                             *
;* Assembly Reference Variables                                               *
;*                                                                             *
;*******************************************************************************

GRAPHICSSRT   = $4000+96     ; 16384+96 ; Tweak actual display start. 
; +96 is playing games with alignment, so the first 100 graphics lines are 
; in the first 4K block, and the next are immediately sequential at the start 
; of the next 4K block.

GRAPHICSSTART = $4000        ; 16384  ; Using this nearest exact page boundary
; value so the C64 screen clearing code works for this.

X_START     = 79
Y_START     = 49


;*******************************************************************************
;*                                                                             *
;* Atari graphics are not hardcoded to specific pages in memory by ANTIC, so 
;* the program must declare it. 
;*
;* Since we set it up ourselves, we can duplicate the 200 scan lines that 
;* the C64 uses (or MORE!) and use a direct plotting solution that will 
;* certainly be faster than the generic OS routines. 
;*                                                                             *
;*******************************************************************************

; Define the screen memory where we want it:
	ORG GRAPHICSSRT
		.ds 8000      ; That's 200 lines * 40 bytes per line.

; Define the display list for ANTIC to render the screen memory:
	.align $0400 ; Make sure display list does not cross 1K boundary.

DisplayList
	.byte DL_BLANK_8  ; Need some blank lines to center display
	.byte DL_BLANK_8
	.byte DL_BLANK_4 ; total 20 blank scan lines before display starts

	mDL_LMS DL_MAP_F, GRAPHICSSRT ; mode F graphics and init the memory scan address
	.rept 99
	.byte DL_MAP_F    ; 99 more lines of mode F graphics (memory scan is automatic)
	.endr
	; Reached the end of a 4K page.  Need to restart the memory scan.
	mDL_LMS DL_MAP_F, GRAPHICSSRT+4000 ; mode F graphics and init the memory scan address
	.rept 99
	.byte DL_MAP_F    ; 99 more lines of mode F graphics (memory scan is automatic)
	.endr

	.byte DL_JUMP_VB  ; End.  Wait for Vertical Blank.
	.word DisplayList ; Restart the Display List


;*******************************************************************************
;*                                                                             *
;* Code Variables                                                              
;*
;* Capitalize on the Atari's ability to load anything anywhere in memory to 
;* put all the working variables in page 0 and automagically initize them 
;* when the program loads...
;*                                                                             *
;*******************************************************************************

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

MemStart
	.word 0

MemEnd
	.byte 0

EvalDelta
	.byte 0

EvalValue 
	.byte 0

EvalMax
	.byte 0


;*******************************************************************************
;*                                                                             *
;* Defined Macros                                                              *
;*
;* Repeated activities should not unpack a big macro inline every time.
;* Ideally, set up some arguments, and then JSR for the larger activity.
;*                                                                             *
;*******************************************************************************

;*******************************************************************************
;*                                                                             *
;* Fill Video Memory Bank With A Value                                         *
;*
;* This is done by looping through pages from a start to 
;* a consecutive destination page, clearing the entire pages between.
;* Arguments must be 16-bit addresses, but the low byte values are discarded.
;*                                                                             *
;*******************************************************************************
.macro ClearMemPages StartAddress, EndAddress ; Specify address.
	.if :0<>2
		.error "ClearMemPages: 2 arguments required (Start Address, End Address)"
	.else
		.if [>:StartAddress] > [>:EndAddress] 
			.error "ClearMemPages: Start Address must be less than End Address"
		.else
			ldy #>[:StartAddress]
			ldx #>[:EndAddress]
			jsr ClearMemoryPages
		.endif
	.endif
.endm

.macro FillVideoMemoryBank    
	; Start Address of Bank
	ClearMemPages GRAPHICSSTART, GRAPHICSSTART+8191
.endm


;*******************************************************************************
;*                                                                             *
;* Evaluate a Delta Code from a Random Number                                  *
;*                                                                             *
;*******************************************************************************

.macro EvaluateNextDeltaNumber DeltaVar
	
	;DX=INT(RND(1)*3-1) Results in a number of either -1, 0, +1
	.if :0<>1
		.error "EvaluateNextDeltaNumber: 1 argument required (Delta Variable)"
	.else
		jsr GetNextDeltaNumber ; returns 1, 0, -1
		sta :DeltaVar
	.endif
.endm


;*******************************************************************************
;*                                                                             *
;* Copy a Word from One Address to Another Address                             *
;*                                                                             *
;*******************************************************************************

.macro CopyWord WordSource, WordTarget
	.if :0<>2
		.error "CopyWord: 2 arguments required (Source Address, Target Address)"
	.else
		lda :WordSource
		sta :WordTarget
		lda :WordSource + 1
		sta :WordTarget + 1
	.endif
.endm


;*******************************************************************************
;*                                                                             *
;* Subtract a Number from a Memory Location and Store result in another location
;*                                                                             *
;*******************************************************************************

.macro SubtractNumberWord  wrdSourceNumber, wrdSubtract, wrdTarget
	.if :0<>3
		.error "SubtractNumberWord: 3 arguments required (Source Number, Subtract Address, Target Address)"
	.else
		sec
		lda #<:wrdSourceNumber
		sbc :wrdSubtract
		sta :wrdTarget
		lda #>:wrdSourceNumber
		sbc :wrdSubtract + 1
		sta :wrdTarget + 1
	.endif
.endm


;*******************************************************************************
;*                                                                             *
;* Multiply a word source by 2, save in destination.                                                 *
;*                                                                             *
;*******************************************************************************

.macro MultiplyWordByTwo  wrdSource, wrdTarget
	.if :0<>2
		.error "MultiplyWordByTwo: 2 arguments required (Source Address, Target Address)"
	.else
		lda :wrdSource
		asl           ; Times 2
		sta :wrdTarget
		lda :wrdSource+1
		rol           ; Times 2
		sta :wrdTarget+1
	.endif
.endm
	
	
;*******************************************************************************
;*                                                                             *
;* Divide the Source Word by eight, and store the result and the remainder     *
;*                                                                             *
;*******************************************************************************

.macro DivideSourceWordByEight  wrdSource, bytResult, bytRemainder
	.if :0<>3
		.error "DivideSourceWordByEight: 3 arguments required (Source Address, Result Address, Remainder Address)"
	.else
		lda :wrdSource
		sta :bytResult
	
		lda :wrdSource+1
		lsr         ; Divide by 2
		ror :bytResult
		lsr         ; Divide By 4
		ror :bytResult
		lsr         ; Divide By 8
		ror :bytResult

		lda :wrdSource
		and #%00000111
		sta :bytRemainder
	.endif
.endm


;*******************************************************************************
;*                                                                             *
;* Apply the delta to X or Y
;*
;* Revised version does not increment or decrement if the action will 
;* exceed min/max values.  Detect limits first, negate the delta if 
;* needed, then update the value.  
;*                                                                             *
;*******************************************************************************

.macro EvaluateDelta Delta, Value, MaxValue
	.if :0<>3
		.error "EvaluateDelta: 3 arguments required (Delta Address, Value Address, Max value Number)"
	.else
		ldx :MaxValue
		ldy :Value
		lda :Delta

		jsr ApplyDelta

		sty :Value
		sta :Delta
	.endif
.endm


;*******************************************************************************
;*                                                                             *
;* This is not a complicated program, so lots of RAM is superfluous.  
;* Just start code at a convenient place after DOS, DUP, etc.
;*                                                                             *
;*******************************************************************************

	ORG LOMEM_DOS_DUP; $3308  From DOS.asm.  First memory after DOS and DUP

;*******************************************************************************
;*                                                                             *
;* Main Routine                                                                *
;*                                                                             *
;*******************************************************************************

START
	cld ; Mostly unnecessary.   Makes me feel better.

	; setup the ANTIC display:
	
	lda #0     ; Turn off screen in case VBI happens
	sta SDMCTL ; OS Shadow for DMA control

	lda #<DisplayList ; point ANTIC to the new display list.
	sta SDLSTL
	lda #>DisplayList
	sta SDLSTH

	lda #ENABLE_DL_DMA|PLAYFIELD_WIDTH_NORMAL ; Turn the display back on.
	sta SDMCTL

	lda #0
	sta COLOR4 ; Border color  (0 is same as COLOR_BLACK)
	sta COLOR1 ; Drawing color
	lda #COLOR_WHITE
	sta COLOR2 ; Background color

Line60              
	;FORI=8192TO8192+8*1024:POKEI,0:NEXT
	; Clear screen memory.

	FillVideoMemoryBank

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

	; check if all values are zero:
	lda Prog_DX
	ora Prog_DY
	beq Line100

Line105
	;Y1=Y:X1=X:GOSUB1000:X1=319-X:GOSUB1000:Y1=199-Y:GOSUB1000:X1=X:GOSUB1000

	CopyWord Prog_Y, Prog_Y1

	CopyWord Prog_X, Prog_X1

	jsr Line1000 ; Plot X, Y

	SubtractNumberWord $013F, Prog_X, Prog_X1 ; $13F == 319

	jsr Line1000 ; Plot 319-X, Y

	SubtractNumberWord $00C7, Prog_Y, Prog_Y1 ; $C7 == 199

	jsr Line1000 ; Plot 319-X, 199-Y

	CopyWord Prog_X, Prog_X1

	jsr Line1000 ; Plot X, 199-Y

Line107
	;Y1=Y*2:X1=X*2:GOSUB1000:Y1=199-Y1:X1=319-X1:GOSUB1000

	MultiplyWordByTwo Prog_Y, Prog_Y1

	MultiplyWordByTwo Prog_X, Prog_X1

	jsr Line1000 ; Plot X*2, Y*2
	
	SubtractNumberWord $00C7, Prog_Y1, Prog_Y1

	SubtractNumberWord $013F, Prog_X1, Prog_X1

	jsr Line1000 ; Plot 319 - X*2, 199 - Y*2

Line110
	;X=X+DX:IFX<0ORX>159THENDX=-DX:GOTO110

	EvaluateDelta Prog_DX, Prog_X, 159
	
Line115
	;Y=Y+DY:IFY<0ORY>99THENDY=-DY:GOTO115

	EvaluateDelta Prog_DY, Prog_Y, 99
	
Line120
	;IFRND(1)>.9THENDX=INT(RND(1)*3-1)

	lda RANDOM
	cmp #225            ; 90% of 256
	bcc Line130
	
	EvaluateNextDeltaNumber Prog_DX

Line130
	;IFRND(1)>.9THENDY=INT(RND(1)*3-1)

	lda RANDOM
	cmp #225            ; 90% of 256
	bcc Line135
	
	EvaluateNextDeltaNumber Prog_DY

Line135
	;IFDX<>0ORDY<>0THEN105
	
	; check if all values are zero:
	lda Prog_DX
	ora Prog_DY
	beq Line140
	
bLine135
	jmp Line105

Line140
	;DX=INT(RND(1)*3-1):DY=INT(RND(1)*3-1):IFDX=0ANDDY=0THEN140

	EvaluateNextDeltaNumber Prog_DX
	
	EvaluateNextDeltaNumber Prog_DY

	; check if all values are zero:
	lda Prog_DX
	ora Prog_DY
	beq Line140
	
Line150
	lda #0
	sta ATRACT
	;GOTO105
	jmp Line105


; Plot a pixel at Prog_X1, Prog_Y1
Line1000
	;YA=INT(Y1/8):YB=Y1-YA*8:XA=INT(X1/8):XB=X1-XA*8

; Logic above is for C64.
; Atari screen memory is linear, so the pixel location 
; determination is 
; Byte Address = GRAPHICSSRT + ( Y * 40 ) + INT( X / 8 )

; This is the INT( X / 8 ) part:
	DivideSourceWordByEight Prog_X1, Prog_XA, Prog_XB

	; XC=P(XB)
	ldy Prog_XB
	lda Prog_PArray,y
	sta Prog_XC
	
; This is the ( Y * 40 ) part. 
; Y * 40 is the same as (Y * 8) + (Y * 32)
; Y cannot be negative and Y cannot be greater than 199,
; so, we only need to start work with the low byte of Y.
	lda Prog_Y1
	sta POINTADDRESS
	lda #0 ; Hold the high byte for rol
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
	adc POINTADDRESS    ; Add the saved *8 in A to the *32 to get *40.
	sta POINTADDRESS    ; save *40 Result
	pla                 ; get high byte *8 from stack
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

Line1010
	; POKEP,PEEK(P)ORXC:RETURN

	ldy #0
	lda (POINTADDRESS),y
	ora Prog_XC
	sta (POINTADDRESS),y

	rts


;*******************************************************************************
;*                                                                             *
;* Clear memory pages
;*
;* loop through consecutive pages from a start page to 
;* an end page, clearing the entire pages, inclusive.
;*                                                                             *
;*******************************************************************************
ClearMemoryPages

	sty MemStart+1
	stx MemEnd
	
	lda #0
	sta MemStart
	tay

LoopBytes
	sta (MemStart),y
	iny
	bne LoopBytes

	ldx MemStart+1
	cpx MemEnd
	beq ExitClearMemoryPages
	
	inx
	stx MemStart+1
	bne LoopBytes
	
ExitClearMemoryPages
	rts


;*******************************************************************************
;*                                                                             *
;* Get a Delta Code from a Random Number 
;* 
;* Return a value -1, 0, +1 ($FF, 0, $01)
;* This is done by getting a random value for the lowest two bits in 
;* a byte.  This results in the values 0, 1, 2, 3.  But, we need only
;* three values.  So, if the fourth value is generated the routine will
;* try again.  Inevitably, it will come up with a value within the 
;* desired range.  Then decrement the 0, 1, 2 value producing -1, 0, 1.
;*
;* A  returns the random value
;*                                                                             *
;*******************************************************************************

GetNextDeltaNumber

	lda RANDOM             ; POKEY hardware random generator
	and #%00000011         ; just keep the 2 least significant bits
	cmp #3                 ; but we need only 0, 1, 2, not 3
	beq GetNextDeltaNumber ; go try again if it is 3.
	sec
	sbc #1                 ; result is 1, 0, -1

	rts


;*******************************************************************************
;*                                                                             *
;* Apply the delta to X or Y
;* 
;* Revised version does not increment or decrement if the action will 
;* exceed min/max values.  
;* 1) Detect value limits first, 
;* 2) negate the delta if needed
;* 3) then update the value per the delta. 
;*                                                                             *
;*******************************************************************************

ApplyDelta
	stx EvalMax
	sty EvalValue
	sta EvalDelta
	
ReapplyDelta
;	lda EvalDelta
	beq bDeltaExit     ; Delta = Zero, do nothing.
	bpl bDeltaPositive ; Delta = +1, go do that
	                   ; Delta = -1, here
	lda EvalValue
	beq bNegateDelta   ; Value is 0, can't subtract.
	dec EvalValue      ; Ok, subtract 1
	jmp bDeltaExit     ; Done with Delta evaluation

bDeltaPositive
	lda EvalValue
	cmp EvalMax
	beq bNegateDelta   ; Value is at limit, can't add
	inc EvalValue      ; Ok, add 1
	jmp bDeltaExit     ; Done with Delta evaluation

bNegateDelta
	lda EvalDelta
	bpl bDelta_Negative ; If Delta is positive, then go set to negative
	lda #1              ; Otherwise, set Delta to positive
	bpl bEnd_Delta      ; and save Delta.
bDelta_Negative
	lda #$FF            ; set Delta to negative...
	
bEnd_Delta
	sta EvalDelta       ; and save Delta.
	bne ReapplyDelta    ; Changed Delta, reapply Delta to Value if non-zero.

bDeltaExit              ; Done with Delta evaluation

	ldy EvalValue       ; Pass these back to the caller.
	lda EvalDelta       
	
	rts

	END
