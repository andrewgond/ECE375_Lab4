;***********************************************************
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;*	 Author: Andrew Gondoputro
;*			Harrison Gregory
;*	   Date: 2-06-2025
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	i = r20					; Loop ctr


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine

		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND)  ; Load low byte of RAMEND into 'mpr'
		out		SPL, mpr  ; Store low byte of RAMEND into SPL (stack pointer low byte)
		ldi		mpr, high(RAMEND)  ; Load high byte of RAMEND into 'mpr'
		out		SPH, mpr  ; Store high byte of RAMEND into SPH (stack pointer high byte)

		; TODO

		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		rcall load_add16 ;
		rcall add16 ; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		nop ; Check ADD16 result (Set Break point here #2)


		; Call function to load SUB16 o	perands
		rcall load_sub16;
		rcall SUB16 ; Check load SUB16 operands (Set Break point here #3)

		; Call SUB16 function to display its results (calculate FCB9 - E420)
		nop ; Check SUB16 result (Set Break point here #4)


		; Call function to load MUL24 operands
		rcall load_mul24	; Check load MUL24 operands (Set Break point here #5)

		; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)
		rcall mul24	;

		nop ; Check MUL24 result (Set Break point here #6)

		; Setup the COMPOUND function direct test
		rcall COMPOUND ; Check load COMPOUND operands (Set Break point here #7)

		; Call the COMPOUND function
		nop ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************




;-----------------------------------------------------------
; Func: Load_ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
Load_ADD16:
		push mpr
		push A
		push B


		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		ldi     ZL, low(OperandA << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandA << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+			;Move data from operand into data memory
		st		X+,	mpr
		lpm		mpr, Z			
		st		X,	mpr			; Load data at address X with mpr
		SBIW	X,	1			; Go to previous address of X
		ldi		YL, low(ADD16_OP2)	; Load low byte of address
		ldi		YH, high(ADD16_OP2)	; Load high byte of address

		ldi     ZL, low(OperandB << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandB << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+				;Move data from operand into data memory
		st		Y+,	mpr
		lpm		mpr, Z
		st		Y,	mpr
		SBIW	Y, 1


		ldi     ZL, low(ADD16_Result)      ; Load low byte of address into ZL
		ldi     ZH, high(ADD16_Result)     ; Load high byte of address into ZH	

		pop	B
		pop A
		pop mpr
		ret

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;		Assumptions: Already set X and Y as Operands and Z and destinations
;-----------------------------------------------------------
ADD16:
		push A
		push B
		push XL
		push XH
		push YL
		push YH
		push ZL
		push ZH
		push mpr


		ld		A, X+			; Get data memory from A
		ld		B, Y+			; Get Data memory from B
		add		A,B				; ADD them to each other
		st		Z+, A			; Store the result in first bit
		ld		A, X;			;Get second bits
		ld		B, Y			;Get the second bit
		adc		A,B;			; add but with carrys
		st		Z+, A			;Go to hte next stop
		BRCC	AddCarrySkip;	; skip if there is a carry
		ldi		mpr, $01;		; move carry to the 0 if it exists
		st		Z, mpr;

AddCarrySkip:		
		pop mpr
		pop ZH
		pop ZL
		pop YH
		pop YL
		pop XH
		pop XL
		pop B
		pop A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
Load_SUB16:
		; Execute the function here
		; Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)	; Load low byte of address
		ldi		XH, high(SUB16_OP1)	; Load high byte of address

		ldi     ZL, low(OperandC << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandC << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+						;Move data from operand into data memory
		st		X+,	mpr
		lpm		mpr, Z
		st		X,	mpr
		SBIW	X, 1

		ldi		YL, low(SUB16_OP2)	; Load low byte of address
		ldi		YH, high(SUB16_OP2)	; Load high byte of address

		ldi     ZL, low(OperandD << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandD << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+				;Move data from operand into data memory
		st		Y+,	mpr
		lpm		mpr, Z
		st		Y,	mpr
		SBIW	Y, 1

		ldi     ZL, low(SUB16_Result)      ; Load low byte of address into ZL
		ldi     ZH, high(SUB16_Result)     ; Load high byte of address into ZH	
		ret

SUB16:
		; Store current register values to the stack for later
		push A
		push B
		push XL
		push XH
		push YL
		push YH
		push ZL
		push ZH
		push mpr

		ld		A, X+;		Load first operand from data
		ld		B, Y+;		load second operand from data
		sub		A,B;		subtract first operation from second
		st		Z+, A;		store result in result
		ld		A, X;		load second digit of operand
		ld		B, Y;		load second digit of operand
		sbc		A,B;		Subtract with carry
		st		Z+, A		;store in second digit

		; Restore Values from Stack to the registers:
		pop mpr
		pop ZH
		pop ZL
		pop YH
		pop YL
		pop XH
		pop XL
		pop B
		pop A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result. Performs factorE * factorF = result by storing
;		factorE and factorF into 
;
;
; Below are the program_memory addresses of factors E and F
/**
; MUL24 operands
OperandE1:
	.DW	0XFFFF				; lower 16 bits (factorE)
OperandE2:
	.DW	0X00FF				; upper 8 bits (factorE)
OperandF1:
	.DW	0XFFFF				; lower 16 bits (factorF)
OperandF2:
	.DW	0X00FF				; upper 8 bits (factorF)
**/
;
;
; Below are the future SRAM addresses of factors E, F, and 
;	result:
/**
.org	$0160				; data memory allocation for operands
MUL24_OP1:					; factorE
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:					; factorF
		.byte 3				; allocate three bytes for second operand of MUL24

.org	$0170				; data memory allocation for results of MUL24
MUL24_Result:
		.byte 6				; allocate 7 bytes for MUL24 result: 
							;        Reason below:
							;     MUL24 multiplies two 24 bit numbers:
							;     24 bits = 3 bytes
							;	  two numbers * 3 bytes = 6 bytes
**/
;-----------------------------------------------------------
MUL24:
;* - Simply adopting MUL16 ideas to MUL24 will not give you steady results. You should come up with different ideas.
	
	;Save all register previous values to Stack:
	push 	A				; Save A register
	push	B				; Save B register
	push	rhi				; Save rhi register
	push	rlo				; Save rlo register
	push	zero			; Save zero register
	push	XH				; Save X-ptr
	push	XL
	push	YH				; Save Y-ptr
	push	YL
	push	ZH				; Save Z-ptr
	push	ZL


	clr		zero			; Maintain zero semantics


;First 8 bits of X multiplied by Y
	rcall MUL24x8		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw X, 1			; Start multiplying with the second 8 bits
;Second 8 bits of X multiplied by Y
	rcall MUL24x8		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw X, 1			; Start multiplying with the second 8 bits

;First 8 bits of X multiplied by Y
	rcall MUL24x8		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw X, 1			; Start multiplying with the second 8 bits


	//Restore all Register's previous values from Stack
	pop		ZL
	pop		ZH
	pop		YL
	pop		YH
	pop		XL
	pop		XH
	pop		zero
	pop		rlo
	pop		rhi
	pop		B
	pop		A


	ret			;End of MUL24


MUL24x8:
	push 	A				; Save A register
	push	B				; Save B register
	push	rhi				; Save rhi register
	push	rlo				; Save rlo register
	push	zero			; Save zero register
	push	XH				; Save X-ptr
	push	XL
	push	YH				; Save Y-ptr
	push	YL
	push	ZH				; Save Z-ptr
	push	ZL


;First 8 bits of X multiplied by the first 8 bits of Y
	rcall MULPLace		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw Y, 1			; Start multiplying with the second 8 bits
;First 8 bits of X multiplied by the second 8 bits of Y
	rcall MULPLace		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw Y, 1			; Start multiplying with the second 8 bits

;First 8 bits of X multiplied by the Third 8 bits of Y
	rcall MULPLace		;

	adiw Z, 1			; shift the next line to the next 0's for adding
	adiw Y, 1			; Start multiplying with the second 8 bits


	pop		ZL			; Pop everything off the stack
	pop		ZH
	pop		YL
	pop		YH
	pop		XL
	pop		XH
	pop		zero
	pop		rlo
	pop		rhi
	pop		B
	pop		A
	ret						;

MULPlace:
;Push things on the stack
	push 	A				; Save A register
	push	B				; Save B register
	push	rhi				; Save rhi register
	push	rlo				; Save rlo register
	push	zero			; Save zero register
	push	XH				; Save X-ptr
	push	XL
	push	YH				; Save Y-ptr
	push	YL
	push	ZH				; Save Z-ptr
	push	ZL

; DO 8x8 Mul
	ld A, X;
	ld B, Y;
	
	MUL A, B;

; ADD Lowest Digit to memory
	ld A, Z
	ADD A, rlo
	st Z+, A 

;ADD second digit of the MUL to current stuff
	ld B, Z
	ADC B,rhi
	st Z+, B

	ldi i, 6;
	ldi	mpr, 0;

PropagateCarry:
	ld A, Z
	adc A, mpr
	st Z+, A
	dec i				;
	brne PropagateCarry	; Loop back if carry is still set

;Pop to restore things

	pop		ZL
	pop		ZH
	pop		YL
	pop		YH
	pop		XL
	pop		XH
	pop		zero
	pop		rlo
	pop		rhi
	pop		B
	pop		A
	ret



;-----------------------------------------------------------
; Func: Load_MUL24
; Desc: Helper Function of MUL24: loads factor1 and factor2
;		into X and Z respec
;-----------------------------------------------------------
Load_MUL24:
		//Get factor1's SRAM storage address as X:
		ldi		XL, low(MUL24_OP1)	; Load low byte of address  (factor1)
		ldi		XH, high(MUL24_OP1)	; Load high byte of address (factor1)

		//Get factor1's address from program memory as Z:
		ldi     ZL, low(OperandE1 << 1)      ; Load low byte of address into ZL 
		ldi     ZH, high(OperandE1 << 1)     ; Load high byte of address into ZH

		// Move things from the operand into the DATA memory
		lpm		mpr, Z+
		st		X+,	mpr
		lpm		mpr, Z
		st		X+,	mpr
		lpm		mpr, Z
		st		X+,	mpr
		

		ldi		YL, low(MUL24_OP2)	; Load low byte of address
		ldi		YH, high(MUL24_OP2)	; Load high byte of address

		ldi     ZL, low(OperandF1 << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandF1 << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+
		st		Y+,	mpr
		lpm		mpr, Z+
		st		Y+,	mpr
		lpm		mpr, Z+
		st		Y+,	mpr


		ldi		XL, low(MUL24_OP1)	; Load low byte of address
		ldi		XH, high(MUL24_OP1)	; Load high byte of address
		ldi		YL, low(MUL24_OP2)	; Load low byte of address
		ldi		YH, high(MUL24_OP2)	; Load high byte of address


		ldi     ZL, low(MUL24_RESULT)      ; Load low byte of address into ZL
		ldi     ZH, high(MUL24_RESULT)     ; Load high byte of address into ZH	
		ret

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:
;LOAD OPERANDS
		ldi		XL, low(COMP_OP1)	; Load low byte of address
		ldi		XH, high(COMP_OP1)	; Load high byte of address

		ldi     ZL, low(OperandG << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandG << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+					; load in data from operand G
		st		X+,	mpr
		lpm		mpr, Z
		st		X,	mpr
		SBIW	X, 1

		ldi		YL, low(COMP_OP2)	; Load low byte of address
		ldi		YH, high(COMP_OP2)	; Load high byte of address

		ldi     ZL, low(OperandH << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandH << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+					;Load in operandH
		st		Y+,	mpr
		lpm		mpr, Z
		st		Y,	mpr
		SBIW	Y, 1

		ldi     ZL, low(Result1)      ; Load low byte of address into ZL
		ldi     ZH, high(Result1)     ; Load high byte of address into ZH	


		; Setup SUB16 with operands G and H
		; Perform subtraction to calculate G - H
		rcall SUB16					;Call the SUB16 Function on the new places
		
		; Setup the ADD16 function with SUB16 result and operand I
		ldi     XL, low(Result1)      ; Load low byte of address into ZL
		ldi     XH, high(Result1)     ; Load high byte of address into ZH
		ldi     YL, low(Result1)      ; Load low byte of address into ZL
		ldi     YH, high(Result1)     ; Load high byte of address into ZH
		
		adiw	Y, 2					;Go to the next two lines to find the place of operandI
		ldi     ZL, low(OperandI << 1)      ; Load low byte of address into ZL
		ldi     ZH, high(OperandI << 1)     ; Load high byte of address into ZH

		lpm		mpr, Z+					;Put data from operand I into Y's memory
		st		Y+,	mpr
		lpm		mpr, Z
		st		Y,	mpr
		SBIW	Y, 1
		
		ldi     ZL, low(Result2)      ; Load low byte of address into ZL
		ldi     ZH, high(Result2)     ; Load high byte of address into ZH	


		; Perform addition next to calculate (G - H) + I
		rcall ADD16
		; Setup the MUL24 function with ADD16 result as both operands
		ldi     XL, low(Result2)      ; Load low byte of address into ZL
		ldi     XH, high(Result2)     ; Load high byte of address into ZH
		ldi     YL, low(Result2)      ; Load low byte of address into ZL
		ldi     YH, high(Result2)     ; Load high byte of address into ZH
		ldi		ZL, low(Result3)
		ldi		ZH, high(result3)
		; Perform multiplication to calculate ((G - H) + I)^2
		rcall MUL24						;

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;       beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF				; 16 bits
OperandE2:
	.DW	0X00FF				; 8 bits (high part of opE)
OperandF1:
	.DW	0XFFFF				; 16 bits
OperandF2:
	.DW	0X00FF				; 8 bits (high part of opF)

; Compoud operands
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.
.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result



.org	$0130				; data memory allocation for operands
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0140				; data memory allocation for results
SUB16_Result:
		.byte 2				; allocate three bytes for SUB16 result


.org	$0160				; data memory allocation for operands
MUL24_OP1:
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:
		.byte 3			; allocate three bytes for second operand of MUL24

.org	$0170				; data memory allocation for results of MUL24
MUL24_Result:
		.byte 6				; allocate 7 bytes for MUL24 result: 
							;        Reason below:
							;     MUL24 multiplies two 24 bit numbers:
							;     24 bits = 3 bytes
							;	  two numbers * 3 bytes = 6 bytes


.org	$0180
COMP_OP1:
	.byte 2
COMP_OP2:
	.byte 2

.org	$0190
RESULT1:
	.byte 2

.org	$01A0
RESULT2:
	.byte 3

.org	$01B0
RESULT3:
	.byte 6


;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
