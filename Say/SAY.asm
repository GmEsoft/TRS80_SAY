;
; 	** CP/M SAY.COM - LS-DOS 6 SAY/CMD and SAY/DVR - v0.2.0-alpha **
;
;
;	Assemble using ZMAC from http://48k.ca/zmac.html
;	- For CP/M (Bondwell 12/14 CP/M):
;		ZMAC --zmac SAY.ASM -P0=1 --od . --oo CIM,LST,BDS
;		and rename SAY.CIM to SAY.COM
;	- For CP/M (Montezuma Micro CP/M 2.x) and Orchestra-90:
;		ZMAC --zmac SAY.ASM -P0=2 --od . --oo CIM,LST,BDS
;		and rename SAY.CIM to SAY.COM
;	- For LS-DOS 6.3 and Orchestra-90 executable:
;		ZMAC --zmac SAY.ASM -P0=3 --od . --oo CMD,LST,BDS
;	- For LS-DOS 6.3 and Orchestra-90 driver:
;		ZMAC --zmac SAY.ASM -P0=4 --od . --oo CIM,LST,BDS
;		and rename SAY.CMD to SAY.DVR
;		To load:
;			SET *SY SAY




;==================================================================================================
;	C O N F I G
;==================================================================================================

_BREAK	EQU	1

	IF	@@0
CONFIG	  EQU	@@0
	ELSE
CONFIG	  EQU	1
	ENDIF


	IF	CONFIG = 1	; Bondwell 12/14 CP/M
CPM	  EQU	1		; CP/M version
LSDOS6	  EQU	0		; LS-DOS version
DACOFFS	  EQU	80H		; Wave output centered at 80H
DACPORT	  EQU	50H		; Bondwell 12/14 audio out
FIXQMRK	  EQU	0		; Don't fix "(?)=." rule
ORCH90	  EQU	0		; Don't add ORCH90 signature
EXEC	  EQU	1		; Executable Mode
DRIVER	  EQU	0		; Driver Mode
	ENDIF

	IF	CONFIG = 2	; TRS-80 Model 4 CP/M + Orchestra 90
CPM	  EQU	1		; CP/M version
LSDOS6	  EQU	0		; LS-DOS version
DACOFFS	  EQU	00H		; Wave output centered at 0
DACPORT	  EQU	75H		; Orchestra 90 right channel
FIXQMRK	  EQU	1		; Fix "(?)=." rule
ORCH90	  EQU	1		; Add ORCH90 signature
EXEC	  EQU	1		; Executable Mode
DRIVER	  EQU	0		; Driver Mode
	ENDIF

	IF	CONFIG = 3	; TRS-80 Model 4 LS-DOS 6 + Orchestra 90
CPM	  EQU	0		; CP/M version
LSDOS6	  EQU	1		; LS-DOS version
DACOFFS	  EQU	00H		; Wave output centered at 0
DACPORT	  EQU	75H		; Orchestra 90 right channel
FIXQMRK	  EQU	1		; Fix "(?)=." rule
ORCH90	  EQU	1		; Add ORCH90 signature
EXEC	  EQU	1		; Executable Mode
DRIVER	  EQU	0		; Driver Mode
	ENDIF

	IF	CONFIG = 4	; TRS-80 Model 4 /DVR for LS-DOS 6 + Orchestra 90
CPM	  EQU	0		; CP/M version
LSDOS6	  EQU	1		; LS-DOS version
DACOFFS	  EQU	00H		; Wave output centered at 0
DACPORT	  EQU	75H		; Orchestra 90 right channel
FIXQMRK	  EQU	1		; Fix "(?)=." rule
ORCH90	  EQU	1		; Add ORCH90 signature
EXEC	  EQU	0		; Executable
DRIVER	  EQU	1		; Driver Mode
	ENDIF

	IFNDEF	EXEC
	 ASSERT	0		; Invalid config selected
	 END
	ENDIF

;-----	Rules fixes
FIXNINE	EQU	1		; Fix 'nineteen', 'ninety'


;==================================================================================================
;	M A C R O S
;==================================================================================================


$BREAK	MACRO
	IF	_BREAK
	  DB	0EDH,0F5H	;$BREAK
	ENDIF
	ENDM

; Define Text-To-Speech transformation rule.
;
; All characters except the last one have their high bit set.
;	Double quotes are typed as [_] but stored as ["].
;	Single quotes are typed as [|] but stored as ['].
DEFRULE	MACRO	STR
	LOCAL	LEN,POS

	; Count characters
LEN	DEFL	0
	IRPC	C,'STR'
LEN	 DEFL	LEN+1
	ENDM

	; Generate output
POS	DEFL	0
	IRPC	C,'STR'
POS	 DEFL	POS+1
	 IFEQ	POS,LEN
	  DB	'C'		; Last character bit 7 is 0
	 ELSE
	  IFEQ	'C','_'
	   DB	22H+80H		; _ -> "
	  ELSE
	   IFEQ	'C','|'
	    DB	27H+80H		; | -> '
	   ELSE
	    DB	'C'+80H		; Previous characters bit 7 is 1
	   ENDIF
	  ENDIF
	 ENDIF
	ENDM

	ENDM

; Define a string of characters with their high bit set.
DEFSTR8	MACRO	STR
	IRPC	C,'STR'
	  DB	'C'+80H
	ENDM
	ENDM

DEFLINE	MACRO	STR
	DB	'STR'
	IF	CPM
	  DB	0DH,0AH
	ENDIF
	IF	LSDOS6
	  DB	0AH
	ENDIF
	ENDM

;-----	LS-DOS 6 SVC equates
	IF	LSDOS6

@BANK	  EQU	102		; RAM bank switching
@CHNIO	  EQU	20		; Device chain character I/O
@DSP	  EQU	2		; Character output to *DO (video display)
@DSPLY	  EQU	10		; Line output to *DO (video display)
@EXIT	  EQU	22		; Exit program with return code
@FLAGS$	  EQU	101		; Obtain system flags pointer
@GTDCB	  EQU	82		; Obtain DCB pointer given devspec
@HIGH$	  EQU	100		; Obtain or alter HIGH$/LOW$
@LOGOT	  EQU	12		; Display and log a message (*DO and *JL)
@MSG	  EQU	13		; Send a message line to a device
@SOUND	  EQU	104		; Generate tone

;-----	  Invoke SVC
$SVC	  MACRO	#NUM
	   LD	A,#NUM
	   RST	28H
	  ENDM

CMDLINE	EQU	0000H		; dummy

	ENDIF

;-----	CP/M BDOS EQUATES equates
	IF	CPM
BDOS	  EQU	0005H
D007C	  EQU	007CH
D0080	  EQU	0080H
CMDLINE	  EQU	0082H
	ENDIF

LF	EQU	10
CR	EQU	13

	IF	CPM
	  ORG	0100H		; CP/M Entry Point
	ENDIF

	IF	LSDOS6
	  ORG	3000H		; LS-DOS 6 Entry Point
	ENDIF

	IF	DRIVER
;==================================================================================================
;	L S - D O S   D R I V E R   L O A D E R
;==================================================================================================
;
;	Module will be loaded at 8000H in bank 1 if available
;	Jumper will be loaded in LOW memory
;

DCB@GET	EQU	01H		; DCB can handle @GET
DCB@PUT	EQU	02H		; DCB can handle @PUT
DCB@CTL	EQU	04H		; DCB can handle @CTL
DCB@NIL	EQU	08H		; DCB is a NIL device
DCB@RUT	EQU	10H		; DCB is routed (R/O)
DCB@LNK	EQU	20H		; DCB is linked (R/0)
DCB@FLT	EQU	40H		; DCB is a filter
DCB@FCB	EQU	80H		; DCB is actually a FCB




;------------------------------------------------------------------------------
;	M O D U L E   I N S T A L L E R
;------------------------------------------------------------------------------

ENTRY:				; Installer entry point
;	$BREAK
	PUSH	DE
	POP	IX		; Get dcb
	LD	(JMPDCB),DE	; Stuff DCB pointer
	LD	HL,MSG_HELP
	$SVC	@DSPLY		; Display title

;*=*=*
; Check if entry from SET command
;*=*=*
	$SVC	@FLAGS$		; Get flags pointer
	BIT	3,(IY+'C'-'A')	; System request?
	JP	Z,VIASET

;*=*=*
; Obtain low memory driver pointer.	Bizarre API here!
;*=*=*
	LD	E,'K'		; Locate pointer to *KI DCB
	LD	D,'I'		; 	via @GTDCB SVC
	$SVC	@GTDCB		; !!EI!!
	JP	NZ,CURDL	; No error unless KI clobbered!
	DEC	HL		; Decrement to driver pointer
	LD	D,(HL)		; P/u hi-order of pointer,
	DEC	HL		;	decrement to and p/u
	LD	E,(HL)		;	lo-order of pointer
	LD	(DEST),DE	; save dest address
	LD	(LOWPTR),HL	; save pointer address

;*=*=*
; Check if driver will fit into [(LCPTR), X'12FF']
;*=*=*
	LD	HL,JMPEND-JMPBGN; Jumper length
	ADD	HL,DE		;
	LD	BC,1300H	; max address + 1
	XOR	A		;
	SBC	HL,BC		; space available in low mem?
	JP	NC,NOLOW	; jump if yes

;*=*=*
; Move module to bank
;*=*=*
;	$BREAK
	LD	BC,0207H	; B=reserve bank, C=max bank #
FBANK:	LD	L,C		; Save bank #
	$SVC	@BANK		; RAM bank switching: reserve bank
	LD	C,L		; restore bank
	JR	Z,FBNKOK	; Jump if successful
	DEC	C		; Previous bank
	JR	NZ,FBANK	; until all banks tested
	JP	NOBANK		; No bank available error
FBNKOK:	LD	A,C		; Put bank # in message
	ADD	A,'0'		;
	LD	(MSGBNKNUM),A	;
	LD	B,3		; Reserve the bank
	LD	L,C		; save bank #
	$SVC	@BANK		; RAM bank switching: reserve bank
	LD	C,L		; restore bank
	EXX			; Save BC, reserved bank #
	LD	HL,MODBGN	; Move banked module to bank
	PUSH	HL		;
	LD	DE,BUFFER	; 1st: from bank 0 to resident memory
	PUSH	DE		;
	LD	BC,MODEND-MODBGN; Banked module length
	PUSH	BC		;
	LDIR			; Move it
	EXX			; Restore reserved bank
	LD	B,0		; select bank
	LD	(BANKNUM),BC	; save bank # in jumper
	$SVC	@BANK		; RAM bank switching
	EXX			; save BC=restore Bank 0
	POP	BC		;
	POP	HL		;
	POP	DE		;
	LDIR			; Move from resident memory to bank C

	; Init say-dvr
	XOR	A
	LD	(INPUT_BUFFER_LEN),A
	LD	(SPEECHBUFFERLEN),A; Number of chars in speech buffer

	; End Init say-dvr
	EXX			; restore BC to reselect bank 0
	$SVC	@BANK		; reselect bank 0

;*=*=*
; Relocate addresses
;*=*=*
RELOC	LD	HL,(DEST)	; calculate relocation offset
	LD	DE,-JMPBGN	;
	ADD	HL,DE		;
	LD	B,H		; offset to BC
	LD	C,L		;
	LD	HL,RELTAB	; fixup table
RELO0	LD	E,(HL)		; get next address to relocate
	INC	HL		;
	LD	D,(HL)		;
	INC	HL		;
	LD	A,D		; test for null
	OR	E		;
	JR	Z,INSTAL	; exit if no more address
	EX	DE,HL		; Swap DE and HL
	LD	A,(HL)		; adjust vector (add BC)
	ADD	A,C		;
	LD	(HL),A		;
	INC	HL		;
	LD	A,(HL)		;
	ADC	A,B		;
	LD	(HL),A		;
	EX	DE,HL		; Restore DE
	JR	RELO0		; loop

;*=*=*
; Install Driver
;*=*=*
INSTAL	LD	HL,JMPBGN	; driver begin
	LD	DE,(DEST)	; top of low memory
	LD	BC,JMPEND-JMPBGN; block length
	LD	(IX+0),DCB@PUT	; Stuff TYPE byte
	LD	(IX+1),E
	LD	(IX+2),D	; Install addr into dcb
	LDIR			; move
	LD	HL,(LOWPTR)	; set new top of low memory
	LD	(HL),E		;
	INC	HL		;
	LD	(HL),D		;

;*=*=*
; Display banner
;*=*=*
	LD	HL,MSGJMPLD	; 'Module loaded in '
	$SVC	@DSPLY
	LD	HL,(LOWHIGH)	; 'LOW' / 'HIGH'
	$SVC	@DSPLY
	LD	HL,MSGMEMORY	; ' memory'
	$SVC	@DSPLY

	PUSH	IX		; Recover DCB in DE
	POP	DE		;
	LD	HL,READY_	; "Ready." message
	$SVC	@MSG		; send to voice driver

;-----	Exit OK:
ENDOK	LD	HL,0		; no error
EXIT	$SVC	@EXIT		; exit to DOS


;*=*=*
; Error messages logging
;*=*=*
CURDL:	LD	HL,CURDL_	; Other error
	DB	0DDH		;
VIASET:	LD	HL,VIASET_	; 'Must install via SET!'
	DB	0DDH		;
NOBANK:	LD	HL,NOBANK_	; 'Banked memory is not available!'
	DB	0DDH		;
NOLOW:	LD	HL,NOLOW_	; 'Low memory is not available!'
	$SVC	@LOGOT		; Display and log a message (*DO and *JL)
	LD	HL,-1		; Return code
	JR	EXIT		; exit to DOS
;

VIASET_:DB	'Must install via SET!',CR
CURDL_: DB	'LS-DOS is curdled!',CR
NOBANK_:DB	'Banked memory is not available!',CR
NOLOW_:	DB	'Low memory is not available!',CR
READY_:	DB	'Ready.',CR

MSGJMPLD:
	DB	'Driver loaded in ',03H

MSGLOW:
	DB	'LOW',03H

MSGHIGH:
	DB	'HIGH',03H

MSGMEMORY:
	DB	' memory and in Bank #'
MSGBNKNUM:
	DB	'0',0AH,0DH


;------------------------------------------------------------------------------
;	J U M P E R   B E G I N
;------------------------------------------------------------------------------

JMPBGN:	JR	JBEGIN		; Branch around linkage
FX00:	DW	JMPEND-1	; To contain last byte used
	DB	JMPDCB-JMPNAM	; Calculate length of 'NAME'
JMPNAM:	DB	'SAY'		; Name of this Jumper
JMPDCB:	DW	$-$		; To contain DCB pointer for Jumper
	DW	0		; Reserved by the DOS

;*=*=*
; Jumper execution start
;*=*=*
; On entry:
;	Z,NC if @PUT
;	C,NZ if @GET
;	NC,NZ if @CTL
;	B = I/O direction code (1=@GET, 2=@PUT, 4=@CTL)
;	C = char code passed to @PUT or @CTL
; On exit:
;	A = char to return for @GET on success, with Z (CP A:RET)
;	A = 0 if no char available for @GET, with NZ (OR 1:LD A,0:RET)
;	A = errnum if an error occurred, with NZ (LD A,n:OR A:RET)
JBEGIN:
;	$BREAK
	DI			; Disable interrupts while banking !
	LD	(JSAVSP),SP	; Save SP
FX01	EQU	$-2
	LD	SP,JMPEND	; Switch to local stack
FX02	EQU	$-2
	CALL	JBEG1		; Exec the jumper
FX03	EQU	$-2
	LD	SP,$-$		; restore SP
JSAVSP	EQU	$-2
	EI			; re-enable interrupts
	RET			; done

JBEG1:	PUSH	AF		; save function flags
	PUSH	BC		; save output char
	LD	BC,0001H	; Select bank 1
BANKNUM	EQU	$-2		; Fixed up by installer
	$SVC	@BANK		; RAM bank switching
	JR	NZ,JERROR	; Jump on error
	LD	L,C		; save old bank
	POP	BC		; Restore char
	POP	AF		; restore flags

	PUSH	BC		; save output char
	PUSH	HL		; save old bank
	CALL	BEGIN		; Execute banked module
	POP	BC		; restore old bank
	PUSH	AF		; save exit condition
	LD	B,0		; restore old bank
	$SVC	@BANK		; RAM bank switching
	POP	AF		; restore exit condition
	POP	BC		; output char
	RET			; return to DOS

JERROR:	POP	BC		; restore output char
	INC	SP		; drop saved function flags
	INC	SP		;
	RET			; return to DOS with NZ

;	DS	400H		; dummy space (to test low memory availability)

	DC	60H,76H		; Local stack
JMPEND:				; Jumper ends here

;------------------------------------------------------------------------------
;	J U M P E R   E N D
;------------------------------------------------------------------------------

;*=*=*
; Relocation table
;*=*=*
RELTAB:	DW	FX00,FX01,FX02,FX03
	DW	0		; End of table

BUCKET	DW	0		; Dummy storage
LOWHIGH	DW	MSGLOW		; can be changed to MSGHIGH
DEST	DS	2		; dest relocation address
LOWPTR	DS	2		; pointer to top of low memory

BUFFER	EQU	$

	ORG	8000H		; Portion to move to banked memory at 8000H

;------------------------------------------------------------------------------
;	M O D U L E   B E G I N
;------------------------------------------------------------------------------

MODBGN:	JR	BEGIN		; Branch around linkage
	DW	MODEND-1	; To contain last byte used
	DB	MODDCB-MODNAM	; Calculate length of 'NAME'
MODNAM:	DB	'SAY-EXT'	; Name of this module
MODDCB:	DW	$-$		; To contain DCB pointer for module
	DW	0		; Reserved by the DOS

;*=*=*
; Module execution start
;*=*=*
; On entry:
;	Z,NC if @PUT
;	C,NZ if @GET
;	NC,NZ if @CTL
;	B = I/O direction code (1=@GET, 2=@PUT, 4=@CTL)
;	C = char code passed to @PUT or @CTL
; On exit:
;	A = char to return for @GET on success, with Z (CP A:RET)
;	A = 0 if no char available for @GET, with NZ (OR 1:LD A,0:RET)
;	A = errnum if an error occurred, with NZ (LD A,n:OR A:RET)
BEGIN:				;
	LD	(SAVSP),SP	; Save SP
	LD	SP,STACK	; switch to local stack
	CALL	BEGIN1		; do the I/O
	LD	SP,$-$		; saved SP
SAVSP	EQU	$-2		;
	RET			; return

BEGIN1:	JR	C,DOGET		; Go if @GET request
	JR	Z,DOPUT		; Go if @PUT request
	JR	DOCTL		; Was @CTL request

; Get a character => No op
DOGET:	OR	1		; clear Z (no char available)
	LD	A,0		; clear A
	RET

; Send a character to SAY
DOPUT:	LD	A,(PARMODE)	; Param mode ('|' or param letter)
	OR	A		;
	JR	NZ,DOPARAM	; Jump if Parameter being parsed
	LD	A,C		; get output char
	CP	20H		; Displayable ?
	JR	C,DOPLAY	; Play buffer if not
	CP	'['		; '[' to start phoneme mode ?
	JR	NZ,DOPUT1	; Skip if not
	CALL	PLAY_BUFFER	; play buffer
	LD	A,0FFH		; set current mode to phoneme
	LD	(MODE),A	; current mode: 0x00=english, 0xff=phoneme
	JR	DOPUTX		; exit driver
DOPUT1:	CP	']'		; ']' to exit phoneme mode ?
	JR	NZ,DOPUT2	; skip if not
	CALL	PLAY_BUFFER	; play buffer
	XOR	A		; set current mode to English
	LD	(MODE),A	; current mode: 0x00=english, 0xff=phoneme
	JR	DOPUTX		; exit driver
DOPUT2:	CP	'|'		; '|' to set parameter ?
	JR	NZ,DOPUT3	; skip if not
	LD	(PARMODE),A	; put '|' as param mode
	JR	DOPLAY		; play buffer and exit driver
DOPUT3:	CALL	PUT_CHAR	; playable character => put to buffer
	JR	DOPUTX		; exit driver
DOPLAY:	CALL	PLAY_BUFFER	; play buffer
	JR	DOPUTX		; exit driver
DORESET:XOR	A		; reset parameter mode
	LD	(PARMODE),HL	;
DOPUTX:	CP	A		; set Z (no error)
	RET			; done

; CTL character => No op
DOCTL:	$BREAK
	CP	A		; set Z (no error)
	RET			; done

; Parse parameter
DOPARAM:
	CP	'|'		; check last char: '|' ?
	JR	NZ,DOPAR1	; jump if not
	LD	A,C		; get param letter
	AND	5FH		; convert to upper case
	CP	'A'		; if not letter, reset and exit
	JR	C,DORESET	;
	CP	'Z'+1		;
	JR	NC,DORESET	;
	LD	(PARMODE),A	; store param letter
	XOR	A		;
	LD	(PARVAL),A	; reset param value
	JR	DOPUTX		; exit driver
DOPAR1:	LD	A,C		; param name known: parse value
	SUB	'0'		; check and get digit value
	JR	C,DOPAR2	; jump if not digit => process param
	CP	10		;
	JR	NC,DOPAR2	;
	LD	C,A		; PARVAL := 10 * PARVAL + digit value
	LD	A,(PARVAL)	;
	LD	B,A		;
	ADD	A,A		;
	ADD	A,A		;
	ADD	A,B		;
	ADD	A,A		;
	ADD	A,C		;
	LD	(PARVAL),A	; store updated PARVAL
	JR	DOPUTX		; exit driver
DOPAR2:	LD	A,(PARVAL)	; process param: get value
	LD	C,A		;
	LD	A,(PARMODE)	; get param letter
	CP	'P'		; 'P' = pitch ?
	JR	NZ,DOPAR3	; skip if not
	LD	A,C		; store new pitch value
	LD	(PITCH),A	;
	JR	DORESET		; reset param and exit
DOPAR3:	CP	'S'		; 'S' = speed ?
	JR	NZ,DOPAR4	; skip if not
	LD	A,C		; store new speed value
	LD	(SPEED),A	;
	JR	DORESET		; reset param and exit
DOPAR4:	CP	'M'		; 'M' = monotone ?
	JR	NZ,DOPAR5	; skip if not
	LD	A,C		; store new monotone flag
	LD	(SONG),A	;
DOPAR5:	CP	'D'		; 'D' = debug mode ?
	JR	NZ,DOPAR6	; skip if not
	LD	A,C		; store new monotone flag
	LD	(DEBUG),A	;
DOPAR6:	CP	'E'		; 'E' = echo mode ?
	JR	NZ,DORESET	; reset param and exit if not
	LD	A,C		; store new monotone flag
	LD	(ECHO),A	;
	JR	DORESET		; reset param and exit

PARMODE	DB	0		; parameter mode ('|' or param letter)
PARVAL	DB	0		; parameter value

;------------------------------------------------------------------------------
;	M O D U L E   E N D
;------------------------------------------------------------------------------

	ENDIF			; DRIVER - Driver Mode


;==================================================================================================
;	E X E C U T A B L E   P A R T
;==================================================================================================

	IF	EXEC		; Executable mode
ENTRY:
	; CP/M entry point at 0100H; LS-DOS6 entry point at 3000H
	IF	LSDOS6
	  EX	DE,HL		; Save HL = command line args ptr
	ENDIF

	LD	HL,STACK	; set local stack
	LD	SP,HL

	XOR	A		; init input buffer
	LD	(INPUT_BUFFER_LEN),A

	IF	CPM
	  LD	A,(D0080)	; Parameters length
	  CP	00H		; == 0 ?
	ENDIF
	IF	LSDOS6
	  EX	DE,HL		; Recover HL = command line args ptr
	  LD	(CMDLINE_ADDR),HL
	  LD	A,(HL)		; Parameters first byte
	  CP	0DH		; CR ?
	ENDIF

	JP	Z,SHOWHELP	; If no parameters, show help and exit
	CALL	CHECK_INPUT_SOU	; try to open specified file. On fail input comes from
	LD	A,00H
	LD	(SPEECHBUFFERLEN),A; Number of chars in speech buffer
READ_LOOP:
	CALL	GET_NEXT_CHAR	; get next char (ret in A)
	CP	00H
	JR	Z,END_READ
	CP	'Z'-40H		; found Ctrl-Z, end of text
	JR	Z,END_READ
	CP	'['
	JR	Z,BEGIN_PHONEME_M; found '[', begin phoneme section
	CP	']'
	JR	Z,BEGIN_ENGLISH_M; found ']', end of phoneme section
	CP	'|'
	JR	Z,SET_PARAM	; found '|', parameter
	CP	'M'-40H		; Ctrl-M / CR
	JR	Z,FOUND_NEWLINE	; found CR or LF
	CP	'J'-40H		; Ctrl-J / LF
	JR	Z,FOUND_NEWLINE	; found CR or LF
	CALL	PUT_CHAR	; put char (A) into the speech buffer
	JP	READ_LOOP

END_READ:
	CALL	PLAY_BUFFER	; play buffer
	LD	A,(SOURCE)	; source: 0x00 = command line, 0xff = file
	CP	00H
	JR	Z,END_READ_EXIT
	LD	DE,FCB
	IF	CPM
	  LD	C,10H		; Close file
	  CALL	BDOS
	ENDIF
	IF	LSDOS6
	  $SVC	60		; @CLOSE
	ENDIF
END_READ_EXIT:
	IF	CPM
	  JP	0000H
	ENDIF
	IF	LSDOS6
	  LD	HL,0		; no error
	  $SVC	22		; @EXIT
	ENDIF

FOUND_NEWLINE:			; found CR or LF
	CALL	PLAY_BUFFER	; play buffer
	JP	READ_LOOP

BEGIN_PHONEME_M:		; found '[', begin phoneme section
	CALL	PLAY_BUFFER	; play buffer
	LD	A,0FFH
	LD	(MODE),A	; current mode: 0x00=english, 0xff=phoneme
	JP	READ_LOOP

BEGIN_ENGLISH_M:		; found ']', end of phoneme section
	CALL	PLAY_BUFFER	; play buffer
	LD	A,00H
	LD	(MODE),A	; current mode: 0x00=english, 0xff=phoneme
	JP	READ_LOOP

SET_PARAM:	; found '|', parameter
	CALL	PLAY_BUFFER	; play buffer
	CALL	GET_NEXT_CHAR	; get next char (ret in A)
	AND	5FH		; to uppercase
	LD	HL,PITCH	; pitch parameter address
	CP	'P'		; 'P' = pitch ?
	JR	Z,SET_PARAM_HL	; set pitch value if yes
	LD	HL,SPEED	; speed parameter address
	CP	'S'		; 'S' = speed ?
	JR	Z,SET_PARAM_HL	; set speed value if yes
	LD	HL,SONG		; song mode parameter address
	CP	'M'		; 'M' = song mode ?
	JR	Z,SET_PARAM_HL	; set song mode value if yes
	LD	HL,DEBUG	; debug parameter address
	CP	'D'		; 'D' = debug ?
	JR	Z,SET_PARAM_HL	; set debug value if yes
	LD	HL,ECHO		; echo parameter address
	CP	'E'		; 'E' = echo ?
	JR	Z,SET_PARAM_HL	; set echo value if yes
	JP	READ_LOOP	; next char

SET_PARAM_HL:
	PUSH	HL		; save pointer
	CALL	DECODE_INTEGER	; get value in A
	POP	HL		; restore pointer
	LD	(HL),A		; store parameter value
	JP	READ_LOOP	; next char

DECODE_INTEGER:
	LD	B,00H
L_DECODE_INTEGE:
	CALL	GET_NEXT_CHAR	; get next char (ret in A)
	SUB	30H
	JR	C,X_DECODE_INTEGE
	CP	10
	JR	NC,X_DECODE_INTEGE
	LD	C,B
	SLA	C
	SLA	B
	SLA	B
	SLA	B
	ADD	A,B
	ADD	A,C
	LD	B,A
	JR	L_DECODE_INTEGE

X_DECODE_INTEGE:
	LD	A,B
	RET


;-----	Get next char in A
GET_NEXT_CHAR:	; get next char (ret in A)
	LD	A,(SOURCE)	; source: 0x00 = command line, 0xff = file
	CP	0FFH
	JR	Z,GET_NEXT_CHAR_F; get next char from file (ret in A)
	LD	HL,(CMDLINE_ADDR); address of next character to read from command line
	LD	A,(HL)
	INC	HL
	LD	(CMDLINE_ADDR),HL; address of next character to read from command line

	IF	LSDOS6
	  CP	0DH		; LS-DOS command line terminates with CR
	  RET	NZ
	  XOR	A		; Change it to 0
	ENDIF

	RET

GET_NEXT_CHAR_F:	; get next char from file (ret in A)
	IF	CPM
	  LD	A,(BLOCKPOS)	; position inside file block (0..127)
	  CP	80H
	  CALL	Z,LOAD_FILE_BLOCK; load a new block (128 bytes) into 0x0080
	  LD	A,(BLOCKPOS)	; position inside file block (0..127)
	  ADD	A,80H
	  LD	L,A
	  LD	H,00H
	  LD	A,(HL)
	  LD	HL,BLOCKPOS	; position inside file block (0..127)
	  INC	(HL)
	ENDIF
	IF	LSDOS6
	  LD	DE,FCB		; File control block
	  $SVC	3		; @GET - read one byte from file
	  RET	Z		; if success
	  XOR	A		; End of file => return 0
	  RET
	ENDIF
	RET

	IF	CPM
LOAD_FILE_BLOCK:		; load a new block (128 bytes) into 0x0080
	  LD	DE,FCB
	  LD	C,14H
	  CALL	BDOS
	  CP	00H
	  JR	Z,X_LOAD_FILE_BLO
	  LD	A,1AH		; End of file (Ctrl-Z)
	  LD	(D0080),A
X_LOAD_FILE_BLO:
	  LD	A,00H
	  LD	(BLOCKPOS),A	; position inside file block (0..127)
	  RET
	ENDIF			; CP/M

	ENDIF			; EXEC - executable mode

;-----	Protected stack driver SVC call
DRVSVC:
	IF	DRIVER		; if LS-DOS driver mode
	  LD	(SVCSP),SP	; save local stack pointer
	  LD	SP,(SAVSP)	; restore low-core memory stack ptr
	ENDIF			; end if LS-DOS driver mode
	RST	28H		; $SVC
	DI			; re-disable interrupts
	IF	DRIVER		; if LS-DOS driver mode
	  LD	SP,$-$		; restore local stack ptr
SVCSP	  EQU	$-2
	ENDIF			; end if LS-DOS driver mode
	RET			; done

;-----	Display char in A
DDISA:
	IF	LSDOS6		; LS-DOS 6 code
	  PUSH	BC
	  LD	C,A		; char to C
	  LD	A,@DSP
	  CALL	DRVSVC		; invoke @DSP
	  POP	BC
	ENDIF			; end LSDOS6
	IF	CPM		; CP/M code
;	  $BREAK
	  PUSH	HL
	  PUSH	DE
	  PUSH	BC
	  LD	E,A		; char to display
	  LD	C,02H		; display char BDOS func
	  PUSH	AF
	  CALL	BDOS
	  POP	AF
	  CP	CR		; CR ?
	  LD	E,LF
	  LD	C,02H
	  CALL	Z,BDOS		; if yes, append LF
	  POP	BC
	  POP	DE
	  POP	HL
	ENDIF
	RET

;-----	Display message @HL (end with 0, ETX or CR)
DMSG:	LD	A,(HL)		; fetch char
	INC	HL		;
	OR	A		; NUL ?
	RET	Z		; ret if yes
	CP	3		; ETX ?
	RET	Z		; ret if yes
	PUSH	AF		; save char
	CALL	DDISA		; display it
	POP	AF		; rest char
	CP	0DH		; CR ?
	JR	NZ,DMSG		; loop if not
	RET			; done

;-----	Put char in speech buffer
PUT_CHAR:	; put char (A) into the speech buffer
	LD	HL,(STORE_ADDR)	; address of next character to store in speech buffer (
	LD	(HL),A
	INC	HL
	LD	(STORE_ADDR),HL	; address of next character to store in speech buffer (
	LD	HL,SPEECHBUFFERLEN; Number of chars in speech buffer
	INC	(HL)
	LD	HL,ECHO		; ECHO mode active ?
	INC	(HL)
	DEC	(HL)
	JP	NZ,DDISA	; echo char if yes
	RET			; done

PLAY_BUFFER:	; play buffer
	LD	A,(SPEECHBUFFERLEN); Number of chars in speech buffer
	CP	00H
	RET	Z
	CALL	PREPROCESS_USER	; 0x86ff = number of characters, 0x8700 = actual chars
	LD	A,(MODE)	; current mode: 0x00=english, 0xff=phoneme
	CP	00H
	JR	Z,PLAY_BUFFER_ENG
	CALL	PROCESS_PHONEME	; Process Phonemes string (S9b61_say_main)
	JR	PLAY_BUFFER_EXI

PLAY_BUFFER_ENG:
	CALL	PROCESS_ENGLISH	; Process English string
PLAY_BUFFER_EXI:
	LD	A,00H
	LD	(SPEECHBUFFERLEN),A; Number of chars in speech buffer
	LD	HL,SPEECH_BUFFER; Speech buffer
	LD	(STORE_ADDR),HL	; address of next character to store in speech buffer (
	RET

	IF	EXEC		; Executable mode
SHOWHELP:	; show help and exit
	IF CPM
	  LD	DE,MSG_HELP	; Help text
	  LD	C,09H
	  CALL	BDOS
	  JP	0000H
	ENDIF
	IF	LSDOS6
	  LD	HL,MSG_HELP
	  $SVC	10		; @DSPLY - display text @HL
	  LD	HL,0		; No error exit
	  $SVC	22		; @EXIT
	ENDIF
	ENDIF			; EXEC - Executable mode

CALC_PITCH:
	PUSH	BC
	LD	A,(PHASE1)	; Phase1/stress amount index (mem43)
	LD	A,B
	LD	A,(PITCH)	; pitch (0..255)
	ADD	A,B
	POP	BC
	RET

LOAD_SPEED:
	LD	A,(SPEED)	; speed (0..225)
	LD	(SAMPLES_CTR),A	; samples counter (mem45)
	RET

	IF	EXEC		; Executable mode
CHECK_INPUT_SOU:	; try to open specified file. On fail input comes from command line
	IF	CPM
	  LD	DE,FCB
	  LD	C,0FH
	  CALL	BDOS
	  CP	0FFH
	  RET	Z
	  LD	A,00H
	  LD	(D007C),A	; used ?
	ENDIF
	IF	LSDOS6
	  LD	DE,FCB		; File control block
	  PUSH	DE
	  $SVC	78		; @FSPEC - extract filespec from @HL to @DE
	  POP	DE
	  LD	HL,FCB_BUF	; Sector buffer
	  LD	B,1		; use @GET to read the file
	  $SVC	59		; @OPEN - open existing file @DE
	  JR	Z,OPEN_OK
	  CP	2AH		; LRL mismatch error code
	  RET	NZ
OPEN_OK:
	ENDIF
	LD	A,0FFH
	LD	(SOURCE),A	; source: 0x00 = command line, 0xff = file
	RET
	ENDIF			; EXEC - Executable mode


; ASSIGN PITCH CONTOUR
;
; This subtracts the F1 frequency from the pitch to create a
; pitch contour. Without this, the output would be at a single
; pitch level (monotone).

ASSIGN_PITCH_CO:	; Avoids monotone output (disabled for songs)
	LD	A,(SONG)	; song mode (monotone output): 0 = song mode disabled,
	CP	00H		; return if song mode
	RET	NZ
	LD	C,00H		; 256 bytes to modify
	LD	HL,PITCH_CONTOUR; pitch contour table
L_ASSIGN_PITCH_:
	LD	A,(HL)		; Get pitch contour value
	INC	H		; Point to FREQUENCY1
	LD	B,(HL)		; Get Frequency 1 (F1) value
	SRL	B		; Divide F1 by 2
	DEC	H		; Point to PITCH_CONTOUR
	SUB	B		; Subtract F1 / 2 from pitch contour
	LD	(HL),A		; Store modified pitch contour
	INC	HL		; next byte
	DEC	C		; until 256 bytes modified
	JR	NZ,L_ASSIGN_PITCH_
	RET			; done

MSG_HELP:	; Help text
	IF	DRIVER
	  DB	"** SAY Speech Synthesizer Driver"
	ENDIF

	IF	EXEC
	  DB	"** SAY Speech Synthesizer"
	ENDIF

	DEFLINE " - v0.2.0-alpha **"
	DEFLINE	""
	DEFLINE	"Reengineered by Fabrizio Di Vittorio"
	DEFLINE	"Re-reengineered by GmEsoft"
	IF	ORCH90
	DEFLINE	"TRS-80 Orchestra-90 version by GmEsoft"
	ENDIF
	IF	EXEC		; Executable mode
	DEFLINE	"Specify English text, Phonemes (inside brackets) or filename to speech."
	DEFLINE	"Parameters:"
	DEFLINE	"  |p => pitch (0..255)"
	DEFLINE	"  |s => speed (0..225)"
	DEFLINE	"  |m => monotone output (0,1)"
	DEFLINE	"Examples:"
	DEFLINE	"  say Hello world!! I''m a [KUMPYUW4TER]"
	DEFLINE	"  say |p20 high pitch |p120 low pitch "
	DEFLINE	"  say |s32 fast speed |s100 slow speed"
	DEFLINE	"  say |m1 song mode |m0 normal mode"
	DEFLINE	"  say speech.txt"
	DEFLINE	""
	ENDIF			; EXEC - Executable mode
	IF	CPM
	  DB	'$'
	ENDIF
	IF	LSDOS6
	  DB	3
	ENDIF


MAKE_UPPERCASE:	; make 'A' upper case and reset bit 7
	CALL	RESET_BIT7	; Reset high bit of A (doesn't need a subroutine...)
	CP	'a'-1
	JR	C,MAKE_UPPERCASE_
	CP	'z'+1
	JR	NC,MAKE_UPPERCASE_
	RES	5,A
MAKE_UPPERCASE_:
	RET

CMDLINE_ADDR:	; address of next character to read from command line
	DW	CMDLINE
MODE:	; current mode: 0x00=english, 0xff=phoneme
	DB	00H
STORE_ADDR:	; address of next character to store in speech buffer (0x870
	DW	SPEECH_BUFFER
PITCH:	; pitch (0..255)
	DB	40H
SPEED:	; speed (0..225)
	DB	48H
SONG:	; song mode (monotone output): 0 = song mode disabled, >0 =
	DB	00H
DEBUG:	; debug mode
	DB	00H
ECHO:	; echo mode
	DB	00H
SOURCE:	; source: 0x00 = command line, 0xff = file
	DB	00H

BLOCKPOS:	; position inside file block (0..127)

	DB	80H

RESET_BIT7:	; Reset high bit of A (doesn't need a subroutine...)
	RES	7,A
	RET

	DS	80H		; Stack

	; end of stack, begin of garbage
STACK	EQU	$


;==================================================================================================
;	E N G L I S H
;==================================================================================================


PROCESS_ENGLISH:	; Process English string
	LD	BC,0001H
	LD	DE,0000H
	LD	HL,ENGLISH_BUFFER; English buffer
	LD	(HL),0A0H
L_PROCESS_ENGLI:
	LD	IX,SPEECH_BUFFER; Speech buffer (256 bytes)
	LD	IY,ENGLISH_BUFFER; English buffer (256 bytes)
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	INC	C
	INC	E
	LD	A,0FFH
	CP	E
	JR	NZ,L_PROCESS_ENGLI
	CALL	CONV_ENG_TO_PHO	; convert English buffer (src in 0x5901) to phonemes (d
	CALL	PROCESS_PHONEME	; Process Phonemes string (S9b61_say_main)
	RET

CONV_ENG_TO_PHO:	; convert English buffer (src in 0x5901) to phonemes (dest t
	LD	B,00H
	LD	D,00H
	LD	HL,MEM61	; (mem61)
	LD	(HL),0FFH
L_CV_ENG2PHO1:	; L_nextrule (run rules for non-alpha character)
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	LD	(HL),0FFH
L_CV_ENG2PHO2:	; L_start_parse (~continue..  doesn't set mem56)
	LD	HL,MEM61	; (mem61)
	INC	(HL)
	LD	C,(HL)
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM64),A	; (mem64_sign2)
	CP	'M'-40H+80H	; Ctrl-M / CR with high bit set
	JR	NZ,J_CV_ENG2PHO1; if (A != '.') break
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	INC	(HL)
	LD	C,(HL)
	LD	HL,SPEECH_BUFFER;Speech buffer
	ADD	HL,BC
	LD	(HL),A
	RET

J_CV_ENG2PHO1:	; if (A != '.') break
	CP	'.'+80H
	JR	NZ,L97A5
	INC	C
	INC	HL
	LD	E,(HL)		;get char
	RES	7,E		;mask high bit
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,DE		;
	BIT	0,(HL)		;bit 0 set ?
	JR	NZ,L97A5	;break loop if yes
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	INC	(HL)
	LD	C,(HL)
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC
	LD	(HL),0AEH	;put final <.> in speech buffer
	JR	L_CV_ENG2PHO2	;next char

L97A5:	LD	A,(MEM64)	; (mem64_sign2)
	RES	7,A
	LD	E,A
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,DE
	LD	A,(HL)
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	BIT	1,A
	JR	Z,J_CV_ENG2PHO3	; use alpha rules
	LD	HL,RULES2	; Rules2: digits and symbols
	LD	(MEM62),HL	; Rule LSB or word (mem62)
	JP	L_NEXT_RULE	; test next rule

J_CV_ENG2PHO3:	; use alpha rules
	CP	00H
	JR	NZ,J_CV_ENG2PHO4; (run rules for alpha character)
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	(HL),' '+80H
	LD	HL,TEMP_CTR	;Temporary/Counter (mem56)
	INC	(HL)
	LD	C,(HL)
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC
	LD	A,C
	CP	78H
	JR	NC,L97DC
	LD	(HL),0A0H
	JR	L_CV_ENG2PHO2	; L_start_parse (~continue..  doesn't set mem56)

L97DC:	LD	(HL),8DH	;final <ENTER>
	LD	A,(MEM61)	; (mem61)
	LD	(LA6F8),A
	CALL	PROCESS_PHONEME	; Process Phonemes string (S9b61_say_main)
	LD	A,(LA6F8)
	LD	(MEM61),A	; (mem61)
	JP	L_CV_ENG2PHO1	; L_nextrule (run rules for non-alpha character)

J_CV_ENG2PHO4:	; (run rules for alpha character)
	BIT	7,A
	JR	NZ,L97F5
	HALT
L97F5:	LD	A,(MEM64)	; (mem64_sign2)
	SUB	'A'+80H
	LD	C,A
	LD	HL,RULESPTR_LSB	; Letters rules pointers LSB table
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM62),A	; Rule LSB or word (mem62)
	LD	HL,RULESPTR_MSB	; Letters rules pointers MSB table
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM63),A	; Rule MSB (mem63)
L_NEXT_RULE:	; test next rule
	LD	HL,(MEM62)	; Rule LSB or word (mem62)
	INC	HL
	LD	(MEM62),HL	; Rule LSB or word (mem62)
	BIT	7,(HL)
	JR	NZ,L_NEXT_RULE	; test next rule
	LD	E,01H
	INC	HL

	;find '('
	LD	A,'('+80H
L_FIND_LPAR:	; loop to find '('
	CP	(HL)
	JR	Z,J_FOUND_LPAR	; found '('
	INC	E
	INC	HL
	JR	L_FIND_LPAR	; loop to find '('

	;find ')'
J_FOUND_LPAR:	; found '('
	LD	A,E
	LD	(MEM66),A	; (mem66)
	LD	A,')'+80H
L_FIND_RPAR:	; loop to find ')'
	INC	E
	INC	HL
	CP	(HL)
	JR	NZ,L_FIND_RPAR	; loop to find ')'
	LD	A,E
	LD	(MEM65),A	; (mem65_sign1)

	;find '='
J_FOUND_RPAR:	; found ')'; loop to find '='
	INC	E
	INC	HL
	LD	A,(HL)
	SET	7,A
	CP	'='+80H
	JR	NZ,J_FOUND_RPAR	; found ')'; loop to find '='
	LD	A,E
	LD	(MEM64),A	; (mem64_sign2)

	LD	A,(MEM61)	; (mem61)
	LD	C,A
	LD	(MEM60),A	; Phoneme code to insert (mem60)

	;compare the string within the bracket
	LD	A,(MEM66)	; (mem66)
	LD	E,A
	INC	E
L_CMPRULE:	; Loop to compare the string within the parentheses
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	LD	HL,(MEM62)	; Rule LSB or word (mem62)
	ADD	HL,DE
	CP	(HL)
	JR	NZ,L_NEXT_RULE	; test next rule
	INC	E
	LD	A,(MEM65)	; (mem65_sign1)
	CP	E
	JR	Z,J_CMPRULE_1	; String in the parentheses is correct
	INC	C
	LD	A,C
	LD	(MEM60),A	; Phoneme code to insert (mem60)
	JR	L_CMPRULE	; Loop to compare the string within the parentheses

J_CMPRULE_1:	; String in the parentheses is correct
	LD	A,(MEM61)	; (mem61)
	LD	(MEM59),A	; Phoneme length to insert (mem59)

L_MATCH_LHS:	; LHS pattern matching loop
	LD	HL,MEM66	; (mem66)
	DEC	(HL)
	LD	E,(HL)
	LD	HL,(MEM62)	; Rule LSB or word (mem62)
	ADD	HL,DE
	LD	A,(HL)
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	BIT	7,A
	JP	Z,J_MATCH_RHS	; RHS pattern handling
	RES	7,A
	LD	C,A
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,BC
	BIT	7,(HL)
	JR	Z,L98A0
	LD	A,(MEM59)	; Phoneme length to insert (mem59)
	LD	C,A
	DEC	C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	CP	(HL)
	JP	NZ,L_NEXT_RULE	; test next rule
	LD	A,C
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	JR	L_MATCH_LHS	; LHS pattern matching loop

	;LHS pattern check
L98A0:	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	CP	' '+80H		;Match word break
	JR	Z,LHS_SPC	; lhs ' ' - Match word break
	CP	'#'+80H		;Match one or more vowels
	JR	Z,LHS_HASH	; lhs '#' - Match one or more vowels
	CP	'.'+80H		;Match one voiced consonant
	JR	Z,LHS_DOT	; lhs '.' - Match one voiced consonant
	CP	'&'+80H		;Match one sibilant
	JR	Z,LHS_AND	; lhs '&' - Match one sibilant
	CP	'@'+80H		;Match one consonant influencing long u
	JR	Z,LHS_AT	; lhs '@' - Match one consonant influencing long u
	CP	'^'+80H		;Match one consonant
	JR	Z,LHS_CARET	; lhs '^' - Match one consonant
	CP	'+'+80H		;Match one front vowel
	JR	Z,LHS_PLUS	; lhs '+' - Match one front vowel
	CP	':'+80H		;Match zero or more consonants
	JR	Z,LHS_COLON	; lhs ':' - Match zero or more consonants
	CALL	PLAY_BELL	; Send <BEL> to console
	CALL	PLAY_BELL	; Send <BEL> to console
	CALL	PLAY_BELL	; Send <BEL> to console
	$BREAK
	HALT

LHS_SPC:	; lhs ' ' - Match word break
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	7,(HL)		; Check if letter or apostrophe
	JP	NZ,L_NEXT_RULE	; break and process next rule if yes
	; Accept pattern element and continue
L98D5:	LD	A,C		; point to previous char
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	JP	L_MATCH_LHS	; LHS pattern matching loop

LHS_HASH:	; lhs '#' - Match one or more vowels
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	6,(HL)		; check vowel bit (A,E,I,O,U,Y)
	; test bit
L98E1:	JR	NZ,L98D5	; accept if yes
	JP	L_NEXT_RULE	; test next rule

LHS_DOT:	; lhs '.' - Match one voiced consonant
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	3,(HL)		; Check Voiced consonant bit (B,D,G,J,L,M,N,R,V,W,Z)
	JR	L98E1

LHS_AND:	; lhs '&' - Match one sibilant
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	4,(HL)		; Sibilant bit set (C,G,J,S,X,Z) ?
	JR	NZ,L98D5	; Accept if yes
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,'H'+80H	; check for 'H'
	CP	(HL)
	JP	NZ,L_NEXT_RULE	; test next rule if not
	DEC	C		; get preceding char
	DEC	HL
	LD	A,(HL)
	CP	'C'+80H		; check if 'H' follows 'C'
	JR	Z,L98D5
	CP	'S'+80H		; ... or S
L9907:	JR	Z,L98D5		; accept if yes
	JP	L_NEXT_RULE	; test next rule

LHS_AT:		; lhs '@' - Match one consonant influencing long u
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	2,(HL)		; Consonant influencing long U bit set ? (D,J,L,N,R,S,T,Z)
	JR	NZ,L98D5	; Accept if yes
				; TODO: check patterns 'TH', 'CH', 'SH' ?
	JP	L_NEXT_RULE	; test next rule

LHS_CARET:	; lhs '^' - Match one consonant
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	5,(HL)		; Consonant bit set ? (B,C,D,F,G,H,J,K,L,M,N,P,Q,R,S,T,V,W,X,Z)
	JR	L98E1		; test result

LHS_PLUS:	; lhs '+' - Match one front vowel (E,I,Y)
	LD	A,(MEM59)	; Phoneme length to insert (mem59)
	LD	C,A		; get previous char
	DEC	C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	CP	'E'+80H		; 'E' ?
	JR	Z,L98D5		; accept if yes
	CP	'I'+80H		; or 'I' ?
	JR	Z,L98D5		; accept if yes
	CP	'Y'+80H		; or 'Y' ?
	JR	L9907		; test result

LHS_COLON:	; lhs ':' - Match zero or more consonants
	CALL	GET_LHS		; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	BIT	5,(HL)		; check consonant bit
	JP	Z,L_MATCH_LHS	; stop if not
	LD	A,C		; Point to previous char
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	JR	LHS_COLON	; lhs ':' - Match zero or more consonants

J_MATCH_RHS:	; RHS pattern handling
	LD	A,(MEM60)	; Phoneme code to insert (mem60)
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
L_MATCH_RHS:	; RHS pattern matching loop
	LD	HL,MEM65	; (mem65_sign1)
	LD	E,(HL)
	INC	E
	LD	A,(MEM64)	; (mem64_sign2)
	CP	E
	JP	Z,L_EMIT_RULE	; emit rule
	INC	(HL)
	LD	HL,(MEM62)	; Rule LSB or word (mem62)
	ADD	HL,DE
	LD	A,(HL)
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	RES	7,A
	LD	C,A
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,BC
	BIT	7,(HL)		; Letter or ' ?
	JR	Z,L9976		; check pattern if not
	LD	HL,MEM58	; Phoneme stress value to insert (mem58)
	LD	C,(HL)
	INC	C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	CP	(HL)
	JR	L99AD

	;RHS pattern check
L9976:	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	CP	' '+80H		; Match word break
	JR	Z,RHS_SPC	; rhs ' ' - Match word break
	CP	'#'+80H		; Match one or more vowels
	JR	Z,RHS_HASH	; rhs '#' - Match one or more vowels
	CP	'.'+80H		; Match one voiced consonant
	JR	Z,RHS_DOT	; rhs '.' - Match one voiced consonant
	CP	'&'+80H		; Match one sibilant
	JR	Z,RHS_AND	; rhs '&' - Match one sibilant (seems unused/unuseable)
	CP	'@'+80H		; Match one consonant influencing long u
	JR	Z,RHS_AT	; rhs '@' - Match one consonant influencing long u
	CP	'^'+80H		; Match one consonant
	JR	Z,RHS_CRT	; rhs '^' - Match one consonant
	CP	'+'+80H		; Match a front vowel
	JR	Z,RHS_PLUS	; rhs '+' - Match a front vowel
	CP	':'+80H		; Match zero or more consonants
	JR	Z,RHS_COL	; rhs ':' - Match zero or more consonants
	CP	'%'+80H		; Match a suffix
	JP	Z,RHS_PCT	; rhs '%' - Match a suffix: -E, -ER, -ING, -ES, -ED, -EFUL
	CALL	PLAY_BELL	; Send <BEL> to console
	CALL	PLAY_BELL	; Send <BEL> to console
	CALL	PLAY_BELL	; Send <BEL> to console
	$BREAK
	HALT
RHS_SPC:	; rhs ' ' - Match word break
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	7,(HL)		; letter or apostrophe ?
L99AD:	JP	NZ,L_NEXT_RULE	; test next rule if yes
L99B0:	LD	A,C		; point to next character
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	JR	L_MATCH_RHS	; RHS pattern matching loop

RHS_HASH:	; rhs '#' - Match one or more vowels
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	6,(HL)		; vowel bit set ?
L99BB:	JR	NZ,L99B0	; accept if yes
	JP	L_NEXT_RULE	; test next rule

RHS_DOT:	; rhs '.' - Match one voiced consonant
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	3,(HL)		; Voiced consonant bit set ?
	JR	L99BB		; test result

RHS_AND:	; rhs '&' - Match one sibilant - (no rule uses it...)
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	4,(HL)		; sibilant bit set ?
	JR	NZ,L99B0	; accept if yes
	; this part seems nonsense: SH/CH already accepted as S/C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,'H'+80H	; 'H' ?
	CP	(HL)
	JP	NZ,L_NEXT_RULE	; go to next rule if not
	INC	C		; test next char
	INC	HL		; TODO: bug ? maybe we should test previous char !
	LD	A,(HL)
	CP	'C'+80H		; 'C' ?
	JR	Z,L99B0		; accept if yes
	CP	'S'+80H		; 'S' ?
	JR	L99AD		; test result

RHS_AT:		; rhs '@' - Match one consonant influencing long u
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	2,(HL)		; consonant influencing long U bit set ?
	JR	NZ,L99B0	; accept if yes
				; TODO: handle 'TH', 'CH', 'SH' ?
	JP	L_NEXT_RULE	; test next rule

RHS_CRT:	; rhs '^' - Match one consonant
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	5,(HL)		; Consonant bit set ?
	JR	L99BB		; test result

RHS_PLUS:	; rhs '+' - Match a front vowel
	LD	A,(MEM58)	; Phoneme stress value to insert (mem58)
	LD	C,A		; get next char
	INC	C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	CP	'E'+80H		; 'E' ?
	JR	Z,L99B0		; accept if yes
	CP	'I'+80H		; 'I' ?
	JR	Z,L99B0		; accept if yes
	CP	'Y'+80H		; 'Y' ?
	JR	L99AD		; test result

RHS_COL:	; rhs ':' - Match zero or more consonants
	CALL	GET_RHS		; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	BIT	5,(HL)
	JP	Z,L_MATCH_RHS	; RHS pattern matching loop
	LD	A,C
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	JR	RHS_COL		; rhs ':' - Match zero or more consonants

RHS_PCT:	; rhs '%' - Match a suffix: -E, -ER, -ING, -ES, -ED, -ELY, -EFUL
	LD	A,(MEM58)	; Phoneme stress value to insert (mem58)
	LD	C,A
	INC	C
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	CP	'E'+80H		; is 'E' ?
	JR	NZ,J_MATCH_ING	; If not, Test match -ING
	INC	C
	INC	HL
	LD	E,(HL)		; get next char
	RES	7,E		; mask high bit
	DEC	C
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,DE		; get flags of next char
	BIT	7,(HL)		; is a letter or apostrophe ?
	JR	Z,J_MATCH_OK	; accept rule if not
	INC	C		; get next char (again?)
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	A,(HL)
	CP	'R'+80H		; 'R' ?
	JR	NZ,J_MATCH_ES_ED_E; if not, Test match -S/-D/-LY
J_MATCH_OK:	; Matching OK
	LD	A,C
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	JP	L_MATCH_RHS	; RHS pattern matching loop

J_MATCH_ES_ED_E:	; Test match -ES/-ED/-ELY
	CP	'S'+80H		; 'S' (-ES)
	JR	Z,J_MATCH_OK	; Accept if yes
	CP	'D'+80H		; 'D' (-ED)
	JR	Z,J_MATCH_OK	; Accept if yes
	CP	'L'+80H		; 'L' (-EL*)
	JR	NZ,J_MATCH_EFUL	; If not, Test match -FUL
	INC	C		; point to next char
	INC	HL
	LD	A,'Y'+80H	; 'Y' (-ELY) ?
	CP	(HL)
	JR	Z,J_MATCH_OK	; Accept if yes
	JP	L_NEXT_RULE	; go to next rule

J_MATCH_EFUL:	; Test match -EFUL
	CP	'F'+80H		; 'F' ?
	JP	NZ,L_NEXT_RULE	; go to next rule if not
	INC	C		; point to next char
	INC	HL
	LD	A,'U'+80H	; 'U' ?
	CP	(HL)
	JP	NZ,L_NEXT_RULE	; go to next rule if not
	INC	C		; point to next char
	INC	HL
	LD	A,'L'+80H	; 'L' ?
	CP	(HL)
	JR	Z,J_MATCH_OK	; Accept if yes
	JP	L_NEXT_RULE	; go to next rule

J_MATCH_ING:	; Test match -ING
	CP	'I'+80H		; 'I' ?
	JP	NZ,L_NEXT_RULE	; test next rule
	INC	C		; go to next rule if not
	INC	HL
	LD	A,'N'+80H	; 'N' ?
	CP	(HL)
	JP	NZ,L_NEXT_RULE	; test next rule if not
	INC	C
	INC	HL
	LD	A,'G'+80H	; 'G'
	CP	(HL)
	JR	Z,J_MATCH_OK	; Accept if yes
	JP	L_NEXT_RULE	; go to next rule

L_EMIT_RULE:	; emit rule
	LD	A,(MEM64)	; (mem64_sign2)
	LD	E,A
	LD	A,(MEM60)	; Phoneme code to insert (mem60)
	LD	(MEM61),A	; (mem61)
L9A93:	LD	HL,(MEM62)	; Rule LSB or word (mem62)
	ADD	HL,DE
	LD	A,(HL)
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	SET	7,A
	CP	'='+80H
	JR	Z,L9AAB
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	INC	(HL)
	LD	C,(HL)
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC
	LD	(HL),A
L9AAB:	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	BIT	7,A
	JP	Z,L_CV_ENG2PHO2	; L_start_parse (~continue..  doesn't set mem56)
	INC	E
	JR	NZ,L9A93
GET_LHS:	; Get LHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM59-1]]
	LD	A,(MEM59)	; Phoneme length to insert (mem59)
	LD	C,A
	DEC	C
J_GET_LHS:
	LD	HL,ENGLISH_BUFFER; English buffer
	ADD	HL,BC
	LD	E,(HL)
	RES	7,E
	LD	HL,CHARFLAGS	; English characters flags (tab36376)
	ADD	HL,DE
	RET

GET_RHS:	; Get RHS address: &CHARFLAGS[ENGLISH_BUFFER[MEM58+1]]
	LD	A,(MEM58)	; Phoneme stress value to insert (mem58)
	LD	C,A
	INC	C
	JR	J_GET_LHS


;==================================================================================================
;	P H O N E M E S
;==================================================================================================

DEBUG_PHONEME:			; trace phonemes on DEBUG mode or error
	LD	A,(ERROR_POS)	; Error found in phonemes string ?
	INC	A		; (if ERROR_POS = $FF)
	JR	NZ,JDBG0	; Jump if yes
	LD	A,(DEBUG)	; DEBUG flag set ?
	OR	A		;
	RET	Z		; return if not
	XOR	A		; clear Z
JDBG0:	LD	A,0DH		;
	CALL	NZ,DDISA	; display CR if error found
	LD	A,'['		; display '['
	CALL	DDISA		;
	LD	HL,SPEECH_BUFFER; point to phonemes string
LDBG1:	LD	A,(HL)		; fetch char
	INC	HL		; mask high bit
	AND	7FH		;
	CP	0DH		; exit loop if end of phonemes string
	JR	Z,JDBG1		;
	CALL	DDISA		; display char
	JR	LDBG1		; next char
JDBG1:	LD	A,']'		; display ']'
	CALL	DDISA		;
	LD	A,(ERROR_POS)	; error found in phonemes string ?
	INC	A		;
	RET	Z		; return if not
	LD	B,A		; error pos to loop counter
	LD	A,0DH		; display CR
	CALL	DDISA		;
LDBG2:	LD	A,' '		; display ' '
	CALL	DDISA		;
	DJNZ	LDBG2		; until under error location
	LD	A,'^'		; display '^' error pointer
	CALL	DDISA		;
	LD	A,0DH		; display CR
	CALL	DDISA		;
	RET			; done



PROCESS_PHONEME:		; Process Phonemes string (S9b61_say_main)
	LD	A,0FFH
	LD	(ERROR_POS),A	; Error position
	CALL	PARSER1		; Parse 'input[]' and populate 'phonemeIndex[]'
	CALL	DEBUG_PHONEME	; trace phonemes string on debug mode or error
	LD	A,(ERROR_POS)	; Error position
	INC	A
	JR	NZ,SAY_UNINIT	; if (error_pos != $ff) goto say_uninit
NDEBUG:	CALL	PARSER2		; Rules based replacement of certain phoneme patterns
	CALL	COPY_STRESS	; Rules based adjustment of stress
	CALL	SET_PHONM_LNGTH	; Change phonemeLength depedendent on stress
	CALL	ADJUST_LENGTH	; Rules based length adjustments
	CALL	EXT_STOP_CONS	; Extend stop consonants
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	LD	BC,0000H	; count phonemes
L9B01:	LD	A,(HL)		; get phoneme ID
	CP	50H		; valid ?
	JR	NC,L9B0C	; exit loop if not
	INC	HL		; next
	INC	C		; up to 255 phonemes
	JR	NZ,L9B01
	JR	L9B0E

L9B0C:	LD	(HL),0FFH	; end of phonemes marker
L9B0E:	CALL	INSERT_BREATH	; Insert breath
	LD	HL,PHONEMEINDEXEND; End of phonemes buffer
	LD	(HL),0FFH	; force end of phonemes at index 254
	CALL	PREPAREOUTPUT	; Prepare output (Sbda3_PrepareOutput)
SAY_UNINIT:	; if (error_pos != $ff) goto say_uninit
	RET

PREPROCESS_USER:	; 0x86ff = number of characters, 0x8700 = actual chars
	LD	B,00H
	LD	A,(SPEECHBUFFERLEN); Number of chars in speech buffer
	LD	C,A
	CP	B
	JR	Z,L9B4A
	LD	(INPUT_BUFFER_LEN),A
	LD	HL,INPUT_BUFFER
	ADD	HL,BC
	EX	DE,HL
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC
	LD	(HL),8DH
	INC	BC
	LDDR
	JR	L9B5B

L9B43:	LD	HL,SPEECH_BUFFER; Speech buffer
	LD	(HL),8DH
	JR	L9B5B

L9B4A:	LD	A,(INPUT_BUFFER_LEN)
	CP	00H
	JR	Z,L9B43
	LD	C,A
	LD	HL,INPUT_BUFFER
	LD	DE,SPEECH_BUFFER; Speech buffer (256 bytes)
	INC	BC
	LDIR
L9B5B:	LD	A,(INPUT_BUFFER_LEN)
	LD	C,A
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC
L9B63:	XOR	A
	CP	C
	RET	Z
	DEC	C
	DEC	HL
	LD	A,(HL)
	CALL	MAKE_UPPERCASE	; make 'A' upper case and reset bit 7
	SET	7,A
	LD	(HL),A
	JR	L9B63


; PARSER1: The input[] buffer contains a string of phonemes and stress markers
; along the lines of:
;
;     DHAX KAET IHZ AH5GLIY. <0x8D>
;
; The byte 0x8D marks the end of the buffer. Some phonemes are 2 bytes
; long, such as "DH" and "AX". Others are 1 byte long, such as "T" and "Z".
; There are also stress markers, such as "5" and ".".
;
; The first character of the phonemes are stored in the table signInputTable1[].
; The second character of the phonemes are stored in the table signInputTable2[].
; The stress characters are arranged in low to high stress order in stressInputTable[].
;
; The following process is used to parse the input[] buffer:
;
; Repeat until the <0x8D> character is reached:
;
;        First, a search is made for a 2 character match for phonemes that do not
;        end with the '*' (wildcard) character. On a match, the index of the phoneme
;        is added to phonemeIndex[] and the buffer position is advanced 2 bytes.
;
;        If this fails, a search is made for a 1 character match against all
;        phoneme names ending with a '*' (wildcard). If this succeeds, the
;        phoneme is added to phonemeIndex[] and the buffer position is advanced
;        1 byte.
;
;        If this fails, search for a 1 character match in the stressInputTable[].
;        If this succeeds, the stress value is placed in the last stress[] table
;        at the same index of the last added phoneme, and the buffer position is
;        advanced by 1 byte.
;
;        If this fails, return a 0.
;
; On success:
;
;    1. phonemeIndex[] will contain the index of all the phonemes.
;    2. The last index in phonemeIndex[] will be 255.
;    3. stress[] will contain the stress value for each phoneme
;
; input[] holds the string of phonemes, each two bytes wide
; signInputTable1[] holds the first character of each phoneme
; signInputTable2[] holds te second character of each phoneme
; phonemeIndex[] holds the indexes of the phonemes after parsing input[]
;
; The parser scans through the input[], finding the names of the phonemes
; by searching signInputTable1[] and signInputTable2[]. On a match, it
; copies the index of the phoneme into the phonemeIndexTable[].
;
; The character <0x8D> marks the end of text in input[]. When it is reached,
; the index 255 is placed at the end of the phonemeIndexTable[], and the
; function returns with a 1 indicating success.
PARSER1:	; Parse 'input[]' and populate 'phonemeIndex[]'
	XOR	A
	LD	(MEM66),A	; (mem66)
	LD	BC,0000H
	LD	DE,0000H
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
L1_PARSER1:	; Clear the stress table
	LD	(HL),B
	INC	HL
	INC	E
	LD	A,0C7H		; end of stress table ?
	CP	E
	JR	NZ,L1_PARSER1	; loop if not
L2_PARSER1:	; Next input (La076_next_input)
	LD	A,C
	CP	0C8H		; end of phonemes ? (TODO: unclear)
	JR	C,NEXT_INPUT	; This code matches the phoneme letters to the table
	LD	HL,SPEECH_BUFFER; Speech buffer
	ADD	HL,BC		; put a End-of-line marker in the speech buffer
	LD	(HL),8DH
	RET

NEXT_INPUT:	; This code matches the phoneme letters to the table
	LD	IX,SPEECH_BUFFER; Speech buffer (L9a15_input)
	ADD	IX,BC
	LD	A,(IX+0)	; get the 1st character from the speech buffer
	CP	8DH		; End-of-line marker ?
	JP	Z,X_PARSER1	; End-of-line marker found => finish
	LD	(MEM65),A	; Store as 1st sign (mem65_sign1)
	INC	C		; get the 2nd character from the speech buffer
	LD	A,(IX+1)
	LD	(MEM64),A	; (mem64_sign2)

	; Now sign1 = first character of phoneme, and sign2 = second character of phoneme

	; Try to match phonemes on two two-character name
	; Ignore phonemes in table ending with wildcards

	; Set index to 0
	LD	E,00H
L9BAC:
	; First character matches?
	LD	HL,SIGNINPUTTBL1; phoneme names 1st char or '*'
	ADD	HL,DE
	LD	A,(MEM65)	; (mem65_sign1)
	CP	(HL)
	JR	NZ,L9BC5	; if (signInputTable1[Y] != sign1) continue;

	; Not a special and matches second character?
	LD	HL,SIGNINPUTTBL2; phoneme names 2nd char or '*'
	ADD	HL,DE
	LD	A,'*'+80H
	CP	(HL)
	JR	Z,L9BC5		; if (signInputTable2[Y] == '*') continue;
	LD	A,(MEM64)	; (mem64_sign2)
	CP	(HL)
	JR	Z,J1_PARSER1	; if (signInputTable2[Y] == sign2) break; //Found match (2 chars match)
L9BC5:	INC	E
	LD	A,51H
	CP	E
	JR	NZ,L9BAC	; while (++Y < $51);
	JR	J3_PARSER1	; Search for a 1 character match against the wildcards

J1_PARSER1:	; Found match (2 chars match)
	INC	C
J2_PARSER1:	; Found match (1 char match)
	; Store the index of the phoneme into the 'phomeneIndex[]' table
	LD	A,E		; phoneme index
	LD	HL,MEM66	; phoneme index pos (mem66_pos)
	LD	E,(HL)		; get pos
	INC	(HL)		; and bump
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,DE		; phoneme index ptr
	LD	(HL),A		; write the new phoneme index
	JR	L2_PARSER1	; Next input (La076_next_input)

	; Reached end of table without an exact (2 character) match.
	; This time, search for a 1 character match against the wildcards

	; Reset the index to point to the start of the phoneme name table
J3_PARSER1:	; Search for a 1 character match against the wildcards
	; Set index to 0
	LD	E,00H
L9BDD:
	; Does the phoneme in the table end with '*'?
	LD	HL,SIGNINPUTTBL2; phoneme names 2nd char or '*'
	ADD	HL,DE
	LD	A,'*'+80H
	CP	(HL)
	JR	NZ,L9BF0	; if (signInputTable2[Y] != '*') not wildcard

	; Does the first character match the first letter of the phoneme
	LD	HL,SIGNINPUTTBL1; phoneme names 1st char or '*'
	ADD	HL,DE
	LD	A,(MEM65)	; (mem65_sign1)
	CP	(HL)
	JR	Z,J2_PARSER1	; if (signInputTable1[Y] == sign1) found match (1 char match)

	; Not wildcard match
L9BF0:	INC	E
	LD	A,51H
	CP	E
	JR	NZ,L9BDD	; while (++Y != $51);

	; Failed to match with a wildcard. Assume this is a stress
	; character. Search through the stress table
	LD	A,(MEM65)	; (mem65_sign1)
	LD	E,08H		; Set index to position 8 (end of stress table)
	LD	HL,STRESSINPUTTBL; stress input codes ('1'..'8')
	ADD	HL,DE

	; Walk back through table looking for a match
	; TODO: could be simple calculation instead of looping over table?
L9BFF:	CP	(HL)
	JR	Z,J4_PARSER1	; if found, set the stress for the prior phoneme
	DEC	HL
	DEC	E
	JR	NZ,L9BFF	; iterate

	; Failed to parse => error
	LD	A,C
	LD	(ERROR_POS),A	; Error position
	IF	CPM
	  LD	E,07H
	  LD	C,02H
	  CALL	BDOS
	  LD	E,07H
	  LD	C,02H
	  CALL	BDOS
	ENDIF
	IF	LSDOS6
	  LD	B,00H
	  LD	A,@SOUND
	  CALL	DRVSVC
	  LD	B,03H
	  LD	A,@SOUND
	  CALL	DRVSVC
	  LD	B,06H
	  LD	A,@SOUND
	  CALL	DRVSVC
	ENDIF
	RET

J4_PARSER1:	; Set the stress for the prior phoneme
	LD	A,E
	LD	HL,MEM66	; (mem66)
	LD	E,(HL)
	DEC	E
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,DE
	LD	(HL),A		; stress[position - 1] = Y
	JP	L2_PARSER1	; Next input (La076_next_input)

X_PARSER1:	; End-of-line marker found => finish
	LD	HL,MEM66	; (mem66)
	LD	E,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,DE
	LD	(HL),0FFH	; end of phonemes marker
	RET


; PARSER2: Rewrites the phonemes using the following rules:
;
;       <DIPHTONG ENDING WITH WX> -> <DIPHTONG ENDING WITH WX> WX
;       <DIPHTONG NOT ENDING WITH WX> -> <DIPHTONG NOT ENDING WITH WX> YX
;       UL -> AX L
;       UM -> AX M -- TODO: UN ??
;       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
;       T R -> CH R
;       D R -> J R
;       <VOWEL> R -> <VOWEL> RX
;       <VOWEL> L -> <VOWEL> LX
;       G S -> G Z
;       K <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
;       G <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
;       S P -> S B
;       S T -> S D
;       S K -> S G
;       S KX -> S GX
;       <ALVEOLAR> UW -> <ALVEOLAR> UX
;       CH -> CH CH' (CH requires two phonemes to represent it)
;       J -> J J' (J requires two phonemes to represent it)
;       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
;       <UNSTRESSED VOWEL> D <PAUSE>  -> <UNSTRESSED VOWEL> DX <PAUSE>
PARSER2:	; Rules based replacement of certain phoneme patterns
	LD	HL,MEM66	; (mem66)
	LD	(HL),00H	; reset position
	LD	B,00H
	LD	D,00H

	; Loop through phonemes
LOOP_PHON_9C3B:
	; Set C (X) to the current position
	LD	C,(HL)

	; Get the phoneme at the current position
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)

	; Is phoneme pause?
	CP	00H
	JP	Z,NEXT_PHON	; next

	; If end of phonemes flag reached, exit routine
	CP	0FFH
	RET	Z

	; Copy the current phoneme index to Y
	LD	E,A

	; RULE:
	;       <DIPHTONG ENDING WITH WX> -> <DIPHTONG ENDING WITH WX> WX
	;       <DIPHTONG NOT ENDING WITH WX> -> <DIPHTONG NOT ENDING WITH WX> YX
	; Example: OIL, COW

	; Check for DIPHTONG
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	4,(HL)		; Diphtong but set ?
	JR	Z,L9C77		; if not, skip

	; Not(?) a DIPTHONG. Get the stress
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57) = pos + 1

	; If ends with IY, use YX, else use WX
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	5,(HL)		; check flag DipthongYX
	LD	A,14H		; 'WX'
	JR	Z,L9C6A		; skip if not set
	INC	A		; else 'YX'
L9C6A:	LD	(MEM60),A	; Phoneme code to insert (mem60)
	CALL	INSERT_PHONEME	; Insert phoneme (mem60,mem59,mem58) at position (mem57
	LD	A,(MEM66)	; get pos to C (mem66)
	LD	C,A		;
	JP	L9D86		; continue with ALVEOLAR rules

;	Next rule
L9C77:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC

	; RULE:
	;       UL -> AX L
	; Example: MEDDLE
	LD	A,(HL)
	CP	4EH		; 'UL' ?
	LD	E,18H		; 'L'
	JR	Z,L9C8D		; if yes, change to 'AX L'

	; RULE:
	;       UM -> AX M
	; Example: ASTRONOMY
	LD	E,1BH		; 'M'
	CP	4FH		; 'UM' ?
	JR	Z,L9C8D		; if yes, change to 'AX M'

	; RULE:
	;       UN -> AX N
	; Example: FUNCTION
	INC	E		; 'N'
	CP	50H		; 'UN' ?
	JR	NZ,L9CAA	; if yes, change to 'AX M', or
L9C8D:	LD	A,E
	LD	(MEM60),A	; Phoneme code to insert (mem60)

	; Get current phoneme stress
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	LD	A,(HL)
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC

	; Change 'U?' to 'AX'
	LD	(HL),0DH	; replace with 'AX'
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	CALL	INSERT_PHONEME	; Insert phoneme 'L', 'M' or 'N'
	JP	NEXT_PHON	; Move to next phoneme

	; RULE:
	;       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
	;	Q = glottal stop / hiatus
	; EXAMPLE: AWAY EIGHT
L9CAA:	LD	E,A
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; is vowel bit set
	JR	Z,L9CE8		; skip if not
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	XOR	A
	CP	(HL)		; is it stressed ?
	JR	Z,L9CE8		; skip if not
	INC	C		; Skip if following phoneme is not a pause
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	CP	(HL)
	JR	NZ,L9CE8
	INC	C		; Skip if phoneme after pause is not a vowel
	INC	HL
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; is vowel bit set
	JR	Z,L9CE8
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	CP	(HL)		; Skip if phoneme after pause is not stressed
	JR	Z,L9CE8
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	XOR	A
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	LD	A,1FH		; 'Q' = glottal stop
	LD	(MEM60),A	; (mem60) = 'Q', code to insert
	CALL	INSERT_PHONEME	; insert the 'Q'
	JP	NEXT_PHON	; Move to next phoneme

	; RULES FOR PHONEMES BEFORE R
	;        T R -> CH R
	; Example: TRACK
L9CE8:	LD	HL,MEM66	; (mem66)
	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)
	CP	17H		; 'R' ?
	JR	NZ,L9D1A	; skip if not
	DEC	C		; get preceding phoneme
	DEC	HL
	LD	A,(HL)
	CP	45H		; 'T' ?
	JR	NZ,L9D00	; skip if not
	LD	A,2AH		; replace with 'CH'
	JR	L9D06

	; RULES FOR PHONEMES BEFORE R
	;        D R -> J R
	; Example: DRY
L9D00:	CP	39H		; 'D' ?
	JR	NZ,L9D0A	; skip if not
	LD	A,2CH		; replace with 'J'
L9D06:	LD	(HL),A		; do it
	JP	L9DA7		; replace 'CH' with 'CH CH+' and 'J' with 'J J+'

	; RULES FOR PHONEMES BEFORE R
	;        <VOWEL> R -> <VOWEL> RX
	; Example: ART
L9D0A:	LD	E,A		; point to phoneme preceding R
	INC	C
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE		;
	BIT	7,(HL)		; if a vowel ?
	JP	Z,NEXT_PHON	; skip if not
	LD	A,12H		; replace 'R' with 'RX'
	JP	DONE_PHON	; done: store A and next

	; RULE:
	;       <VOWEL> L -> <VOWEL> LX
	; Example: ALL
L9D1A:	CP	18H		; 'L' ?
	JR	NZ,L9D2E	; skip if not
	DEC	HL		; preceding phoneme
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; get the phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; vowel ?
	JP	Z,NEXT_PHON	; skip if not
	LD	A,13H		; change to 'LX'
	JP	DONE_PHON	; done: store A and next

	; RULE:
	;       G S -> G Z
	;
	; TODO: Can't get to fire:
	;       1. The G -> GX rule intervenes
	;       2. Reciter already replaces GS -> GZ
L9D2E:	CP	20H		; 'S' ?
	JR	NZ,L9D3E	; skip if not
	DEC	HL
	LD	A,(HL)		; get previous phoneme
	CP	3CH		; 'G' ?
	JP	NZ,NEXT_PHON	; skip if not
	LD	A,26H		; change to 'Z'
	JP	DONE_PHON	;done: store A and next

    ; RULE:
    ;       K <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
    ; Example: COW
L9D3E:	CP	48H		; 'K'
	JP	NZ,L9D55	; skip if not
	INC	HL		; next phoneme
	LD	E,(HL)		; get it
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE		; get the flags
	BIT	5,(HL)		; is vowel or diphtong not ending with 'IY' ?
	JR	NZ,L9D69	; skip if not
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	(HL),4BH	; change to 'KX'
	JR	L9D69		; next rule

	; RULE:
	;       G <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
	; Example: GO
L9D55:	CP	3CH		; 'G' ?
	JR	NZ,L9D69	; skip if not
	INC	HL
	LD	E,(HL)		; get next phoneme
	LD	HL,PHONM_FLAGS1	; get phoneme flags 1
	ADD	HL,DE
	BIT	5,(HL)		; is vowel or diphtong not ending with 'IY' ?
	JP	NZ,NEXT_PHON	; skip if not
	LD	A,3FH		; change to 'GX'
	JP	DONE_PHON	;done: store A and next

    ; RULE:
    ;      S P -> S B
    ;      S T -> S D
    ;      S K -> S G
    ;      S KX -> S GX
    ; Examples: SPY, STY, SKY, SCOWL
L9D69:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	0,(HL)		; Is plosive ? ('P' 'T' 'K' 'KX')
	JR	Z,L9D86		; skip rule if not
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	DEC	HL
	LD	A,(HL)		; get the previous phoneme
	CP	20H		; is a 'S' ?
	LD	A,E
	JR	NZ,L9DBE	; skip several rules if not
	SUB	0CH		; shift the phonemes 'P' 'T' 'K' 'KX' to 'B' 'D' 'G' 'GX'
	JP	DONE_PHON	; done: store A and next

	; RULE:
	;      <ALVEOLAR> UW -> <ALVEOLAR> UX
	;
	; Example: NEW, DEW, SUE, ZOO, THOO, TOO
L9D86:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)
	CP	35H		; 'UW' ?
	JR	NZ,L9D9F	; skip rule if not
	DEC	HL
	LD	E,(HL)		; previous phoneme
	LD	HL,PHONM_FLAGS2	; get phoneme flags 2
	ADD	HL,DE
	BIT	2,(HL)		; is alveolar ?
	JP	Z,NEXT_PHON	; go to next phoneme if not
	LD	A,10H		; change to 'UX'
	JP	DONE_PHON	; done: store A and next

	; RULE:
	;       'CH' -> 'CH CH+' (CH requires two phonemes to represent it)
	; Example: CHEW
L9D9F:	CP	2AH		; is 'CH' ?
	JR	Z,L9DA7		; go if yes

	; RULE:
	;       J -> J J+ (J requires two phonemes to represent it)
	; Example: JAY
	CP	2CH		; is 'J' ?
	JR	NZ,L9DBE	; skip rule if not
L9DA7:	INC	A		; insert 'J+' after 'J', 'CH+' after 'CH'
	LD	(MEM60),A	; Phoneme code to insert (mem60)
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	DEC	C
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	LD	A,(HL)		; get the current stress
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	CALL	INSERT_PHONEME	; Insert phoneme (mem60,mem59,mem58) at position (mem57
	JR	NEXT_PHON	; Move to next phoneme

	; RULE: Soften T following vowel
	; NOTE: This rule fails for cases such as "ODD"
	;       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
	;       <UNSTRESSED VOWEL> D <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
	; Example: PARTY, TARDY
	; TODO: this description seems inaccurate ...
	; 	seems to be:
	;       <VOWEL> T <UNSTRESSED VOWEL> -> <VOWEL> DX <UNSTRESSED VOWEL>
	;       <VOWEL> D <UNSTRESSED VOWEL> -> <VOWEL> DX <UNSTRESSED VOWEL>
	;       <VOWEL> T <PAUSE> <VOWEL>    -> <VOWEL> DX <PAUSE> <VOWEL>
	;       <VOWEL> D <PAUSE> <VOWEL>    -> <VOWEL> DX <PAUSE> <VOWEL>
L9DBE:
	; Past this point, only process if phoneme is T or D
	CP	45H		; is a 'T' ?
	JP	Z,L9DC7		; go if yes
	CP	39H		; is a 'D' ?
	JR	NZ,NEXT_PHON	; next phoneme if not

	; If prior phoneme is not a vowel, continue processing phonemes
L9DC7:	DEC	C		; point to prior phoneme
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	INC	C		; TODO: would be easier to DEC HL instead of DEC C/INC C...
	LD	E,(HL)		; get it
	LD	HL,PHONM_FLAGS1	; get phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; is a vowel ?
	JR	Z,NEXT_PHON	; next phoneme if not
	INC	C		; get next phoneme
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)		; get it
	CP	00H		; is a pause ?
	JR	Z,L9DFA		; go if yes
	LD	E,A
	LD	HL,PHONM_FLAGS1	; get next phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; is a vowel ?
	JR	Z,NEXT_PHON	; next phoneme if not
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC		;
	LD	A,(HL)		; get stress value for next phoneme
	CP	00H		; is it stressed ?
	JR	NZ,NEXT_PHON	; next phoneme if yes
L9DF2:	LD	A,(MEM66)	; (mem66)
	LD	C,A		; get current pos
	LD	A,1EH		; change to 'DX'
	JR	DONE_PHON	; done: store A and next

	; we have a pause
L9DFA:	INC	C		; skip the pause
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)		; get phoneme following the pause
	LD	HL,PHONM_FLAGS1	; get the flags phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)		; is a vowel ?
	JR	NZ,L9DF2	; if yes, change to 'DX'
	JR	NEXT_PHON	; else next phoneme

DONE_PHON:	; done: store A and next
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	(HL),A
NEXT_PHON:	; Move to next phoneme
	LD	HL,MEM66	; (mem66)
	INC	(HL)
	JP	LOOP_PHON_9C3B


; COPY_STRESS: Iterates through the phoneme buffer, copying the stress value from
; the following phoneme under the following circumstance:
;
;     1. The current phoneme is voiced, excluding plosives and fricatives
;     2. The following phoneme is voiced, excluding plosives and fricatives, and
;     3. The following phoneme is stressed
;
;  In those cases, the stress value+1 from the following phoneme is copied.
;
; For example, the word LOITER is represented as LOY5TER, with as stress
; of 5 on the diphtong OY. This routine will copy the stress value of 6 (5+1)
; to the L that precedes it.
COPY_STRESS:	; Rules based adjustment of stress
	LD	HL,MEM66	; (mem66)
	LD	(HL),00H
	LD	B,00H
	LD	D,00H
L9E1F:	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,0FFH
	CP	(HL)
	RET	Z
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	6,(HL)
	JR	Z,L9E53
	INC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)
	JR	Z,L9E53
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	BIT	7,(HL)
	JR	NZ,L9E53
	LD	A,(HL)
	CP	00H
	JR	Z,L9E53
	INC	A
	DEC	C
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	LD	(HL),A
L9E53:	LD	HL,MEM66	; (mem66)
	INC	(HL)
	JR	L9E1F

; SET_PHONM_LNGTH: Change phonemeLength depedendent on stress
SET_PHONM_LNGTH:
	LD	B,00H
	LD	DE,0000H
L9E5E:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,DE
	LD	A,(HL)
	CP	0FFH
	RET	Z
	LD	C,A
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,DE
	LD	A,(HL)
	CP	00H
	JR	Z,L9E79
	BIT	7,A
	JR	NZ,L9E79
	LD	HL,PHONEMESTRSLEN; Phoneme stressed lengths table
	JR	L9E7C

L9E79:	LD	HL,PHONEMENORMLEN; Phoneme normal lengths table
L9E7C:	ADD	HL,BC
	LD	A,(HL)
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,DE
	LD	(HL),A
	INC	E
	JR	L9E5E


; ADJUST_LENGTH: Applies various rules that adjust the lengths of phonemes
;
;	Lengthen <FRICATIVE> or <VOICED> between <VOWEL> and <PUNCTUATION> by 1.5
;	<VOWEL> <RX | LX> <CONSONANT> - decrease <VOWEL> length by 1
;	<VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/8th
;	<VOWEL> <UNVOICED CONSONANT> - increase vowel by 1/2 + 1
;	<NASAL> <STOP CONSONANT> - set nasal = 5, consonant = 6
;	<VOICED STOP CONSONANT> {optional silence} <STOP CONSONANT> - shorten both to 1/2 + 1
;	<LIQUID CONSONANT> <DIPHTONG> - decrease by 2
ADJUST_LENGTH:	; Rules based length adjustments
	LD	BC,0000H
	LD	D,00H
L9E8B:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,0FFH
	CP	(HL)
	JR	Z,L9EE9
	LD	E,(HL)
	LD	HL,PHONM_FLAGS2	; phoneme flags 2
	ADD	HL,DE
	BIT	0,(HL)
	JR	NZ,L9EA0
	INC	C
	JR	L9E8B

L9EA0:	LD	A,C
	LD	(MEM66),A	; (mem66)
L9EA4:	DEC	C
	XOR	A
	CP	C
	JR	Z,L9EE9
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)
	JR	Z,L9EA4
L9EB6:	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS2	; phoneme flags 2
	ADD	HL,DE
	BIT	5,(HL)
	JR	Z,L9ECB
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	2,(HL)
	JR	Z,L9EDE
L9ECB:	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	LD	(HL),A
	SRL	A
	ADD	A,(HL)
	ADD	A,01H
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	(HL),A
L9EDE:	INC	C
	LD	A,(MEM66)	; (mem66)
	CP	C
	JR	NZ,L9EB6
	INC	C
	JP	L9E8B

L9EE9:	LD	HL,MEM66	; (mem66)
	LD	(HL),00H
L9EEE:	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,0FFH
	CP	(HL)
	RET	Z
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	7,(HL)
	JP	Z,L9F6E
	INC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	LD	A,(HL)
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
	BIT	6,A
	JR	Z,L9F4B
	BIT	2,A
	JR	Z,L9F30
	DEC	C
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	LD	(HL),A
	SRL	A
	SRL	A
	ADD	A,(HL)
	ADD	A,01H
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	(HL),A
	JP	L9FE2

L9F30:	BIT	0,A
	JP	Z,L9FE2
	DEC	C
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	SRL	A
	SRL	A
	SRL	A
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
	LD	E,A
	LD	A,(HL)
	SUB	E
	LD	(HL),A
	JP	L9FE2

L9F4B:	LD	A,E
	CP	12H
	JR	Z,L9F55
	CP	13H
	JP	NZ,L9FE2
L9F55:	INC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	6,(HL)
	JR	Z,L9FE2
	LD	A,(MEM66)	; (mem66)
	LD	C,A
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	DEC	(HL)
	JR	L9FE2

L9F6E:	LD	HL,PHONM_FLAGS2	; phoneme flags 2
	ADD	HL,DE
	BIT	3,(HL)
	JR	Z,L9F8F
	INC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	1,(HL)
	JR	Z,L9FE2
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	(HL),06H
	DEC	HL
	LD	(HL),05H
	JR	L9FE2

L9F8F:	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	1,(HL)
	JR	Z,L9FC3
L9F97:	INC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	XOR	A
	CP	E
	JR	Z,L9F97
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	1,(HL)
	JR	Z,L9FE2
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	SRL	A
	ADD	A,01H
	LD	(HL),A
	LD	A,(MEM66)	; (mem66)
	LD	C,A
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	SRL	A
	ADD	A,01H
	LD	(HL),A
	JR	L9FE2

L9FC3:	LD	HL,PHONM_FLAGS2	; phoneme flags 2
	ADD	HL,DE
	BIT	4,(HL)
	JR	Z,L9FE2
	DEC	C
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	HL,PHONM_FLAGS1	; phoneme flags 1
	ADD	HL,DE
	BIT	1,(HL)
	JR	Z,L9FE2
	INC	C
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	SUB	02H
	LD	(HL),A
L9FE2:	LD	HL,MEM66	; (mem66)
	INC	(HL)
	JP	L9EEE


; EXT_STOP_CONS: Extends stop consonant phonemes in the following cases:
; - non-plosive: always
; - plosive: if the mouth is not opening on the next phoneme
;
; The extension is done by:
; - inserting after the current stop consonant, the code of the phoneme
;   incremented by one, with a length extracted from an extended phoneme
;   length table and with the same stress level as the original phoneme;
; - inserting after the current stop consonant, the code of the phoneme
;   incremented by two, with a length extracted from an extended phoneme
;   length table and with the same stress level as the original phoneme;
EXT_STOP_CONS:	; Extend stop consonants
	LD	B,00H
	LD	D,00H
	LD	HL,MEM66	; (mem66)
	LD	(HL),00H

	; Loop on phonemes
L9FF2:	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)
	CP	0FFH
	RET	Z
	LD	(MEM60),A	; Phoneme code to insert (mem60)
	LD	E,A
L9FFF:	LD	HL,PHONM_FLAGS1	; get phoneme flags 1
	ADD	HL,DE
	BIT	1,(HL)		; Stop consonant ?
	JR	NZ,LA00D	; proceed if yes

; increment phoneme position and continue
LA007:	LD	HL,MEM66	; (mem66)
	INC	(HL)
	JR	L9FF2		; else next phoneme

LA00D:	BIT	0,(HL)		; Plosive ?
	JR	NZ,LA04B	; jump if yes

; In case of non plosive stop consonants,
; this will insert 2 special phoneme codes (code+1) and (code+2)
; after the current phoneme (code).
	LD	HL,MEM60	; Phoneme code to insert (mem60)
	INC	(HL)		; increment it
	LD	E,(HL)		; get it
	LD	HL,PHONEMENORMLEN; Phoneme normal lengths table
	ADD	HL,DE
	LD	A,(HL)		; get it
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC		; get stress value of current phoneme
	LD	A,(HL)
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	CALL	INSERT_PHONEME	; Insert phoneme (mem60,mem59,mem58) at position (mem57
	LD	HL,MEM60	; Phoneme code to insert (mem60)
	INC	(HL)		; increment it
	LD	E,(HL)		; get it
	LD	HL,PHONEMENORMLEN; Phoneme normal lengths table
	ADD	HL,DE
	LD	A,(HL)		; get it
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
LA040:	CALL	INSERT_PHONEME	; Insert phoneme (mem60,mem59,mem58) at position (mem57
	LD	HL,MEM66	; (mem66)
	INC	(HL)		; increment the position by 3
	INC	(HL)
	INC	(HL)
	JR	L9FF2		; continue

; Plosive stop consonants
LA04B:	INC	C		; point to next phoneme
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	A,(HL)		; get the code
	CP	00H		; pause ?
	JR	Z,LA04B		; ignore it and try again
	LD	(TEMP_CTR),A	; (mem56) := next phoneme code
	CP	0FFH		; end marker ?
	JR	Z,LA070		; proceed if yes
	LD	E,A
	LD	HL,PHONM_FLAGS1	; get the next phoneme flags 1
	ADD	HL,DE
	BIT	3,(HL)		; is <F1_UNKNOWN> set ? (mouth opening ?)
	JR	NZ,LA007	; continue if yes
	LD	A,(TEMP_CTR)	; recover next phoneme code (mem56)
	CP	24H		; '/H' ?
	JR	Z,LA007		; continue if yes
	CP	25H		; '/X' ?
	JR	Z,LA007		; continue if yes
LA070:	LD	HL,MEM66	; (mem66)
	LD	C,(HL)		; recover the current phoneme pointer
	LD	HL,STRESS	; Phoneme stress values buffer (L9de0_stress)
	ADD	HL,BC
	LD	A,(HL)		; get the stress value of the current phoneme
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	INC	C
	LD	A,C		; insertion point after current phoneme
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	LD	HL,MEM60	; Phoneme code to insert (mem60) := phoneme code + 1
	INC	(HL)
	LD	C,(HL)
	LD	HL,PHONEMENORMLEN; Phoneme normal lengths table
	ADD	HL,BC		;
	LD	A,(HL)		; get the additional phoneme 1 length
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	CALL	INSERT_PHONEME	; Insert phoneme (mem60,mem59,mem58) at position (mem57
	LD	HL,MEM57	; Phoneme insertion point (mem57)
	INC	(HL)		; bump insertion point
	INC	C		; bump phoneme code
	LD	A,C
	LD	(MEM60),A	; Phoneme code to insert (mem60) := phoneme code + 2
	LD	HL,PHONEMENORMLEN; Phoneme normal lengths table
	ADD	HL,BC
	LD	A,(HL)		; get the additional phoneme 1 length
	LD	(MEM59),A	; Phoneme length to insert (mem59)
	JR	LA040		; insert phoneme and continue


; Insert breath
INSERT_BREATH:
	XOR	A
	LD	(MEM66),A	; (mem66)
	LD	(LA6EB),A
	DEC	A
	LD	(LA6EA),A
	LD	B,00H
	LD	D,00H
	LD	HL,MEM66	; (mem66)
LA0B6:	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	E,(HL)
	LD	A,E
	CP	0FFH
	RET	Z
	LD	HL,PHONEMELENGTHS; Phoneme lengths buffer
	ADD	HL,BC
	LD	A,(HL)
	LD	HL,LA6EB
	ADD	A,(HL)
	LD	(HL),A
	CP	0E8H
	JR	NC,LA0EE
	LD	HL,PHONM_FLAGS2	; phoneme flags 2
	ADD	HL,DE
	BIT	0,(HL)
	JR	Z,LA0E0
	CALL	INSERT_PAUSE	; Insert pause after current phoneme (at position C)
	LD	HL,MEM66	; (mem66)
	INC	(HL)
	INC	(HL)
	JR	LA0B6

LA0E0:	XOR	A
	CP	E
	JR	NZ,LA0E8
	LD	A,C
	LD	(LA6EA),A
LA0E8:	LD	HL,MEM66	; (mem66)
	INC	(HL)
	JR	LA0B6

LA0EE:	LD	HL,LA6EA
	LD	C,(HL)
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	(HL),1FH
	INC	H
	LD	(HL),04H
	INC	H
	LD	(HL),00H
	CALL	INSERT_PAUSE	; Insert pause after current phoneme (at position C)
	INC	C
	LD	HL,MEM66	; (mem66)
	LD	(HL),C
	JR	LA0B6


; INSERT_PAUSE: Insert pause after current phoneme (at position C)
INSERT_PAUSE:
	INC	C
	LD	A,C
	LD	(MEM57),A	; Phoneme insertion point (mem57)
	XOR	A
	LD	(LA6EB),A
	LD	(MEM58),A	; Phoneme stress value to insert (mem58)
	LD	A,0FEH
	LD	(MEM60),A	; Phoneme code to insert (mem60)


; INSERT_PHONEME: Insert phoneme (mem60,mem59,mem58) at position (mem57)
INSERT_PHONEME:
	CALL	STO_ACE		; store A, C and E to memory
	LD	A,0C7H
	LD	C,A
	INC	A
	LD	E,A
LA121:	DEC	C
	DEC	E
	LD	IX,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	LD	IY,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	CALL	IXBC_TO_IYDE	; move byte from (IX+BC) to (IY+DE)
	LD	IX,PHONEMELENGTHS; Phoneme lengths buffer
	LD	IY,PHONEMELENGTHS; Phoneme lengths buffer
	CALL	IXBC_TO_IYDE	; move byte from (IX+BC) to (IY+DE)
	LD	IX,STRESS	; Phoneme stress values buffer (L9de0_stress)
	LD	IY,STRESS	; Phoneme stress values buffer (L9de0_stress)
	CALL	IXBC_TO_IYDE	; move byte from (IX+BC) to (IY+DE)
	LD	A,(MEM57)	; Phoneme insertion point (mem57)
	CP	C
	JR	NZ,LA121
	LD	HL,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	HL,BC
	LD	IX,MEM58	; Phoneme stress value to insert (mem58)
	LD	A,(IX+2)
	LD	(HL),A
	INC	H
	LD	A,(IX+1)
	LD	(HL),A
	INC	H
	LD	A,(IX+0)
	LD	(HL),A
	CALL	RCL_ACE		; recall A, C and E from memory
	RET


; STO_ACE: store A, C and E to memory
STO_ACE:
	LD	HL,MEM63	; Rule MSB (mem63)
	LD	(HL),A
	DEC	HL
	LD	(HL),C
	DEC	HL
	LD	(HL),E
	RET


; RCL_ACE: recall A, C and E from memory
RCL_ACE:
	LD	HL,MEM63	; Rule MSB (mem63)
	LD	A,(HL)
	DEC	HL
	LD	C,(HL)
	DEC	HL
	LD	E,(HL)
	RET


;==================================================================================================
;	V O I C E   O U T P U T
;==================================================================================================


; PREPAREOUTPUT: Prepare output (Sbda3_PrepareOutput)
;
; For each block of phonemes in the input table (separated by 0xFE),
; copy the phoneme ids, the lengths and the stress values to the
; output table, and render them to audio.
PREPAREOUTPUT:
	LD	BC,0000H
	LD	DE,0000H
L1_PREP_OUTPUT:	; Prepare output main phonemes loop
	LD	IX,PHONEMEINDEX	; Phonemes buffer (L9be0_phonemeIndex)
	ADD	IX,BC
	LD	A,(IX+0)	; get phoneme code
	CP	0FFH		; End of phonemes ?
	JR	NZ,LA197	; jump if not
	LD	IY,PHONINDEX_OUT; phonemes table for output (PhonemeIndexOutput)
	ADD	IY,DE		; put end marker in PhonemeIndexOutput
	LD	(IY+0),0FFH	;
	CALL	RENDER		; render the phonemes
	RET

LA197:	CP	0FEH		; Block separator ? (maybe unused...)
	JR	NZ,LA1AD	; jump if not
	INC	C		; next input
	LD	IY,PHONINDEX_OUT; phonemes table for output (PhonemeIndexOutput)
	ADD	IY,DE		; put end marker in PhonemeIndexOutput
	LD	(IY+0),0FFH
	CALL	RENDER		; render the phonemes
	LD	E,00H		; reset the PhonemeIndexOutput index
	JR	L1_PREP_OUTPUT	; continue

LA1AD:	CP	00H		; Null phoneme ?
	JR	NZ,LA1B4	; jump if not
	INC	C		; next input
	JR	L1_PREP_OUTPUT	; continue

LA1B4:	LD	IY,PHONINDEX_OUT; phonemes table for output (PhonemeIndexOutput)
	ADD	IY,DE		; copy phoneme code to PhonemeIndexOutput
	LD	(IY+0),A
	LD	IX,PHONEMELENGTHS; Phoneme lengths buffer
	LD	IY,PHONLENGTH_OUT; phoneme lengths table for output (phonemeLengthOutpu
	CALL	IXBC_TO_IYDE	; copy phoneme length
	LD	IX,STRESS	; Phoneme stress values buffer (L9de0_stress)
	LD	IY,STRESS_OUT	;stress output table for output (stressOutput)
	CALL	IXBC_TO_IYDE	; copy phoneme stress
	INC	C		; next input
	INC	E		; next output
	JR	L1_PREP_OUTPUT	; continue




; RENDER: RENDER THE PHONEMES IN THE LIST
;
; The phoneme list is converted into sound through the steps:
;
; 1. Copy each phoneme <length> number of times into the frames list,
;    where each frame represents 10 milliseconds of sound.
;
; 2. Determine the transitions lengths between phonemes, and linearly
;    interpolate the values across the frames.
;
; 3. Offset the pitches by the fundamental frequency.
;
; 4. Render the each frame.
;
;void Code47574()
RENDER:	; render the phoneme
	IF	LSDOS6 && EXEC
	  DI			; Disable interrupts for LS-DOS 6 executable
	ENDIF
	PUSH	BC		; save the table indexes
	PUSH	DE

	LD	A,(PHONINDEX_OUT);phonemes table for output (PhonemeIndexOutput)
	CP	0FFH		; end of table ?
	JP	Z,X_RENDER	; Jump if yes

	LD	BC,0000H	; init the phoneme index
	XOR	A
	LD	(PHONEME_INDEX),A; phoneme index (mem44)

	; CREATE FRAMES
	;
	; The length parameter in the list corresponds to the number of frames
	; to expand the phoneme to. Each frame represents 10 milliseconds of time.
	; So a phoneme with a length of 7 = 7 frames = 70 milliseconds duration.
	;
	; The parameters are copied from the phoneme to the frame verbatim.


	; pos47587:
L_CREATE_FRAMES:	; create frames loop
	LD	D,00H
	LD	A,(PHONEME_INDEX); phoneme index (mem44)
	LD	E,A
	LD	HL,PHONINDEX_OUT; phonemes table for output (PhonemeIndexOutput)
	ADD	HL,DE
	LD	A,(HL)
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
	CP	0FFH
	JP	Z,CREAT_TRNSTIONS; if terminal phoneme, exit the loop; create transitio
	CP	01H
	JP	Z,PERIOD_PHONEME; period phoneme *.
	CP	02H
	JP	Z,QMARK_PHONEME	; question mark phoneme?
LA207:	LD	HL,STRESS_OUT	; stress output table for output (stressOutput)
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	HL,PHONLENGTH_OUT; phoneme lengths table for output (phonemeLengthOutpu
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
	LD	D,00H
	LD	HL,PHASE1	; Phase1/stress amount index (mem43)

	; TODO: What is this?
	LD	E,(HL)
	INC	E
	LD	IX,STRESS_AMOUNTS; stress amount (add to pitch) (tab47492)
	ADD	IX,DE
	LD	A,(IX+0)
	LD	(HL),A
	LD	D,00H
	LD	A,(TEMP_CTR)	; Temporary/Counter (mem56)
	LD	E,A
COPY_FRAMES:	; copy from the source to the frames list
	LD	IX,FREQ1DATA
	LD	IY,FREQUENCY1	; Frequency 1 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,FREQ2DATA
	LD	IY,FREQUENCY2	; Frequency 2 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,FREQ3DATA
	LD	IY,FREQUENCY3	; Frequency 3 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,AMPL1DATA
	LD	IY,AMPLITUDE1	; Amplitude 1 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,AMPL2DATA
	LD	IY,AMPLITUDE2	; Amplitude 2 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,AMPL3DATA
	LD	IY,AMPLITUDE3	; Amplitude 3 frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	LD	IX,SAMPLEDCONSONAN; Looks like it's used as bit flags. High bits masked
	LD	IY,CONSONANTFLAG; Consonants flags frames
	CALL	IXDE_TO_IYBC	; move byte from (IX+DE) to (IY+BC)
	CALL	CALC_PITCH
	NOP
	NOP
	LD	HL,PITCH_CONTOUR;
	ADD	HL,BC
	LD	(HL),A
	INC	C
	LD	HL,PHASE2	; Phase2/number of frames to write (mem42)
	DEC	(HL)
	JR	NZ,COPY_FRAMES	; copy from the source to the frames list
	LD	HL,PHONEME_INDEX; phoneme index (mem44)
	INC	(HL)
	JP	NZ,L_CREATE_FRAMES; create frames loop

	; -------------------
	;pos47694:

	; CREATE TRANSITIONS
	;
	; Linear transitions are now created to smoothly connect the
	; end of one sustained portion of a phoneme to the following
	; phoneme.
	;
	; To do this, three tables are used:
	;
	;  Table         Purpose
	;  =========     ==================================================
	;  blendRank     Determines which phoneme's blend values are used.
	;
	;  blendOut      The number of frames at the end of the phoneme that
	;                will be used to transition to the following phoneme.
	;
	;  blendIn       The number of frames of the following phoneme that
	;                will be used to transition into that phoneme.
	;
	; In creating a transition between two phonemes, the phoneme
	; with the HIGHEST rank is used. Phonemes are ranked on how much
	; their identity is based on their transitions. For example,
	; vowels are and diphthongs are identified by their sustained portion,
	; rather than the transitions, so they are given low values. In contrast,
	; stop consonants (P, B, T, K) and glides (Y, L) are almost entirely
	; defined by their transitions, and are given high rank values.
	;
	; Here are the rankings used by SAM:
	;
	;     Rank    Type                         Phonemes
	;     2       All vowels                   IY, IH, etc.
	;     5       Diphthong endings            YX, WX, ER
	;     8       Terminal liquid consonants   LX, WX, YX, N, NX
	;     9       Liquid consonants            L, RX, W
	;     10      Glide                        R, OH
	;     11      Glide                        WH
	;     18      Voiceless fricatives         S, SH, F, TH
	;     20      Voiced fricatives            Z, ZH, V, DH
	;     23      Plosives, stop consonants    P, T, K, KX, DX, CH
	;     26      Stop consonants              J, GX, B, D, G
	;     27-29   Stop consonants (internal)   **
	;     30      Unvoiced consonants          /H, /X and Q*
	;     160     Nasal                        M
	;
	; To determine how many frames to use, the two phonemes are
	; compared using the blendRank[] table. The phoneme with the
	; higher rank is selected. In case of a tie, a blend of each is used:
	;
	;      if blendRank[phoneme1] ==  blendRank[phomneme2]
	;          ; use lengths from each phoneme
	;          outBlendFrames = outBlend[phoneme1]
	;          inBlendFrames = outBlend[phoneme2]
	;      else if blendRank[phoneme1] > blendRank[phoneme2]
	;          ; use lengths from first phoneme
	;          outBlendFrames = outBlendLength[phoneme1]
	;          inBlendFrames = inBlendLength[phoneme1]
	;      else
	;          ; use lengths from the second phoneme
	;          ; note that in and out are SWAPPED!
	;          outBlendFrames = inBlendLength[phoneme2]
	;          inBlendFrames = outBlendLength[phoneme2]
	;
	; Blend lengths can't be less than zero.
	;
	; Transitions are assumed to be symetrical, so if the transition
	; values for the second phoneme are used, the inBlendLength and
	; outBlendLength values are SWAPPED.
	;
	; For most of the parameters, SAM interpolates over the range of the last
	; outBlendFrames-1 and the first inBlendFrames.
	;
	; The exception to this is the Pitch[] parameter, which is interpolates the
	; pitch from the CENTER of the current phoneme to the CENTER of the next
	; phoneme.
	;
	; Here are two examples. First, For example, consider the word "SUN" (S AH N)
	;
	;    Phoneme   Duration    BlendWeight    OutBlendFrames    InBlendFrames
	;    S         2           18             1                 3
	;    AH        8           2              4                 4
	;    N         7           8              1                 2
	;
	; The formant transitions for the output frames are calculated as follows:
	;
	;     flags ampl1 freq1 ampl2 freq2 ampl3 freq3 pitch
	;    ------------------------------------------------
	; S
	;    241     0     6     0    73     0    99    61   Use S (weight 18) for transition instead of AH (weight 2)
	;    241     0     6     0    73     0    99    61   <-- (OutBlendFrames-1) = (1-1) = 0 frames
	; AH
	;      0     2    10     2    66     0    96    59 * <-- InBlendFrames = 3 frames
	;      0     4    14     3    59     0    93    57 *
	;      0     8    18     5    52     0    90    55 *
	;      0    15    22     9    44     1    87    53
	;      0    15    22     9    44     1    87    53
	;      0    15    22     9    44     1    87    53   Use N (weight 8) for transition instead of AH (weight 2).
	;      0    15    22     9    44     1    87    53   Since N is second phoneme, reverse the IN and OUT values.
	;      0    11    17     8    47     1    98    56 * <-- (InBlendFrames-1) = (2-1) = 1 frames
	; N
	;      0     8    12     6    50     1   109    58 * <-- OutBlendFrames = 1
	;      0     5     6     5    54     0   121    61
	;      0     5     6     5    54     0   121    61
	;      0     5     6     5    54     0   121    61
	;      0     5     6     5    54     0   121    61
	;      0     5     6     5    54     0   121    61
	;      0     5     6     5    54     0   121    61
	;
	; Now, consider the reverse "NUS" (N AH S):
	;
	;     flags ampl1 freq1 ampl2 freq2 ampl3 freq3 pitch
	;    ------------------------------------------------
	; N
	;     0     5     6     5    54     0   121    61
	;     0     5     6     5    54     0   121    61
	;     0     5     6     5    54     0   121    61
	;     0     5     6     5    54     0   121    61
	;     0     5     6     5    54     0   121    61
	;     0     5     6     5    54     0   121    61   Use N (weight 8) for transition instead of AH (weight 2)
	;     0     5     6     5    54     0   121    61   <-- (OutBlendFrames-1) = (1-1) = 0 frames
	; AH
	;     0     8    11     6    51     0   110    59 * <-- InBlendFrames = 2
	;     0    11    16     8    48     0    99    56 *
	;     0    15    22     9    44     1    87    53   Use S (weight 18) for transition instead of AH (weight 2)
	;     0    15    22     9    44     1    87    53   Since S is second phoneme, reverse the IN and OUT values.
	;     0     9    18     5    51     1    90    55 * <-- (InBlendFrames-1) = (3-1) = 2
	;     0     4    14     3    58     1    93    57 *
	; S
	;   241     2    10     2    65     1    96    59 * <-- OutBlendFrames = 1
	;   241     0     6     0    73     0    99    61

CREAT_TRNSTIONS:	;if terminal phoneme, exit the loop; create transitions
	LD	A,00H
	LD	(PHONEME_INDEX),A; phoneme index (mem44)
	LD	(MEM49),A	; (mem49)
	LD	BC,0000H
LA29E:	LD	IX,PHONINDEX_OUT; phonemes table for output (PhonemeIndexOutput)
	ADD	IX,BC
	LD	E,(IX+0)
	LD	D,00H
	INC	C
	LD	A,(IX+1)
	CP	0FFH
	JP	Z,LA418
	LD	C,A
	LD	HL,BLENDRANK	; Used to decide which phoneme's blend lengths. The can
	ADD	HL,BC
	LD	A,(HL)
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
	LD	IX,BLENDRANK	; Used to decide which phoneme's blend lengths. The can
	ADD	IX,DE
	LD	A,(IX+0)
	CP	(HL)
	JR	Z,LA2EF
	JR	C,LA2DC
	LD	HL,OUTBLENDLENGTH; Number of frames at the end of a phoneme devoted to
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	HL,INBLENDLENGTH; Number of frames at beginning of a phoneme devoted to
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
	JP	LA2FF

LA2DC:	LD	HL,INBLENDLENGTH; Number of frames at beginning of a phoneme devoted to
	ADD	HL,BC
	LD	A,(HL)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	HL,OUTBLENDLENGTH; Number of frames at the end of a phoneme devoted to
	ADD	HL,BC
	LD	A,(HL)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
	JP	LA2FF

LA2EF:	LD	HL,OUTBLENDLENGTH; Number of frames at the end of a phoneme devoted to
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	HL,OUTBLENDLENGTH; Number of frames at the end of a phoneme devoted to
	ADD	HL,BC
	LD	A,(HL)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
LA2FF:	LD	A,(PHONEME_INDEX); phoneme index (mem44)
	LD	D,00H
	LD	E,A
	LD	HL,PHONLENGTH_OUT; phoneme lengths table for output (phonemeLengthOutpu
	ADD	HL,DE
	LD	A,(MEM49)	; (mem49)
	ADD	A,(HL)
	LD	(MEM49),A	; (mem49)
	LD	HL,PHASE2	; Phase2/number of frames to write (mem42)
	ADD	A,(HL)
	LD	(SAMPLES_CTR),A	; samples counter (mem45)
	LD	HL,PITCH_CONTOUR;
	LD	(SAMPLES_PTR),HL; samples pointer (mem46)
	LD	A,(MEM49)	; (mem49)
	LD	HL,PHASE1	; Phase1/stress amount index (mem43)
	SUB	(HL)
	LD	(PHASE3),A	; Phase3 (mem41)
	LD	A,(PHASE2)	; Phase2/number of frames to write (mem42)
	ADD	A,(HL)
	LD	(LA6DA),A
	SUB	02H
	JP	M,LA410
LA333:	LD	A,(LA6DA)
	LD	(LA6DC),A
	LD	A,(SAMPLES_PTR_HI); samples pointer high byte (mem47)
	CP	HIGH PITCH_CONTOUR
	JR	NZ,LA39C
	LD	D,00H
	LD	A,(PHONEME_INDEX); phoneme index (mem44)
	LD	E,A
	LD	IX,PHONLENGTH_OUT; phoneme lengths table for output (phonemeLengthOutpu
	ADD	IX,DE
	LD	A,(IX+0)
	SRL	A
	LD	(LA6D8),A
	LD	A,(IX+1)
	SRL	A
	LD	HL,LA6D9
	LD	(HL),A
	LD	A,(LA6D8)
	ADD	A,(HL)
	LD	(LA6DC),A
	LD	A,(MEM49)	; (mem49)
	ADD	A,(HL)
	LD	(HL),A
	LD	D,00H
	LD	E,A
	LD	A,(MEM49)	; (mem49)
	LD	HL,LA6D8
	SUB	(HL)
	LD	(HL),A
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	A,(LA6D8)
	LD	E,A
	LD	A,(HL)
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	SUB	(HL)
	LD	(D_SAMPLE),A	; sample (mem53)
	LD	A,(LA6DC)
	LD	(D_DIVISOR),A	; divisor (mem52)
	CALL	SGND_DIV_8BIT	; Signed div @A6E9 by @A6E8 to @A6E9, rmdr to @A6E7, si
	LD	B,00H
	LD	A,(LA6DC)
	LD	C,A
	LD	D,00H
	LD	A,(LA6D8)
	LD	E,A
	JR	LA3C8

LA39C:	LD	D,00H
	LD	A,(SAMPLES_CTR)	; samples counter (mem45)
	LD	E,A
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	A,(PHASE3)	; Phase3 (mem41)
	LD	E,A
	LD	A,(HL)
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	SUB	(HL)
	LD	(D_SAMPLE),A	; sample (mem53)
	LD	A,(LA6DC)
	LD	(D_DIVISOR),A	; divisor (mem52)
	CALL	SGND_DIV_8BIT	; Signed div @A6E9 by @A6E8 to @A6E9, rmdr to @A6E7, si
	LD	B,00H
	LD	A,(LA6DC)
	LD	C,A
	LD	D,00H
	LD	A,(PHASE3)	; Phase3 (mem41)
	LD	E,A
LA3C8:	LD	A,00H
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
LA3CD:	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	A,(D_SAMPLE)	; sample (mem53)
	ADD	A,(HL)
	LD	(INFLEX_DIR),A	; Inflexion direction (mem48)
	INC	E
	DEC	C
	JP	Z,LA406
	LD	HL,TEMP_CTR	; Temporary/Counter (mem56)
	LD	A,(D_REMAINDER)	; remainder (mem51)
	ADD	A,(HL)
	LD	(HL),A
	LD	HL,LA6DC
	SUB	(HL)
	JR	C,LA3FC
	LD	(TEMP_CTR),A	; Temporary/Counter (mem56)
	LD	HL,INFLEX_DIR	; Inflexion direction (mem48)
	LD	A,(D_DIV_SIGN)	; dividend sign (mem50)
	BIT	7,A
	JR	NZ,LA3FB
	INC	(HL)
	JR	NZ,LA3FC
LA3FB:	DEC	(HL)
LA3FC:	LD	A,(INFLEX_DIR)	; Inflexion direction (mem48)
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	(HL),A
	JR	LA3CD

LA406:	LD	HL,SAMPLES_PTR_HI; samples pointer high byte (mem47)
	INC	(HL)
	LD	A,(HL)
	CP	HIGH CONSONANTFLAG; stop at consonant flags
	JP	NZ,LA333
LA410:	LD	HL,PHONEME_INDEX; phoneme index (mem44)
	INC	(HL)
	LD	C,(HL)
	JP	LA29E

LA418:	LD	D,00H
	LD	A,(PHONEME_INDEX); phoneme index (mem44)
	LD	E,A
	LD	A,(MEM49)	; (mem49)
	LD	HL,PHONLENGTH_OUT; phoneme lengths table for output (phonemeLengthOutpu
	ADD	HL,DE
	ADD	A,(HL)
	LD	(INFLEX_DIR),A	; Inflexion direction (mem48)
	CALL	ASSIGN_PITCH_CO	; Avoids monotone output (disabled for songs)

	LD	A,00H
	LD	(PHASE3),A	; Phase3 (mem41)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	(MEM49),A	; (mem49)
	CALL	LOAD_SPEED

	; RESCALE AMPLITUDE
	;
	; Rescale volume from a linear scale to decibels.
	;
	; amplitude rescaling

	; mem56 tracks how many amplitude tables are remaining for rescale
	LD	HL,AMPLITUDE1	; Amplitude 1 frames
	LD	B,00H
	LD	DE,0300H
LA455:	LD	C,(HL)
	LD	IX,AMPLITUDERESCAL
	ADD	IX,BC
	LD	A,(IX+0)
	LD	(HL),A
	INC	HL
	DEC	E
	JR	NZ,LA455
	DEC	D
	JR	NZ,LA455
	LD	A,(PITCH_CONTOUR);
	LD	(PHONEME_INDEX),A; phoneme index (mem44)
	LD	C,A
	SRL	C
	SRL	C
	SUB	C
	LD	(LA6DA),A
	JR	L_SOUNDOUT2	; vowel output loop


	; PROCESS THE FRAMES
	;
	; In traditional vocal synthesis, the glottal pulse drives filters, which
	; are attenuated to the frequencies of the formants.
	;
	; SAM generates these formants directly with sin and rectangular waves.
	; To simulate them being driven by the glottal pulse, the waveforms are
	; reset at the beginning of each glottal pulse.

	;finally the loop for sound output
	;pos48078:
LA478:	CALL	RENDER_SAMPLE	; Render a sampled sound from the sampleTable.

	; Skip ahead two in the phoneme buffer (2-letter codes)
	INC	E
	INC	E
	LD	HL,INFLEX_DIR	; Inflexion direction (mem48)
	DEC	(HL)
	DEC	(HL)
	JP	LA4DA

; CombineGlottalAndFormants?
L_SOUNDOUT2:	; vowel output loop
	LD	HL,CONSONANTFLAG; Consonants flags frames
	ADD	HL,DE
	LD	A,(HL)
	LD	(CUR_PHONM_INDEX),A; current phoneme index (mem39)
	AND	0F8H
	JR	NZ,LA478
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	LD	A,E
	EXX
	LD	L,A
	LD	D,HIGH MULTTABLE
	LD	H,00H
	LD	BC,AMPLITUDE1	; Amplitude 1 frames
	ADD	HL,BC
	LD	A,(PHASE1)	; Phase1/stress amount index (mem43)
	LD	C,A
	LD	B,HIGH SINUS
	LD	A,(BC)		; get sinus(phase1)
	OR	(HL)		; get amplitude1(phase1)
	LD	E,A
	LD	A,(DE)		; get multtable
	EXX
	LD	B,A
	EXX
	LD	A,(PHASE2)	; Phase2/number of frames to write (mem42)
	LD	C,A
	LD	A,(BC)		; get sinus(phase2)
	INC	H		; bump to amplitude2 table
	OR	(HL)		; get amplitude2(phase2)
	LD	E,A
	LD	A,(DE)		; get multtable
	EXX
	LD	C,A
	EXX
	LD	A,(PHASE3)	; Phase3 (mem41)
	LD	C,A
	INC	B		; bump to rectangle table
	LD	A,(BC)		; get rectangle(phase3)
	INC	H		; bump to amplitude3 table
	OR	(HL)		; get amplitude3(phase3)
	LD	E,A
	LD	A,(DE)		; get multtable
	EXX
	ADD	A,B		; sum the 3 formants
	ADD	A,C
	ADD	A,DACOFFS

	OUT	(DACPORT),A	; output the wave
	LD	HL,SAMPLES_CTR	; samples counter (mem45)
	DEC	(HL)
	JR	NZ,LA4E1
	NOP
	NOP
	INC	E
	LD	HL,INFLEX_DIR	; Inflexion direction (mem48)
	DEC	(HL)
LA4DA:	JR	Z,X_RENDER
	CALL	LOAD_SPEED
LA4E1:	LD	HL,PHONEME_INDEX; phoneme index (mem44) - glottal pulse counter
	DEC	(HL)		;
	JR	NZ,LA50B	; jump if end of glottal pulse reached
	; end of glottal pulse => next pulse; reset formants' phases
	CALL	DELAY0		;  27 cycles (incl CALL)
LA4EA:	LD	HL,PITCH_CONTOUR;
	ADD	HL,DE
	LD	A,(HL)
	LD	(PHONEME_INDEX),A; phoneme index (mem44) - glottal pulse counter
	LD	C,A		; A := A*3/4
	SRL	C
	SRL	C
	SUB	C
	LD	(LA6DA),A	; mem38 = 3/4 * length of glottal pulse
	; reset the phase of the formants to match the pulse
	XOR	A		; clear:
	LD	(PHASE3),A	; Phase3 (mem41)
	LD	(PHASE2),A	; Phase2/number of frames to write (mem42)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	JP	L_SOUNDOUT2	; vowel output loop

X_RENDER:
	POP	DE		; restore the table indexes
	POP	BC
	IF	LSDOS6 && EXEC
	  EI			; Re-enable interrupts for LS-DOS 6 executable
	ENDIF
	RET

	; the glottal pulse is not completed
LA50B:
	; Within the first 75% of the glottal pulse?
	LD	HL,LA6DA	; decrease the 75% counter
	DEC	(HL)
	JR	NZ,LA51E	; jump if 75% not reached

	; Is the count is non-zero and the sampled flag is zero?
	LD	A,(CUR_PHONM_INDEX); current phoneme index (mem39)
	CP	00H
	JR	Z,LA51E		; interleave the sample if needed
	CALL	RENDER_SAMPLE	; Render a sampled sound from the sampleTable.
	JP	LA4EA		; glottal pulse completed

LA51E:
	; Update the 3 phases using the frequencies
	CALL	DELAY0		;  27 cycles (incl CALL)
	LD	HL,FREQUENCY1	; Frequency 1 frames
	ADD	HL,DE
	LD	BC,PHASE1	; Phase1/stress amount index (mem43)
	LD	A,(BC)
	ADD	A,(HL)
	LD	(BC),A
	DEC	BC		; phase2 (mem42)
	INC	H		; Frequency 2
	LD	A,(BC)
	ADD	A,(HL)
	LD	(BC),A
	DEC	BC		; phase3 (mem41)
	INC	H		; Frequency 3
	LD	A,(BC)
	ADD	A,(HL)
	LD	(BC),A
	JP	L_SOUNDOUT2	; vowel output loop

; PERIOD_PHONEME: Create a falling inflexion for statement endings.
PERIOD_PHONEME:	; period phoneme *.
	LD	A,01H
	JR	ADD_INFLEXION	; Create a rising or falling inflection 30 frames prior

; QMARK_PHONEME: Create a falling inflexion for questions.
QMARK_PHONEME:	; question mark phoneme?
	LD	A,0FFH


; ADD_INFLEXION: Create a rising or falling inflection 30 frames prior to
; index X. A rising inflection is used for questions, and a falling
; inflection is used for statements.
ADD_INFLEXION:
	LD	(INFLEX_DIR),A	; Inflexion direction (mem48)
	LD	A,C
	LD	(MEM49),A	; (mem49)
	SUB	1EH		; < 30 ?
	JR	NC,LA54B
	LD	A,00H
LA54B:	LD	C,A

	; Advance start position while pitches[X] == $7f
LA54C:	LD	IX,PITCH_CONTOUR;
	ADD	IX,BC
	LD	A,(IX+0)
	CP	7FH
	JR	NZ,LA55C
	INC	C
	JR	LA54C

LA55C:	LD	HL,INFLEX_DIR	; Inflexion direction (mem48)
	ADD	A,(HL)
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	IX,PITCH_CONTOUR;
	ADD	IX,BC
	LD	(IX+0),A
LA56C:	INC	C
	LD	A,C
	LD	HL,MEM49	; (mem49)
	CP	(HL)
	JP	Z,LA207
	LD	IX,PITCH_CONTOUR;
	ADD	IX,BC
	LD	A,(IX+0)
	CP	0FFH
	JR	Z,LA56C
	LD	A,(PHASE1)	; Phase1/stress amount index (mem43)
	JR	LA55C



; -------------------------------------------------------------------------
; RENDER_SAMPLE: Render a sampled sound from the sampleTable.
;
;   Phoneme   Sample Start   Sample End
;   32: S*    15             255
;   33: SH    257            511
;   34: F*    559            767
;   35: TH    583            767
;   36: /H    903            1023
;   37: /X    1135           1279
;   38: Z*    84             119
;   39: ZH    340            375
;   40: V*    596            639
;   41: DH    596            631
;
;   42: CH
;   43: CH+   399            511
;
;   44: J*
;   45: J+    257            276
;   46: ** (?)
;
;   66: P*
;   67: P*'   743            767
;   68: P*"
;
;   69: T*
;   70: T*'   231            255
;   71: T*"
;
; The SampledPhonemesTable[] holds flags indicating if a phoneme is
; voiced or not. If the upper 5 bits are zero, the sample is voiced.
;
; Samples in the sampleTable are compressed, with bits being converted to
; bytes from high bit to low, as follows:
;
;   unvoiced 0 bit   -> X
;   unvoiced 1 bit   -> 5
;
;   voiced 0 bit     -> 6
;   voiced 1 bit     -> 24
;
; Where X is a value from the table:
;
;   { 0x18, 0x1A, 0x17, 0x17, 0x17 };
;
; The index into this table is determined by masking off the lower
; 3 bits from the SampledPhonemesTable:
;
;        index = (SampledPhonemesTable[i] & 7) - 1;
;
; For voices samples, samples are interleaved between voiced output.


; Code48227()
RENDER_SAMPLE:	; Render a sampled sound from the sampleTable.
	EXX
	XOR	A
	LD	D,A
	LD	B,A
	LD	A,(CUR_PHONM_INDEX); current phoneme index (mem39)
  	; mask low three bits and subtract 1 get value to
	; convert 0 bits on unvoiced samples.
	LD	E,A
	AND	07H
	DEC	A
	PUSH	AF
	LD	C,A

	; determine which offset to use from table { 0x18, 0x1A, 0x17, 0x17, 0x17 }
	; T, S, Z                0          0x18
	; CH, J, SH, ZH          1          0x1A
	; P, F*, V, TH, DH       2          0x17
	; /H                     3          0x17
	; /X                     4          0x17
	LD	HL,CONSONANT_TAB; table { 0x18, 0x1A, 0x17, 0x17, 0x17 } (tab48426)
	ADD	HL,BC
	LD	A,(HL)
	LD	(D_SAMPLE),A	; sample (mem53)
	POP	AF
	ADD	A,HIGH SAMPLETABLE_000
	LD	(SAMPLES_PTR_HI),A; samples pointer high byte (mem47)
	LD	A,LOW  SAMPLETABLE_000
	LD	(SAMPLES_PTR),A	; samples pointer (mem46)
	; If the upper 5 bits are non-zero, the sample is unvoiced
	LD	A,E
	AND	0F8H
	JR	Z,VOICED_PHONEME; voiced portion
	; Else the sample is voiced: Z*, ZH, V*, DH
	CALL	DELAY0		;  27 cycles (incl CALL)
	CPL
	LD	E,A
L1_VOICELESS:	; Voiceless consonants outer loop (256-E bytes)
	LD	B,08H
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	C,(HL)
L2_VOICELESS:	; Voiceless consonants inner loop (8 bits)
	SLA	C
	JR	NC,LA5C4
	LD	A,(D_SAMPLE)	; sample (mem53)
	OUT	(DACPORT),A
	JP	LA5CB

LA5C4:	LD	A,DACOFFS-2CH
	OUT	(DACPORT),A
	NOP
	NOP
	NOP
LA5CB:	CALL	DELAY1		; 165 cycles (incl CALL)
	DEC	B
	JR	NZ,L2_VOICELESS	; Voiceless consonants inner loop (8 bits)
	NOP
	NOP
	INC	E
	JR	NZ,L1_VOICELESS	; Voiceless consonants outer loop (256-E bytes)
	LD	A,01H
	LD	(PHONEME_INDEX),A; phoneme index (mem44)
	EXX
	RET

VOICED_PHONEME:	; voiced portion
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	EXX
	LD	HL,PITCH_CONTOUR;
	ADD	HL,DE
	LD	A,(HL)
	EXX
	SRL	A
	SRL	A
	SRL	A
	SRL	A
	CPL
	LD	(PHASE1),A	; Phase1/stress amount index (mem43)
	LD	A,(MEM66)	; (mem66)
	LD	E,A
L1_VOICED:	; Voiced consonants outer loop (256-E bytes)
	LD	B,08H
	LD	HL,(SAMPLES_PTR); samples pointer (mem46)
	ADD	HL,DE
	LD	C,(HL)
L2_VOICED:	; Voiced consonants inner loop (8 bits)
	SLA	C
	JR	NC,LA60B
	LD	A,DACOFFS+1AH
	OUT	(DACPORT),A
	JR	LA611

LA60B:	LD	A,DACOFFS-1CH
	OUT	(DACPORT),A
	NOP
	NOP
LA611:	CALL	DELAY2		; 155 cycles (incl CALL)
	DEC	B
	JR	NZ,L2_VOICED	; Voiced consonants inner loop (8 bits)
	NOP
	INC	E
	LD	HL,PHASE1	; Phase1/stress amount index (mem43)
	INC	(HL)
	JR	NZ,L1_VOICED	; Voiced consonants outer loop (256-E bytes)
	LD	A,01H
	LD	(PHONEME_INDEX),A; phoneme index (mem44)
	LD	A,E
	LD	(MEM66),A	; (mem66)
	EXX
	RET

IXDE_TO_IYBC:	; move byte from (IX+DE) to (IY+BC)
	ADD	IX,DE
	ADD	IY,BC
	LD	A,(IX+0)
	LD	(IY+0),A
	RET

IXBC_TO_IYDE:	; move byte from (IX+BC) to (IY+DE)
	ADD	IX,BC
	ADD	IY,DE
	LD	A,(IX+0)
	LD	(IY+0),A
	RET

DELAY1:	; 165 cycles (incl CALL)
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
DELAY0:	;  27 cycles (incl CALL)
	RET

DELAY2:	; 155 cycles (incl CALL)
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
	EX	(SP),IX
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	RET

SGND_DIV_8BIT:	; Signed div @A6E9 by @A6E8 to @A6E9, rmdr to @A6E7, sign to
	LD	E,00H
	LD	HL,D_SAMPLE	; sample (mem53)
	LD	IX,D_DIVISOR	; divisor (mem52)
	BIT	7,(HL)
	JR	Z,LA672
	LD	A,00H
	SUB	(HL)
	LD	(HL),A
	LD	E,80H
LA672:	LD	A,E
	LD	(D_DIV_SIGN),A	; dividend sign (mem50)
	LD	A,00H
	LD	C,08H
LA67A:	SLA	(HL)
	RLA
	CP	(IX+0)
	JR	C,LA686
	SUB	(IX+0)
	INC	(HL)
LA686:	DEC	C
	JR	NZ,LA67A
	LD	(D_REMAINDER),A	; remainder (mem51)
	LD	A,(D_DIV_SIGN)	; dividend sign (mem50)
	BIT	7,A
	JR	Z,LA697
	LD	A,00H
	SUB	(HL)
	LD	(HL),A
LA697:	RET

PLAY_BELL:	; Send <BEL> to console
	IF	CPM
	  LD	E,07H
	  LD	C,02H
	  JP	BDOS
	ENDIF
	IF	LSDOS6
	  LD	B,0
	  LD	A,@SOUND
	  JP	DRVSVC
	ENDIF

SETHIGHBIT:	; put "1" in bit 7 of all bytes in (HL), for "C" bytes
	SET	7,(HL)
	INC	HL
	DEC	C
	JR	NZ,SETHIGHBIT	; put "1" in bit 7 of all bytes in (HL), for "C" bytes
	RET


;==================================================================================================
;	V A R I A B L E S
;==================================================================================================

	DB	00H
LA6D8:	DB	00H
LA6D9:	DB	00H
LA6DA:	DB	00H
CUR_PHONM_INDEX:	; current phoneme index (mem39)
	DB	00H
LA6DC:	DB	00H
PHASE3:	; Phase3 (mem41)
	DB	00H
PHASE2:	; Phase2/number of frames to write (mem42)
	DB	00H
PHASE1:	; Phase1/stress amount index (mem43)
	DB	00H
PHONEME_INDEX:	; phoneme index (mem44)
	DB	00H
SAMPLES_CTR:	; samples counter (mem45)
	DB	00H
SAMPLES_PTR:	; samples pointer (mem46)
	DW	0000H
SAMPLES_PTR_HI:	EQU	$-1
INFLEX_DIR:	; Inflexion direction (mem48)
	DB	00H
MEM49:	; (mem49)
	DB	00H
D_DIV_SIGN:	; dividend sign (mem50)
	DB	00H
D_REMAINDER:	; remainder (mem51)
	DB	00H
D_DIVISOR:	; divisor (mem52)
	DB	00H
D_SAMPLE:	; sample (mem53)
	DB	00H
LA6EA:	DB	00H
LA6EB:	DB	00H
TEMP_CTR:	; Temporary/Counter (mem56)
	DB	00H
MEM57:	; Phoneme insertion point (mem57)
	DB	00H
MEM58:	; Phoneme stress value to insert (mem58)
	DB	00H
MEM59:	; Phoneme length to insert (mem59)
	DB	00H
MEM60:	; Phoneme code to insert (mem60)
	DB	00H
MEM61:	; (mem61)
	DB	00H
MEM62:	; Rule LSB or word (mem62)
	DW	0000H
MEM63:	EQU	$-1
MEM64:	; (mem64_sign2)
	DB	00H
MEM65:	; (mem65_sign1)
	DB	00H
MEM66:	; (mem66)
	DB	00H
ERROR_POS:	; Error position
	DB	00H
LA6F8:	DB	00H
INPUT_BUFFER_LEN:	DB	00H		; Command line/Input buffer length


;==================================================================================================
;	D A T A   T A B L E S
;==================================================================================================


; Character flags (ASCII 00H-5FH)
CF_NONE		EQU	00H	; No flag set
CF_DIGIT	EQU	01H	; Digits: '0' .. '9'
CF_NAMEDCHAR	EQU	02H	; Named characters: digits and punctuation
CF_INFLONGU	EQU	04H	; Consonant influencing long 'U'
CF_VOICED	EQU	08H	; Voiced consonant
CF_SIBILANT	EQU	10H	; Sibilant
CF_CONSONANT	EQU	20H+80H	; Consonant (with letter flag)
CF_VOWEL	EQU	40H+80H	; Vowel (with letter flag)
CF_LETTERAPOS	EQU	80H	; Letters or apostrophe

CHARFLAGS:	; English characters flags (tab36376)
	DC	32,CF_NONE					; Control characters
	DB	CF_NONE						; ' '
	DC	6,CF_NAMEDCHAR					; '!', '"', '#", "$", '%', '&'
	DB	CF_LETTERAPOS+CF_NAMEDCHAR			; Apostrophe '
	DC	2,CF_NONE					; '(', ')'
	DC	6,CF_NAMEDCHAR					; '*', '+', ',', '-', '.', '/'
	DC	10,CF_NAMEDCHAR+CF_DIGIT			; '0' .. '9'
	DC	6,CF_NAMEDCHAR					; ':', ';', '<', '=', '>', '?'
	DB	CF_NAMEDCHAR					; '@'
	DB	CF_VOWEL					; 'A'
	DB	CF_CONSONANT+CF_VOICED				; 'B'
	DB	CF_CONSONANT+CF_SIBILANT			; 'C'
	DB	CF_CONSONANT+CF_VOICED+CF_INFLONGU		; 'D'
	DB	CF_VOWEL					; 'E'
	DB	CF_CONSONANT					; 'F'
	DB	CF_CONSONANT+CF_SIBILANT+CF_VOICED		; 'G'
	DB	CF_CONSONANT					; 'H'
	DB	CF_VOWEL					; 'I'
	DB	CF_CONSONANT+CF_SIBILANT+CF_VOICED+CF_INFLONGU	; 'J'
	DB	CF_CONSONANT					; 'K'
	DB	CF_CONSONANT+CF_VOICED+CF_INFLONGU		; 'L'
	DB	CF_CONSONANT+CF_VOICED				; 'M'
	DB	CF_CONSONANT+CF_VOICED+CF_INFLONGU		; 'N'
	DB	CF_VOWEL					; 'O'
	DB	CF_CONSONANT					; 'P'
	DB	CF_CONSONANT					; 'Q'
	DB	CF_CONSONANT+CF_VOICED+CF_INFLONGU		; 'R'
	DB	CF_CONSONANT+CF_SIBILANT+CF_INFLONGU		; 'S'
	DB	CF_CONSONANT+CF_INFLONGU			; 'T'
	DB	CF_VOWEL					; 'U'
	DB	CF_CONSONANT+CF_VOICED				; 'V'
	DB	CF_CONSONANT+CF_VOICED				; 'W'
	DB	CF_CONSONANT+CF_SIBILANT			; 'X'
	DB	CF_VOWEL					; 'Y'
	DB	CF_CONSONANT+CF_SIBILANT+CF_VOICED+CF_INFLONGU	; 'Z'
	DC	3,CF_NONE					; '[', '\', ']'
	DB	CF_NAMEDCHAR					; '^'
	DB	CF_NONE						; '_'

RULESPTR_LSB:	; Letters rules pointers LSB table
	DB	LOW RULES_A,LOW RULES_B,LOW RULES_C,LOW RULES_D
	DB	LOW RULES_E,LOW RULES_F,LOW RULES_G,LOW RULES_H
	DB	LOW RULES_I,LOW RULES_J,LOW RULES_K,LOW RULES_L
	DB	LOW RULES_M,LOW RULES_N,LOW RULES_O,LOW RULES_P
	DB	LOW RULES_Q,LOW RULES_R,LOW RULES_S,LOW RULES_T
	DB	LOW RULES_U,LOW RULES_V,LOW RULES_W,LOW RULES_X
	DB	LOW RULES_Y,LOW RULES_Z

RULESPTR_MSB:	; Letters rules pointers MSB table
	DB	HIGH RULES_A,HIGH RULES_B,HIGH RULES_C,HIGH RULES_D
	DB	HIGH RULES_E,HIGH RULES_F,HIGH RULES_G,HIGH RULES_H
	DB	HIGH RULES_I,HIGH RULES_J,HIGH RULES_K,HIGH RULES_L
	DB	HIGH RULES_M,HIGH RULES_N,HIGH RULES_O,HIGH RULES_P
	DB	HIGH RULES_Q,HIGH RULES_R,HIGH RULES_S,HIGH RULES_T
	DB	HIGH RULES_U,HIGH RULES_V,HIGH RULES_W,HIGH RULES_X
	DB	HIGH RULES_Y,HIGH RULES_Z

RULESPTR_END:	; Letters rules pointers table end

RULES2:	; Rules2: digits and symbols
	DEFRULE	"(A)="
	DEFRULE	"(!)=."
	DEFRULE	"(_) =-AH5NKWOWT- "
	DEFRULE	"(_)=KWOW4T-"
	DEFRULE	"(#)= NAH4MBER"
	DEFRULE	"($)= DAA4LER"
	DEFRULE	"(%)= PERSEH4NT"
	DEFRULE	"(&)= AEND"
	DEFRULE	"(|)="
	DEFRULE	"(*)= AE4STERIHSK"
	DEFRULE	"(+)= PLAH4S"
	DEFRULE	"(,)=,"
	DEFRULE	" (-) =-"
	DEFRULE	"(-)="
	DEFRULE	"(.)= POYNT"
	DEFRULE	"(/)= SLAE4SH"
	DEFRULE	"(0)= ZIY4ROW"
	DEFRULE	" (1ST)=FER4ST"
	DEFRULE	" (10TH)=TEH4NTH"
	DEFRULE	"(1)= WAH4N"
	DEFRULE	" (2ND)=SEH4KUND"
	DEFRULE	"(2)= TUW4"
	DEFRULE	" (3RD)=THER4D"
	DEFRULE	"(3)= THRIY4"
	DEFRULE	"(4)= FOH4R"
	DEFRULE	" (5TH)=FIH4FTH"
	DEFRULE	"(5)= FAY4V"
	DEFRULE	"(6)= SIH4KS"
	DEFRULE	"(7)= SEH4VUN"
	DEFRULE	" (8TH)=EY4TH"
	DEFRULE	"(8)= EY4T"
	DEFRULE	"(9)= NAY4N"
	DEFRULE	"(:)=. "
	DEFRULE	"(;)=."
	DEFRULE	"(<)= LEH4S DHAEN"
	DEFRULE	"(=)= IY4KWULZ"
	DEFRULE	"(>)= GREY4TER DHAEN"
	IF	FIXQMRK
	DEFRULE	"(?)=?"
	ELSE
	DEFRULE	"(?)=."
	ENDIF
	DEFRULE	"(@)= AE6T"
	DEFRULE	"(^)= KAE4RIXT"

; From: https://apps.dtic.mil/sti/pdfs/ADA021929.pdf
;
;   #   One or more vowels (A, E, I, O, U, Y)
;   .   One of B, D, V, G, J, L, M, N, R, W, and Z: a voiced consonant
;   %   One of (ER, E, ES, ED, ING, ELY): a suffix
;   &   One of (S, C, G, Z, X, J, CH, SH): a sibilant
;   @   One of (T, S, R, D, L, Z, N, J, TH, CH, SH): a consonant influencing the sound of a
;       following long u (cf. rule and mule)
;   ^   One consonant (B, C, D, F, G, H, J, K, L, M, N, P, Q, R, S, T, V, W, X, Z)
;   +   One of (E, I, Y): a front vowel
;   :   Zero or more consonants
;
; Note: '*' and '$' from paper are not used/implemented by SAM.

RULES_A:	; Rules for 'A'
	DEFRULE	"]A"
	DEFRULE	" (A.)=EH4Y. "
	DEFRULE	"(A) =AH "
	DEFRULE	" (ARE) =AAR"
	DEFRULE	" (AR)O=AXR"
	DEFRULE	"(AR)#=EH4R "
	DEFRULE	" ^(AS)#=EY4S"
	DEFRULE	"(A)WA=AX"
	DEFRULE	"(AW)=AO5 "
	DEFRULE	" :(ANY)=EH4NIY"
	DEFRULE	"(A)^+#=EY5 "
	DEFRULE	"#:(ALLY)=ULIY"
	DEFRULE	" (AL)#=UL"
	DEFRULE	"(AGAIN)=AXGEH4N"
	DEFRULE	"#:(AG)E=IHJ"
	DEFRULE	"(A)^%=EY"
	DEFRULE	"(A)^+:#=AE"
	DEFRULE	" :(A)^+ =EY4 "
	DEFRULE	" (ARR)=AXR"
	DEFRULE	"(ARR)=AE4R"
	DEFRULE	" ^(AR) =AA5R"
	DEFRULE	"(AR)=AA5R"
	DEFRULE	"(AIR)=EH4R"
	DEFRULE	"(AI)=EY4 "
	DEFRULE	"(AY)=EY5 "
	DEFRULE	"(AU)=AO4 "
	DEFRULE	"#:(AL) =UL"
	DEFRULE	"#:(ALS) =ULZ"
	DEFRULE	"(ALK)=AO4K"
	DEFRULE	"(AL)^=AOL"
	DEFRULE	" :(ABLE)=EY4BUL"
	DEFRULE	"(ABLE)=AXBUL"
	DEFRULE	"(A)VO=EY4"
	DEFRULE	"(ANG)+=EY4NJ"
	DEFRULE	"(ATARI)=AHTAA4RIY"
	DEFRULE	"(A)TOM=AE"
	DEFRULE	"(A)TTI=AE"
	DEFRULE	" (AT) =AET"
	DEFRULE	" (A)T=AH"
	DEFRULE	"(A)=AE"

RULES_B:	; Rules for 'B'
	DEFRULE	"]B"
	DEFRULE	" (B) =BIY4"
	DEFRULE	" (BE)^#=BIH"
	DEFRULE	"(BEING)=BIY4IHNX"
	DEFRULE	" (BOTH) =BOW4TH"
	DEFRULE	" (BUS)#=BIH4Z "
	DEFRULE	"(BREAK)=BREY5K"
	DEFRULE	"(BUIL)=BIH4L"
	DEFRULE	"(B)=B"

RULES_C:	; Rules for 'C'
	DEFRULE	"]C"
	DEFRULE	" (C) =SIY4"
	DEFRULE	" (CH)^=K"
	DEFRULE	"^E(CH)=K"
	DEFRULE	"(CHA)R#=KEH5"
	DEFRULE	"(CH)=CH"
	DEFRULE	" S(CI)#=SAY4 "
	DEFRULE	"(CI)A=SH"
	DEFRULE	"(CI)O=SH"
	DEFRULE	"(CI)EN=SH"
	DEFRULE	"(CITY)=SIHTIY"
	DEFRULE	"(C)+=S"
	DEFRULE	"(CK)=K"
	DEFRULE	"(COM)=KAHM"
	DEFRULE	"(CUIT)=KIHT"
	DEFRULE	"(CREA)=KRIYEY"
	DEFRULE	"(C)=K"

RULES_D:	; Rules for 'D'
	DEFRULE	"]D"
	DEFRULE	" (D) =DIY4"
	DEFRULE	" (DR.) =DAA4KTER"
	DEFRULE	"#:(DED) =DIHD"
	DEFRULE	".E(D) =D"
	DEFRULE	"#:^E(D) =T"
	DEFRULE	" (DE)^#=DIH"
	DEFRULE	" (DO) =DUW"
	DEFRULE	" (DOES)=DAHZ"
	DEFRULE	"(DONE) =DAH5N"
	DEFRULE	"(DOING)=DUW4IHNX"
	DEFRULE	" (DOW)=DAW"
	DEFRULE	"#(DU)A=JUW"
	DEFRULE	"#(DU)^#=JAX "
	DEFRULE	"(D)=D"

RULES_E:	; Rules for 'E'
	DEFRULE	"]E"
	DEFRULE	" (E) =IYIY4"
	DEFRULE	"#:(E) ="
	DEFRULE	"|:^(E) ="
	DEFRULE	" :(E) =IY"
	DEFRULE	"#(ED) =D"
	DEFRULE	"#:(E)D ="
	DEFRULE	"(EV)ER=EH4V"
	DEFRULE	"(E)^%=IY4 "
	DEFRULE	"(ERI)#=IY4RIY"
	DEFRULE	"(ERI)=EH4RIH"
	DEFRULE	"#:(ER)#=ER"
	DEFRULE	"(ERROR)=EH4ROHR"
	DEFRULE	"(ERASE)=IHREY5S "
	DEFRULE	"(ER)#=EHR"
	DEFRULE	"(ER)=ER"
	DEFRULE	" (EVEN)=IYVEHN"
	DEFRULE	"#:(E)W="
	DEFRULE	"@(EW)=UW"
	DEFRULE	"(EW)=YUW"
	DEFRULE	"(E)O=IY"
	DEFRULE	"#:&(ES) =IHZ"
	DEFRULE	"#:(E)S ="
	DEFRULE	"#:(ELY) =LIY"
	DEFRULE	"#:(EMENT)=MEHNT"
	DEFRULE	"(EFUL)=FUHL"
	DEFRULE	"(EE)=IY4 "
	DEFRULE	"(EARN)=ER5N"
	DEFRULE	" (EAR)^=ER5 "
	DEFRULE	"(EAD)=EHD"
	DEFRULE	"#:(EA) =IYAX"
	DEFRULE	"(EA)SU=EH5 "
	DEFRULE	"(EA)=IY5 "
	DEFRULE	"(EIGH)=EY4 "
	DEFRULE	"(EI)=IY4 "
	DEFRULE	" (EYE)=AY4 "
	DEFRULE	"(EY)=IY"
	DEFRULE	"(EU)=YUW5 "
	DEFRULE	"(EQUAL)=IY4KWUL"
	DEFRULE	"(E)=EH"

RULES_F:	; Rules for 'F'
	DEFRULE	"]F"
	DEFRULE	" (F) =EH4F"
	DEFRULE	"(FUL)=FUHL"
	DEFRULE	"(FRIEND)=FREH5ND"
	DEFRULE	"(FATHER)=FAA4DHER"
	DEFRULE	"(F)F="
	DEFRULE	"(F)=F"

RULES_G:	; Rules for 'G'
	DEFRULE	"]G"
	DEFRULE	" (G) =JIY4"
	DEFRULE	"(GIV)=GIH5V"
	DEFRULE	" (G)I^=G"
	DEFRULE	"(GE)T=GEH5 "
	DEFRULE	"SU(GGES)=GJEH4S "
	DEFRULE	"(GG)=G"
	DEFRULE	" B#(G)=G"
	DEFRULE	"(G)+=J"
	DEFRULE	"(GREAT)=GREY4T "
	DEFRULE	"(GON)E=GAO5N"
	DEFRULE	"#(GH)="
	DEFRULE	" (GN)=N"
	DEFRULE	"(G)=G"

RULES_H:	; Rules for 'H'
	DEFRULE	"]H"
	DEFRULE	" (H) =EY4CH"
	DEFRULE	" (HAV)=/HAE6V"
	DEFRULE	" (HERE)=/HIYR"
	DEFRULE	" (HOUR)=AW5ER"
	DEFRULE	"(HOW)=/HAW"
	DEFRULE	"(H)#=/H"
	DEFRULE	"(H)="

RULES_I:	; Rules for 'I'
	DEFRULE	"]I"
	DEFRULE	" (IN)=IHN"
	DEFRULE	" (I) =AY4 "
	DEFRULE	"(I) =AY"
	DEFRULE	"(IN)D=AY5N"
	DEFRULE	"SEM(I)=IY "
	DEFRULE	" ANT(I)=AY"
	DEFRULE	"(IER)=IYER"
	DEFRULE	"#:R(IED) =IYD"
	DEFRULE	"(IED) =AY5D"
	DEFRULE	"(IEN)=IYEHN"
	DEFRULE	"(IE)T=AY4EH"
	DEFRULE	"(I|)=AY5"
	DEFRULE	" :(I)^%=AY5 "
	DEFRULE	" :(IE) =AY4"
	DEFRULE	"(I)%=IY"
	DEFRULE	"(IE)=IY4 "
	DEFRULE	" (IDEA)=AYDIY5AH"
	DEFRULE	"(I)^+:#=IH"
	DEFRULE	"(IR)#=AYR"
	DEFRULE	"(IZ)%=AYZ"
	DEFRULE	"(IS)%=AYZ"
	DEFRULE	"I^(I)^#=IH"
	DEFRULE	"+^(I)^+=AY "
	DEFRULE	"#:^(I)^+=IH"
	DEFRULE	"(I)^+=AY"
	DEFRULE	"(IR)=ER"
	DEFRULE	"(IGH)=AY4 "
	DEFRULE	"(ILD)=AY5LD "
	DEFRULE	" (IGN)=IHGN"
	DEFRULE	"(IGN) =AY4N"
	DEFRULE	"(IGN)^=AY4N "
	DEFRULE	"(IGN)%=AY4N"
	DEFRULE	"(ICRO)=AY4KROH"
	DEFRULE	"(IQUE)=IY4K"
	DEFRULE	"(I)=IH"

RULES_J:	; Rules for 'J'
	DEFRULE	"]J"
	DEFRULE	" (J) =JEY4"
	DEFRULE	"(J)=J"

RULES_K:	; Rules for 'K'
	DEFRULE	"]K"
	DEFRULE	" (K) =KEY4"
	DEFRULE	" (K)N="
	DEFRULE	"(K)=K"

RULES_L:	; Rules for 'L'
	DEFRULE	"]L"
	DEFRULE	" (L) =EH4L"
	DEFRULE	"(LO)C#=LOW"
	DEFRULE	"L(L)="
	DEFRULE	"#:^(L)%=UL"
	DEFRULE	"(LEAD)=LIYD"
	DEFRULE	" (LAUGH)=LAE4F"
	DEFRULE	"(L)=L"

RULES_M:	; Rules for 'M'
	DEFRULE	"]M"
	DEFRULE	" (M) =EH4M"
	DEFRULE	" (MR.) =MIH4STER"
	DEFRULE	" (MS.)=MIH5Z"
	DEFRULE	" (MRS.) =MIH4SIXZ"
	DEFRULE	"(MOV)=MUW4V"
	DEFRULE	"(MACHIN)=MAHSHIY5N"
	DEFRULE	"M(M)="
	DEFRULE	"(M)=M"

RULES_N:	; Rules for 'N'
	DEFRULE	"]N"
	DEFRULE	" (N) =EH4N"
	IF	FIXNINE		; Fix 'nineteen', 'ninety'
	DEFRULE	" (NINE)=NAY4N"
	ENDIF
	DEFRULE	"E(NG)+=NJ"
	DEFRULE	"(NG)R=NXG"
	DEFRULE	"(NG)#=NXG"
	DEFRULE	"(NGL)%=NXGUL"
	DEFRULE	"(NG)=NX"
	DEFRULE	"(NK)=NXK"
	DEFRULE	" (NOW) =NAW4 "
	DEFRULE	"N(N)="
	DEFRULE	"(NON)E=NAH4N"
	DEFRULE	"(N)=N"

RULES_O:	; Rules for 'O'
	DEFRULE	"]O"
	DEFRULE	" (O) =OH4W"
	DEFRULE	"(OF) =AHV"
	DEFRULE	" (OH) =OW5 "
	DEFRULE	"(OROUGH)=ER4OW"
	DEFRULE	"#:(OR) =ER"
	DEFRULE	"#:(ORS) =ERZ"
	DEFRULE	"(OR)=AOR"
	DEFRULE	" (ONE)=WAHN"
	DEFRULE	"#(ONE) =WAHN"
	DEFRULE	"(OW)=OW"
	DEFRULE	" (OVER)=OW5VER"
	DEFRULE	"PR(O)V=UW4"
	DEFRULE	"(OV)=AH4V"
	DEFRULE	"(O)^%=OW5 "
	DEFRULE	"(O)^EN=OW"
	DEFRULE	"(O)^I#=OW5 "
	DEFRULE	"(OL)D=OW4L"
	DEFRULE	"(OUGHT)=AO5T "
	DEFRULE	"(OUGH)=AH5F"
	DEFRULE	" (OU)=AW"
	DEFRULE	"H(OU)S#=AW4 "
	DEFRULE	"(OUS)=AXS"
	DEFRULE	"(OUR)=OHR"
	DEFRULE	"(OULD)=UH5D "
	DEFRULE	"(OU)^L=AH5 "
	DEFRULE	"(OUP)=UW5P "
	DEFRULE	"(OU)=AW"
	DEFRULE	"(OY)=OY"
	DEFRULE	"(OING)=OW4IHNX"
	DEFRULE	"(OI)=OY5 "
	DEFRULE	"(OOR)=OH5R"
	DEFRULE	"(OOK)=UH5K"
	DEFRULE	"F(OOD)=UW5D"
	DEFRULE	"L(OOD)=AH5D "
	DEFRULE	"M(OOD)=UW5D"
	DEFRULE	"(OOD)=UH5D"
	DEFRULE	"F(OOT)=UH5T"
	DEFRULE	"(OO)=UW5 "
	DEFRULE	"(O|)=OH"
	DEFRULE	"(O)E=OW"
	DEFRULE	"(O) =OW"
	DEFRULE	"(OA)=OW4 "
	DEFRULE	" (ONLY)=OW4NLIY"
	DEFRULE	" (ONCE)=WAH4NS"
	DEFRULE	"(ON|T)=OW4NT"
	DEFRULE	"C(O)N=AA"
	DEFRULE	"(O)NG=AO"
	DEFRULE	" :^(O)N=AH"
	DEFRULE	"I(ON)=UN"
	DEFRULE	"#:(ON) =UN"
	DEFRULE	"#^(ON)=UN"
	DEFRULE	"(O)ST =OW"
	DEFRULE	"(OF)^=AO4F"
	DEFRULE	"(OTHER)=AH5DHER"
	DEFRULE	"R(O)B=RAA"
	DEFRULE	"PR(O):#=ROW5"
	DEFRULE	"(OSS) =AO5S"
	DEFRULE	"#:^(OM)=AHM"
	DEFRULE	"(O)=AA"

RULES_P:	; Rules for 'P'
	DEFRULE	"]P"
	DEFRULE	" (P) =PIY4"
	DEFRULE	"(PH)=F"
	DEFRULE	"(PEOPL)=PIY5PUL "
	DEFRULE	"(POW)=PAW4 "
	DEFRULE	"(PUT) =PUHT"
	DEFRULE	"(P)P="
	DEFRULE	" (P)N="
	DEFRULE	" (P)S="
	DEFRULE	" (PROF.)=PROHFEH4SER"
	DEFRULE	"(P)=P"

RULES_Q:	; Rules for 'Q'
	DEFRULE	"]Q"
	DEFRULE	" (Q) =KYUW4"
	DEFRULE	"(QUAR)=KWOH5R "
	DEFRULE	"(QU)=KW"
	DEFRULE	"(Q)=K"

RULES_R:	; Rules for 'R'
	DEFRULE	"]R"
	DEFRULE	" (R) =AA4R"
	DEFRULE	" (RE)^#=RIY"
	DEFRULE	"(R)R="
	DEFRULE	"(R)=R"

RULES_S:	; Rules for 'S'
	DEFRULE	"]S"
	DEFRULE	" (S) =EH4S"
	DEFRULE	"(SH)=SH"
	DEFRULE	"#(SION)=ZHUN"
	DEFRULE	"(SOME)=SAHM"
	DEFRULE	"#(SUR)#=ZHER"
	DEFRULE	"(SUR)#=SHER"
	DEFRULE	"#(SU)#=ZHUW"
	DEFRULE	"#(SSU)#=SHUW"
	DEFRULE	"#(SED) =ZD"
	DEFRULE	"#(S)#=Z"
	DEFRULE	"(SAID)=SEHD"
	DEFRULE	"^(SION)=SHUN"
	DEFRULE	"(S)S="
	DEFRULE	".(S) =Z"
	DEFRULE	"#:.E(S) =Z"
	DEFRULE	"#:^#(S) =S"
	DEFRULE	"U(S) =S"
	DEFRULE	" :#(S) =Z"
	DEFRULE	"##(S) =Z"
	DEFRULE	" (SCH)=SK"
	DEFRULE	"(S)C+="
	DEFRULE	"#(SM)=ZUM"
	DEFRULE	"#(SN)|=ZUN"
	DEFRULE	"(STLE)=SUL"
	DEFRULE	"(S)=S"

RULES_T:	; Rules for 'T'
	DEFRULE	"]T"
	DEFRULE	" (T) =TIY4"
	DEFRULE	" (THE) #=DHIY"
	DEFRULE	" (THE) =DHAX"
	DEFRULE	"(TO) =TUX"
	DEFRULE	" (THAT)=DHAET"
	DEFRULE	" (THIS) =DHIHS"
	DEFRULE	" (THEY)=DHEY"
	DEFRULE	" (THERE)=DHEHR"
	DEFRULE	"(THER)=DHER"
	DEFRULE	"(THEIR)=DHEHR"
	DEFRULE	" (THAN) =DHAEN"
	DEFRULE	" (THEM) =DHEHM"
	DEFRULE	"(THESE) =DHIYZ"
	DEFRULE	" (THEN)=DHEHN"
	DEFRULE	"(THROUGH)=THRUW4 "
	DEFRULE	"(THOSE)=DHOHZ"
	DEFRULE	"(THOUGH) =DHOW"
	DEFRULE	"(TODAY)=TUXDEY"
	DEFRULE	"(TOMO)RROW=TUMAA5"
	DEFRULE	"(TO)TAL=TOW5"
	DEFRULE	" (THUS)=DHAH4S "
	DEFRULE	"(TH)=TH"
	DEFRULE	"#:(TED) =TIXD"
	DEFRULE	"S(TI)#N=CH"
	DEFRULE	"(TI)O=SH"
	DEFRULE	"(TI)A=SH"
	DEFRULE	"(TIEN)=SHUN"
	DEFRULE	"(TUR)#=CHER"
	DEFRULE	"(TU)A=CHUW"
	DEFRULE	" (TWO)=TUW"
	DEFRULE	"&(T)EN="
	DEFRULE	"F(T)EN="
	DEFRULE	"(T)=T"

RULES_U:	; Rules for 'U'
	DEFRULE	"]U"
	DEFRULE	" (U) =YUW4"
	DEFRULE	" (UN)I=YUWN"
	DEFRULE	" (UN)=AHN"
	DEFRULE	" (UPON)=AXPAON"
	DEFRULE	"@(UR)#=UH4R"
	DEFRULE	"(UR)#=YUH4R"
	DEFRULE	"(UR)=ER"
	DEFRULE	"(U)^ =AH"
	DEFRULE	"(U)^^=AH5 "
	DEFRULE	"(UY)=AY5 "
	DEFRULE	" G(U)#="
	DEFRULE	"G(U)%="
	DEFRULE	"G(U)#=W"
	DEFRULE	"#N(U)=YUW"
	DEFRULE	"@(U)=UW"
	DEFRULE	"(U)=YUW"

RULES_V:	; Rules for 'V'
	DEFRULE	"]V"
	DEFRULE	" (V) =VIY4"
	DEFRULE	"(VIEW)=VYUW5 "
	DEFRULE	"(V)=V"

RULES_W:	; Rules for 'W'
	DEFRULE	"]W"
	DEFRULE	" (W) =DAH4BULYUW"
	DEFRULE	" (WERE)=WER"
	DEFRULE	"(WA)SH=WAA"
	DEFRULE	"(WA)ST=WEY"
	DEFRULE	"(WA)S=WAH"
	DEFRULE	"(WA)T=WAA"
	DEFRULE	"(WHERE)=WHEHR"
	DEFRULE	"(WHAT)=WHAHT"
	DEFRULE	"(WHOL)=/HOWL"
	DEFRULE	"(WHO)=/HUW"
	DEFRULE	"(WH)=WH"
	DEFRULE	"(WAR)#=WEHR"
	DEFRULE	"(WAR)=WAOR"
	DEFRULE	"(WOR)^=WER"
	DEFRULE	"(WR)=R"
	DEFRULE	"(WOM)A=WUHM"
	DEFRULE	"(WOM)E=WIHM"
	DEFRULE	"(WEA)R=WEH"
	DEFRULE	"(WANT)=WAA5NT"
	DEFRULE	"ANS(WER)=ER "
	DEFRULE	"(W)=W"

RULES_X:	; Rules for 'X'
	DEFRULE	"]X"
	DEFRULE	" (X) =EH4KS"
	DEFRULE	" (X)=Z"
	DEFRULE	"(X)=KS"

RULES_Y:	; Rules for 'Y'
	DEFRULE	"]Y"
	DEFRULE	" (Y) =WAY4"
	DEFRULE	"(YOUNG)=YAHNX"
	DEFRULE	" (YOUR)=YOHR"
	DEFRULE	" (YOU)=YUW"
	DEFRULE	" (YES)=YEHS"
	DEFRULE	" (Y)=Y"
	DEFRULE	"F(Y)=AY"
	DEFRULE	"PS(YCH)=AYK "
	DEFRULE	"#:^(Y) =IY"
	DEFRULE	"#:^(Y)I=IY"
	DEFRULE	" :(Y) =AY"
	DEFRULE	" :(Y)#=AY"
	DEFRULE	" :(Y)^+:#=IH"
	DEFRULE	" :(Y)^#=AY"
	DEFRULE	"(Y)=IH"

RULES_Z:	; Rules for 'Z'
	DEFRULE	"]Z"
	DEFRULE	" (Z) =ZIY4"
	DEFRULE	"(Z)=Z"

RULES_END:	; End of Rules

	DS	LOW( 100H - LOW $ ) ; Align on page boundary

SINUS:	; Sinus table (signed 4-bit values in high nibble)

	ASSERT	LOW $ = 0	; Must be on page boundary

	DB	00H,00H,00H,10H,10H,10H,10H,10H
	DB	10H,20H,20H,20H,20H,20H,20H,30H
	DB	30H,30H,30H,30H,30H,30H,40H,40H
	DB	40H,40H,40H,40H,40H,50H,50H,50H
	DB	50H,50H,50H,50H,50H,60H,60H,60H
	DB	60H,60H,60H,60H,60H,60H,60H,60H
	DB	60H,70H,70H,70H,70H,70H,70H,70H
	DB	70H,70H,70H,70H,70H,70H,70H,70H
	DB	70H,70H,70H,70H,70H,70H,70H,70H
	DB	70H,70H,70H,70H,70H,70H,70H,70H
	DB	60H,60H,60H,60H,60H,60H,60H,60H
	DB	60H,60H,60H,60H,50H,50H,50H,50H
	DB	50H,50H,50H,50H,40H,40H,40H,40H
	DB	40H,40H,40H,30H,30H,30H,30H,30H
	DB	30H,30H,20H,20H,20H,20H,20H,20H
	DB	10H,10H,10H,10H,10H,10H,00H,00H
	DB	00H,00H,00H,0F0H,0F0H,0F0H,0F0H,0F0H
	DB	0F0H,0E0H,0E0H,0E0H,0E0H,0E0H,0E0H,0D0H
	DB	0D0H,0D0H,0D0H,0D0H,0D0H,0D0H,0C0H,0C0H
	DB	0C0H,0C0H,0C0H,0C0H,0C0H,0B0H,0B0H,0B0H
	DB	0B0H,0B0H,0B0H,0B0H,0B0H,0A0H,0A0H,0A0H
	DB	0A0H,0A0H,0A0H,0A0H,0A0H,0A0H,0A0H,0A0H
	DB	0A0H,90H,90H,90H,90H,90H,90H,90H
	DB	90H,90H,90H,90H,90H,90H,90H,90H
	DB	90H,90H,90H,90H,90H,90H,90H,90H
	DB	90H,90H,90H,90H,90H,90H,90H,90H
	DB	0A0H,0A0H,0A0H,0A0H,0A0H,0A0H,0A0H,0A0H
	DB	0A0H,0A0H,0A0H,0A0H,0B0H,0B0H,0B0H,0B0H
	DB	0B0H,0B0H,0B0H,0B0H,0C0H,0C0H,0C0H,0C0H
	DB	0C0H,0C0H,0C0H,0D0H,0D0H,0D0H,0D0H,0D0H
	DB	0D0H,0D0H,0E0H,0E0H,0E0H,0E0H,0E0H,0E0H
	DB	0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,00H,00H

RECTANGLE:	; Rectangle Table (values are signed in high nibble)
	ASSERT	LOW $ = 0	; Must be on page boundary
	ASSERT	$ = SINUS+0100H	; Must immediately follow SINUS

	DC	80H,90H		; 128 times 90H
	DC	80H,70H		; 128 times 70H

MULTTABLE:	; Multiply Table (Signed 8-bit = signed 4-bit * signed 4-bit
	ASSERT	LOW $ = 0	; Must be on page boundary

	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,01H,01H,02H,02H,03H,03H
	DB	04H,04H,05H,05H,06H,06H,07H,07H
	DB	00H,01H,02H,03H,04H,05H,06H,07H
	DB	08H,09H,0AH,0BH,0CH,0DH,0EH,0FH
	DB	00H,01H,03H,04H,06H,07H,09H,0AH
	DB	0CH,0DH,0FH,10H,12H,13H,15H,16H
	DB	00H,02H,04H,06H,08H,0AH,0CH,0EH
	DB	10H,12H,14H,16H,18H,1AH,1CH,1EH
	DB	00H,02H,05H,07H,0AH,0CH,0FH,11H
	DB	14H,16H,19H,1BH,1EH,20H,23H,25H
	DB	00H,03H,06H,09H,0CH,0FH,12H,15H
	DB	18H,1BH,1EH,21H,24H,27H,2AH,2DH
	DB	00H,03H,07H,0AH,0EH,11H,15H,18H
	DB	1CH,1FH,23H,26H,2AH,2DH,31H,34H
	DB	00H,0FCH,0F8H,0F4H,0F0H,0ECH,0E8H,0E4H
	DB	0E0H,0DCH,0D8H,0D4H,0D0H,0CCH,0C8H,0C4H
	DB	00H,0FCH,0F9H,0F5H,0F2H,0EEH,0EBH,0E7H
	DB	0E4H,0E0H,0DDH,0D9H,0D6H,0D2H,0CFH,0CBH
	DB	00H,0FDH,0FAH,0F7H,0F4H,0F1H,0EEH,0EBH
	DB	0E8H,0E5H,0E2H,0DFH,0DCH,0D9H,0D6H,0D3H
	DB	00H,0FDH,0FBH,0F8H,0F6H,0F3H,0F1H,0EEH
	DB	0ECH,0E9H,0E7H,0E4H,0E2H,0DFH,0DDH,0DAH
	DB	00H,0FEH,0FCH,0FAH,0F8H,0F6H,0F4H,0F2H
	DB	0F0H,0EEH,0ECH,0EAH,0E8H,0E6H,0E4H,0E2H
	DB	00H,0FEH,0FDH,0FBH,0FAH,0F8H,0F7H,0F5H
	DB	0F4H,0F2H,0F1H,0EFH,0EEH,0ECH,0EBH,0E9H
	DB	00H,0FFH,0FEH,0FDH,0FCH,0FBH,0FAH,0F9H
	DB	0F8H,0F7H,0F6H,0F5H,0F4H,0F3H,0F2H,0F1H
	DB	00H,0FFH,0FFH,0FEH,0FEH,0FDH,0FDH,0FCH
	DB	0FCH,0FBH,0FBH,0FAH,0FAH,0F9H,0F9H,0F8H



FREQ1DATA:
	; Formant 1 frequencies (throat)
	DB	00H		;   0:  '*'
	DB	13H		;   1: '.*'
	DB	13H		;   2: '?*'
	DB	13H		;   3: ',*'
	DB	13H		;   4: '-*'
	DB	0AH		;   5: 'IY'
	DB	0EH		;   6: 'IH'
	DB	13H		;   7: 'EH'
	DB	18H		;   8: 'AE'
	DB	1BH		;   9: 'AA'
	DB	17H		;  10: 'AH'
	DB	15H		;  11: 'AO'
	DB	10H		;  12: 'UH'
	DB	14H		;  13: 'AX'
	DB	0EH		;  14: 'IX'
	DB	12H		;  15: 'ER'
	DB	0EH		;  16: 'UX'
	DB	12H		;  17: 'OH'
	DB	12H		;  18: 'RX'
	DB	10H		;  19: 'LX'
	DB	0DH		;  20: 'WX'
	DB	0FH		;  21: 'YX'
	DB	0BH		;  22: 'WH'
	DB	12H		;  23: 'R*'
	DB	0EH		;  24: 'L*'
	DB	0BH		;  25: 'W*'
	DB	09H		;  26: 'Y*'
	DB	06H		;  27: 'M*'
	DB	06H		;  28: 'N*'
	DB	06H		;  29: 'NX'
	DB	06H		;  30: 'DX'
	DB	11H		;  31: 'Q*'
	DB	06H		;  32: 'S*'
	DB	06H		;  33: 'SH'
	DB	06H		;  34: 'F*'
	DB	06H		;  35: 'TH'
	DB	0EH		;  36: '/H'
	DB	10H		;  37: '/X'
	DB	09H		;  38: 'Z*'
	DB	0AH		;  39: 'ZH'
	DB	08H		;  40: 'V*'
	DB	0AH		;  41: 'DH'
	DB	06H		;  42: 'CH'
	DB	06H		;  43: '**' CH+ inserted after CH
	DB	06H		;  44: 'J*'
	DB	05H		;  45: '**' J+ inserted after J
	DB	06H		;  46: '**' ??
	DB	00H		;  47: '**' ??
	DB	13H		;  48: 'EY'
	DB	1BH		;  49: 'AY'
	DB	15H		;  50: 'OY'
	DB	1BH		;  51: 'AW'
	DB	12H		;  52: 'OW'
	DB	0DH		;  53: 'UW'
	DB	06H		;  54: 'B*'
	DB	06H		;  55: '**' B*' (to extend stop consonants)
	DB	06H		;  56: '**' B*"
	DB	06H		;  57: 'D*'
	DB	06H		;  58: '**' D*'
	DB	06H		;  59: '**' D*"
	DB	06H		;  60: 'G*'
	DB	06H		;  61: '**' G*'
	DB	06H		;  62: '**' G*"
	DB	06H		;  63: 'GX'
	DB	06H		;  64: '**' GX'
	DB	06H		;  65: '**' GX"
	DB	06H		;  66: 'P*'
	DB	06H		;  67: '**' P*'
	DB	06H		;  68: '**' P*"
	DB	06H		;  69: 'T*'
	DB	06H		;  70: '**' T*'
	DB	06H		;  71: '**' T*"
	DB	06H		;  72: 'K*'
	DB	0AH		;  73: '**' K*'
	DB	0AH		;  74: '**' K*"
	DB	06H		;  75: 'KX'
	DB	06H		;  76: '**' KX'
	DB	06H		;  77: '**' KX"
	DB	2CH		;  78: 'UL'
	DB	13H		;  79: 'UM'
				;  80: 'UN'

FREQ2DATA:
	;Formant 2 frequencies (mouth)
	DB	00H		;   0:  '*'
	DB	43H		;   1: '.*'
	DB	43H		;   2: '?*'
	DB	43H		;   3: ',*'
	DB	43H		;   4: '-*'
	DB	54H		;   5: 'IY'
	DB	49H		;   6: 'IH'
	DB	43H		;   7: 'EH'
	DB	3FH		;   8: 'AE'
	DB	28H		;   9: 'AA'
	DB	2CH		;  10: 'AH'
	DB	1FH		;  11: 'AO'
	DB	25H		;  12: 'UH'
	DB	2CH		;  13: 'AX'
	DB	49H		;  14: 'IX'
	DB	31H		;  15: 'ER'
	DB	24H		;  16: 'UX'
	DB	1EH		;  17: 'OH'
	DB	33H		;  18: 'RX'
	DB	25H		;  19: 'LX'
	DB	1DH		;  20: 'WX'
	DB	45H		;  21: 'YX'
	DB	18H		;  22: 'WH'
	DB	32H		;  23: 'R*'
	DB	1EH		;  24: 'L*'
	DB	18H		;  25: 'W*'
	DB	53H		;  26: 'Y*'
	DB	2EH		;  27: 'M*'
	DB	36H		;  28: 'N*'
	DB	56H		;  29: 'NX'
	DB	36H		;  30: 'DX'
	DB	43H		;  31: 'Q*'
	DB	49H		;  32: 'S*'
	DB	4FH		;  33: 'SH'
	DB	1AH		;  34: 'F*'
	DB	42H		;  35: 'TH'
	DB	49H		;  36: '/H'
	DB	25H		;  37: '/X'
	DB	33H		;  38: 'Z*'
	DB	42H		;  39: 'ZH'
	DB	28H		;  40: 'V*'
	DB	2FH		;  41: 'DH'
	DB	4FH		;  42: 'CH'
	DB	4FH		;  43: '**'
	DB	42H		;  44: 'J*'
	DB	4FH		;  45: '**'
	DB	6EH		;  46: '**'
	DB	00H		;  47: '**'
	DB	48H		;  48: 'EY'
	DB	27H		;  49: 'AY'
	DB	1FH		;  50: 'OY'
	DB	2BH		;  51: 'AW'
	DB	1EH		;  52: 'OW'
	DB	22H		;  53: 'UW'
	DB	1AH		;  54: 'B*'
	DB	1AH		;  55: '**'
	DB	1AH		;  56: '**'
	DB	42H		;  57: 'D*'
	DB	42H		;  58: '**'
	DB	42H		;  59: '**'
	DB	6EH		;  60: 'G*'
	DB	6EH		;  61: '**'
	DB	6EH		;  62: '**'
	DB	54H		;  63: 'GX'
	DB	54H		;  64: '**'
	DB	54H		;  65: '**'
	DB	1AH		;  66: 'P*'
	DB	1AH		;  67: '**'
	DB	1AH		;  68: '**'
	DB	42H		;  69: 'T*'
	DB	42H		;  70: '**'
	DB	42H		;  71: '**'
	DB	6DH		;  72: 'K*'
	DB	56H		;  73: '**'
	DB	6DH		;  74: '**'
	DB	54H		;  75: 'KX'
	DB	54H		;  76: '**'
	DB	54H		;  77: '**'
	DB	7FH		;  78: 'UL'
	DB	7FH		;  79: 'UM' - 80: 'UN'

FREQ3DATA:
	DB	00H		;   0:  '*'
	DB	5BH		;   1: '.*'
	DB	5BH		;   2: '?*'
	DB	5BH		;   3: ',*'
	DB	5BH		;   4: '-*'
	DB	6EH		;   5: 'IY'
	DB	5DH		;   6: 'IH'
	DB	5BH		;   7: 'EH'
	DB	58H		;   8: 'AE'
	DB	59H		;   9: 'AA'
	DB	57H		;  10: 'AH'
	DB	58H		;  11: 'AO'
	DB	52H		;  12: 'UH'
	DB	57H		;  13: 'AX'
	DB	5DH		;  14: 'IX'
	DB	3EH		;  15: 'ER'
	DB	52H		;  16: 'UX'
	DB	58H		;  17: 'OH'
	DB	3EH		;  18: 'RX'
	DB	6EH		;  19: 'LX'
	DB	50H		;  20: 'WX'
	DB	5DH		;  21: 'YX'
	DB	5AH		;  22: 'WH'
	DB	3CH		;  23: 'R*'
	DB	6EH		;  24: 'L*'
	DB	5AH		;  25: 'W*'
	DB	6EH		;  26: 'Y*'
	DB	51H		;  27: 'M*'
	DB	79H		;  28: 'N*'
	DB	65H		;  29: 'NX'
	DB	79H		;  30: 'DX'
	DB	5BH		;  31: 'Q*'
	DB	63H		;  32: 'S*'
	DB	6AH		;  33: 'SH'
	DB	51H		;  34: 'F*'
	DB	79H		;  35: 'TH'
	DB	5DH		;  36: '/H'
	DB	52H		;  37: '/X'
	DB	5DH		;  38: 'Z*'
	DB	67H		;  39: 'ZH'
	DB	4CH		;  40: 'V*'
	DB	5DH		;  41: 'DH'
	DB	65H		;  42: 'CH'
	DB	65H		;  43: '**'
	DB	79H		;  44: 'J*'
	DB	65H		;  45: '**'
	DB	79H		;  46: '**'
	DB	00H		;  47: '**'
	DB	5AH		;  48: 'EY'
	DB	58H		;  49: 'AY'
	DB	58H		;  50: 'OY'
	DB	58H		;  51: 'AW'
	DB	58H		;  52: 'OW'
	DB	52H		;  53: 'UW'
	DB	51H		;  54: 'B*'
	DB	51H		;  55: '**'
	DB	51H		;  56: '**'
	DB	79H		;  57: 'D*'
	DB	79H		;  58: '**'
	DB	79H		;  59: '**'
	DB	70H		;  60: 'G*'
	DB	6EH		;  61: '**'
	DB	6EH		;  62: '**'
	DB	5EH		;  63: 'GX'
	DB	5EH		;  64: '**'
	DB	5EH		;  65: '**'
	DB	51H		;  66: 'P*'
	DB	51H		;  67: '**'
	DB	51H		;  68: '**'
	DB	79H		;  69: 'T*'
	DB	79H		;  70: '**'
	DB	79H		;  71: '**'
	DB	65H		;  72: 'K*'
	DB	65H		;  73: '**'
	DB	70H		;  74: '**'
	DB	5EH		;  75: 'KX'
	DB	5EH		;  76: '**'
	DB	5EH		;  77: '**'
	DB	08H		;  78: 'UL'
	DB	01H		;  79: 'UM' - 80: 'UN'

AMPL1DATA:
	DB	00H		;   0:  '*'
	DB	00H		;   1: '.*'
	DB	00H		;   2: '?*'
	DB	00H		;   3: ',*'
	DB	00H		;   4: '-*'
	DB	0DH		;   5: 'IY'
	DB	0DH		;   6: 'IH'
	DB	0EH		;   7: 'EH'
	DB	0FH		;   8: 'AE'
	DB	0FH		;   9: 'AA'
	DB	0FH		;  10: 'AH'
	DB	0FH		;  11: 'AO'
	DB	0FH		;  12: 'UH'
	DB	0EH		;  13: 'AX'
	DB	0DH		;  14: 'IX'
	DB	0CH		;  15: 'ER'
	DB	0FH		;  16: 'UX'
	DB	0FH		;  17: 'OH'
	DB	0DH		;  18: 'RX'
	DB	0DH		;  19: 'LX'
	DB	0DH		;  20: 'WX'
	DB	0EH		;  21: 'YX'
	DB	0DH		;  22: 'WH'
	DB	0CH		;  23: 'R*'
	DB	0DH		;  24: 'L*'
	DB	0DH		;  25: 'W*'
	DB	0DH		;  26: 'Y*'
	DB	0CH		;  27: 'M*'
	DB	09H		;  28: 'N*'
	DB	09H		;  29: 'NX'
	DB	00H		;  30: 'DX'
	DB	00H		;  31: 'Q*'
	DB	00H		;  32: 'S*'
	DB	00H		;  33: 'SH'
	DB	00H		;  34: 'F*'
	DB	00H		;  35: 'TH'
	DB	00H		;  36: '/H'
	DB	00H		;  37: '/X'
	DB	0BH		;  38: 'Z*'
	DB	0BH		;  39: 'ZH'
	DB	0BH		;  40: 'V*'
	DB	0BH		;  41: 'DH'
	DB	00H		;  42: 'CH'
	DB	00H		;  43: '**'
	DB	01H		;  44: 'J*'
	DB	0BH		;  45: '**'
	DB	00H		;  46: '**'
	DB	02H		;  47: '**'
	DB	0EH		;  48: 'EY'
	DB	0FH		;  49: 'AY'
	DB	0FH		;  50: 'OY'
	DB	0FH		;  51: 'AW'
	DB	0FH		;  52: 'OW'
	DB	0DH		;  53: 'UW'
	DB	02H		;  54: 'B*'
	DB	04H		;  55: '**'
	DB	00H		;  56: '**'
	DB	02H		;  57: 'D*'
	DB	04H		;  58: '**'
	DB	00H		;  59: '**'
	DB	01H		;  60: 'G*'
	DB	04H		;  61: '**'
	DB	00H		;  62: '**'
	DB	01H		;  63: 'GX'
	DB	04H		;  64: '**'
	DB	00H		;  65: '**'
	DB	00H		;  66: 'P*'
	DB	00H		;  67: '**'
	DB	00H		;  68: '**'
	DB	00H		;  69: 'T*'
	DB	00H		;  70: '**'
	DB	00H		;  71: '**'
	DB	00H		;  72: 'K*'
	DB	0CH		;  73: '**'
	DB	00H		;  74: '**'
	DB	00H		;  75: 'KX'
	DB	00H		;  76: '**'
	DB	00H		;  77: '**'
	DB	0FH		;  78: 'UL'
	DB	0FH		;  79: 'UM' - 80: 'UN'

AMPL2DATA:
	DB	00H		;   0:  '*'
	DB	00H		;   1: '.*'
	DB	00H		;   2: '?*'
	DB	00H		;   3: ',*'
	DB	00H		;   4: '-*'
	DB	0AH		;   5: 'IY'
	DB	0BH		;   6: 'IH'
	DB	0DH		;   7: 'EH'
	DB	0EH		;   8: 'AE'
	DB	0DH		;   9: 'AA'
	DB	0CH		;  10: 'AH'
	DB	0CH		;  11: 'AO'
	DB	0BH		;  12: 'UH'
	DB	0BH		;  13: 'AX'
	DB	0BH		;  14: 'IX'
	DB	0BH		;  15: 'ER'
	DB	0CH		;  16: 'UX'
	DB	0CH		;  17: 'OH'
	DB	0CH		;  18: 'RX'
	DB	08H		;  19: 'LX'
	DB	08H		;  20: 'WX'
	DB	0CH		;  21: 'YX'
	DB	08H		;  22: 'WH'
	DB	0AH		;  23: 'R*'
	DB	08H		;  24: 'L*'
	DB	08H		;  25: 'W*'
	DB	0AH		;  26: 'Y*'
	DB	03H		;  27: 'M*'
	DB	09H		;  28: 'N*'
	DB	06H		;  29: 'NX'
	DB	00H		;  30: 'DX'
	DB	00H		;  31: 'Q*'
	DB	00H		;  32: 'S*'
	DB	00H		;  33: 'SH'
	DB	00H		;  34: 'F*'
	DB	00H		;  35: 'TH'
	DB	00H		;  36: '/H'
	DB	00H		;  37: '/X'
	DB	03H		;  38: 'Z*'
	DB	05H		;  39: 'ZH'
	DB	03H		;  40: 'V*'
	DB	04H		;  41: 'DH'
	DB	00H		;  42: 'CH'
	DB	00H		;  43: '**'
	DB	00H		;  44: 'J*'
	DB	05H		;  45: '**'
	DB	0AH		;  46: '**'
	DB	02H		;  47: '**'
	DB	0EH		;  48: 'EY'
	DB	0DH		;  49: 'AY'
	DB	0CH		;  50: 'OY'
	DB	0DH		;  51: 'AW'
	DB	0CH		;  52: 'OW'
	DB	08H		;  53: 'UW'
	DB	00H		;  54: 'B*'
	DB	01H		;  55: '**'
	DB	00H		;  56: '**'
	DB	00H		;  57: 'D*'
	DB	01H		;  58: '**'
	DB	00H		;  59: '**'
	DB	00H		;  60: 'G*'
	DB	01H		;  61: '**'
	DB	00H		;  62: '**'
	DB	00H		;  63: 'GX'
	DB	01H		;  64: '**'
	DB	00H		;  65: '**'
	DB	00H		;  66: 'P*'
	DB	00H		;  67: '**'
	DB	00H		;  68: '**'
	DB	00H		;  69: 'T*'
	DB	00H		;  70: '**'
	DB	00H		;  71: '**'
	DB	00H		;  72: 'K*'
	DB	0AH		;  73: '**'
	DB	00H		;  74: '**'
	DB	00H		;  75: 'KX'
	DB	0AH		;  76: '**'
	DB	00H		;  77: '**'
	DB	00H		;  78: 'UL'
	DB	00H		;  79: 'UM' - 80: 'UN'

AMPL3DATA:
	DB	00H		;   0:  '*'
	DB	00H		;   1: '.*'
	DB	00H		;   2: '?*'
	DB	00H		;   3: ',*'
	DB	00H		;   4: '-*'
	DB	08H		;   5: 'IY'
	DB	07H		;   6: 'IH'
	DB	08H		;   7: 'EH'
	DB	08H		;   8: 'AE'
	DB	01H		;   9: 'AA'
	DB	01H		;  10: 'AH'
	DB	00H		;  11: 'AO'
	DB	01H		;  12: 'UH'
	DB	00H		;  13: 'AX'
	DB	07H		;  14: 'IX'
	DB	05H		;  15: 'ER'
	DB	01H		;  16: 'UX'
	DB	00H		;  17: 'OH'
	DB	06H		;  18: 'RX'
	DB	01H		;  19: 'LX'
	DB	00H		;  20: 'WX'
	DB	07H		;  21: 'YX'
	DB	00H		;  22: 'WH'
	DB	05H		;  23: 'R*'
	DB	01H		;  24: 'L*'
	DB	00H		;  25: 'W*'
	DB	08H		;  26: 'Y*'
	DB	00H		;  27: 'M*'
	DB	00H		;  28: 'N*'
	DB	03H		;  29: 'NX'
	DB	00H		;  30: 'DX'
	DB	00H		;  31: 'Q*'
	DB	00H		;  32: 'S*'
	DB	00H		;  33: 'SH'
	DB	00H		;  34: 'F*'
	DB	00H		;  35: 'TH'
	DB	00H		;  36: '/H'
	DB	00H		;  37: '/X'
	DB	00H		;  38: 'Z*'
	DB	01H		;  39: 'ZH'
	DB	00H		;  40: 'V*'
	DB	00H		;  41: 'DH'
	DB	00H		;  42: 'CH'
	DB	00H		;  43: '**'
	DB	00H		;  44: 'J*'
	DB	01H		;  45: '**'
	DB	0EH		;  46: '**'
	DB	01H		;  47: '**'
	DB	09H		;  48: 'EY'
	DB	01H		;  49: 'AY'
	DB	00H		;  50: 'OY'
	DB	01H		;  51: 'AW'
	DB	00H		;  52: 'OW'
	DB	00H		;  53: 'UW'
	DB	00H		;  54: 'B*'
	DB	00H		;  55: '**'
	DB	00H		;  56: '**'
	DB	00H		;  57: 'D*'
	DB	00H		;  58: '**'
	DB	00H		;  59: '**'
	DB	00H		;  60: 'G*'
	DB	00H		;  61: '**'
	DB	00H		;  62: '**'
	DB	00H		;  63: 'GX'
	DB	00H		;  64: '**'
	DB	00H		;  65: '**'
	DB	00H		;  66: 'P*'
	DB	00H		;  67: '**'
	DB	00H		;  68: '**'
	DB	00H		;  69: 'T*'
	DB	00H		;  70: '**'
	DB	00H		;  71: '**'
	DB	00H		;  72: 'K*'
	DB	07H		;  73: '**'
	DB	00H		;  74: '**'
	DB	00H		;  75: 'KX'
	DB	05H		;  76: '**'
	DB	00H		;  77: '**'
	DB	13H		;  78: 'UL'
	DB	10H		;  79: 'UM' - 80: 'UN'

	; Phonemes stressed lengths
PHONEMESTRSLEN:	; Phoneme stressed lengths table
	DB	00H		;   0:  '*'
	DB	12H		;   1: '.*'
	DB	12H		;   2: '?*'
	DB	12H		;   3: ',*'
	DB	08H		;   4: '-*'
	DB	0BH		;   5: 'IY'
	DB	09H		;   6: 'IH'
	DB	0BH		;   7: 'EH'
	DB	0EH		;   8: 'AE'
	DB	0FH		;   9: 'AA'
	DB	0BH		;  10: 'AH'
	DB	10H		;  11: 'AO'
	DB	0CH		;  12: 'UH'
	DB	06H		;  13: 'AX'
	DB	06H		;  14: 'IX'
	DB	0EH		;  15: 'ER'
	DB	0CH		;  16: 'UX'
	DB	0EH		;  17: 'OH'
	DB	0CH		;  18: 'RX'
	DB	0BH		;  19: 'LX'
	DB	08H		;  20: 'WX'
	DB	08H		;  21: 'YX'
	DB	0BH		;  22: 'WH'
	DB	0AH		;  23: 'R*'
	DB	09H		;  24: 'L*'
	DB	08H		;  25: 'W*'
	DB	08H		;  26: 'Y*'
	DB	08H		;  27: 'M*'
	DB	08H		;  28: 'N*'
	DB	08H		;  29: 'NX'
	DB	03H		;  30: 'DX'
	DB	05H		;  31: 'Q*'
	DB	02H		;  32: 'S*'
	DB	02H		;  33: 'SH'
	DB	02H		;  34: 'F*'
	DB	02H		;  35: 'TH'
	DB	02H		;  36: '/H'
	DB	02H		;  37: '/X'
	DB	06H		;  38: 'Z*'
	DB	06H		;  39: 'ZH'
	DB	08H		;  40: 'V*'
	DB	06H		;  41: 'DH'
	DB	06H		;  42: 'CH'
	DB	02H		;  43: '**'
	DB	09H		;  44: 'J*'
	DB	04H		;  45: '**'
	DB	02H		;  46: '**'
	DB	01H		;  47: '**'
	DB	0EH		;  48: 'EY'
	DB	0FH		;  49: 'AY'
	DB	0FH		;  50: 'OY'
	DB	0FH		;  51: 'AW'
	DB	0EH		;  52: 'OW'
	DB	0EH		;  53: 'UW'
	DB	08H		;  54: 'B*'
	DB	02H		;  55: '**'
	DB	02H		;  56: '**'
	DB	07H		;  57: 'D*'
	DB	02H		;  58: '**'
	DB	01H		;  59: '**'
	DB	07H		;  60: 'G*'
	DB	02H		;  61: '**'
	DB	02H		;  62: '**'
	DB	07H		;  63: 'GX'
	DB	02H		;  64: '**'
	DB	02H		;  65: '**'
	DB	08H		;  66: 'P*'
	DB	02H		;  67: '**'
	DB	02H		;  68: '**'
	DB	06H		;  69: 'T*'
	DB	02H		;  70: '**'
	DB	02H		;  71: '**'
	DB	07H		;  72: 'K*'
	DB	02H		;  73: '**'
	DB	04H		;  74: '**'
	DB	07H		;  75: 'KX'
	DB	01H		;  76: '**'
	DB	04H		;  77: '**'
	DB	05H		;  78: 'UL'
	DB	05H		;  79: 'UM' - 80: 'UN'

	; Phonemes normal lengths
PHONEMENORMLEN:	; Phoneme normal lengths table
	DB	00H		;   0:  '*'
	DB	12H		;   1: '.*'
	DB	12H		;   2: '?*'
	DB	12H		;   3: ',*'
	DB	08H		;   4: '-*'
	DB	08H		;   5: 'IY'
	DB	08H		;   6: 'IH'
	DB	08H		;   7: 'EH'
	DB	08H		;   8: 'AE'
	DB	0BH		;   9: 'AA'
	DB	06H		;  10: 'AH'
	DB	0CH		;  11: 'AO'
	DB	0AH		;  12: 'UH'
	DB	05H		;  13: 'AX'
	DB	05H		;  14: 'IX'
	DB	0BH		;  15: 'ER'
	DB	0AH		;  16: 'UX'
	DB	0AH		;  17: 'OH'
	DB	0AH		;  18: 'RX'
	DB	09H		;  19: 'LX'
	DB	08H		;  20: 'WX'
	DB	07H		;  21: 'YX'
	DB	09H		;  22: 'WH'
	DB	07H		;  23: 'R*'
	DB	06H		;  24: 'L*'
	DB	08H		;  25: 'W*'
	DB	06H		;  26: 'Y*'
	DB	07H		;  27: 'M*'
	DB	07H		;  28: 'N*'
	DB	07H		;  29: 'NX'
	DB	02H		;  30: 'DX'
	DB	05H		;  31: 'Q*'
	DB	02H		;  32: 'S*'
	DB	02H		;  33: 'SH'
	DB	02H		;  34: 'F*'
	DB	02H		;  35: 'TH'
	DB	02H		;  36: '/H'
	DB	02H		;  37: '/X'
	DB	06H		;  38: 'Z*'
	DB	06H		;  39: 'ZH'
	DB	07H		;  40: 'V*'
	DB	06H		;  41: 'DH'
	DB	06H		;  42: 'CH'
	DB	02H		;  43: '**'
	DB	08H		;  44: 'J*'
	DB	03H		;  45: '**'
	DB	01H		;  46: '**'
	DB	1EH		;  47: '**'
	DB	0DH		;  48: 'EY'
	DB	0CH		;  49: 'AY'
	DB	0CH		;  50: 'OY'
	DB	0CH		;  51: 'AW'
	DB	0EH		;  52: 'OW'
	DB	09H		;  53: 'UW'
	DB	06H		;  54: 'B*'
	DB	01H		;  55: '**'
	DB	02H		;  56: '**'
	DB	05H		;  57: 'D*'
	DB	01H		;  58: '**'
	DB	01H		;  59: '**'
	DB	06H		;  60: 'G*'
	DB	01H		;  61: '**'
	DB	02H		;  62: '**'
	DB	06H		;  63: 'GX'
	DB	01H		;  64: '**'
	DB	02H		;  65: '**'
	DB	08H		;  66: 'P*'
	DB	02H		;  67: '**'
	DB	02H		;  68: '**'
	DB	04H		;  69: 'T*'
	DB	02H		;  70: '**'
	DB	02H		;  71: '**'
	DB	06H		;  72: 'K*'
	DB	01H		;  73: '**'
	DB	04H		;  74: '**'
	DB	06H		;  75: 'KX'
	DB	01H		;  76: '**'
	DB	04H		;  77: '**'
	DB	0C7H		;  78: 'UL'
	DB	0FFH		;  79: 'UM' - 80: 'UN'

OUTBLENDLENGTH:	; Number of frames at the end of a phoneme devoted to interp
	DB	00H		;   0:  '*'
	DB	02H		;   1: '.*'
	DB	02H		;   2: '?*'
	DB	02H		;   3: ',*'
	DB	02H		;   4: '-*'
	DB	04H		;   5: 'IY'
	DB	04H		;   6: 'IH'
	DB	04H		;   7: 'EH'
	DB	04H		;   8: 'AE'
	DB	04H		;   9: 'AA'
	DB	04H		;  10: 'AH'
	DB	04H		;  11: 'AO'
	DB	04H		;  12: 'UH'
	DB	04H		;  13: 'AX'
	DB	04H		;  14: 'IX'
	DB	04H		;  15: 'ER'
	DB	04H		;  16: 'UX'
	DB	04H		;  17: 'OH'
	DB	03H		;  18: 'RX'
	DB	02H		;  19: 'LX'
	DB	04H		;  20: 'WX'
	DB	04H		;  21: 'YX'
	DB	02H		;  22: 'WH'
	DB	02H		;  23: 'R*'
	DB	02H		;  24: 'L*'
	DB	02H		;  25: 'W*'
	DB	02H		;  26: 'Y*'
	DB	01H		;  27: 'M*'
	DB	01H		;  28: 'N*'
	DB	01H		;  29: 'NX'
	DB	01H		;  30: 'DX'
	DB	01H		;  31: 'Q*'
	DB	01H		;  32: 'S*'
	DB	01H		;  33: 'SH'
	DB	01H		;  34: 'F*'
	DB	01H		;  35: 'TH'
	DB	01H		;  36: '/H'
	DB	01H		;  37: '/X'
	DB	02H		;  38: 'Z*'
	DB	02H		;  39: 'ZH'
	DB	02H		;  40: 'V*'
	DB	01H		;  41: 'DH'
	DB	00H		;  42: 'CH'
	DB	01H		;  43: '**'
	DB	00H		;  44: 'J*'
	DB	01H		;  45: '**'
	DB	00H		;  46: '**'
	DB	05H		;  47: '**'
	DB	05H		;  48: 'EY'
	DB	05H		;  49: 'AY'
	DB	05H		;  50: 'OY'
	DB	05H		;  51: 'AW'
	DB	04H		;  52: 'OW'
	DB	04H		;  53: 'UW'
	DB	02H		;  54: 'B*'
	DB	00H		;  55: '**'
	DB	01H		;  56: '**'
	DB	02H		;  57: 'D*'
	DB	00H		;  58: '**'
	DB	01H		;  59: '**'
	DB	02H		;  60: 'G*'
	DB	00H		;  61: '**'
	DB	01H		;  62: '**'
	DB	02H		;  63: 'GX'
	DB	00H		;  64: '**'
	DB	01H		;  65: '**'
	DB	02H		;  66: 'P*'
	DB	00H		;  67: '**'
	DB	02H		;  68: '**'
	DB	02H		;  69: 'T*'
	DB	00H		;  70: '**'
	DB	01H		;  71: '**'
	DB	03H		;  72: 'K*'
	DB	00H		;  73: '**'
	DB	02H		;  74: '**'
	DB	03H		;  75: 'KX'
	DB	00H		;  76: '**'
	DB	02H		;  77: '**'
	DB	0A0H		;  78: 'UL'
	DB	0A0H		;  79: 'UM' - 80: 'UN'

INBLENDLENGTH:	; Number of frames at beginning of a phoneme devoted to inte
	DB	00H		;   0:  '*'
	DB	02H		;   1: '.*'
	DB	02H		;   2: '?*'
	DB	02H		;   3: ',*'
	DB	02H		;   4: '-*'
	DB	04H		;   5: 'IY'
	DB	04H		;   6: 'IH'
	DB	04H		;   7: 'EH'
	DB	04H		;   8: 'AE'
	DB	04H		;   9: 'AA'
	DB	04H		;  10: 'AH'
	DB	04H		;  11: 'AO'
	DB	04H		;  12: 'UH'
	DB	04H		;  13: 'AX'
	DB	04H		;  14: 'IX'
	DB	04H		;  15: 'ER'
	DB	04H		;  16: 'UX'
	DB	04H		;  17: 'OH'
	DB	03H		;  18: 'RX'
	DB	03H		;  19: 'LX'
	DB	04H		;  20: 'WX'
	DB	04H		;  21: 'YX'
	DB	03H		;  22: 'WH'
	DB	03H		;  23: 'R*'
	DB	03H		;  24: 'L*'
	DB	03H		;  25: 'W*'
	DB	03H		;  26: 'Y*'
	DB	01H		;  27: 'M*'
	DB	02H		;  28: 'N*'
	DB	03H		;  29: 'NX'
	DB	02H		;  30: 'DX'
	DB	01H		;  31: 'Q*'
	DB	03H		;  32: 'S*'
	DB	03H		;  33: 'SH'
	DB	03H		;  34: 'F*'
	DB	03H		;  35: 'TH'
	DB	01H		;  36: '/H'
	DB	01H		;  37: '/X'
	DB	03H		;  38: 'Z*'
	DB	03H		;  39: 'ZH'
	DB	03H		;  40: 'V*'
	DB	02H		;  41: 'DH'
	DB	02H		;  42: 'CH'
	DB	03H		;  43: '**'
	DB	02H		;  44: 'J*'
	DB	03H		;  45: '**'
	DB	00H		;  46: '**'
	DB	00H		;  47: '**'
	DB	05H		;  48: 'EY'
	DB	05H		;  49: 'AY'
	DB	05H		;  50: 'OY'
	DB	05H		;  51: 'AW'
	DB	04H		;  52: 'OW'
	DB	04H		;  53: 'UW'
	DB	02H		;  54: 'B*'
	DB	00H		;  55: '**'
	DB	02H		;  56: '**'
	DB	02H		;  57: 'D*'
	DB	00H		;  58: '**'
	DB	03H		;  59: '**'
	DB	02H		;  60: 'G*'
	DB	00H		;  61: '**'
	DB	04H		;  62: '**'
	DB	02H		;  63: 'GX'
	DB	00H		;  64: '**'
	DB	03H		;  65: '**'
	DB	02H		;  66: 'P*'
	DB	00H		;  67: '**'
	DB	02H		;  68: '**'
	DB	02H		;  69: 'T*'
	DB	00H		;  70: '**'
	DB	02H		;  71: '**'
	DB	03H		;  72: 'K*'
	DB	00H		;  73: '**'
	DB	03H		;  74: '**'
	DB	03H		;  75: 'KX'
	DB	00H		;  76: '**'
	DB	03H		;  77: '**'
	DB	0B0H		;  78: 'UL'
	DB	0A0H		;  79: 'UM' - 80: 'UN'

BLENDRANK:	; Used to decide which phoneme's blend lengths. The candidat
	DB	00H		;   0:  '*'
	DB	1FH		;   1: '.*'
	DB	1FH		;   2: '?*'
	DB	1FH		;   3: ',*'
	DB	1FH		;   4: '-*'
	DB	02H		;   5: 'IY'
	DB	02H		;   6: 'IH'
	DB	02H		;   7: 'EH'
	DB	02H		;   8: 'AE'
	DB	02H		;   9: 'AA'
	DB	02H		;  10: 'AH'
	DB	02H		;  11: 'AO'
	DB	02H		;  12: 'UH'
	DB	02H		;  13: 'AX'
	DB	05H		;  14: 'IX'
	DB	05H		;  15: 'ER'
	DB	02H		;  16: 'UX'
	DB	0AH		;  17: 'OH'
	DB	02H		;  18: 'RX'
	DB	08H		;  19: 'LX'
	DB	05H		;  20: 'WX'
	DB	05H		;  21: 'YX'
	DB	0BH		;  22: 'WH'
	DB	0AH		;  23: 'R*'
	DB	09H		;  24: 'L*'
	DB	08H		;  25: 'W*'
	DB	08H		;  26: 'Y*'
	DB	0A0H		;  27: 'M*'
	DB	08H		;  28: 'N*'
	DB	08H		;  29: 'NX'
	DB	17H		;  30: 'DX'
	DB	1FH		;  31: 'Q*'
	DB	12H		;  32: 'S*'
	DB	12H		;  33: 'SH'
	DB	12H		;  34: 'F*'
	DB	12H		;  35: 'TH'
	DB	1EH		;  36: '/H'
	DB	1EH		;  37: '/X'
	DB	14H		;  38: 'Z*'
	DB	14H		;  39: 'ZH'
	DB	14H		;  40: 'V*'
	DB	14H		;  41: 'DH'
	DB	17H		;  42: 'CH'
	DB	17H		;  43: '**'
	DB	1AH		;  44: 'J*'
	DB	1AH		;  45: '**'
	DB	1DH		;  46: '**'
	DB	1DH		;  47: '**'
	DB	02H		;  48: 'EY'
	DB	02H		;  49: 'AY'
	DB	02H		;  50: 'OY'
	DB	02H		;  51: 'AW'
	DB	02H		;  52: 'OW'
	DB	02H		;  53: 'UW'
	DB	1AH		;  54: 'B*'
	DB	1DH		;  55: '**'
	DB	1BH		;  56: '**'
	DB	1AH		;  57: 'D*'
	DB	1DH		;  58: '**'
	DB	1BH		;  59: '**'
	DB	1AH		;  60: 'G*'
	DB	1DH		;  61: '**'
	DB	1BH		;  62: '**'
	DB	1AH		;  63: 'GX'
	DB	1DH		;  64: '**'
	DB	1BH		;  65: '**'
	DB	17H		;  66: 'P*'
	DB	1DH		;  67: '**'
	DB	17H		;  68: '**'
	DB	17H		;  69: 'T*'
	DB	1DH		;  70: '**'
	DB	17H		;  71: '**'
	DB	17H		;  72: 'K*'
	DB	1DH		;  73: '**'
	DB	17H		;  74: '**'
	DB	17H		;  75: 'KX'
	DB	1DH		;  76: '**'
	DB	17H		;  77: '**'
	DB	17H		;  78: 'UL'
	DB	17H		;  79: 'UM' - 80: 'UN'

; Sampled Consonants Flags
; Looks like it's used as bit flags
; High bits masked by 248 (11111000)
;
; 32: S*    241         11110001
; 33: SH    226         11100010
; 34: F*    211         11010011
; 35: TH    187         10111011
; 36: /H    124         01111100
; 37: /X    149         10010101
; 38: Z*    1           00000001
; 39: ZH    2           00000010
; 40: V*    3           00000011
; 41: DH    3           00000011
; 43: CH+   114         01110010
; 45: J*+   2           00000010
; 67: P*'   27          00011011
; 70: T*'   25          00011001
; tab45936
SAMPLEDCONSONAN:	; Looks like it's used as bit flags. High bits masked by 248
	DB	00H		;   0:  '*'
	DB	00H		;   1: '.*'
	DB	00H		;   2: '?*'
	DB	00H		;   3: ',*'
	DB	00H		;   4: '-*'
	DB	00H		;   5: 'IY'
	DB	00H		;   6: 'IH'
	DB	00H		;   7: 'EH'
	DB	00H		;   8: 'AE'
	DB	00H		;   9: 'AA'
	DB	00H		;  10: 'AH'
	DB	00H		;  11: 'AO'
	DB	00H		;  12: 'UH'
	DB	00H		;  13: 'AX'
	DB	00H		;  14: 'IX'
	DB	00H		;  15: 'ER'
	DB	00H		;  16: 'UX'
	DB	00H		;  17: 'OH'
	DB	00H		;  18: 'RX'
	DB	00H		;  19: 'LX'
	DB	00H		;  20: 'WX'
	DB	00H		;  21: 'YX'
	DB	00H		;  22: 'WH'
	DB	00H		;  23: 'R*'
	DB	00H		;  24: 'L*'
	DB	00H		;  25: 'W*'
	DB	00H		;  26: 'Y*'
	DB	00H		;  27: 'M*'
	DB	00H		;  28: 'N*'
	DB	00H		;  29: 'NX'
	DB	00H		;  30: 'DX'
	DB	00H		;  31: 'Q*'
	DB	0F1H		;  32: 'S*'
	DB	0E2H		;  33: 'SH'
	DB	0D3H		;  34: 'F*'
	DB	0BBH		;  35: 'TH'
	DB	7CH		;  36: '/H'
	DB	95H		;  37: '/X'
	DB	01H		;  38: 'Z*'
	DB	02H		;  39: 'ZH'
	DB	03H		;  40: 'V*'
	DB	03H		;  41: 'DH'
	DB	00H		;  42: 'CH'
	DB	72H		;  43: '**'
	DB	00H		;  44: 'J*'
	DB	02H		;  45: '**'
	DB	00H		;  46: '**'
	DB	00H		;  47: '**'
	DB	00H		;  48: 'EY'
	DB	00H		;  49: 'AY'
	DB	00H		;  50: 'OY'
	DB	00H		;  51: 'AW'
	DB	00H		;  52: 'OW'
	DB	00H		;  53: 'UW'
	DB	00H		;  54: 'B*'
	DB	00H		;  55: '**'
	DB	00H		;  56: '**'
	DB	00H		;  57: 'D*'
	DB	00H		;  58: '**'
	DB	00H		;  59: '**'
	DB	00H		;  60: 'G*'
	DB	00H		;  61: '**'
	DB	00H		;  62: '**'
	DB	00H		;  63: 'GX'
	DB	00H		;  64: '**'
	DB	00H		;  65: '**'
	DB	00H		;  66: 'P*'
	DB	1BH		;  67: '**'
	DB	00H		;  68: '**'
	DB	00H		;  69: 'T*'
	DB	19H		;  70: '**'
	DB	00H		;  71: '**'
	DB	00H		;  72: 'K*'
	DB	00H		;  73: '**'
	DB	00H		;  74: '**'
	DB	00H		;  75: 'KX'
	DB	00H		;  76: '**'
	DB	00H		;  77: '**'
	DB	00H		;  78: 'UL'
	DB	00H		;  79: 'UM' - 80: 'UN'

; BUG? Starts 15 bytes forward, why? maybe missing a masking statement...
; (seems to exist also in the original C64 version)
; Defines 5 tables of random noise of different kinds,
; each table is 256 bytes long
; (240 bytes max, 15 bytes always skipped...)
SAMPLETABLE_000:
	DB	38H,84H,6BH,19H,0C6H,63H,18H,86H
	DB	73H,98H,0C6H,0B1H,1CH,0CAH,31H,8CH
	DB	0C7H,31H,88H,0C2H,30H,98H,46H,31H
	DB	18H,0C6H,35H,0CH,0CAH,31H,0CH,0C6H
	DB	21H,10H,24H,69H,12H,0C2H,31H,14H
	DB	0C4H,71H,08H,4AH,22H,49H,0ABH,6AH
	DB	0A8H,0ACH,49H,51H,32H,0D5H,52H,88H
	DB	93H,6CH,94H,22H,15H,54H,0D2H,25H
	DB	96H,0D4H,50H,0A5H,46H,21H,08H,85H
	DB	6BH,18H,0C4H,63H,10H,0CEH,6BH,18H
	DB	8CH,71H,19H,8CH,63H,35H,0CH,0C6H
	DB	33H,99H,0CCH,6CH,0B5H,4EH,0A2H,99H
	DB	46H,21H,28H,82H,95H,2EH,0E3H,30H
	DB	9CH,0C5H,30H,9CH,0A2H,0B1H,9CH,67H
	DB	31H,88H,66H,59H,2CH,53H,18H,84H
	DB	67H,50H,0CAH,0E3H,0AH,0ACH,0ABH,30H
	DB	0ACH,62H,30H,8CH,63H,10H,94H,62H
	DB	0B1H,8CH,82H,28H,96H,33H,98H,0D6H
	DB	0B5H,4CH,62H,29H,0A5H,4AH,0B5H,9CH
	DB	0C6H,31H,14H,0D6H,38H,9CH,4BH,0B4H
	DB	86H,65H,18H,0AEH,67H,1CH,0A6H,63H
	DB	19H,96H,23H,19H,84H,13H,08H,0A6H
	DB	52H,0ACH,0CAH,22H,89H,6EH,0ABH,19H
	DB	8CH,62H,34H,0C4H,62H,19H,86H,63H
	DB	18H,0C4H,23H,58H,0D6H,0A3H,50H,42H
	DB	54H,4AH,0ADH,4AH,25H,11H,6BH,64H
	DB	89H,4AH,63H,39H,8AH,23H,31H,2AH
	DB	0EAH,0A2H,0A9H,44H,0C5H,12H,0CDH,42H
	DB	34H,8CH,62H,18H,8CH,63H,11H,48H
	DB	66H,31H,9DH,44H,33H,1DH,46H,31H
	DB	9CH,0C6H,0B1H,0CH,0CDH,32H,88H,0C4H
	DB	73H,18H,86H,73H,08H,0D6H,63H,58H

SAMPLETABLE_100:	; Sample table for CH, J, SH, ZH
	DB	07H,81H,0E0H,0F0H,3CH,07H,87H,90H
	DB	3CH,7CH,0FH,0C7H,0C0H,0C0H,0F0H,7CH
	DB	1EH,07H,80H,80H,00H,1CH,78H,70H
	DB	0F1H,0C7H,1FH,0C0H,0CH,0FEH,1CH,1FH
	DB	1FH,0EH,0AH,7AH,0C0H,71H,0F2H,83H
	DB	8FH,03H,0FH,0FH,0CH,00H,79H,0F8H
	DB	61H,0E0H,43H,0FH,83H,0E7H,18H,0F9H
	DB	0C1H,13H,0DAH,0E9H,63H,8FH,0FH,83H
	DB	83H,87H,0C3H,1FH,3CH,70H,0F0H,0E1H
	DB	0E1H,0E3H,87H,0B8H,71H,0EH,20H,0E3H
	DB	8DH,48H,78H,1CH,93H,87H,30H,0E1H
	DB	0C1H,0C1H,0E4H,78H,21H,83H,83H,0C3H
	DB	87H,06H,39H,0E5H,0C3H,87H,07H,0EH
	DB	1CH,1CH,70H,0F4H,71H,9CH,60H,36H
	DB	32H,0C3H,1EH,3CH,0F3H,8FH,0EH,3CH
	DB	70H,0E3H,0C7H,8FH,0FH,0FH,0EH,3CH
	DB	78H,0F0H,0E3H,87H,06H,0F0H,0E3H,07H
	DB	0C1H,99H,87H,0FH,18H,78H,70H,70H
	DB	0FCH,0F3H,10H,0B1H,8CH,8CH,31H,7CH
	DB	70H,0E1H,86H,3CH,64H,6CH,0B0H,0E1H
	DB	0E3H,0FH,23H,8FH,0FH,1EH,3EH,38H
	DB	3CH,38H,7BH,8FH,07H,0EH,3CH,0F4H
	DB	17H,1EH,3CH,78H,0F2H,9EH,72H,49H
	DB	0E3H,25H,36H,38H,58H,39H,0E2H,0DEH
	DB	3CH,78H,78H,0E1H,0C7H,61H,0E1H,0E1H
	DB	0B0H,0F0H,0F0H,0C3H,0C7H,0EH,38H,0C0H
	DB	0F0H,0CEH,73H,73H,18H,34H,0B0H,0E1H
	DB	0C7H,8EH,1CH,3CH,0F8H,38H,0F0H,0E1H
	DB	0C1H,8BH,86H,8FH,1CH,78H,70H,0F0H
	DB	78H,0ACH,0B1H,8FH,39H,31H,0DBH,38H
	DB	61H,0C3H,0EH,0EH,38H,78H,73H,17H
	DB	1EH,39H,1EH,38H,64H,0E1H,0F1H,0C1H

SAMPLETABLE_200:	; Sample table for P, F*, V, TH, DH
	DB	4EH,0FH,40H,0A2H,02H,0C5H,8FH,81H
	DB	0A1H,0FCH,12H,08H,64H,0E0H,3CH,22H
	DB	0E0H,45H,07H,8EH,0CH,32H,90H,0F0H
	DB	1FH,20H,49H,0E0H,0F8H,0CH,60H,0F0H
	DB	17H,1AH,41H,0AAH,0A4H,0D0H,8DH,12H
	DB	82H,1EH,1EH,03H,0F8H,3EH,03H,0CH
	DB	73H,80H,70H,44H,26H,03H,24H,0E1H
	DB	3EH,04H,4EH,04H,1CH,0C1H,09H,0CCH
	DB	9EH,90H,21H,07H,90H,43H,64H,0C0H
	DB	0FH,0C6H,90H,9CH,0C1H,5BH,03H,0E2H
	DB	1DH,81H,0E0H,5EH,1DH,03H,84H,0B8H
	DB	2CH,0FH,80H,0B1H,83H,0E0H,30H,41H
	DB	1EH,43H,89H,83H,50H,0FCH,24H,2EH
	DB	13H,83H,0F1H,7CH,4CH,2CH,0C9H,0DH
	DB	83H,0B0H,0B5H,82H,0E4H,0E8H,06H,9CH
	DB	07H,0A0H,99H,1DH,07H,3EH,82H,8FH
	DB	70H,30H,74H,40H,0CAH,10H,0E4H,0E8H
	DB	0FH,92H,14H,3FH,06H,0F8H,84H,88H
	DB	43H,81H,0AH,34H,39H,41H,0C6H,0E3H
	DB	1CH,47H,03H,0B0H,0B8H,13H,0AH,0C2H
	DB	64H,0F8H,18H,0F9H,60H,0B3H,0C0H,65H
	DB	20H,60H,0A6H,8CH,0C3H,81H,20H,30H
	DB	26H,1EH,1CH,38H,0D3H,01H,0B0H,26H
	DB	40H,0F4H,0BH,0C3H,42H,1FH,85H,32H
	DB	26H,60H,40H,0C9H,0CBH,01H,0ECH,11H
	DB	28H,40H,0FAH,04H,34H,0E0H,70H,4CH
	DB	8CH,1DH,07H,69H,03H,16H,0C8H,04H
	DB	23H,0E8H,0C6H,9AH,0BH,1AH,03H,0E0H
	DB	76H,06H,05H,0CFH,1EH,0BCH,58H,31H
	DB	71H,66H,00H,0F8H,3FH,04H,0FCH,0CH
	DB	74H,27H,8AH,80H,71H,0C2H,3AH,26H
	DB	06H,0C0H,1FH,05H,0FH,98H,40H,0AEH

SAMPLETABLE_300:	; Sample table for /H
	DB	01H,7FH,0C0H,07H,0FFH,00H,0EH,0FEH
	DB	00H,03H,0DFH,80H,03H,0EFH,80H,1BH
	DB	0F1H,0C2H,00H,0E7H,0E0H,18H,0FCH,0E0H
	DB	21H,0FCH,80H,3CH,0FCH,40H,0EH,7EH
	DB	00H,3FH,3EH,00H,0FH,0FEH,00H,1FH
	DB	0FFH,00H,3EH,0F0H,07H,0FCH,00H,7EH
	DB	10H,3FH,0FFH,00H,3FH,38H,0EH,7CH
	DB	01H,87H,0CH,0FCH,0C7H,00H,3EH,04H
	DB	0FH,3EH,1FH,0FH,0FH,1FH,0FH,02H
	DB	83H,87H,0CFH,03H,87H,0FH,3FH,0C0H
	DB	07H,9EH,60H,3FH,0C0H,03H,0FEH,00H
	DB	3FH,0E0H,77H,0E1H,0C0H,0FEH,0E0H,0C3H
	DB	0E0H,01H,0DFH,0F8H,03H,07H,00H,7EH
	DB	70H,00H,7CH,38H,18H,0FEH,0CH,1EH
	DB	78H,1CH,7CH,3EH,0EH,1FH,1EH,1EH
	DB	3EH,00H,7FH,83H,07H,0DBH,87H,83H
	DB	07H,0C7H,07H,10H,71H,0FFH,00H,3FH
	DB	0E2H,01H,0E0H,0C1H,0C3H,0E1H,00H,7FH
	DB	0C0H,05H,0F0H,20H,0F8H,0F0H,70H,0FEH
	DB	78H,79H,0F8H,02H,3FH,0CH,8FH,03H
	DB	0FH,9FH,0E0H,0C1H,0C7H,87H,03H,0C3H
	DB	0C3H,0B0H,0E1H,0E1H,0C1H,0E3H,0E0H,71H
	DB	0F0H,00H,0FCH,70H,7CH,0CH,3EH,38H
	DB	0EH,1CH,70H,0C3H,0C7H,03H,81H,0C1H
	DB	0C7H,0E7H,00H,0FH,0C7H,87H,19H,09H
	DB	0EFH,0C4H,33H,0E0H,0C1H,0FCH,0F8H,70H
	DB	0F0H,78H,0F8H,0F0H,61H,0C7H,00H,1FH
	DB	0F8H,01H,7CH,0F8H,0F0H,78H,70H,3CH
	DB	7CH,0CEH,0EH,21H,83H,0CFH,08H,07H
	DB	8FH,08H,0C1H,87H,8FH,80H,0C7H,0E3H
	DB	00H,07H,0F8H,0E0H,0EFH,00H,39H,0F7H
	DB	80H,0EH,0F8H,0E1H,0E3H,0F8H,21H,9FH

SAMPLETABLE_400:	; Sample table for /X
	DB	0C0H,0FFH,03H,0F8H,07H,0C0H,1FH,0F8H
	DB	0C4H,04H,0FCH,0C4H,0C1H,0BCH,87H,0F0H
	DB	0FH,0C0H,7FH,05H,0E0H,25H,0ECH,0C0H
	DB	3EH,84H,47H,0F0H,8EH,03H,0F8H,03H
	DB	0FBH,0C0H,19H,0F8H,07H,9CH,0CH,17H
	DB	0F8H,07H,0E0H,1FH,0A1H,0FCH,0FH,0FCH
	DB	01H,0F0H,3FH,00H,0FEH,03H,0F0H,1FH
	DB	00H,0FDH,00H,0FFH,88H,0DH,0F9H,01H
	DB	0FFH,00H,70H,07H,0C0H,3EH,42H,0F3H
	DB	0DH,0C4H,7FH,80H,0FCH,07H,0F0H,5EH
	DB	0C0H,3FH,00H,78H,3FH,81H,0FFH,01H
	DB	0F8H,01H,0C3H,0E8H,0CH,0E4H,64H,8FH
	DB	0E4H,0FH,0F0H,07H,0F0H,0C2H,1FH,00H
	DB	7FH,0C0H,6FH,80H,7EH,03H,0F8H,07H
	DB	0F0H,3FH,0C0H,78H,0FH,82H,07H,0FEH
	DB	22H,77H,70H,02H,76H,03H,0FEH,00H
	DB	0FEH,67H,00H,7CH,0C7H,0F1H,8EH,0C6H
	DB	3BH,0E0H,3FH,84H,0F3H,19H,0D8H,03H
	DB	99H,0FCH,09H,0B8H,0FH,0F8H,00H,9DH
	DB	24H,61H,0F9H,0DH,00H,0FDH,03H,0F0H
	DB	1FH,90H,3FH,01H,0F8H,1FH,0D0H,0FH
	DB	0F8H,37H,01H,0F8H,07H,0F0H,0FH,0C0H
	DB	3FH,00H,0FEH,03H,0F8H,0FH,0C0H,3FH
	DB	00H,0FAH,03H,0F0H,0FH,80H,0FFH,01H
	DB	0B8H,07H,0F0H,01H,0FCH,01H,0BCH,80H
	DB	13H,1EH,00H,7FH,0E1H,40H,7FH,0A0H
	DB	7FH,0B0H,00H,3FH,0C0H,1FH,0C0H,38H
	DB	0FH,0F0H,1FH,80H,0FFH,01H,0FCH,03H
	DB	0F1H,7EH,01H,0FEH,01H,0F0H,0FFH,00H
	DB	7FH,0C0H,1DH,07H,0F0H,0FH,0C0H,7EH
	DB	06H,0E0H,07H,0E0H,0FH,0F8H,06H,0C1H
	DB	0FEH,01H,0FCH,03H,0E0H,0FH,00H,0FCH


AMPLITUDERESCAL:
	DB	00H,01H,02H,02H,02H,03H,03H,04H
	DB	04H,05H,06H,08H,09H,0BH,0DH,0FH

STRESS_AMOUNTS:	; stress amount (add to pitch) (tab47492)
	DB	00H,00H,0E0H,0E6H,0ECH,0F3H,0F9H,00H
	DB	06H,0CH,06H

SPEECHBUFFERLEN:	; Number of chars in speech buffer
	DB	0BDH

CONSONANT_TAB:	; table { 0x18, 0x1A, 0x17, 0x17, 0x17 } (tab48426)
	DB	80H^80H^DACOFFS
	DB	0A4H^80H^DACOFFS
	DB	70H^80H^DACOFFS
	DB	70H^80H^DACOFFS
	DB	6CH^80H^DACOFFS

STRESSINPUTTBL:	; stress input codes ('1'..'9')
	DEFSTR8	"*123456789"

SIGNINPUTTBL1:	; phoneme names 1st char or '*'
	DEFSTR8	" .?,-IIEAAAAUAIE"
	DEFSTR8	"UORLWYWRLWYMNNDQ"
	DEFSTR8	"SSFT//ZZVDC*J***"
	DEFSTR8	"EAOAOUB**D**G**G"
	DEFSTR8	"**P**T**K**K**UU"
	DEFSTR8	"U"


SIGNINPUTTBL2:	; phoneme names 2nd char or '*'
	DEFSTR8	"*****YHHEAHOHXXR"
	DEFSTR8	"XHXXXXH******XX*"
	DEFSTR8	"*H*HHX*H*HH*****"
	DEFSTR8	"YYYWWW*********X"
	DEFSTR8	"***********X**LM"
	DEFSTR8	"N"

	; phoneme flags 1
F1_NONE       		EQU	00H	; No flag
F1_PLOSIVE      	EQU	01H	; Plosive
F1_STOPCONSONANT	EQU	02H	; Stop consonant
F1_VOICED       	EQU	04H	; Voiced consonant or vowel
F1_UNKNOWN         	EQU	08H   	; TODO: M* N* NX DX Q* CH J* B* D* G* GX P* T* K* KX
					; seems to be a 'mouth opening' flag
F1_DIPTHONG        	EQU	10H	; Diphtong
F1_DIPTHONGYX      	EQU	20H	; Diphtong ending in YX (seems also used for front vowels)
					; TODO: rename F1_DIPTHONGYX to F1_FRONT
F1_CONSONANT       	EQU	40H	; Consonant
F1_VOWEL           	EQU	80H	; Vowel

PHONM_FLAGS1:	; phoneme flags 1
    	DB	F1_NONE                                                    ;   0:  '*'  = $00
    	DB	F1_NONE                                                    ;   1: '.*'  = $00
    	DB	F1_NONE                                                    ;   2: '?*'  = $00
    	DB	F1_NONE                                                    ;   3: ',*'  = $00
    	DB	F1_NONE                                                    ;   4: '-*'  = $00
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;   5: 'IY'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;   6: 'IH'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;   7: 'EH'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;   8: 'AE'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;   9: 'AA'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;  10: 'AH'  = $a4
    	DB	F1_VOICED | F1_VOWEL                                       ;  11: 'AO'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  12: 'UH'  = $84
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;  13: 'AX'  = $a4
    	DB	F1_VOICED | F1_DIPTHONGYX | F1_VOWEL                       ;  14: 'IX'  = $a4
    	DB	F1_VOICED | F1_VOWEL                                       ;  15: 'ER'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  16: 'UX'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  17: 'OH'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  18: 'RX'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  19: 'LX'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  20: 'WX'  = $84
    	DB	F1_VOICED | F1_VOWEL                                       ;  21: 'YX'  = $84
    	DB	F1_VOICED | F1_CONSONANT                                   ;  22: 'WH'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  23: 'R*'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  24: 'L*'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  25: 'W*'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  26: 'Y*'  = $44
    	DB	F1_VOICED | F1_UNKNOWN | F1_CONSONANT                      ;  27: 'M*'  = $4c
    	DB	F1_VOICED | F1_UNKNOWN | F1_CONSONANT                      ;  28: 'N*'  = $4c
    	DB	F1_VOICED | F1_UNKNOWN | F1_CONSONANT                      ;  29: 'NX'  = $4c
    	DB	F1_UNKNOWN | F1_CONSONANT                                  ;  30: 'DX'  = $48
    	DB	F1_VOICED | F1_UNKNOWN | F1_CONSONANT                      ;  31: 'Q*'  = $4c
    	DB	F1_CONSONANT                                               ;  32: 'S*'  = $40
    	DB	F1_CONSONANT                                               ;  33: 'SH'  = $40
    	DB	F1_CONSONANT                                               ;  34: 'F*'  = $40
    	DB	F1_CONSONANT                                               ;  35: 'TH'  = $40
    	DB	F1_CONSONANT                                               ;  36: '/H'  = $40
    	DB	F1_CONSONANT                                               ;  37: '/X'  = $40
    	DB	F1_VOICED | F1_CONSONANT                                   ;  38: 'Z*'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  39: 'ZH'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  40: 'V*'  = $44
    	DB	F1_VOICED | F1_CONSONANT                                   ;  41: 'DH'  = $44
    	DB	F1_UNKNOWN | F1_CONSONANT                                  ;  42: 'CH'  = $48
    	DB	F1_CONSONANT                                               ;  43: '**'  = $40
    	DB	F1_VOICED | F1_UNKNOWN | F1_CONSONANT                      ;  44: 'J*'  = $4c
    	DB	F1_VOICED | F1_CONSONANT                                   ;  45: '**'  = $44
    	DB	F1_NONE                                                    ;  46: '**'  = $00
    	DB	F1_NONE                                                    ;  47: '**'  = $00
    	DB	F1_VOICED | F1_DIPTHONG | F1_DIPTHONGYX | F1_VOWEL         ;  48: 'EY'  = $b4
    	DB	F1_VOICED | F1_DIPTHONG | F1_DIPTHONGYX | F1_VOWEL         ;  49: 'AY'  = $b4
    	DB	F1_VOICED | F1_DIPTHONG | F1_DIPTHONGYX | F1_VOWEL         ;  50: 'OY'  = $b4
    	DB	F1_VOICED | F1_DIPTHONG | F1_VOWEL                         ;  51: 'AW'  = $94
    	DB	F1_VOICED | F1_DIPTHONG | F1_VOWEL                         ;  52: 'OW'  = $94
    	DB	F1_VOICED | F1_DIPTHONG | F1_VOWEL                         ;  53: 'UW'  = $94
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  54: 'B*'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  55: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  56: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  57: 'D*'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  58: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  59: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  60: 'G*'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  61: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  62: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  63: 'GX'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  64: '**'  = $4e
    	DB	F1_STOPCONSONANT | F1_VOICED | F1_UNKNOWN | F1_CONSONANT   ;  65: '**'  = $4e
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  66: 'P*'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  67: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  68: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  69: 'T*'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  70: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  71: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  72: 'K*'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  73: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  74: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  75: 'KX'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  76: '**'  = $4b
    	DB	F1_PLOSIVE | F1_STOPCONSONANT | F1_UNKNOWN | F1_CONSONANT  ;  77: '**'  = $4b

	;phoneme flags 2
F2_NONE            	EQU	00H	; No flag
F2_PUNCTUATION     	EQU	01H	; Punctuation
;F2_NOTUSED         	EQU	02H	; not used
F2_ALVEOLAR        	EQU	04H	; Alveolar
F2_NASAL           	EQU	08H	; Nasal
F2_LIQUIDCONSONANT 	EQU	10H	; Liquid Consonant
F2_FRICATIVE       	EQU	20H	; Fricative
F2_CONSONANT       	EQU	40H	; Consonant (for punctuation - not sure if used)
F2_VOWEL           	EQU	80H	; Vowel (for punctuation - not sure if used)

PHONM_FLAGS2:	;phoneme flags 2
	DB	F2_VOWEL                                 ;  0: '*'   = $80
	DB	F2_PUNCTUATION | F2_CONSONANT | F2_VOWEL ;  1: '.*'  = $c1
	DB	F2_PUNCTUATION | F2_CONSONANT | F2_VOWEL ;  2: '?*'  = $c1
	DB	F2_PUNCTUATION | F2_CONSONANT | F2_VOWEL ;  3: ',*'  = $c1
	DB	F2_PUNCTUATION | F2_CONSONANT | F2_VOWEL ;  4: '-*'  = $c1
	DB	F2_NONE                                  ;  5: 'IY'  = $00
	DB	F2_NONE                                  ;  6: 'IH'  = $00
	DB	F2_NONE                                  ;  7: 'EH'  = $00
	DB	F2_NONE                                  ;  8: 'AE'  = $00
	DB	F2_NONE                                  ;  9: 'AA'  = $00
	DB	F2_NONE                                  ; 10: 'AH'  = $00
	DB	F2_NONE                                  ; 11: 'AO'  = $00
	DB	F2_NONE                                  ; 12: 'UH'  = $00
	DB	F2_NONE                                  ; 13: 'AX'  = $00
	DB	F2_NONE                                  ; 14: 'IX'  = $00
	DB	F2_NONE                                  ; 15: 'ER'  = $00
	DB	F2_NONE                                  ; 16: 'UX'  = $00
	DB	F2_NONE                                  ; 17: 'OH'  = $00
	DB	F2_NONE                                  ; 18: 'RX'  = $00
	DB	F2_NONE                                  ; 19: 'LX'  = $00
	DB	F2_NONE                                  ; 20: 'WX'  = $00
	DB	F2_NONE                                  ; 21: 'YX'  = $00
	DB	F2_NONE                                  ; 22: 'WH'  = $00
	DB	F2_LIQUIDCONSONANT                       ; 23: 'R*'  = $10
	DB	F2_LIQUIDCONSONANT                       ; 24: 'L*'  = $10
	DB	F2_LIQUIDCONSONANT                       ; 25: 'W*'  = $10
	DB	F2_LIQUIDCONSONANT                       ; 26: 'Y*'  = $10
	DB	F2_NASAL                                 ; 27: 'M*'  = $08
	DB	F2_ALVEOLAR | F2_NASAL                   ; 28: 'N*'  = $0c
	DB	F2_NASAL                                 ; 29: 'NX'  = $08
	DB	F2_ALVEOLAR                              ; 30: 'DX'  = $04
	DB	F2_CONSONANT                             ; 31: 'Q*'  = $40
	DB	F2_ALVEOLAR | F2_FRICATIVE               ; 32: 'S*'  = $24
	DB	F2_FRICATIVE                             ; 33: 'SH'  = $20
	DB	F2_FRICATIVE                             ; 34: 'F*'  = $20
	DB	F2_ALVEOLAR | F2_FRICATIVE               ; 35: 'TH'  = $24
	DB	F2_NONE                                  ; 36: '/H'  = $00
	DB	F2_NONE                                  ; 37: '/X'  = $00
	DB	F2_ALVEOLAR | F2_FRICATIVE               ; 38: 'Z*'  = $24
	DB	F2_FRICATIVE                             ; 39: 'ZH'  = $20
	DB	F2_FRICATIVE                             ; 40: 'V*'  = $20
	DB	F2_ALVEOLAR | F2_FRICATIVE               ; 41: 'DH'  = $24
	DB	F2_FRICATIVE                             ; 42: 'CH'  = $20
	DB	F2_FRICATIVE                             ; 43: '**'  = $20
	DB	F2_NONE                                  ; 44: 'J*'  = $00
	DB	F2_FRICATIVE                             ; 45: '**'  = $20
	DB	F2_NONE                                  ; 46: '**'  = $00
	DB	F2_NONE                                  ; 47: '**'  = $00
	DB	F2_NONE                                  ; 48: 'EY'  = $00
	DB	F2_NONE                                  ; 49: 'AY'  = $00
	DB	F2_NONE                                  ; 50: 'OY'  = $00
	DB	F2_NONE                                  ; 51: 'AW'  = $00
	DB	F2_NONE                                  ; 52: 'OW'  = $00
	DB	F2_NONE                                  ; 53: 'UW'  = $00
	DB	F2_NONE                                  ; 54: 'B*'  = $00
	DB	F2_NONE                                  ; 55: '**'  = $00
	DB	F2_NONE                                  ; 56: '**'  = $00
	DB	F2_ALVEOLAR                              ; 57: 'D*'  = $04
	DB	F2_ALVEOLAR                              ; 58: '**'  = $04
	DB	F2_ALVEOLAR                              ; 59: '**'  = $04
	DB	F2_NONE                                  ; 60: 'G*'  = $00
	DB	F2_NONE                                  ; 61: '**'  = $00
	DB	F2_NONE                                  ; 62: '**'  = $00
	DB	F2_NONE                                  ; 63: 'GX'  = $00
	DB	F2_NONE                                  ; 64: '**'  = $00
	DB	F2_NONE                                  ; 65: '**'  = $00
	DB	F2_NONE                                  ; 66: 'P*'  = $00
	DB	F2_NONE                                  ; 67: '**'  = $00
	DB	F2_NONE                                  ; 68: '**'  = $00
	DB	F2_ALVEOLAR                              ; 69: 'T*'  = $04
	DB	F2_ALVEOLAR                              ; 70: '**'  = $04
	DB	F2_ALVEOLAR                              ; 71: '**'  = $04
	DB	F2_NONE                                  ; 72: 'K*'  = $00
	DB	F2_NONE                                  ; 73: '**'  = $00
	DB	F2_NONE                                  ; 74: '**'  = $00
	DB	F2_NONE                                  ; 75: 'KX'  = $00
	DB	F2_NONE                                  ; 76: '**'  = $00
	DB	F2_NONE                                  ; 77: '**'  = $00

PHONM_FLAGS_END:		; end of phoneme flags

MODEND:				; Module ends here

;==================================================================================================
;	B U F F E R S
;==================================================================================================

	IF	CPM
FCB	  EQU	005CH
FCB_END	  EQU	007CH
	ENDIF

	IF	LSDOS6
FCB	  DS	0020H		; DOS file control block
FCB_BUF	  DS	0100H		; DOS sector buffer
UREC	  DS	0080H		; User record buffer
	ENDIF

PHONINDEX_OUT:	; phonemes table for output (PhonemeIndexOutput)
	DS	60

STRESS_OUT:	; stress output table for output (stressOutput)
	DS	60

PHONLENGTH_OUT:	; phoneme lengths table for output (phonemeLengthOutput)
	DS	100

	DS	LOW(100H-LOW $)	; Align to page
PHONEMEINDEX:	; Phonemes buffer (L9be0_phonemeIndex)
	DS	256
PHONEMEINDEXEND	EQU	$-2	; End of Phoneme Index
PHONEMELENGTHS:	; Phoneme lengths (TODO: check)
	ASSERT	$ = PHONEMEINDEX+0100H ; Must be PHONEMEINDEX+0100H
	DS	256
STRESS:	; stress value for each phoneme (L9de0_stress)
	ASSERT	$ = PHONEMEINDEX+0200H ; Must be PHONEMEINDEX+0200H
	DS	256

INPUT_BUFFER:
	DS	256		; buffer

ENGLISH_BUFFER:	; English buffer
	DS	256

SPEECH_BUFFER:
	DS	256

; The 8 following buffers must be 256-bytes and contiguous.
	DS	LOW(100H-LOW $)	; Align to page

PITCH_CONTOUR:
	DS	256

FREQUENCY1:	; Frequency 1 frames
	DS	256

FREQUENCY2:	; Frequency 2 frames
	DS	256

FREQUENCY3:	; Frequency 3 frames
	DS	256

	; Amplitude 1 frames
	; signed 4-bit values in low nibble
AMPLITUDE1:	; Amplitude 1 frames
	DS	256

AMPLITUDE2:	; Amplitude 2 frames
	DS	256

AMPLITUDE3:	; Amplitude 3 frames
	DS	256

CONSONANTFLAG:	; Consonants flags frames
	DS	256

END_FRAMES:	; End of frames data
	ASSERT	$ = PITCH_CONTOUR+0800H ;


	END	ENTRY

