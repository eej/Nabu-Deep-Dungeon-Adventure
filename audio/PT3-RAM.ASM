		; --- THIS FILE MUST BE COMPILED IN RAM ---

		; --- PT3 WORKAREA [self-modifying code patched] ---

PT3_SETUP:		BLOCK	1	;set bit0 to 1, if you want to play without looping
					        ;bit7 is set each time, when loop point is passed
PT3_MODADDR:		BLOCK	2
PT3_CrPsPtr:		BLOCK	2  ; Patter# = CrPsPtr-song_buffer-101;
PT3_SAMPTRS:		BLOCK	2
PT3_OrnPtrs:		BLOCK	2
PT3_PDSP:		BLOCK	2
PT3_CSP:		BLOCK	2
PT3_PSP:		BLOCK	2
PT3_PrNote:		BLOCK	1
PT3_PrSlide:		BLOCK	2
PT3_AdInPtA:		BLOCK	2
PT3_AdInPtB:		BLOCK	2
PT3_AdInPtC:		BLOCK	2
PT3_LPosPtr:		BLOCK	2
PT3_PatsPtr:		BLOCK	2
PT3_Delay:		BLOCK	1
PT3_AddToEn:		BLOCK	1
PT3_Env_Del:		BLOCK	1
PT3_ESldAdd:		BLOCK	2

VARS: BLOCK 0

ChanA:			BLOCK	29			;CHNPRM_Size
ChanB:			BLOCK	29			;CHNPRM_Size
ChanC:			BLOCK	29			;CHNPRM_Size

;GlobalVars
DelyCnt:		BLOCK	1
CurESld:		BLOCK	2
CurEDel:		BLOCK	1

Ns_Base_AddToNs:    BLOCK 0
Ns_Base:		BLOCK	1
AddToNs:		BLOCK	1

AYREGS:     BLOCK 0
VT_:			BLOCK	14
EnvBase:		BLOCK	2
VAR0END:		BLOCK	240
