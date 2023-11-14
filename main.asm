
VDP0	equ	0xa0
VDP1	equ	0xa1

        IFDEF CPM
                org $100
                ld sp, 0xffee ; we need a lot of ram
        ELSE
                ORG $140D
                nop
                nop
                nop
        ENDIF

INIT:
        di
        ld a, 0xff ; High bits of interrupt vector
        ld i, a
        im 2 ; set interrupt mode 2

        ld	hl, INTERRUPT;						
        ld (0xff00 + 6), hl;
        ld      hl, KEEBINT
        ld      (0xff00 + 4), hl
			


        ld        a, 0x07             ; Select reg 7
        out       (0x41), a
        ld        a, 0x7F             ; Port A write, B read, all mixer inputs off
        out       (0x40), a
        ld        a, 0x0E             ; Select port A.
        out       (0x41), a
        ld        a, 0x30             ; VBLANK and keyboard interrupts.
        out       (0x40), a

        
        ld      a,-5
        ld      (_psg_vol_fix),a

        call    ayFX_SETUP

        ;call chgmod       ; screen 2 
        call disscr
                ; clear sprites
        ld hl,0x1B00
        call setwrt
        ld  b,0
        xor a
1:      out (VDP0),a
        djnz    1b
       

        ; easter egg check
        ld c, 30
eloop:  call vWait
        ld a, (lastkey)
        cp 0x1b ; Esc
        jp z, runegg 
        dec c
        jr nz, eloop


INIT2:


        ;call SETPAGES48K
        call intro_START
        di
        ;call RESTOREBIOS

;        call SETGAMEPAGE0
;        call intro_START
;;; out int handler stuff

        ld	hl, INTERRUPT;						
        ld (0xff00 + 6), hl;
        ld      hl, KEEBINT
        ld      (0xff00 + 4), hl

        call chgmod
        call disscr

;;; done for now
        
        call    CheckIf60Hz
        dec a
        ld (vsf),a                ; 0=>60Hz, !0=>50Hz

        ld      hl,cnt
        ld      (hl),1               ; reset the tic counter

        ;ld      hl,INTERRUPT
        ;ld      (0xFD9F+1),hl
        ;ld      a,0xC3
        ;ld      (0xFD9F),a          ; install INTERRUPT


        xor a
        ld     [PT3_SETUP],a       ; LOOP the song
        call    PT3_MUTE

        ei					       ; Enable interrupts

        jp      startAdr
;-------------------------------------
; tile data
;-------------------------------------
firepat:
           incbin intro/tiles.pat.bin.miz

;-------------------------------------
; intro data
;-------------------------------------

firecol:
           incbin intro/tiles.col.bin.miz

chgmod:
setvdpregisters:
	ld	b, setvdpdataend-setvdpdata
	ld	c, VDP1
	ld	hl, setvdpdata					;VDP data to write
	di
regloop:
	outi
	jp	nz,regloop				;Set all VDP Regiters
	ei
	ret

runegg:
        call disscr

        ld  hl, easteregg
        ld  de, 09000h
        call mom_depack_rom
        jp 09000h


easteregg:
            ;incbin   intruder4K_redux/loader.bin;,7
            incbin   intruder4K_redux/game2k.bin.miz

        include "setdiff.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
seq:
        ld  hl,(PT3_CrPsPtr)
        ld  de,(PT3_MODADDR)
        and a
        sbc hl,de
        ld  de,101+100
        and a
        sbc hl,de
        ret

KEEBINT:
        push    af
        push    bc
        push    hl
	in a,(0x90)     ; Read the first keyboard port
        cp 0x94
        jr      z, doneInt ; heartbeat
        cp 0x80         ; 0x80 == joystick1
        jr        z, joyread
	cp 0x81
	jr        z, joyread

        ld hl, arrows
        cp 0xe0 ; pushed right
        jr nz, 1f
        set 0, (hl)
        jr doneInt
1:      cp 0xf0 ; released right
        jr nz, 1f
        res 0, (hl)
        jr doneInt
1:      cp 0xe1 ; pushed left
        jr nz, 1f
        set 1, (hl)
        jr doneInt
1:      cp 0xf1 ; released left
        jr nz, 1f
        res 1, (hl)
        jr doneInt
1:      cp 0xe2 ; pushed up
        jr nz, 1f
        set 2, (hl)
        jr doneInt
1:      cp 0xf2 ; released up
        jr nz, 1f
        res 2, (hl)
        jr doneInt
1:      cp 0xe3 ; pushed down
        jr nz, 1f
        set 3, (hl)
        jr doneInt
1:      cp 0xf3 ; released down
        jr nz, 1f
        res 3, (hl)
        jr doneInt

        ; maybe some ascii?
1:      ld (lastkey), a
        jr doneInt

joyread:
	in        a, (0x91)           ; Wait for the data byte to appear
        bit       1, a
        jr        z, joyread
        in        a, (0x90)           ; Read joystick
        ld        (lastjoy1), a
doneInt:
        pop hl
        pop bc
        pop af
        ei
        reti        

lastjoy1: db        0x00
lastkey: db         0xff
arrows: db          0x00

checkInput:
        push bc
        ld a, (arrows)
        ld b, a
        ld a, (lastkey)
        and b
        pop bc
        ret

getKey:
        push bc
        ld a, (lastkey)
        ld b, a
        ld a, 0xff
        ld (lastkey), a
        ld a, b
        pop bc
        ret

waitKey:
        call getKey
        inc a
        jr z, 1f
        inc a
        ret
1:      halt
        jr waitKey

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INTERRUPT:
        push af
        push bc
        push de
        push hl
        push ix
        push iy
        exx
        ex      af,af'
        push    hl
        push    de
        push    bc
        push    af

          ld hl, (jiffy)  ; inc jiffy count
          inc hl
          ld (jiffy), hl

          ld      a,(vsf)
          and     a
          jp      nz,PAL               ; if PAL call at any interrupt

NTSC:
          ld      hl,cnt               ; if NTSC call 5 times out of 6
          dec     (hl)
          jp      nz,PAL               ; skip one tic out of 6 when at 60hz

          ld      (hl),6               ; reset the tic counter
          jr    INTDONE

PAL:                             ; execute the PSG and ayFX core

        call    updateAni

        ; --- Place this instruction on interrupt or after HALT instruction to synchronize music ---
        call	PT3_ROUT			; Write values on PSG registers

	    ; --- To speed up VDP writes you can place this instruction after all of them, but before next INT ---
        call	PT3_PLAY			; Calculates PSG values for next frame


		; --- You can place here your favourite FX system and write the values to AYREGS label ---
        call	ayFX_FRAME			; Calculates PSG values for next frame

		; --- so on next frame the FX will be played automatically when calling PT3_ROUT       ---
INTDONE:

        ld a, 1
        ld (onSync), a
        in	a, (VDP1)

        pop     af
        pop     bc
        pop     de
        pop     hl
        ex      af,af'
        exx
        pop iy
        pop ix
        pop hl
        pop de
        pop bc
        pop af

        ei
        reti

vWait:
    xor a
    ld (onSync), a
1:  halt
    ld a, (onSync)
    cp 0
    jr z, 1b
    ret

;;;;;;;;;;;;;;;;;;;;;
;; Keyboard testing
;; ayFX
;
;        ld	a,(ayFX_PRIORITY)
;        cp      255
;        ret     nz              ; play only if no sfx is active
;
;;  0 "7" "6" "5" "4" "3" "2" "1" "0"
;
;        ld      e,0
;        call    checkkbd
;
;        ld      b,8
;        ld      c,a
;1:
;        ld      a,b
;        dec     a
;        ld      l,a
;
;        ld      a,c
;        add     a,a
;        ld      c,a
;
;        push    bc
;        ld      a,l
;        ld      c,0            ;   Priority
;
;        call    nc,ayFX_INIT
;        pop     bc
;        djnz    1B
;        ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; returns 1 in a and clears z flag if vdp is 60Hz
;;
CheckIf60Hz:	; Always 60 on Nabu
        ld      a, 1
	ret

;-------------------------------------
; checkkbd: ckeck keyboard line
; syntax:checkkbd <keyboar line #>
; in:  e
; out: l
;-------------------------------------

/*i8255portb  equ 0a9h        ; keyboard column input
i8255portc  equ 0aah        ; leds, motor, cassette, kbd line

checkkbd:
        in  a,(i8255portc)
        and 011110000B          ; upper 4 bits contain info to preserve
        or  e
        out (i8255portc),a
        in  a,(i8255portb)
        ld  l,a
        ret*/


        include intro/introplayer.asm

		; --- INCLUDE ACTUAL GAME code ---


        INCLUDE	"WHACK_Z80.ASM"

        INCLUDE hitanimation.asm



		; --- INCLUDE PT3-ROM.ASM in ROM code ---

REPLAYER:	INCLUDE	"audio/PT3-ROM.ASM"

		; --- INCLUDE MUSIC in ROM code (don't forget to strip first 100 bytes of PT3 module ---


MUSIC0:		INCLUDE	"audio/music0.asm"
MUSIC1: 	INCLUDE	"audio/music3.asm"
MUSIC2:		INCLUDE	"audio/music2.asm"
MUSIC3:		INCLUDE	"audio/music1.asm"
MUSIC4:		INCLUDE	"audio/music4.asm"
MUSIC5:		INCLUDE	"audio/music5.asm"
MUSIC6:		INCLUDE	"audio/music6.asm"
;-------------------------------------
;
;      AYFX replayer
;
;-------------------------------------
;
; --- INCLUDE ayFX-ROM.ASM

        INCLUDE "audio/ayFX-ROM.ASM"

; --- INCLUDE ayFX - data

ayFX_STREAMS:
        dw sfx0,  sfx1, sfx2, sfx3, sfx4, sfx5, sfx6, sfx7,sfx8,sfx9,sfx10,sfx11

sfx0:   incbin   "audio/afx/04_chest.afx"
sfx1:   incbin   "audio/afx/05_coin.afx"
sfx2:   incbin   "audio/afx/01_stairdwn.afx"
sfx3:   incbin   "audio/afx/02_stairup.afx"
sfx4:   incbin   "audio/afx/03_opendoor.afx"
sfx5:   incbin   "audio/afx/06_powerup.afx"
sfx6:   incbin   "audio/afx/07_potionsm.afx"
sfx7:   incbin   "audio/afx/08_potionbg.afx"
sfx8:   incbin   "audio/afx/09_enehit_1.afx"
sfx9:   incbin   "audio/afx/10_enehit_2.afx"
sfx10:  incbin   "audio/afx/11_plyr_hit.afx"
sfx11:  incbin   "audio/afx/plink2.afx"


        
;-------------------------------------
; Padding, align rom image to a power of two.
;-------------------------------------

		;ds	$C000 - $

MAXLASTLEVEL = 60
MAXCHESTS    = 5

		; --- RAM SECTION ---
        ;MAP 0xc000




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setvdpdata:
                db #02,#80 ; mode 2
                db #e0,#81 ; 16k, screen enabled, gen interrupts, (not mode 1 or 3)
                db #06,#82 ; PN to 0x1800
                db #9f,#83 ; use single CT(?) starting at 0x2000
                db #00,#84 ; use single PT starting at 0x0
                db #36,#85 ; sprite attribute table at $1b00
                db #07,#86 ; sprite gen table at $3800
                db #01,#87 ; FG and BG colour
setvdpdataend:

vsf         db 1          ; 0 if 50Hz, !0 if 60Hz
cnt         db 1          ; couter to compensate NTSC machines

	; --- INCLUDE PT3-RAM.ASM in RAM section (no code output, but label definitions) ---

        INCLUDE	"audio/PT3-RAM.ASM"
        INCLUDE "audio/ayFX-RAM.ASM"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

onSync db 1


;nround:;      BLOCK 3
;nround = 0
nround:
PAGE0 = nround+3
;PAGE0       BLOCK 256        ; emulate 6502 page 0
;buffer = PAGE0 + 256
;buffer      BLOCK 2048       ; buffer for unpack
hidden  = PAGE0 + 256
;hidden  = buffer + 2048
;hidden      BLOCK 1024       ; hidden level (emulate VIC color ram)
visible = hidden + 1024
;visible     BLOCK 1024       ; visible area (emulate VIC color ram)
;dummy:
dummy = visible + 1024
;dummy       BLOCK 100
        IFDEF CPM
MUSICBUFF = dummy + 100
        ELSE        
MUSICBUFF = 0
        ENDIF
;MUSICBUFF = dummy + 100
;MUSICBUFF   BLOCK 4096

storedseed = MUSICBUFF + 4096
;storedseed  BLOCK 1

levels = storedseed + 1
;levels      BLOCK 256        ; dungeon level pseudo-random seeds

seeds = levels + 256
;seeds       BLOCK MAXLASTLEVEL              ; store the seed for each level
memchst = seeds + MAXLASTLEVEL
;memchst     BLOCK MAXLASTLEVEL              ; store the chest status in each floor of the dungeon
locchst = memchst + MAXLASTLEVEL
;locchst     BLOCK 2*MAXLASTLEVEL*MAXCHESTS  ; store the chest position in each floor of the dungeon

        IFDEF CPM
buffer = locchst + 2*MAXLASTLEVEL*MAXCHESTS
        ELSE
buffer = dummy + 100
        ENDIF

UNPACK = nround - (nround % 0x100) + 0x100 ; this wants to be aligned to a 0x100 boundary apparently?
        ;ENDMAP
