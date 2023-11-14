
; VDP ports
;
vdpport0    equ 0a0h        ; VRAM read/write
vdpport1    equ 0a1h        ; VDP registers read/write
VDP_DATA    equ vdpport0

PSG_REGS  equ $41
PSG_DATA  equ $40

PNT   equ    1800h ; Pattern Name Table
PGT   equ    0000h ; Pattern Generator Table
PCT   equ    2000h ; Pattern Color Table
SAT   equ    1b00h ; Sprite Attribute Table
SPT   equ    3800h ; Sprite Pattern Table



;Ln B_7 B_6 B_5 B_4 B_3 B_2 B_1 B_0
; 0 "7" "6" "5" "4" "3" "2" "1" "0"
; 1 ";" "]" "[" "\" "=" "-" "9" "8"
; 2 "B" "A" ??? "/" "." "," "'" "`"
; 3 "J" "I" "H" "G" "F" "E" "D" "C"
; 4 "R" "Q" "P" "O" "N" "M" "L" "K"
; 5 "Z" "Y" "X" "W" "V" "U" "T" "S"
; 6 F3 F2  F1 CODE CAP GRAPH CTR SHIFT
; 7 RET SEL BS STOP TAB ESC F5  F4
; 8 RIGHT DOWN UP LEFT DEL INS HOME SPACE


;; MSX bios calls

wrtvdp:   di
          ld        a, b
          out       (0xA1), a
          ld        a, c
          or        0x80
          out       (0xA1), a
          ei
          ret

wrtvrm:   di
          push      af
          ld        a, l
          out       (0xA1), a
          ld        a, h
          and       0x3F
          or        0x40
          out       (0xA1), a
          pop       af
          out       (0xA0), a
          ei
          ret

setrd:    di
          ld        a, l
          out       (0xA1), a
          ld        a, h
          and       0x3F
          out       (0xA1), a
          ei
          ret

setwrt:   di
          ld        a, l
          out       (0xA1), a
          ld        a, h
          and       0x3F
          or        0x40
          out       (0xA1), a
          ei
          ret

enascr: 
        di
        ld a, 0b11100010
        out (vdpport1), a
        ld a, 0x81
        out (vdpport1), a
        ei
        ret

disscr:
        di
        ld a, 0b10100010
        out (vdpport1), a
        ld a, 0x81
        out (vdpport1), a
        ei
        ret


;--------------------------------
; $0056 FILVRM
; Function : fill VRAM with value
; Input    : A  - data byte
;            BC - length of the area to be written
;            HL - start address:
;                 * SCREEN 0..4 -> 14-bit address
;                 * SCREEN 5+ -> 17-bit address (uses ACPAGE)
;                 Using 14-bit address for SCREEN4 doesn't really make sense,
;                 but that's what we have to follow to be compatible.
; Registers: AF, BC
filvrm:
                push    af
                call    setwrt
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
                ; Note: Interrupts should be enabled here.
                ;       Penguin Adventure can hang on boot if the interrupt
                ;       comes just after our RET, which is certain if the
                ;       memory block written is large enough.
filvrm_lp:
                out     (VDP_DATA),a
                ; wait (at least) 29 t-states between VRAM accesses
                dec b
                jr      nz,filvrm_lp
                dec     c
                jr      nz,filvrm_lp
                ret


;--------------------------------
; $0059 LDIRMV
; Function : Block transfer from VRAM to memory
; Input    : BC - blocklength
;            DE - Start address of memory
;            HL - Start address of VRAM
; Registers: AF BC DE
; Note     : the function doesn't destroy HL
; Note     : the routine doesn't change IM
ldirmv:
                call    setrd
                push    hl
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
ldirmv_lp:
                ; wait (at least) 29 t-states between VRAM accesses
                ini
                jr nz, ldirmv_lp
                dec     a
                jr      nz,ldirmv_lp
                pop     hl
                ret


; $005C LDIRVM
; Function : Block transfer from memory to VRAM
; Input    : BC - blocklength
;            DE - Start address of VRAM
;            HL - Start address of memory
; Note     : the routine doesn't change IM
; Registers: All
ldirvm:
                ex      de,hl
                call    setwrt
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
ldirvm_lp:
                ; wait (at least) 29 t-states between VRAM accesses
                outi
                jr nz, ldirvm_lp
                dec     a
                jr      nz,ldirvm_lp
                ; Note: Without this, Quinpl shows glitches.
                ; TODO: Investigate why.
                ex      de,hl
                ret

;--------------------------------
; $0093 WRTPSG
; Function : Writes data to PSG-register
; Input    : A  - PSG register number
;            E  - data write
WRTPSG:
                di
                out     (PSG_REGS),a
                push    af
                ld      a,e
                out     (PSG_DATA),a
                ei
                pop     af
                ret

;--------------------------------
; $0096 RDPSG
; Function : Reads value from PSG-register
; Input    : A  - PSG-register read
; Output   : A  - value read
RDPSG:
                out     (PSG_REGS),a
                in      a,(PSG_DATA)
                ret

;--------------------------------
; $0020 DCOMPR
; Function : Compared HL to DE
; Output   : flags influenced like CP instruction
; Registers: A
dcompr:
                ld      a,h
                cp      d
                ret     nz
                ld      a,l
                cp      e
                ret

;--------------------------------
; $0090 GICINI  Initialize Sound IC
; Function : Initialises PSG and sets initial value for the PLAY statement
; Registers: All
GICINI:
                ld      e,$00
                ld      a,$08
                call    WRTPSG
                inc     a
                call    WRTPSG
                inc     a
                call    WRTPSG
                inc     a

                ;ld      e,$B8
                ld      e, $78
                ld      a,$07
                call    WRTPSG

                ret

;wrtvdp: equ $0047
;rdvrm:  equ $004a
;wrtvrm: equ $004d
;setwrt: equ $0053
;filvrm: equ $0056
;ldirmv: equ $0059
;ldirvm: equ $005c
;chgmod: equ $005F
;clrspr: equ $0069
;initxt: equ $006c
;init32: equ $006f

;GICINI: equ $0090

;chput:  equ $00a2
;erafnk: equ $00cc
;gtstck: equ $00d5
;gttrig: equ $00d8
;breakx: equ $00b7
;cls:    equ $00c3



;linl40: equ $f3ae
;linl32: equ $f3af
;cnsdfg: equ $f3de
;csry:   equ $f3dc
;csrx:   equ $f3dd
;forclr: equ $f3e9
;bakclr: equ $f3ea
;bdrclr: equ $f3eb
;rndx:   equ $f857
;htimi:  equ $fd9f
;rg1sav: equ $f3e0
;crtcnt: equ $f3b1

;WRTPSG    equ     00093h
;RDPSG     equ     00096h

;JIFFY     equ     0FC9Eh

;RG0SAV:   equ     0F3DFH
;RG1SAV    equ     0F3E0H
;RG8SAV:   equ     0F3E7H        ; VDP Register 8 Save copy.

RG8SAV db 0

  STRUCT NPCS_DATA
X              ds 2
Y              ds 2
Xsize          ds 1 ; Xsize
Ysize          ds 1 ; Ysize

DX             ds 1
DY             ds 1

FRAME          ds 1
CLR            ds 1

STATUS         ds 1
TYPE           ds 1
COUNT          ds 1
;FLAG           ds 1         ;   b7,b6,b5,b4,b3,b2,b1,b0    
;                            ;   b0 up
;                            ;   b1 down
;                            ;   b2 right
;                            ;   b3 left
  ENDS

SWITCH:        equ 239
DIGITS:        equ 240
NNPCS:         equ 31
NRBTS:         equ 2
NBLLTS:        equ NNPCS-NRBTS

cell_len_X:    equ 10
cell_len_Y:    equ 8

vroomsize      equ 64

MPNT:          equ 0C800h
VMPNT:         equ MPNT+vroomsize*32

nodeA_coord:   equ 1*cell_len_X+1*cell_len_Y*vroomsize+MPNT + vroomsize/2
nodeB_coord:   equ 2*cell_len_X+1*cell_len_Y*vroomsize+MPNT + vroomsize/2 + 1
nodeC_coord:   equ 2*cell_len_X+2*cell_len_Y*vroomsize+MPNT + vroomsize/2 + 1
nodeD_coord:   equ 1*cell_len_X+2*cell_len_Y*vroomsize+MPNT + vroomsize/2


MAZEX:   equ 4
MAZEY:   equ 4

