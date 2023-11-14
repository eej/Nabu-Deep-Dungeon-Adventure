ntiles:     equ     256
nf:         equ     14

animdealay  equ     7

;-------------------------------------
; Entry point
;-------------------------------------
intro_START:
            ld	    hl, vblankir;						
            ld      (0xff00 + 6), hl;
			
            xor     a
            ld      (animate),a

            call    RESET_VDP

            ld      a,animdealay
            ld      (delframe),a
            ld      (animate),a

            ld      hl,SAMPLE_START
            ld      (curframe),hl

            ld      a,nf
            ld      (numframe),a

            ei

            ld      b,60*1
            call    wait

            call    fadeinloop

            ld      b,60*4
            call    wait

            call    fadeoutloop

            ld      b,60*2
            call    wait

            ret




;-------------------------------------
; INTERRUPT CODE
;-------------------------------------

vblankir:
          ;ds	$0038 - $
          push    af
          ld      a,(delframe)
          dec     a
          ld      (delframe),a
          jr      nz,1f

          ld      a,animdealay
          ld      (delframe),a

          ld      a,(animate)
          and     a
          call    nz,loopframe

1:
          ld a, 1
          ld (onSync), a
	  in	a, (VDP1)
          pop     af
          ei
          reti


; Input

skipInput:
                push    af
                push    hl
		in a,(0x90)
		cp 0x80
		jr        z, joyr
		cp 0x81
		jr        z, joyr
                cp 32
                jr      z, setSkip
		jr	donein

joyr:
		in        a, (0x91)           ; Wait for the data byte to appear
                bit       1, a
                jr        z, joyr
                in        a, (0x90)           ; Read joystick
                and     0x10
                jr      z, donein
setSkip:
        ld      hl, doSkip
        ld      (hl), 1
donein:		
                pop     hl
                pop     af
		ei
		reti

doSkip:         dw      0

;-------------------------------------
;
;-------------------------------------
fadeoutloop: 
            ld      b,7
1:          exx
            call    fadeout
            exx
            djnz    1b
            ret

;-------------------------------------
;
;-------------------------------------
fadeinloop: 
            ld      b,7
1:          exx
            call    fadein
            exx
            djnz    1b
            ret
;-------------------------------------
;
;-------------------------------------
wait:
1:          call vWait
            call    skip
            djnz    1b
            ret
skip:

            call MAIN._joy
            and    00010000B
            jr     z,2f        ; skip on joystick button
            ld      e,8

            ;call    checkkbd  ; skip on space
            ;and    1
            call getKey
            cp 0x20
            jr     z,2f
            ret

2:          ld b,1
            ret

;-------------------------------------
; Instead of CHGMOD
;-------------------------------------

_vdpinit:   di
            ld hl,_vdpregs
            ld bc,0x8000 + VDP1
1:          outi
            inc b
            out (c),b
            inc b
            bit 3,b
            jr z,1b
            ret

_vdpregs:
            defb 0x02 ; Reg# 0 000000[M3][EV]
            defb 0xE2 ; Reg# 1 [4/16k][BLANK][IE][M1][M2]0[SIZE][MAG]
            defb 0x06 ; Reg# 2 0000[NAME TABLE BASE ADDRESS]          = 1800h
            defb 0xFF ; Reg# 3 [COLOR BASE ADDRESS]                   = 2000h     ; regular mode for colors
            defb 0x03 ; Reg# 4 00000[PATTERN GENERATOR BASE ADDRESS]  = 0000h     ; regular mode for patterns

            defb 0x36 ; Reg# 5 0[SPRITE ATTRIBUTE TABLE BASE ADDRESS] = 1b00h
            defb 0x07 ; Reg# 6 00000[SPRITE PTRN GNRTR BASE ADDRESS]  = 3800h
            defb 0x01 ; Reg# 7 [TEXT COLOR 4bts][BACKDROP COLOR 4bts]

;-------------------------------------
; fading data
;-------------------------------------

colorfadingout:
            db 0,0,4,13,5,7,14,15

colorfadingin:
            db 15,14,7,5,13,4,0,0


;-------------------------------------
; fading in & out
;-------------------------------------
        include fadeinout.asm

;-------------------------------------
; support i/o code
;-------------------------------------

outs768:

        ld      de,ntiles*8*3
2:
        ld      c,VDP0

1:      outi
        dec     e
        jr      nz,1b
        dec     d
        jr      nz,1b

        ret

        nop

;-------------------------------------
; Resets the vdp
;-------------------------------------

RESET_VDP:
        call disscr

        IFDEF CPM
                ld      de,UNPACK
                ld      hl,firepat
                call    mom_depack_rom
                ld      hl,firecol
                call    mom_depack_rom

                ld      hl,UNPACK;  patterns and colors

                xor     a
                out     (VDP1),a
                ld      a,0x0000/256 + 0x40 ; PGT
                out     (VDP1),a

                call    outs768

                xor     a
                out     (VDP1),a
                ld      a,0x2000/256 + 0x40 ; PCT
                out     (VDP1),a

                call    outs768

        ELSE
                ld      de,UNPACK
                ld      hl,firepat
                call    mom_depack_rom


                ld      hl,UNPACK

                xor     a
                out     (VDP1),a
                ld      a,0x0000/256 + 0x40 ; PGT
                out     (VDP1),a

                call    outs768

                ld hl, UNPACK + 256 ; we're tight on ram, so copy the pattern block we need for animation
                ld de, 0            ; to the bottom of memory where there is some empty space 
                ld bc, 256*8
                ldir

                ld      de,UNPACK
                ld      hl,firecol
                call    mom_depack_rom

                ld      hl,UNPACK

                xor     a
                out     (VDP1),a
                ld      a,0x2000/256 + 0x40 ; PCT
                out     (VDP1),a

                call    outs768

        ENDIF

;-------------------------------------
;       expand frames
;-------------------------------------

          ld  hl,SAMPLE_START
          ld  (curframe),hl

          ld  a,nf
          ld  (numframe),a

          call    loopframe           ; set first frame

          ei
          ld      b,1
          exx
          call    fadeout

            
          call    _vdpinit
          ret

;-------------------------------------
; replayer core
;-------------------------------------

; PLAY_SAMPLE:
; 
; 2:      call    setframe
; 
;         ld      e,8
;         push    hl
;         call    checkkbd               ; test space or any arrow
;         pop     hl
;         inc     a
;         ret     nz
; 
;         exx
;         dec     e
;         exx
; 
;         jp     nz,2b               ; last frame
; 
;         ret



; -------------------------------------------------------
; MSX-O-Mizer v1.5f datas depacker    *ROM based version*
; Improved from Metalbrain's z80 version.
; -------------------------------------------------------
; source in hl
; dest in de

; 328 bytes which must be aligned on 8 bits boundary
mom_map_bits_rom    =       0xF800
; 26 bytes located in ram
mom_offset_table    =       0xF800 + 328

mom_depack_rom:     push    de
                    ld      bc, mom_offset_table
                    push    bc
                    ld      de, bc
                    ld      bc, 26
                    ldir                        
                    push    hl
                    pop     af
                    pop     hl
                    push    af
                    ld      iy, mom_map_bits_rom + 0xf0
                    ld      b, 52
mom_init_bits_rom:  ld      a, iyl
                    and     15
                    jr      nz, mom_node_rom
                    ld      de, 1
mom_node_rom:       rrd
                    ld      (iy), a
                    ld      (iy + 36), e
                    ld      (iy + 72), d
                    inc     iyl
                    inc     a
                    push    hl
                    ld      hl, 0
                    scf
mom_set_bit_rom:    adc     hl, hl
                    dec     a
                    jr      nz, mom_set_bit_rom
                    add     hl, de
                    ex      de, hl
                    pop     hl
                    bit     0, b
                    jr      z, mom_wait_step_rom
                    inc     hl
mom_wait_step_rom:  djnz    mom_init_bits_rom
                    pop     hl
                    ld      a, (hl)
                    inc     hl
                    ld      ixh, a
                    pop     de
mom_lit_copy_rom:   ldi
mom_main_loop_rom:  call    mom_get_bit_rom
                    jr      c, mom_lit_copy_rom
                    ld      c, -17
mom_get_index_rom:  call    mom_get_bit_rom
                    inc     c
                    jr      nc, mom_get_index_rom
                    ld      a, c
                    ret     z
                    push    de
                    call    mom_get_pair_rom
                    push    bc
                    jr      nz, mom_out_range_rom
                    ld      de, 0x0220
                    dec     c
                    jr      z, mom_go_for_it_rom
                    ld      de, 0x0410
                    dec     c
                    jr      z, mom_go_for_it_rom
mom_out_range_rom:  ld      de, 0x0400
mom_go_for_it_rom:  pop     af
                    ex      af, af'
                    call    mom_get_bits_rom
                    add     a, e
                    call    mom_get_pair_rom
                    pop     de
                    push    hl
                    ld      h, d
                    ld      l, e
                    sbc     hl, bc
                    ex      af, af'
                    push    af
                    pop     bc
                    ldir
                    pop     hl
                    jr      mom_main_loop_rom


mom_get_pair_rom:   ld      iyl, a
                    ld      d, (iy)
                    call    mom_get_bits_rom
                    add     (iy + 36)
                    ld      c, a
                    ld      a, b
                    adc     (iy + 72)
                    ld      b, a
                    ret


mom_get_bits_rom:   ld      bc, 0
mom_getting_bits_rom:
                    dec     d
                    ld      a, c
                    ret     m
                    call    mom_get_bit_rom
                    rl      c
                    rl      b
                    jr      mom_getting_bits_rom


mom_get_bit_rom:    ld      a, ixh
                    add     a
                    jr      nz, mom_byte_done_rom
                    ld      a, (hl)
                    inc     hl
                    rla
mom_byte_done_rom:  ld      ixh, a
                    ret

;-------------------------------------
;
;-------------------------------------
            include irg.asm
;-------------------------------------
; tile data
;-------------------------------------
;firepat:
;            incbin tiles.pat.bin.miz

;-------------------------------------
; intro data
;-------------------------------------

;firecol:
;            incbin tiles.col.bin.miz

;-------------------------------------
; Sample data
;-------------------------------------
SAMPLE_START:

        incbin frames.bin

SAMPLE_END:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;        MAP 0xF100

curframe dw 2
numframe dw 1
delframe dw 1
animate  dw 1

;        ENDMAP

FINISH:













