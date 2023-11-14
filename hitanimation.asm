
/*

This is implimented in a pretty sketchy and bad way. You shouldn't change the
VDP's address pointers during an interrupt because there is no way to save and
restore it. I get away with it here because the timing works out that the update
doesn't conflict with the main game loop accessing the vdp.

*/
animInit:
    ld a, 0
    ld (framecount), a

    call vWait

    ld hl, $3800
    call setwrt

    ld b, 32 ; empty frame at 0
    ld a, 0
1:  out (VDP0), a
    djnz 1b

    ld hl, blood1
    ld c, VDP0

    ld d, 2

2:  ld b, 8
1:  outi
    jr nz, 1b

    ld b, 24
    xor 0
1:  out (VDP0), a
    djnz 1b

    dec d       ; load multiple sprites
    jr nz, 2b


    ld hl, $1b00 + 2*4
    call setwrt
    ld a, 12
    out (VDP0), a
    ld a, 10
    out (VDP0), a
    ld a, 4 ; pattern 4 (really pattern 1 because 16x16 sprites)
    out (VDP0), a
    ld a, 0 ; transparent
    out (VDP0), a

    ld a, 12
    out (VDP0), a
    ld a, 10
    out (VDP0), a
    ld a, 8 ; pattern 8 (really pattern 2 because 16x16 sprites)
    out (VDP0), a
    ld a, 0 ; transparent
    out (VDP0), a
    ld a, $d0 ; disable further sprites
    out (VDP0), a

    ret

updateAni:
    ld a, (framecount)
    cp 0
    ret z
    dec a
    ld (framecount), a

    cp 9 ; show sprite after short delay
    jr nz, 1f

    ld hl, $1b00 + 2*4 + 3
    call setwrtint
    ld a, 15 ; white
    out (VDP0), a

    ld hl, $1b00 + 2*4 + 7
    call setwrtint
    ld a, 6 ; red
    out (VDP0), a

    jr 2f

1:  cp 0 ; hide sprite at end of countdown
    jr nz, 2f

    ld hl, $1b00 + 2*4 + 3
    call setwrtint
    ld a, 0 ; transparent
    out (VDP0), a
    out (VDP0), a
    out (VDP0), a
    out (VDP0), a

2:  ret


hitManAni:
    ld a, (framecount)
    cp 0
    ret nz

    call vWait

    ld hl, $1b00 + 2*4
    call setwrt

    ld a, (ix+many)
    dec a
    ld c, a
    xor a
    ld b, 8
1:  add c
    djnz 1b
    dec a
    dec a
    dec a
    out (VDP0), a
    ld d, a


    ld a, (ix+manx)
    ld c, a
    xor a
    ld b, 8
1:  add c
    djnz 1b
    dec a
    out (VDP0), a

    ld e, a

    ld a, 4 ; sprite pattern
    out (VDP0), a
    ld a, 0 ; 
    out (VDP0), a

    ld a, d
    out (VDP0), a

    ld a, e
    out (VDP0), a
    ld a, 8
    out (VDP0), a
    ld a, 0
    out (VDP0), a

    ld a, 11 ; framecount
    ld (framecount), a

    ret

setwrtint:
          ld        a, l
          out       (0xA1), a
          ld        a, h
          and       0x3F
          or        0x40
          out       (0xA1), a
          ret    


framecount db 0

; Sprite data

blood1:
    db 00000000b
    db 00000000b
    db 00000000b
    db 00000000b
    db 00000000b
    db 00100000b
    db 00000000b
    db 00000000b

blood2:
    db 00010000b
    db 00010000b
    db 00111000b
    db 00111000b
    db 01111100b
    db 01111100b
    db 01111100b    
    db 00111000b

