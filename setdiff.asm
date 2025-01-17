

keebThing: 
        ld a, -1 ; hack, always say no input
        ret  ; call   0x0141???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; set difficulty level
;
setdiff:
        ld      a,(jiffy)         ; init random seed from jiffy clock
        ld      (storedseed),a


1:
         jr 2f                  ; not sure about this, lets just bypass for now
         call MAIN._joy         ; block until input, but this might catch previous input?  Testing leaves me confused
         and    00011111B
         cp     00011111B
         jr     nz,2f           ; joystick input, continue 

         /*ld     a,8
         call   keebThing
         inc    a
         jr     nz,1b           ; no keyboard, loop*/

2:       call    MAIN.cls



        ld      b,1         ; option: 0 = EASY, 1 = NORMAL, 2 = HARD

selection_loop:
        rept    10
            call vWait
        endr


        call    disparrow
        rept    20
                call vWait
        endr

1:
         call MAIN._joy         ; wait for all clear
         and    00011111B
         cp     00011111B
         jr     z,2f            ; joystick buttons are clear, exit loop

         jr     1b

         ;ld     a,8
         ;call   keebThing
         ;inc    a
         ;jr     nz,1b           ; keboard row is not clear, loop
2:

;----- keyboard
; 8 RIGHT DOWN UP LEFT DEL INS HOME SPACE
1:
         call MAIN._joy         ; l has joystick state
         ld     h,l             ; copy to h
         and    00011111B       ; mkay?  a also has joystick state I guess? So clear top bits
         cp     00011111B       ; compare to all 1's, which means to input? Z-flag is set if same
         ld     a,255           ; mkay? doesn't set flags I guess; masks out any keyboard input if we do have joystick intpu
         jr     nz,2f           ; jump if not all 1s, so we have joystick data

         ;ld     a,8             ; No joystick input, so check the keyboard I guess?
         ;call   keebThing
         ;inc    a
         ;jr     z,1b
         ;dec    a

         call getArrows
         cp 0
         jr nz, 2f

         call getKey
         cp 0x20 ; check for space
         jr z, _space_pressed
         jr 1b

2:       ld     l,a                     ; in L the keyboard and in H the joystick

         push    hl             ; we have input, so make a sound?  Maybe?
         ld      a,1                    ; plyr_hit SFX
         ld      c,0                    ;   Priority
         call    ayFX_INIT
         pop     hl

         bit    2,l                ; DOWN
         jr     nz,1f

         inc    b
         ld     a,4                ; reached last item
         cp     b
         jr     nz,1f

         ld     b,0

1:       bit    3,l                ; UP
         jr     nz,1f

         dec    b
         jp     p,1f

         ld     b,3                ; reached first item

1:       ;bit    0,l                ; space
         ;jr     z,_space_pressed

;;;;;;;

         bit    1,h                ; DOWN
         jr     nz,1f

         inc    b
         ld     a,4                ; reached last item
         cp     b
         jr     nz,1f

         ld     b,0

1:       bit    0,h                ; UP
         jr     nz,1f

         dec    b
         jp     p,1f

         ld     b,3                ; reached first item

1:       bit    4,h                ; space
         jr     z,_space_pressed


         jp selection_loop

_space_pressed:

1:
         call MAIN._joy
         and    00010000B
         jr     nz,1f

1:

         ld     a,3
         cp     b
         jr     z,manageseed

        inc     b

        ld      hl,EASY
        dec     b
        jr      z,setparam

        ld      hl,NORMAL
        dec     b
        jr      z,setparam

        ld      hl,HARD
        dec     b
        jr      z,setparam
_default:
        ld      hl,DEFAULT
setparam:

        ld      de,diffparameters+PAGE0
        ld      bc,18
        ldir

        call    MAIN.cls
        
        ret
;;;;;;;;;;;;;;;;;;;;;;;;;

manageseed:
           call    inputhex         ; init from the keyboard
           ld      (storedseed),a
           ld      b,3
           jp      selection_loop

;;;;;;;;;;;;;;;;;;;;;;;;;


EASY:   db 0x20,0x15,1,2,3,1,2,3,4,2,2,3,4,2, 5,64,128,180
DEFAULT:
NORMAL: db 0x30,0x20,1,2,3,1,1,2,3,1,1,2,3,1, 4,64,150,170
HARD:   db 0x35,0x20,1,2,3,1,1,2,3,1,1,2,3,1, 3,96,145,165


;
; _lastlevel = 0x30    ; last dungeon level (bcd)
; _dragonlevel = 0x20  ; dungeon level (bcd) where dragons are
; ; 
; _heal0 = 0x02 ; healing effect of a potion (bcd)
; _heal1 = 0x03 ; healing effect of a potion (bcd)
; _heal2 = 0x04 ; healing effect of a potion (bcd)
; _heal3 = 0x05 ; healing effect of a potion (bcd)
; ; 
; _weap0 = 0x02 ; attack bonus
; _weap1 = 0x03 ; attack bonus
; _weap2 = 0x04 ; attack bonus
; _weap3 = 0x05 ; attack bonus
; ; 
; _shld0 = 0x02 ; shield bonus
; _shld1 = 0x03 ; shield bonus
; _shld2 = 0x04 ; shield bonus
; _shld3 = 0x05 ; shield bonus
; ; 
; _chests = 0x04 ; number of chests
; ;
; _batgobac = 0xc0 ; higher value = harder to kill
; _dragonac = 0xf0
; _ghostac =  0xf7

;;;;;;;;;;;;;;;;;;;;;;;;;



disparrow:
        push    bc
        ld      de,menutext
        ld      hl,0x1800+7*32+10
        ld      b,6
        ld      a,12
        call    MAIN._printtext
        pop     bc

        push    bc
        inc     b

        ld      hl,0x1800+11*32+10            ; B == 1?
        ld      de,0x1800+11*32+21

        dec     b
        jr      z,1f

        ld      hl,0x1800+13*32+10            ; B == 2?
        ld      de,0x1800+13*32+21

        dec     b
        jr      z,1f

        ld      hl,0x1800+15*32+10            ; B == 3?
        ld      de,0x1800+15*32+21

        dec     b
        jr      z,1f

        ld      hl,0x1800+17*32+10            ; B == 4?
        ld      de,0x1800+17*32+21

1:
        ld      a,218
        call    wrtvrm

        inc     a
        ex      de,hl
        call    wrtvrm

        pop     bc
        ret


menutext:
    db  'S'+127,'E'+127,'L'+127,'E'+127,'C'+127,'T'+127,255    ,'L'+127,'E'+127,'V'+127,'E'+127,'L'+127
    db  255    ,255    ,255    ,255    ,255    ,255    ,255    ,255    ,255    ,255    ,255    ,255
    db  255    ,255    ,255    ,255    ,'E'+127,'A'+127,'S'+127,'Y'+127,255    ,255    ,255    ,255
    db  255    ,255    ,255    ,'N'+127,'O'+127,'R'+127,'M'+127,'A'+127,'L'+127,255    ,255    ,255
    db  255    ,255    ,255    ,255    ,'H'+127,'A'+127,'R'+127,'D'+127,255    ,255    ,255    ,255

    db  255    ,'S'+127,'E'+127,'E'+127,'D'+127,255    ,'I'+127,'N'+127,'P'+127,'U'+127,'T'+127,255
