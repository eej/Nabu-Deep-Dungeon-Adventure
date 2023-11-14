
; Bit    	Description
;0        Input joystick pin 1      (up)
;1        Input joystick pin 2      (down)
;2        Input joystick pin 3      (left)
;3        Input joystick pin 4      (right)
;4        Input joystick pin 6      (trigger A)
;5        Input joystick pin 7      (trigger B)

;_joy:
;		ret ; HACK
;        di
;        ld      a,15
;        out     (0xa0),a    ; read/write from r#15
;
;        in      a,(0xa2)
;        and     255-64
;        out     (0xa1),a    ; set joystick in port 1
;
;        ld      a,14
;        out     (0xa0),a    ; read/write from r#14
;
;        in      a,(0xa2)
;        ld      l,a
;        ei
;        ret

_joy:
    ;ret
joytran:  push      bc
          ld        a, (lastjoy1)
joytran3: ld        b, a
          ld        a, 0x3F
          bit       0, b                ; Left
          jr        z, joytran4
          and       0x3B
joytran4: bit       1, b                ; Down
          jr        z, joytran5
          and       0x3D
joytran5: bit       2, b                ; Right
          jr        z, joytran6
          and       0x37
joytran6: bit       3, b                ; Up
          jr        z, joytran7
          and       0x3E
joytran7: bit       4, b                ; fire!
          jr        z, joytran8
          and       0x2F
joytran8: pop       bc
          ld        l,a
          ret

