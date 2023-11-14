

loopframe:
           push   hl
           push   bc
           push   de

          ld     hl,(curframe)
          ld      bc,0x0000 + VDP0

          xor a
          out     (VDP1),a
          ld      a,0x18 + 64
          out     (VDP1),a

          otir                       ; plot 768 bytes
          otir
          otir
          ld  (curframe),hl

          ld  a,(numframe)
          dec a
          ld  (numframe),a

          jr  nz,intend

          ld  hl,SAMPLE_START
          ld  (curframe),hl

          ld  a,nf
          ld  (numframe),a
intend:
           pop   de
           pop   bc
           pop   hl

           ret
