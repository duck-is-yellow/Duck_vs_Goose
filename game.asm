    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Declaration starying from mem adr $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

DuckXPos        byte ;P0
DuckYPos        byte ;P0
DuckSpritePtr   word
DuckColourPtr   word
DuckAnim        byte

BreadCount      byte
Life            byte
Temp            byte
OnesOffset      word

GooseXPos       byte ;P1
GooseYPos       byte ;P1
GooseSpritePtr  word
GooseColourPtr  word
GooseAnim       byte

BreadXPos       byte
BreadYPos       byte
BreadSpritePtr  word
BreadColourPtr  word

Random           byte
BreadCountSprite byte
LifeSprite       byte

GrassCol         byte
TerrainCol       byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Define constants.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DUCK_HEIGHT = 9
GOOSE_HEIGHT = 11
DEFAULT_HEIGHT = 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Start our ROM segment starting at $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000

Reset:
    CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Initialize RAM Variables and TIA regisers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta DuckYPos
    lda #60
    sta DuckXPos
    lda #83
    sta GooseYPos
    lda #54
    sta GooseXPos
    lda #50
    sta BreadYPos
    lda #70
    sta BreadXPos
    lda #%11010100
    sta Random
    lda #0
    sta BreadCount
    lda #3
    sta Life

    lda #$D5            ;BG col = Green
    sta GrassCol
    lda #$C1            ;Playfield col = Dark Green
    sta TerrainCol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Initialize the piinters to the correct lookup table addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<DuckRight
    sta DuckSpritePtr
    lda #>DuckRight
    sta DuckSpritePtr+1

    lda #<DuckColour
    sta DuckColourPtr
    lda #>DuckColour
    sta DuckColourPtr+1

    lda #<GooseRight
    sta GooseSpritePtr
    lda #>GooseRight
    sta GooseSpritePtr+1

    lda #<GooseColour
    sta GooseColourPtr
    lda #>GooseColour
    sta GooseColourPtr+1

    lda #<Bread
    sta BreadSpritePtr
    lda #>Bread
    sta BreadSpritePtr+1

    lda #<BreadColour
    sta BreadColourPtr
    lda #>BreadColour
    sta BreadColourPtr+1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2              ;same as %00000010
    sta VBLANK          ; turn on VBLANK
    sta VSYNC           ; turn on the VSYNC
    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC
    REPEAT 33            ; 37 minus Xpos stuff in next section.
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda DuckXPos
    ldy #0
    jsr SetObjectXPos    ; Set Duck horizontal position.

    lda GooseXPos
    ldy #1
    jsr SetObjectXPos    ; Set Goose horizontal position.

    jsr CalculateDigitsOffset

    sta WSYNC
    sta HMOVE

    lda #0
    sta VBLANK          ;Turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 20 scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF    ;Do not reflect. Dispable playfield reflection.

    lda #$38
    sta COLUBK

    lda #$1C
    sta COLUPF

    ldx #DEFAULT_HEIGHT

.DigitLoop:
    ;lda #%11110000 ;Change with bread symbol
    ;and #$F0
    ;sta BreadCountSprite

    ;ldy OnesOffset
    ;lda Digits,Y
    ;and #$0F
    ;ora BreadCountSprite
    ;sta BreadCountSprite
    ;sta WSYNC
    ;sta PF1

    lda #%11110000 ;Change with heart symbol.
    and #$F0
    sta LifeSprite

    ldy OnesOffset+1
    lda Digits,Y
    and #$0F
    ora LifeSprite
    sta LifeSprite

    jsr Sleep12Cycles ;Waste cycles for display purposes.
    sta PF1

    ldy LifeSprite   ;Preload for the next scanline.
    sta WSYNC

    sty PF1
    inc OnesOffset
    inc OnesOffset+1

    jsr Sleep12Cycles

    dex
    sta PF1
    bne .DigitLoop
    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 192 scanlines of main game. 85 with kernel in total now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameVisLines:
    lda GrassCol            ;BG col = Green
    sta COLUBK
    lda TerrainCol           ;Playfield col = Dark Green
    sta COLUPF
    lda #%00000001
    sta CTRLPF

    ; Create top border.
    lda #%11111111
    sta PF0
    sta PF1
    sta PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Main field.
    lda #%00010000        ;Always use this for a border on the playfield.
    sta PF0
    lda #0
    sta PF1
    sta PF2

    ldx #71 ;158/2 because of two line kernel
.GameLineLoop:

.AreWeInsideDuckSprite:
    txa
    sec
    sbc DuckYPos
    cmp #DUCK_HEIGHT ;are we inside sprite?
    bcc .DrawSpriteDuck ;if result is < SpriteHeight, call draw routine.
    lda #0 ;else set lookup index to 0
.DrawSpriteDuck:
    clc
    adc DuckAnim
    tay             ;Y is only register that allows us indirect addressing
    ;lda #%00000000
    ;sta NUSIZ0
    lda (DuckSpritePtr),Y    ;load duck bitmap data from lookup table.
    sta WSYNC
    sta GRP0

    lda #0
    tya
    sec
    sbc DuckAnim
    tay
    lda (DuckColourPtr),Y   ;Load duck colour data.
    sta COLUP0

.AreWeInsideGooseSprite:
    txa
    sec
    sbc GooseYPos
    cmp #GOOSE_HEIGHT
    bcc .DrawSpriteGoose
    lda #0
.DrawSpriteGoose:
    clc
    adc GooseAnim
    tay
    ;lda #%00000101
    ;sta NUSIZ1
    lda (GooseSpritePtr),Y
    sta WSYNC
    sta GRP1

    lda #0
    tya
    sec
    sbc GooseAnim
    tay
    lda (GooseColourPtr),Y
    sta COLUP1

    dex
    bne .GameLineLoop

    ; Create bottom border
    lda #%11111111
    sta PF0
    sta PF1
    sta PF2
    REPEAT 7
        sta WSYNC
    REPEND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 30 overscan lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckDuckUP:
    lda #%00010000
    bit SWCHA
    bne CheckDuckDOWN
    lda DuckYPos
    cmp #63
    bpl CheckDuckDOWN
    inc DuckYPos

    lda #0
    sta DuckAnim

CheckDuckDOWN:
    lda #%00100000
    bit SWCHA
    bne CheckDuckLEFT
    lda DuckYPos
    cmp #2
    bmi CheckDuckLEFT
    dec DuckYPos

    lda #0
    sta DuckAnim

CheckDuckLEFT:
    lda #%01000000
    bit SWCHA
    bne CheckDuckRIGHT
    ;lda DuckXPos
    ;cmp #0
    ;bmi CheckDuckRIGHT
    dec DuckXPos
    lda #DUCK_HEIGHT
    sta DuckAnim

CheckDuckRIGHT:
    lda #%10000000
    bit SWCHA
    bne DuckDefault
    lda DuckXPos
    cmp #139
    bpl DuckDefault
    inc DuckXPos
    lda #0
    sta DuckAnim

DuckDefault:
    ; Fallback when no input was performed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateGoosePos:
    lda GooseYPos
    clc
    cmp #3
    bmi .ResetGoosePos
    dec GooseYPos
    jmp EndPositionUpdate
.ResetGoosePos
    jsr GetRandGoosePos

.SetLifeValue
    sed
    lda Life
    clc
    sbc #1
    sta Life

    cld

EndPositionUpdate:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision check, frame by frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000 ;CXPPMM bit 7 depects collion between p0 and p1
    bit CXPPMM
    bne .CollisionP0P1
    jsr SetDefaultCol
    jmp EndCollisionCheck

.CollisionP0P1:
    jsr GameOver

EndCollisionCheck:     ;Fallback
    sta CXCLR      ;clear all collision flags before next frame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetDefaultCol subroutine
    lda #$D5            ;BG col = Green
    sta GrassCol
    lda #$C1            ;Playfield col = Dark Green
    sta TerrainCol
    rts

SetObjectXPos subroutine
    sta WSYNC
    sec
.Div15Loop:
    sbc #15
    bcs .Div15Loop
    eor #7
    asl
    asl
    asl
    asl
    sta HMP0,Y
    sta RESP0,Y
    rts
GameOver subroutine
    lda #$30
    sta GrassCol
    sta TerrainCol

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    lda #0
    sta BreadCount
    lda #3
    sta Life
    rts

GetRandGoosePos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random               ; performs a series of shifts and bit operations

    lsr
    sta GooseXPos           ; save it to the variable BomberXPos
    lda #4
    adc GooseXPos           ; adds 30 + BomberXPos to compensate for left PF
    sta GooseXPos           ; and sets the new value to the bomber x-position

    lda #92
    sta GooseYPos           ; set the y-position to the top of the screen

    rts

CalculateDigitsOffset subroutine
    ldx #1
.PrepareBreadCountLoop
    lda BreadCount,X    ;load A with Life (x=1) or BreadCount (X=0)
    and #$0F            ;Masking 4 last bits.
    sta Temp
    asl
    asl
    adc Temp
    sta OnesOffset,X

    dex
    bpl .PrepareBreadCountLoop

    rts


Sleep12Cycles subroutine   ;jsr takes 6 cycles and rts takes 6 cycles.
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitmaps!!
;;ROM lookup Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

DuckRight:
    .byte #%00000000
    .byte #%00011000
    .byte #%00010000
    .byte #%01111100
    .byte #%00111100
    .byte #%00001000
    .byte #%00001111
    .byte #%00001010
    .byte #%00000100
DuckLeft:
    .byte #%00000000
    .byte #%00011000
    .byte #%00001000
    .byte #%00111110
    .byte #%00111100
    .byte #%00010000
    .byte #%11110000
    .byte #%01010000
    .byte #%00100000

DuckColour:
    .byte #$00
    .byte #$38
    .byte #$38
    .byte #$1C
    .byte #$1C
    .byte #$1C
    .byte #$38
    .byte #$1C
    .byte #$1C

GooseRight:
    .byte #%00000000
    .byte #%00001100
    .byte #%00001000
    .byte #%00001110
    .byte #%00111110
    .byte #%00011110
    .byte #%00000110
    .byte #%00000100
    .byte #%00000100
    .byte #%00000111
    .byte #%00000110

GooseLeft:
    .byte #%00000000
    .byte #%00110000
    .byte #%00010000
    .byte #%00111000
    .byte #%00111110
    .byte #%00111100
    .byte #%00110000
    .byte #%00010000
    .byte #%00010000
    .byte #%01110000
    .byte #%00110000

GooseColour:
    .byte #$00
    .byte #$38
    .byte #$38
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$38
    .byte #$40

Bread:
    .byte #%00000000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%01100000

BreadColour:
    .byte #$00
    .byte #$29
    .byte #$2B
    .byte #$29
    .byte #$29

Heart:
    .byte #%00000000
    .byte #%00000000
    .byte #%01100000
    .byte #%11110000
    .byte #%10010000

HeartColour:
    .byte #$00
    .byte #$00
    .byte #$45
    .byte #$45
    .byte #$45

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COmplete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Reset
    .word Reset
