// helpful labels
.const CLEAR  = $E544
.const GETIN  = $FFE4
.const SCNKEY = $FF9F

.const zptmp0 = $20
.const zptmp2 = $22
.const zptmp4 = $24
.const zptmp6 = $26

.const zparg0 = $28
.const zparg1 = $2a
.const zparg2 = $2c
.const zparg3 = $2e

.const EDIT_AT      = 0
.const EDIT_DK      = 1
.const EDIT_FRQ     = 2
.const EDIT_PULSE   = 3

:BasicUpstart2(mainStartup)

.import source "macros.asm"

//----------------------------------------------------------
//              Variables
//----------------------------------------------------------
.var            debug = true

.label          partIrqStartLine = $14
.label          musicIrqStartLine = $30

//----------------------------------------------------------
//              Main Startup Code
//----------------------------------------------------------
mainStartup:
* = mainStartup "Main Startup"

    lda #GREY
    jsr clearcolor
    jsr clearscreen
    jsr soundfx.init

    lda #0
    sta 204       // turn cursor on during a GET

    // no repeat for any keys
    // TODO add other repeater keys maybe?
    lda #64
    sta 650

    sei
    :SetupIRQ(partIrqStart, partIrqStartLine, false)
    lda #0
    sta framecount
    cli

    lda #0
    sta $d020
    sta $d021

    jsr draw_gui

infloop:
    jsr SCNKEY  //get key
    jsr GETIN   //put key in A

    cmp #' '
    bne not_space
    jmp playsound
not_space:

    cmp #'A'
    beq key_atk

    cmp #'D'
    beq key_dec

    cmp #'F'
    beq key_frq

    cmp #$9d      // cursor left is decrease value
    beq key_down

    cmp #$1d      // cursor right is increase value
    beq key_up

    jmp infloop

key_up:
    lda #1
    sta zptmp0
    lda #0
    sta zptmp0+1
    jsr key_up_down
    jmp playsound

key_down:
    lda #$ff
    sta zptmp0
    lda #$ff
    sta zptmp0+1
    jsr key_up_down
    jmp playsound

key_up_down:
    lda editing
    cmp #EDIT_AT
    bne not_at
    lda atval
    clc
    adc zptmp0
    sta atval
    rts
not_at:
    cmp #EDIT_DK
    bne not_dk
    lda dkval
    clc
    adc zptmp0
    sta dkval
    rts
not_dk:
    cmp #EDIT_FRQ
    bne not_frq
    .const DIR = 00
    add16(soundfx.ifrq, soundfx.ifrq, zptmp0)
    rts
not_frq:
    rts

key_atk:
    lda #EDIT_AT
    sta editing
    jmp playsound

key_dec:
    lda #EDIT_DK
    sta editing
    jmp playsound

key_frq:
    lda #EDIT_FRQ
    sta editing
    jmp playsound

playsound:
    jsr draw_gui

    // some voice values are mirrored in GUI code, so move from here to sound
    // player.
    lda atval
    and #$0f
    sta zptmp0
    lda dkval
    asl
    asl
    asl
    asl
    ora zptmp0
    sta soundfx.iatdk

    // See also ADSR bug:
    // http://csdb.dk/forums/?roomid=11&topicid=110547
    lda #$08
    sta $d404

    lda #0
    sta soundfx.effect
    jmp infloop

.macro drawstring(x, y, str, str_end) {
    ldx #x
    ldy #y
    sta zparg0
    lda #$00
    sta zparg3
    mov16imm(zparg1, str)
    lda #(str_end - str)
    sta zparg2
    jsr draw_str
}

.macro drawstringcol(x, y, col, str, str_end) {
    ldx #x
    ldy #y
    lda #col
    sta zparg0
    lda #$00
    sta zparg3
    mov16imm(zparg1, str)
    lda #(str_end - str)
    sta zparg2
    jsr draw_str
}

.macro drawhex4(x, y, numaddr) {
    ldx #x
    ldy #y
    sta zparg0
    mov16imm(zparg1, numaddr)
    jsr draw_hex4
}

.macro drawhex8(x, y, numaddr) {
    ldx #x
    ldy #y
    sta zparg0
    mov16imm(zparg1, numaddr)
    jsr draw_hex8
}

.macro drawhex16(x, y, numaddr) {
    ldx #x
    ldy #y
    sta zparg0
    mov16imm(zparg1, numaddr)
    jsr draw_hex16
}

editing: .byte EDIT_AT

atval: .byte 0
dkval: .byte 0

nurpastr: .text "sid editor by nurpasoft 2018"
nurpastr_end:

frqstr: .text "frq"
frqstr_end:

pulsestr: .text "pulse"
pulsestr_end:

atstr: .text "atk"
atstr_end:

dkstr: .text "dec"
dkstr_end:

.macro geteditcol(mode) {
    lda editing
    cmp #mode
    beq selected_color
    lda #0
    jmp done
selected_color:
    lda #$80
done:
    sta zparg3
    lda #WHITE
}

draw_gui:
    drawstringcol(0, 0, WHITE, nurpastr, nurpastr_end)

    drawstringcol(0, 3, GRAY, frqstr, frqstr_end)
    geteditcol(EDIT_FRQ)
    drawhex16(12, 3, soundfx.ifrq)

    drawstringcol(0, 4, GRAY, pulsestr, pulsestr_end)
    geteditcol(EDIT_PULSE)
    drawhex16(12, 4, soundfx.ipulse)

    drawstringcol(0, 5, GRAY, atstr, atstr_end)
    geteditcol(EDIT_AT)
    drawhex4(12, 5, atval)

    drawstringcol(0, 6, GRAY, dkstr, dkstr_end)
    geteditcol(EDIT_DK)
    drawhex4(12, 6, dkval)

    rts


// zparg0 = color
// zparg1 = str
// zparg2 = len
// zparg3 = invert bit ($00 or $80)
draw_str: {
    stx zptmp4
    lda #0
    sta zptmp4+1

    sty zptmp6
    sta zptmp6+1
    mulimm(zptmp6, 40)
    add16(zptmp6, zptmp6, zptmp4)

    lda zptmp6+0
    sta zptmp4+0
    lda zptmp6+1
    sta zptmp4+1

    add16_imm16(zptmp4, $00, $04)
    add16_imm16(zptmp6, $00, $d8)
    ldx zparg2
    ldy #0
loop:
    lda (zparg1),y
    ora zparg3 // invert color
    sta (zptmp4), y
    lda zparg0
    sta (zptmp6), y
    iny
    dex
    bne loop
done:
    rts
}

hex4str: .text "$0"
hex4str_end:

hex8str: .text "$00"
hex8str_end:

hex16str: .text "$0000"
hex16str_end:

hextbl: .text "0123456789abcdef"

// zparg0 = color
// zparg1 = number address
draw_hex8:
    stx zptmp4
    sty zptmp4+1
    ldy #0
    lda (zparg1), y
    and #15
    tax
    lda hextbl, x
    sta hex8str+2

    lda (zparg1), y
    lsr
    lsr
    lsr
    lsr
    and #15
    tax
    lda hextbl, x
    sta hex8str+1

    ldx zptmp4
    ldy zptmp4+1

    mov16imm(zparg1, hex8str)
    lda #(hex8str_end - hex8str)
    sta zparg2
    jsr draw_str
    rts

// zparg0 = color
// zparg1 = number address
draw_hex4:
    stx zptmp4
    sty zptmp4+1
    ldy #0
    lda (zparg1), y
    and #15
    tax
    lda hextbl, x
    sta hex4str+1
    ldx zptmp4
    ldy zptmp4+1

    mov16imm(zparg1, hex4str)
    lda #(hex4str_end - hex4str)
    sta zparg2
    jsr draw_str
    rts

// zparg0 = color
// zparg1 = number address
draw_hex16:
    stx zptmp4
    sty zptmp4+1

    ldy #0
    lda (zparg1), y
    and #15
    tax
    lda hextbl, x
    sta hex16str+4

    lda (zparg1), y
    lsr
    lsr
    lsr
    lsr
    and #15
    tax
    lda hextbl, x
    sta hex16str+3

    ldy #1
    lda (zparg1), y
    and #15
    tax
    lda hextbl, x
    sta hex16str+2

    lda (zparg1), y
    lsr
    lsr
    lsr
    lsr
    and #15
    tax
    lda hextbl, x
    sta hex16str+1

    ldx zptmp4
    ldy zptmp4+1

    mov16imm(zparg1, hex16str)
    lda #(hex16str_end - hex16str)
    sta zparg2
    jsr draw_str
    rts

clearcolor: {
    ldx #0
loop:
    sta $d800, x
    sta $d800 + $100, x
    sta $d800 + $200, x
    sta $d800 + $300, x
    inx
    bne loop
    rts
}

clearscreen: {
    ldx #0
    lda #$20
loop:
    sta $0400, x
    sta $0400 + $100, x
    sta $0400 + $200, x
    sta $0400 + $300, x
    inx
    bne loop
    rts
}

framecount: .byte 0

//----------------------------------------------------------
partIrqStart: {
                lda #0
                sta $d020
                sta $d021

                inc framecount

                :EndIRQ(musicIrqStart,musicIrqStartLine,false)
}

musicIrqStart: {
    DebugRaster(4)
//    jsr music.play

    jsr soundfx.play

    DebugRaster(0)
    :EndIRQ(partIrqStart, partIrqStartLine,false)
}

.macro DebugRaster(color) {
    .if (debug) {
        pha
        lda #0 + color
        sta $d020
        sta $d021
        pla
    }
}

.import source "soundfx.asm"
