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

    lda #WHITE
    jsr clearcolor
    jsr clearscreen
    jsr sound_init

    lda #0
    sta 204       // turn cursor on during a GET

    sei
    :SetupIRQ(partIrqStart, partIrqStartLine, false)
    lda #0
    sta framecount
    cli

    lda #0
    sta $d020
    sta $d021

infloop:
    jsr draw_gui

    jsr SCNKEY  //get key
    jsr GETIN   //put key in A

    cmp #' '
    beq playsound

    cmp #'A'
    beq key_atk

    cmp #'D'
    beq key_dec

    cmp #'F'
    beq frq_down
    cmp #'G'
    beq frq_up

    jmp infloop

playsound:
    // See also ADSR bug:
    // http://csdb.dk/forums/?roomid=11&topicid=110547
    lda #$08
    sta $d404

    lda #0
    sta effect
    jmp infloop

key_atk:
    lda iatdk
    clc
    adc #$10
    sta iatdk
    jmp playsound

key_dec:
    lda iatdk
    clc
    adc #1
    and #$0f
    tax
    stx zptmp4

    lda iatdk
    and #$f0
    ora zptmp4
    sta iatdk
    jmp playsound

frq_down: {
    .const DIR = -100
    add16_imm16(ifrq, DIR & 255, DIR >> 8)
    jmp playsound
}
frq_up: {
    .const DIR = 100
    add16_imm16(ifrq, DIR & 255, DIR >> 8)
    jmp playsound
}

.macro drawstring(x, y, col, str, str_end) {
    ldx #x
    ldy #y
    lda #col
    sta zparg0
    mov16imm(zparg1, str)
    lda #(str_end - str)
    sta zparg2
    jsr draw_str
}

.macro drawhex8(x, y, col, numaddr) {
    ldx #x
    ldy #y
    lda #col
    sta zparg0
    mov16imm(zparg1, numaddr)
    jsr draw_hex8
}

.macro drawhex16(x, y, col, numaddr) {
    ldx #x
    ldy #y
    lda #col
    sta zparg0
    mov16imm(zparg1, numaddr)
    jsr draw_hex16
}

nurpastr: .text "sid editor by nurpasoft 2018"
nurpastr_end:

frqstr: .text "frq"
frqstr_end:

pulsestr: .text "pulse"
pulsestr_end:

atdkstr: .text "at/dk ($ad)"
atdkstr_end:

draw_gui:
    drawstring(0, 0, WHITE, nurpastr, nurpastr_end)

    drawstring(0, 3, WHITE, frqstr, frqstr_end)
    drawhex16(12, 3, WHITE, ifrq)

    drawstring(0, 4, WHITE, pulsestr, pulsestr_end)
    drawhex16(12, 4, WHITE, ipulse)

    drawstring(0, 5, WHITE, atdkstr, atdkstr_end)
    drawhex8(12, 5, WHITE, iatdk)

    rts


// zparg0 = color
// zparg1 = str
// zparg2 = len
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
    sta (zptmp4), y
    lda zparg0
    sta (zptmp6), y
    iny
    dex
    bne loop
done:
    rts
}

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

    jsr sound_irq

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
