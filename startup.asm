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
.const zparg4 = $30

.const EDIT_AT       = 0
.const EDIT_DK       = 1
.const EDIT_SUS      = 2
.const EDIT_REL      = 3
.const EDIT_DURATION = 4
.const EDIT_FRQ      = 5
.const EDIT_PULSE    = 6

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

    // no repeat for some keys (but leaves cursor key repeat)
    lda #0
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

    cmp #'A'
    bne not_atk
    lda #EDIT_AT
    jmp save_edit_and_play
not_atk:
    cmp #'D'
    bne not_dec
    lda #EDIT_DK
    jmp save_edit_and_play
not_dec:
    cmp #'S'
    bne not_sus
    lda #EDIT_SUS
    jmp save_edit_and_play
not_sus:
    cmp #'R'
    bne not_rel
    lda #EDIT_REL
    jmp save_edit_and_play
not_rel:
    cmp #'F'
    bne not_frq
    lda #EDIT_FRQ
    jmp save_edit_and_play
not_frq:
    cmp #'L'
    bne not_dur
    lda #EDIT_DURATION
    jmp save_edit_and_play
not_dur:

    cmp #$9d      // cursor left is decrease value
    beq key_down
    cmp #$1d      // cursor right is increase value
    beq key_up

    cmp #$5b      // non-repeating
    beq key_down
    cmp #$5d      // non-repeating
    beq key_up

    cmp #' '
    bne not_space
    jmp playsound
not_space:

    sec
    sbc #'1'
    cmp #4
    bcc key_number
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

// assumes x=voice
.macro addparam8(param, dir) {
    clc
    lda param,x
    adc dir
    sta param,x
}

// assumes x=voice
.macro addparam16(param, dir) {
    txa
    asl
    tax
    clc
    lda param,x
    adc dir
    sta param,x
    inx
    lda param,x
    adc dir+1
    sta param,x
    txa
    lsr
    tax
}

key_number:
    sta voice
    jmp playsound

key_up_down: {
    ldx voice
    lda editing
    cmp #EDIT_AT
    bne not_at
    addparam8(atval, zptmp0)
    rts
not_at:
    cmp #EDIT_DK
    bne not_dk
    addparam8(dkval, zptmp0)
    rts
not_dk:
    cmp #EDIT_SUS
    bne not_sus
    addparam8(susval, zptmp0)
    rts
not_sus:
    cmp #EDIT_REL
    bne not_rel
    addparam8(relval, zptmp0)
    rts
not_rel:
    cmp #EDIT_FRQ
    bne not_frq
    addparam16(soundfx.ifrq, zptmp0)
    rts
not_frq:
    cmp #EDIT_DURATION
    bne not_dur
    addparam8(soundfx.icount, zptmp0)
    rts
not_dur:
    rts
}

save_edit_and_play:
    sta editing
    jmp playsound

playsound:
    jsr draw_gui

    // some voice values are mirrored in GUI code, so move from here to sound
    // player.
    ldx voice
    lda dkval,x
    and #$0f
    sta zptmp0
    lda atval,x
    asl
    asl
    asl
    asl
    ora zptmp0
    sta soundfx.iatdk,x

    // some voice values are mirrored in GUI code, so move from here to sound
    // player.
    lda relval,x
    and #$0f
    sta zptmp0
    lda susval,x
    asl
    asl
    asl
    asl
    ora zptmp0
    sta soundfx.isurl,x

    jsr soundfx.reset_voice

    lda voice
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

voice:  .byte 0
atval:  .byte 4, 4, 4, 4 // TODO add more maybe?
dkval:  .byte 4, 4, 4, 4
susval: .byte 0, 0, 0, 0
relval: .byte 0, 0, 0, 0

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

nums: .byte 0, 1, 2, 3

.macro loadparam8(dst, src) {
    ldx voice
    lda src, x
    sta dst+0
}

.macro loadparam16(dst, src) {
    lda voice
    asl
    tax
    lda src, x
    sta dst+0
    lda src+1, x
    sta dst+1
}

nurpastr: .text "sid editor by nurpasoft 2018"
nurpastr_end:

voiceselstr: .text "voice"
voiceselstr_end:

durationstr: .text "l duration"
durationstr_end:

frqstr: .text "f frq"
frqstr_end:

pulsestr: .text "p pulse"
pulsestr_end:

atstr: .text "a atk"
atstr_end:

dkstr: .text "d dec"
dkstr_end:

susstr: .text "s sustain"
susstr_end:

relstr: .text "r release"
relstr_end:

draw_gui:
    drawstringcol(0, 0, WHITE, nurpastr, nurpastr_end)

    .var ypos = 3
    drawstringcol(0, ypos, GRAY, voiceselstr, voiceselstr_end)
    lda #0
    sta $10
    sta $11
numloop:
    ldy #ypos

    lda $10
    ldx #$00
    cmp voice
    bne notselected
    ldx #$80
notselected:
    stx zparg3

    asl
    asl
    clc
    adc #12
    tax

    lda #LIGHT_BLUE
    sta zparg0
    mov16imm(zparg1, nums)
    add16(zparg1, zparg1, $10)
    jsr draw_hex4

    inc $10
    lda $10
    cmp #4
    bne numloop

    .eval ypos = ypos + 2
    drawstringcol(0, ypos, GRAY, durationstr, durationstr_end)
    loadparam8(zparg4, soundfx.icount)
    geteditcol(EDIT_DURATION)
    drawhex8(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, frqstr, frqstr_end)
    loadparam16(zparg4, soundfx.ifrq)
    geteditcol(EDIT_FRQ)
    drawhex16(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, atstr, atstr_end)
    loadparam8(zparg4, atval)
    geteditcol(EDIT_AT)
    drawhex4(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, dkstr, dkstr_end)
    loadparam8(zparg4, dkval)
    geteditcol(EDIT_DK)
    drawhex4(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, susstr, susstr_end)
    loadparam8(zparg4, susval)
    geteditcol(EDIT_SUS)
    drawhex4(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, relstr, relstr_end)
    loadparam8(zparg4, relval)
    geteditcol(EDIT_REL)
    drawhex4(12, ypos, zparg4)

    .eval ypos = ypos + 1
    drawstringcol(0, ypos, GRAY, pulsestr, pulsestr_end)
    loadparam16(zparg4, soundfx.ipulse)
    geteditcol(EDIT_PULSE)
    drawhex16(12, ypos, zparg4)

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
