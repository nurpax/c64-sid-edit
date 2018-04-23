// helpful labels
.const CLEAR  = $E544
.const GETIN  = $FFE4
.const SCNKEY = $FF9F

.const zptmp0 = $20
.const zptmp2 = $22
.const zptmp4 = $24
.const zptmp6 = $26

.const zparg0 = $28

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

    sei
    :SetupIRQ(partIrqStart, partIrqStartLine, false)
    lda #0
    sta framecount
    cli

    lda #0
    sta $d020
    sta $d021

infloop:
    jmp infloop


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
