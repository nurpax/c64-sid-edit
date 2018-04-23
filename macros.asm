.const          irqpointer = $0314

//----------------------------------------------------------
//  Macros
//----------------------------------------------------------
.macro SetupIRQ(IRQaddr,IRQline,IRQlineHi) {
                lda #<IRQaddr
                sta irqpointer
                lda #>IRQaddr
                sta irqpointer+1
                lda #$1f
                sta $dc0d
                sta $dd0d
                lda #$81
                sta $d01a
                sta $d019
        .if(IRQlineHi) {
                lda #$9b
        } else {
                lda #$1b
        }
                sta $d011
                lda #IRQline
                sta $d012
}
//----------------------------------------------------------
.macro EndIRQ(nextIRQaddr,nextIRQline,IRQlineHi) {
                asl $d019
                lda #<nextIRQaddr
                sta irqpointer
                lda #>nextIRQaddr
                sta irqpointer+1
                lda #nextIRQline
                sta $d012
        .if(IRQlineHi) {
                lda $d011
                ora #$80
                sta $d011
        }
                jmp $febc
}


// move two bytes from n1 to res
.macro mov16(res, n1) {
    lda n1
    sta res+0
    lda n1+1
    sta res+1
}

// add 2 16bit memory locations, store in res
.macro add16(res, n1, n2) {
    clc
    lda n1
    adc n2
    sta res+0
    lda n1+1
    adc n2+1
    sta res+1
}

// add 2 16bit memory locations, store in res
.macro add16_imm16(res, lo, hi) {
    clc
    lda res
    adc #lo
    sta res+0
    lda res+1
    adc #hi
    sta res+1
}

// add 2 16bit memory locations, store in res
.macro add16_imm8(res, lo) {
    clc
    lda res
    adc #lo
    sta res+0
    lda res+1
    adc #0
    sta res+1
}

// immediate multiply
.macro mulimm(m, imm) {
    .if (imm == 3) {
        add16(zptmp0, m, m)
        add16(m, zptmp0, m)
    } else {
        .error "unsupported"
    }
}

.macro fixArchitectFontData(data) {
    .for (var i = 0; i < 64; i++) {
        .if (i == $20) {
            .fill 8, 0
        } else .if (i == $2c) { // ','
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00110000
            .byte %01100000
            .byte %00000000
        } else .if (i == $2e) { // '.'
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %01100000
            .byte %01100000
            .byte %00000000
        } else {
            .for (var c = 0; c < 8; c++) {
                .byte data.get(i*8+c+2)
            }
        }
    }
    // $40
    .fill 8, $ff
    // $41
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000001
    // $42
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %10000000
    // $43
    .byte %10000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    // $44
    .byte %00000001
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    // $45
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %11111111
    // $46
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    // $47
    .byte %11111111
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    // $48
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
    .byte %00000001
}
