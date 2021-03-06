// Sound effect player
//
// Originally pulled from
// http://codebase64.org/doku.php?id=base:sound_fx_player but it's mostly a
// rewrite now.
//
// The original had a bunch of bugs.  For example, it was accessing the ifrq
// array with a stride of one byte so it really only worked with the first
// sound.

.filenamespace soundfx

    .const sid = $D400
    .const raster = 50

    .const voice = 2
    .const voffs = voice*7
    .const sidvregs = sid + voffs

    .const pstep = 2
    .const pfreq = 6

// Initialize sounds
init: {
    lda #$0f
    sta $d418     // Select Filter Mode and Volume
    rts
}

reset_voice: {
    // See also ADSR bug:
    // http://csdb.dk/forums/?roomid=11&topicid=110547
    lda #$08
    sta sidvregs+4
    rts
}

// Call this from a raster IRQ
play: {
    lda effect      // LOAD FX NUMBER
    cmp #128        // 128 MEANS NO NEW EFFECT NUMBER WAS ASK FOR
    beq playsounds // PLAY SOUNDS
    jmp resetsound // RESET SOUND DATA
// PLAY SOUNDS
playsounds:
    lda voiceparams     // CHECK IF SOUND IS STILL PLAYING
    tax
    cmp #128            // 128 MEANS NO
    beq done
    jmp continuesound
done:
    rts

// OVER WRITE SOUND DATA FROM ARRAY TO SID CHIP
// A=which effect to play
resetsound:
    tax
    // RUN FROM HERE IS SOUND IS NOT PLAYING YET
    lda voiceparams   // HAS SOUND STOPPED
    cmp #128
    beq miss2         // SOUND NOT PLAYING

    // CHECK IF WE CAN OVER WRITE A SOUND THAT IS ALLREADY PLAYING
    lda iwrite,x       // LOAD REWRITE FLAG
    cmp #1             // IF ONE THEN WE MUST WAIT FOR SOUND TO STOP
    beq playsounds

miss2:
    // word sized:
    // ifrq, istep, ipulse
    txa
    asl
    tax
    lda istep,x
    sta voiceparams+pstep
    lda istep+1,x
    sta voiceparams+pstep+1
    txa
    lsr
    tax

    lda istepway,x
    sta voiceparams+4

    lda icount,x
    sta voiceparams+5

    txa
    asl
    tax
    lda ifrq,x
    sta voiceparams+pfreq
    sta sidvregs
    lda ifrq+1,x
    sta voiceparams+pfreq+1
    sta sidvregs+1

    lda ipulse,x       // LOAD LOW BYTE PULSE FREQUENCY VALUE
    sta sidvregs+2        // WRITE TO SID CHIP

    lda ipulse+1,x     // LOAD HIGH BYTE PULSE FREQUENCY VALUE
    sta sidvregs+3        // WRITE TO SID CHIP
    txa
    lsr
    tax

    lda iatdk,x
    sta sidvregs+5

    lda isurl,x
    sta sidvregs+6

    // SET FILTER MODE
    lda ifilterl       // LOW BYTE OF FILTER FREQUENCY VALUE
    sta sid+21         // WRITE TO SID CHIP

    lda ifilterh       // HIGH BYTE OF FILTER FREQUENCY VALUE
    sta sid+22         // WRITE TO SID CHIP

    lda ifiltercon     // SET TO FILTER ONLY
    sta sid+23         // WRITE TO SID CHIP

    clc
    lda ifiltermode    // SELECT FILTER BAND-PASS MODE AND MAXIMUM VOLUME
    adc #15
    sta sid+24         // WRITE TO SID CHIP

    lda icreg,x        // LOAD VALUE FOR CONTROL REGISTER
    sta sidvregs+4     // WRITE TO SID CHIP CONTROL REGISTER
    lda #0
    sta voiceparams    // SET TO ZERO FOR SOUND RUNNING
    stx voiceparams+1  // which soundfx are we playing, needed to set the gate bit for CR
    lda #128
    sta effect         // CLEAR THE FX NUMBER
    jmp playsounds

// CONTINUE WITH SOUND FX
continuesound:
    lda voiceparams+5
    cmp #0             // should sound stop?
    bne minusone
    ldx voiceparams+1
    lda icreg,x
    and #$fe           // clear gate to start release phase
    sta sidvregs+4
    lda #128
    sta voiceparams
    jmp done

minusone:
    sec
    lda voiceparams+5
    sbc #1
    sta voiceparams+5

    // step value
    lda voiceparams+2
    cmp #0
    bne stepit
    jmp done

stepit:
    // CHANGE FREQ VALUE OF SOUND
    lda voiceparams+4
    cmp #1
    bne subit
    clc
    lda voiceparams+pfreq
    adc voiceparams+pstep
    sta voiceparams+pfreq
    sta sidvregs
    lda voiceparams+pfreq+1
    adc voiceparams+pstep+1
    sta voiceparams+pfreq+1
    sta sidvregs+1
    jmp done

// TODO TODO this doesnt work
subit:
    sec
    lda voiceparams+pfreq
    sbc voiceparams+2
    sta sidvregs
    lda voiceparams+pfreq+1
    sbc voiceparams+3
    sta sidvregs+1
    jmp done      // QUIT BACK TO PLAY SOUNDS
}

effect:
    .byte 128     // WERE FX NUMBER IS WRITEN TO

* = $3000-2
sound_len: .word sound_end - sound_begin
sound_begin:
// VOICE 1
voiceparams:
    .byte 128     // 0   128 MEANING NO SOUND FX IS BEING USED
    .byte 0       // 1   soundfx idx
    .byte 0       // 2   frequency step
    .byte 0       // 3   frequency step
    .byte 0       // 4   ARE WE ADDING A STEP FREQUENCY VALUE
    .byte 0       // 5   HOW LONG SOUND WILL PLAY FOR
    .byte 0       // 6   current frequency
    .byte 0       // 7   current frequency

// HOW LONG SOUND WILL PLAY FOR
icount:
    .byte 25
    .byte 15
    .byte 25
    .byte 25
    .byte 32
    .byte 32
    .byte 32
    .byte 32


// START NOTE FREQUENCY RANGE (268 to 64814 )
ifrq:
    .word $ff0
    .word $7ff
    .word $200
    .word 9000
    .word 5000
    .word 6000
    .word 7000
    .word 8000

// PULSE NOTE FREQUENCY
// $D402 IS THE LOW BYTE OF THE PULSE WIDTH (LPW = 0 THROUGH 255).
// $D403 IS THE HIGH 4 BITS (HPW = 0 THROUGH 15).
ipulse:
    .word 3000
    .word 0
    .word 0
    .word 0
    .word 0
    .word 0
    .word 0
    .word 0

// UP SCALE OR DOWN SCALE
istep:
    .word 0
    .word 0
    .word 0
    .word 100
    .word 0
    .word 0
    .word 0
    .word 0

// PLUS=1 OR MINUS=2
istepway:
    .byte 0
    .byte 0
    .byte 0
    .byte 1
    .byte 0
    .byte 0
    .byte 0
    .byte 0

//SID-ADR-Table:
//
//     VALUE    ATTACK    DECAY/RELEASE
//   +-------+----------+---------------+
//   |   0   |    2 ms  |      6 ms     |
//   |   1   |    8 ms  |     24 ms     |
//   |   2   |   16 ms  |     48 ms     |
//   |   3   |   24 ms  |     72 ms     |
//   |   4   |   38 ms  |    114 ms     |
//   |   5   |   56 ms  |    168 ms     |
//   |   6   |   68 ms  |    204 ms     |
//   |   7   |   80 ms  |    240 ms     |
//   |   8   |  100 ms  |    300 ms     |
//   |   9   |  240 ms  |    720 ms     |
//   |   A   |  500 ms  |    1.5 s      |
//   |   B   |  800 ms  |    2.4 s      |
//   |   C   |    1 s   |      3 s      |
//   |   D   |    3 s   |      9 s      |
//   |   E   |    5 s   |     15 s      |
//   |   F   |    8 s   |     24 s      |
//   +-------+----------+---------------+

// ATTACK / DECAY CYCLE CONTROL
// Bits 7-4 Select ATTACK Cycle Duration: 0-15
// Bits 3-0 Select DECAY Cycle Duration: 0-15
iatdk:
    .byte $06
    .byte $25
    .byte $5a
    .byte $7f
    .byte $7f
    .byte $7f
    .byte $7f
    .byte $7f

// SUSTAIN / RELEASE CYCLE CONTROL
// Bits 7-4 Select Sustain Cycle Duration: 0-15
// Bits 3-0 Select Release Cycle Duration: 0-15
isurl:
    .byte $10
    .byte $f3
    .byte $f4
    .byte $f3
    .byte $f3
    .byte $f3
    .byte $f3
    .byte $f3

// WAVEFORM/GATE BIT SET
// BIT 7 SELECT RANDOM NOISE WAVEFORM,   1 = ON
// BIT 6 SELECT PULSE WAVEFORM,          1 = ON
// BIT 5 SELECT SAWTOOTH WAVEFORM,       1 = ON
// BIT 4 SELECT TRIANGLE WAVEFORM,       1 = ON
// BIT 3 TEST BIT: 1 = DISABLE OSCILLATOR
// BIT 2 RING MODULATE OSC. 1 WITH OSC. 3 OUTPUT, 1 = ON
// BIT 1 SYNCHRONIZE OSC. 1 WITH OSC. 3 FREQUENCY, 1 = ON
// BIT 0 GATE BIT: 1 = START ATT/DEC/SUS, 0 = START RELEASE
// TRIANGLE              ON 17   OFF 16
// SAWTOOTH              ON 33   OFF 32
// PULSE                 ON 65   OFF 64
// NOISE WAVEFORM        ON 129  OFF 128
icreg:
    .byte 65
    .byte 33
    .byte 129
    .byte 17
    .byte 17
    .byte 17
    .byte 17
    .byte 17


//$D415  FILTER CUTOFF FREQUENCY: LOW-NYBBLE (BITS 2-0) (0 to 7)
ifilterl:
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0

//$D416  FILTER CUTOFF FREQUENCY: HIGH-BYTE
ifilterh:
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0

// FILTERCON 7-4
// $D417 FILTER RESONANCE CONTROL / VOICE INPUT CONTROL
// 7-4   SELECT FILTER RESONANCE: 0-15
// 3     FILTER EXTERNAL INPUT: 1 = YES, 0 = NO
// 2     FILTER VOICE 3 OUTPUT: 1 = YES, 0 = NO
// 1     FILTER VOICE 2 OUTPUT: 1 = YES, 0 = NO
// 0     FILTER VOICE 1 OUTPUT: 1 = YES, 0 = NO
ifiltercon:
    .byte 0
    .byte 0
    .byte 0 // %11000010
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0

// FILTERMODE
// $D418 SELECT FILTER MODE AND VOLUME
// 7     CUT-OFF VOICE 3 OUTPUT: 1 = OFF, 0 = ON
// 6     SELECT FILTER HIGH-PASS MODE: 1 = ON
// 5     SELECT FILTER BAND-PASS MODE: 1 = ON
// 4     SELECT FILTER LOW-PASS MODE: 1 = ON
// 3-0   SELECT OUTPUT VOLUME: 0-15

ifiltermode:
    .byte 0
    .byte 0
    .byte 0 // %00100000
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0

// OVER WRITE SOUND VALUES WITH NEW SOUND VALUES 0=rewrite 1=not to rewrite
iwrite:
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
sound_end:
