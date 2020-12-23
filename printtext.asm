;
;
;printtext
;
;
;

; INPUT 64 6/86  INPUT-ASS, 6502/6510 Macro-Assembler 

org $c000

        sei
        cld
        jsr $c6a2
        jmp main
:text   b   "text"
:main   lda#<text
        sta $a6
        lda#>text
        sta $a7
        lda#04
        sta $a8
        jsr $c3e7
:end    jmp end

