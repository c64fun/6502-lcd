;
; conout           Jan 1990 by Oliver Kaltstein
;

; INPUT 64 6/86  INPUT-ASS, 6502/6510 Macro-Assembler 

; controls the Hitachi HD61830B LCD Driver

;attention:   ESC sequences and BELL still have to be written


:datawr  = 32768
:ctrlwr  = datawr+1
:datard  = ctrlwr+1
:ctrlrd  = datard+1
:latchwr = ctrlrd+1        ;writeonly

:textlo  = $a6             ;read & write
:texthi  = textlo+1        ;read & write
:textlen = texthi+1        ;read & write

:locrsrpos = textlen+1     ;readonly
:hicrsrpos = locrsrpos+1   ;readonly

:loscrnpos = hicrsrpos+1   ;readonly
:hiscrnpos = loscrnpos+1   ;readonly

:col = hiscrnpos+1         ;readonly
:lin = col+1               ;readonly

:latch  = lin+1            ;read & write

:tab = latch+1             ;read & write


org $c000

               ldy #00
:getascii      tya
               pha
               lda (textlo),y
               pha
               and #$e0           ;if >= 32 then send ascii to lcd
               tax
               cpx #$00
               bne print
               pla
               cmp #13            ;check for return
               beq ctrl13
               cmp #10
               beq ctrl10
               cmp #07
               beq ctrl7
               cmp #08
               beq ctrl8
               cmp #09
               beq ctrl9
               cmp #19
               beq ctrl19
               cmp #28
               beq ctrl28
               cmp #29
               beq ctrl29
               cmp #30
               beq ctrl30
               cmp #31
               beq ctrl31 
               cmp #12
               beq ctrl12

:next          pla
               tay
               iny
               cpy textlen
               bne getascii
               rts

:ctrl7         jsr bell
               jmp next
:ctrl8         jsr backspace
               jmp next
:ctrl9         jsr tabulator
               jmp next
:ctrl10        jsr linefeed
               jmp next
:ctrl12        jsr clrhome
               jmp next
:ctrl13        jsr careturn
               jmp next
:ctrl19        jsr home
               jmp next
:ctrl28        jsr crsrup
               jmp next
:ctrl29        jsr linefeed       ;used as crsrdown
               jmp next
:ctrl30        jsr crsrright
               jmp next
:ctrl31        jsr crsrleft
               jmp next

:print         jsr wait
               lda #$0c
               sta ctrlwr         ;switch to char out mode
               jsr wait
               pla                ;jis for lcd
               sta datawr         ;send char to lcd
               inc col
               inc locrsrpos
               bne skip1
               inc hicrsrpos      ;updates information in zero page
:skip1         lda col            ;now it has to be checked it crsrpos is a
               cmp #65            ;(multiple of 66)-2
               beq skip3          ;margin reached
               jmp next
:skip3         jsr newlin
               jmp next


:newlin        lda #00            ;this procedure should only be activated
               sta col            ;when crsr has reached right margin
               lda #$0c           ;it moves crsr to next line and scrools
               jsr wait           ;up if necessary
               sta ctrlwr
               lda #32
               jsr wait
               sta datawr
               inc locrsrpos
               bne skip2
               inc hicrsrpos      ;updates information in zero page
:skip2         lda lin
               cmp #07
               beq addlin         ;branch to create new line on lcd
               inc lin            ;move down on the lcd without scooling
               rts
:addlin        ldx #66            ;write 66 spaces to erase following line
               lda #32
:loop3         dex
               jsr wait
               sta datawr
               cpx #00
               bne loop3
               lda #10            ;crsrpos address
               jsr wait
               sta ctrlwr
               lda locrsrpos
               jsr wait
               sta datawr
               lda #11
               jsr wait
               sta ctrlwr
               lda hicrsrpos
               jsr wait
               sta datawr
               lda #66            ;move scrnstart 66 further away
               clc
               adc loscrnpos
               sta loscrnpos
               bcc nohi
               inc hiscrnpos
:nohi          lda #08            ;update lcd screenposition
               jsr wait
               sta ctrlwr
               lda loscrnpos
               jsr wait
               sta datawr
               lda #09
               jsr wait
               sta ctrlwr
               lda hiscrnpos
               jsr wait
               sta datawr
               rts


:careturn      lda #65            ;procedure places cursor onto right margin
               sec
               sbc col            ;find number of spaces to the right margin
               clc
               adc locrsrpos      ;add difference to crsrposition
               sta locrsrpos
               bcc nohi2
               inc hicrsrpos
:nohi2         lda #10            ;move crsr on screen into the right margin
               jsr wait
               sta ctrlwr
               lda locrsrpos
               jsr wait
               sta datawr         ;send lowbyte
               lda #11
               jsr wait
               sta ctrlwr
               lda hicrsrpos
               jsr wait
               sta datawr         ;send hibyte
               jsr newlin
               rts

:bell          rts

:linefeed      lda col
               pha                ;save old col
               jsr careturn
               pla
               sta col
               clc
               adc locrsrpos      ;move crsr from col 0 to old col
               sta locrsrpos
               bcc setcrsr
               inc hicrsrpos
:setcrsr       lda #10
               jsr wait
               sta ctrlwr
               lda locrsrpos
               jsr wait
               sta datawr
               lda #11
               jsr wait
               sta ctrlwr
               lda hicrsrpos
               jsr wait
               sta datawr
               rts


:home          lda loscrnpos
               sta locrsrpos      ;make crsrpos equal to scrnpos
               lda hiscrnpos
               sta hicrsrpos
               lda #00
               sta lin            ;update pos info
               sta col
               jsr setcrsr
               rts


:clrhome       ldx #$10
:loop4         txa
               pha
               jsr careturn
               pla
               tax
               bne loop4
               jsr home
               rts


:tabulator     lda tab
               beq quit           ;if tabsize is 0 then forget it
:tabtest       tax
               sec
               sbc #65            ;if tabpos >= 64 then quit also
               bpl quit
               txa                ;get the tabpos back
               sec                ;if col < tabpos then jmp to next tab
               sbc #01            ;(substitute for dea)
               sec
               sbc col            ;tabpos - 1 - col
               bmi nexttab        ;if col >= tab, inc tab and try again
               txa                ;now place cursor at tabpos
               sec
               sbc col            ;distance between tabpos and col
               clc
               adc locrsrpos
               sta locrsrpos
               bcc nohi4
               inc hicrsrpos
:nohi4         txa
               sta col
               jsr setcrsr        ;inform screen of changed crsrpos
               rts                ;stop after tab has been done
:nexttab       txa                ;tabpos=tabpos + tab
               clc
               adc tab            ;prepare to test next tab location
               jmp tabtest
:quit          rts


:crsrup        lda lin
               beq quit5
               dec lin
               lda locrsrpos
               sec
               sbc #66            ;above crsr
               sta locrsrpos
               bcs skip6
               dec hicrsrpos
:skip6         jsr setcrsr
:quit5         rts


:crsrleft      lda col
               beq quit2
               dec col
               dec locrsrpos
               lda locrsrpos
               cmp #$ff
               bne skip4
               dec hicrsrpos
:skip4         jsr setcrsr
:quit2         rts


:backspace     lda col
               beq quit4
               dec col
               dec locrsrpos
               lda locrsrpos
               cmp #$ff
               bne skip5
               dec hicrsrpos
:skip5         jsr setcrsr
               lda #12
               jsr wait
               sta ctrlwr
               lda #32            ;space
               jsr wait
               sta datawr
               jsr setcrsr
:quit4         rts


:crsrright     lda col
               cmp #64            ;if crsr on right then don't move
               beq quit3
               inc col
               lda #01
               clc
               adc locrsrpos
               sta locrsrpos
               bcc nohi3
               inc hicrsrpos
:nohi3         jsr setcrsr
:quit3         rts


:clrscr        jsr wait           ;fill screen memory with spaces
               lda #12
               sta ctrlwr
               ldy #$10
               ldx #$00           ;loop 4096 times
:loop2         dey
:loop1         dex
               jsr wait
               lda #32            ;space
               sta datawr
               cpx #00
               bne loop1
               cpy #00
               bne loop2

               jsr wait
               lda #08
               sta ctrlwr
               jsr wait
               lda #00            ;lobyte display starting address
               sta datawr
               jsr wait
               lda #09
               sta ctrlwr
               jsr wait
               lda #00            ;hibyte display starting address
               sta datawr
               sta loscrnpos      ;update zeropage
               sta hiscrnpos

               jsr wait
               lda #10
               sta ctrlwr
               jsr wait
               lda #00
               sta datawr         ;lobyte cursor address
               jsr wait
               lda #11
               sta ctrlwr
               jsr wait
               lda #00            ;hibyte cursor address
               sta datawr
               sta locrsrpos
               sta hicrsrpos
               sta col
               sta lin
               rts

:wait          pha
               lda ctrlrd
               and #$80
               bne wait
               pla
               rts

:lcdreset      jsr wait
               lda #00            ;Write Reg 0
               sta ctrlwr
               jsr wait
               lda #$38           ;Display on, character blink,
               sta datawr         ;no graphics, internals charset

               jsr wait
               lda #01
               sta ctrlwr
               jsr wait
               lda #117           ;8 lines, 66.6 col
               sta datawr

               jsr wait
               lda #02
               sta ctrlwr
               jsr wait
               lda #65            ;66 char per line
               sta datawr

               jsr wait
               lda #03
               sta ctrlwr
               jsr wait
               lda #31            ;32 vertical dots
               sta datawr

               jsr wait
               lda #04
               sta ctrlwr
               jsr wait
               lda #07            ;cursor pos at 8     ???
               sta datawr

               lda #08            ;set brightness
               sta latchwr        ;latch of lcd
               sta latch          ;latch of page zero

               lda #08            ;init tab with 8
               sta tab

               lda latch
               ora #%01000000     ;set bit 7 to activate page 2
               sta latch
               sta latchwr
               jsr clrscr
               lda latch
               and #%10111111     ;change back to first page
               sta latch
               sta latchwr
               jsr clrscr

               rts

