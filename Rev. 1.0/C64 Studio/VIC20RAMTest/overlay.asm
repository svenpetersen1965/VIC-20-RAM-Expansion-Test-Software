; *********************************************
; RAM test for the VIC-20 memory extensions
; by Sven Petersen 
; http://www.github.com/svenpetersen1965
; *********************************************
; revision history:
; Rev. 0.0:  initial release
; Rev. 1.0:  Code optimized, 
;            added 1k Blocks @ /IO2 and /IO3 
;
; assembler used: C64 Studio
; *********************************************

; options:
;          in ROM (auto start cartridge@ $A000)
;          in RAM (sys 4864 = $1300)

; for ROM version: compile and build ramtest_a000.asm and remove the first two bytes 
;                  of the ramtest_a000.asm to get a working binary file
;
; for Floppy version: compile and build this file (overlay.asm)
;                     overlay.prg can be loaded with ,8,1 and started
;                     with SYS4864
;                     for build and run: configure XVIC from VICE
;                     start the software with SYS4864
; to create a *.D64 image: build vic20ramtest.bas and put 
;                     vic20ramtest.prg and overlay.prg in a floppy disk image
;                     load vic20ramtest with LOAD"*",8 (not ,8,1!)
  
!ifdef ROM {
              *= $A000
              ; cartridge autostart header
           !BYTE <start, >start, $ad, $fe, $41, $30, $c3, $c2, $cd   
start      jsr InitMem  ; initialize memory and system contents;
           jsr Restor   ; set the system vectors
           jsr InitVIA  ; initialize the 6522 VIA register
           jsr InitSK   ; initialize the 6560 VIC chip registers and screen
           cli          ; enable all interrupts
           jmp main
} else {
RAM=1              
              *= $1300
           jmp main
}

; Basic/Kernal Subroutines
clearScreen = $e55f
printStr    = $cb1e     ; LSB(StrAdr) -> .A, MSB(StrAdr) -> .Y
posCursor   = $fff0     ; clc, row -> .X, column -> .Y
PrtFix      = $ddcd     ; prints a fix decimal number (MSB in .A, LSB in .Y)
InitMem     = $fd8d     ; Initialize system memory
Restor      = $fd52     ; Cause the RAM system vectors to be reset to provided defaults
InitVIA     = $fdf9     ; Initialize the 6522 VIA registers
InitSK      = $e518     ; Initialize the 6550 VIC chip, screen, and related pointers


; Kernal variables
COLOR       = $0286     ; Cursor: original color at this screen location

; used variables
CountL      = $3b       ; Count LSB
CountH      = $3c       ; Cound MSB
Result      = $92       ; RAM Test Result ($00 is good)
TestBlk     = $94       ; block to test
BlockOk     = $95       ; result flags (1=ok)
XLnk        = $96       ; cross linked flag (1=cross linked)
BitPattern  = $9b       ; bit pattern for error flags
PRN         = $9c       ; storage for pseudo random number         
StickyBad   = $9e       ; Sticky bad flags
StickyXLnk  = $9f       ; Sticky cross link flags
index1      = $a3       ; index variable #1
index2      = $a4       ; index variable #2
RAMfound    = $be       ; flags for found RAM blocks (bit set = found)
TestResult  = $bf       ; temporary test result storage
RAMPtrL     = $fb       ; pointer to RAM block (LSB)
RAMPtrH     = $fc       ; pointer to RAM block (MSB)         
NumPages    = $fd       ; number of pages
RAMfill     = $fe       ; fill value/test value

;===============================================


           
; Texts
title      !text "VIC-20 RAM EXPANSION", 0
title1     !text "TEST V1.0",0

exp3k      !text "3K@0400",0
expblk1    !text "8K@2000",0       
expblk2    !text "8K@4000",0       
expblk3    !text "8K@6000",0 
expio2     !text "1K@9800",0
expio3     !text "1K@9C00",0
expblk5    !text "8K@A000",0       

expblk_lo  !byte <exp3k, <expblk1, <expblk2, <expblk3, <expio2, <expio3, <expblk5 
expblk_hi  !byte >exp3k, >expblk1, >expblk2, >expblk3, >expio2, >expio3, >expblk5 
pagetab    !byte $04, $20, $40, $60, $98, $9C, $A0 
numpagetab !byte $0C, $20, $20, $20, $04, $04, $20

bittab     !byte $01, $02, $04, $08, $10, $20, $40, $80

tabtitle   !text "BLOCK   FND TEST CROSS",0
counttxt   !text "COUNT: ",0
resulttxt  !text "RESULT:",0
txtok      !text "OK ",0
txtbad     !text "BAD",0
txtyes     !text "YES",0
txtno      !text "NO ",0
txtrun     !text "RUN",0
txtleg1    !text "FND:   RAM BLOCK FOUND",0
txtleg2    !text "CROSS: CROSS LINKED TO",0
txtleg3    !text "       OTHER RAM BLOCK",0
txttrail   !text "    ",0



; table coordinates
titlerow   =3               ; row of table title 
foundcol   =8               ; column of "FND"
testcol    =12              ; column of "TEST"
crosscol   =18              ; column of "CROSS"
countrow   =13              ; row of "COUNT:"
countcol   =7               ; column of the count
resultrow  =14
; constants
colred     =2               ; value of red
colblue    =6               ; value of blue
colblk     =0               ; value of black

PRNPoly    =%10001100       ; polynome for pseudo ramndom number generation (X^8 + X^5 + X^4 + 1)
PRNSeed    =$01             ; seed value for pseudo random numbers   

; ==== Subroutines ====

; set blue cursor
setBlue    lda #colblue
           sta COLOR
           rts

; set red cursor
setRed     lda #colred
           sta COLOR
           rts           

; set black cursor
setBlk     lda #colblk
           sta COLOR
           rts           

; ===========================================           
;           Fill memory area
; pointer to memory address in      RAMPtrL/H           
; number of pages (=256bytes) in    NumPages 
; content in                        RAMfill
; ===========================================

fillMem    lda RAMPtrL              ; save variables to stack
           pha
           lda RAMPtrH       
           pha
           lda NumPages
           pha
           
fillMem1   lda RAMfill              ; content to acc
           ldy #$00                 ; reset y (index within page)
fillMem2   sta (RAMPtrL),Y          ; store content indirect, y indexed
           iny                      ; advance page index
           bne fillMem2             ; is zero? no: loop
           inc RAMPtrH              ; yes: increment memory pointer (MSB)
           dec NumPages             ; decrement the number of pages to go
           bne fillMem1             ; loop
           
           pla                      ; retrieve variables from stack 
           sta NumPages
           pla 
           sta RAMPtrH
           pla
           sta RAMPtrL
           rts

; ===========================================           
;           test memory area
; pointer to memory address in      RAMPtrL/H           
; number of pages (=256bytes) in    NumPages 
; content in                        RAMfill
; ===========================================

testMem    lda RAMPtrL              ; save variables to stack
           pha
           lda RAMPtrH       
           pha
           lda NumPages
           pha
           
           ldy #$00                 ; reset y (index within page)
           sty TestResult           ; reset temp. result

testMem2   lda (RAMPtrL),Y          ; load byte from memory indirect, y indexed
           cmp RAMfill              ; compare with RAMfil
           bne testMemErr           ; if not equal, it is a error
           iny                      ; advance page index
           bne testMem2             ; is zero? no: loop
           inc RAMPtrH              ; yes: increment memory pointer (MSB)
           dec NumPages             ; decrement the number of pages to go
           bne testMem2             ; loop
           jmp testMem3             ; finished with no error
           
testMemErr lda #$ff                 ; set TestResult to Error
           sta TestResult
           
testMem3   pla                      ; retrieve variables from stack 
           sta NumPages
           pla 
           sta RAMPtrH
           pla
           sta RAMPtrL
           
           rts   
   
;=============================================
; fillZero
; fills all blocks with $00
;=============================================
fillZero   
!ifdef  RAM {
           lda #$06
} else { 
           lda #$05
}
           sta index2
fillZero_Loop           
           lda #$00                 ; set LSB of RAMPtrH
           sta RAMPtrL
           sta RAMfill              ; set RAMfill = $00    
           
           ldx index2
           lda bittab,x             ; get the corresponding bit pattern
           and RAMfound             ; is this block present?
           beq fillZero1            ; no?
           
           ; yes!
           
           lda pagetab,x            ; pointer to the block address (high byte)
           sta RAMPtrH
           
           lda numpagetab,x         ; get number of pages of the indexed BlockOk
           sta NumPages
           
           jsr fillMem              ; and fill the block

fillZero1  
           dec index2
           bpl fillZero_Loop
           rts

;===================================================
; about pseudo random numbers:
; they are actally a fix sequence of numbers, depending
; on the polynome and the seed value. They repeat after
; an odd interval, so they are good to detect address
; signal problems
; https://en.wikipedia.org/wiki/Pseudorandom_number_generator           
;===================================================
; nextPRN
; calculates the next pseudo ramdom number
; this works like a CRC calculation: 
; https://en.wikipedia.org/wiki/Cyclic_redundancy_check
; polynome like (Polynome: Dallas CRC-8 for 1wire bus)
; do not destroy .x and carry flag
; this pattern repeats after 186 bytes
;===================================================
nextPRN    txa                      ; value in .x -> .a
           ror                      ; rotate right, LSB in carry flag
           bcc nextPRN1             ; branch if carry clear
           eor #PRNPoly             ; if carry set, apply polynome 
                                    ; = invert the bits, that are contained in polynome
nextPRN1   tax                      ; save result in .x
           rts
 
; initialize the PRN generation ====================
initPRN    ldx #PRNSeed             ; seed value -> .x           
           clc                      ; clear carry
           rts
           
;===================================================
; fillPRN
; fills the memory pointed to in RAMPtrL/RAMPtrH
; with pseudo random numbers
; length of the fill (in pages) is in NumPages
;
;===================================================
fillPRN    lda RAMPtrL              ; save variables to stack
           pha
           lda RAMPtrH       
           pha
           lda NumPages
           pha

           ldy #$00                 ; initialize y
           jsr initPRN              ; initialize .x and carry
fillPRN1   jsr nextPRN              ; calculate next pseudo random number
           sta (RAMPtrL),Y          ; store it in RAM
           iny                      ; increment the index .y
           bne fillPRN1             
           inc RAMPtrH              ; yes: increment memory pointer (MSB)
           dec NumPages             ; decrement the number of pages to go
           bne fillPRN1             ; loop

           pla                      ; retrieve variables from stack 
           sta NumPages
           pla 
           sta RAMPtrH
           pla
           sta RAMPtrL
           rts

;===================================================
; testPRN
; 
;===================================================
testPRN    lda RAMPtrL              ; save variables to stack
           pha
           lda RAMPtrH       
           pha
           lda NumPages
           pha
           
           ldy #$00                 ; initialize y
           sty TestResult           ; initialize TestResult
           jsr initPRN              ; initialize .x and carry    
testPRN1   jsr nextPRN              ; calculate next pseudo random number           
           php                      ; save status register (CMP changed the carry flag)
           cmp (RAMPtrL),Y          ; compare with RAM content
           bne testPRNErr           ; if not equal, it is a error
           plp                      ; retrieve the status register    
           iny                      ; advance page index
           bne testPRN1             ; is zero? no: loop
           inc RAMPtrH              ; yes: increment memory pointer (MSB)
           dec NumPages             ; decrement the number of pages to go
           bne testPRN1             ; loop
           jmp testPRN2             ; finished with no error
           
testPRNErr plp                      ; retrieve the status register
           lda #$ff                 ; set TestResult to Error
           sta TestResult  
           
testPRN2   pla                      ; retrieve variables from stack 
           sta NumPages
           pla 
           sta RAMPtrH
           pla
           sta RAMPtrL
           rts

;===================================================
; detectBlock
; detects, wether a RAM block is present
; block number (0..6) in TestBlk           
; 0: 0400-0fff
; 1: 2000-3fff
; 2: 4000-5fff
; 3: 6000-7fff
; 4: 9800-9bff
; 5: 9C00-9fff
; 6: a000-bfff (not ROM version)
;===================================================

detectBlock 
           lda #1               ; only one page tested
           sta NumPages
          
           lda #$00
           sta RAMPtrL
           
           ldx TestBlk          ; Use TestBlk as table index X
           lda pagetab,x        ; load the high byte (page) of the RAM block to test
           sta RAMPtrH          ; and store bit
           lda bittab,x         ; load the bit pattern from table
           sta BitPattern       ; and store it
           
           ;start the actual test
;detBlockSt                    
           lda #$aa
           sta RAMfill           ; fill with $aa
           jsr fillMem
           jsr testMem           ; test for $aa
           lda TestResult
           bne detBlockFail      ; TestResult != 0 => RAM not found
           
           lda #$55
           sta RAMfill           ; fill with $55
           jsr fillMem
           jsr testMem           ; test for $55
           lda TestResult        ; TestResult != 0 => RAM not found
           bne detBlockFail
           
           ; detection passed
           lda RAMfound          ; mark block in bitset RAMfound
           ora BitPattern        ; 1=present
           sta RAMfound
           jmp detBlockEnd
detBlockFail
           nop
           
detBlockEnd
           rts           
           
;===================================================
; testBlock
; tests a block
; block number (0..6) in TestBlk           
; 0: 0400-0fff
; 1: 2000-3fff
; 2: 4000-5fff
; 3: 6000-7fff
; 4: 9800-9bff
; 5: 9C00-9fff
; 6: a000-bfff (not ROM version)
;===================================================
testBlock  ; setting up the test:
           ; block address, size and a bit patterns
           ; depending on the variable TestBlk
           
           
           lda #$00
           sta RAMPtrL
           
           ldx TestBlk          ; Use TestBlk as table index X
           lda pagetab,x        ; load the high byte (page) of the RAM block to test
           sta RAMPtrH          ; and store it
           lda numpagetab,x     ; load the number of pages of that block
           sta NumPages
           lda bittab,x         ; load the bit pattern from table
           sta BitPattern       ; and store it
           
           ; execute the actual test
;testBlockSt 
           lda #$00
           sta RAMPtrL           ; initialize RAMPtrL (always $00)
           lda #$55              
           sta RAMfill
           jsr fillMem           ; fill block with $55
           jsr testMem           ; test block for all $55 
           
           lda TestResult        ; is TestResult = 0? (which means ok)
           bne testBlockErr      ; no? Then it is an error
           
           lda #$aa              
           sta RAMfill
           jsr fillMem           ; fill block with $aa (inverted $55)
           jsr testMem           ; test block for all $55
           
           lda TestResult        ; is TestResult = 0?
           bne testBlockErr      ; no? Then it is an error
           
           jsr fillPRN           ; fill the block with a sequence of _pseudo_ random numbers
           jsr testPRN           ; test for this sequence
           lda TestResult        ; is TestResult = 0?
           bne testBlockErr      ; no? Then it is an error
           
           ; test ok 
           lda BitPattern        ; mark the block as ok in the
           ora BlockOk           ; bitset contained in BlockOr
           sta BlockOk           ; 1 = ok
            
testBlockErr 
           nop                 ; no further action, yet

;testBlockEnd 
           rts

;===================================================
; testXlnk
; tests all "other" blocks if they are fillZero
; excluded block in TestBlk
;===================================================
testXlnk
          lda TestResult       ; save variable TestResult
          pha                  ; on stack
          
!ifdef  RAM {
          lda #$06
} else { 
          lda #$05
}
          sta index2
          
          
          lda #$00             ; set RAMPtrL for all test
          sta RAMPtrL
          sta XLnk             ; reset crolls link flags

testXlnk_Loop          
          lda TestBlk          
          cmp index2           ; is the block that is tested equal to the indexed (index2) block?
          beq xlnkBlk1         ; yes: skip block
          ; no:...
          ldx index2
          lda bittab,x         ; get the bit pattern, that corresponds with index2
          and RAMfound         ; is this block detected?
          beq xlnkBlk1         ; no: skip block 0
          
          lda #$00      
          sta RAMfill          ; test for #$00
          
          lda numpagetab,x     ; get the number of pages for this block               
          sta NumPages         ; and store it
          
          lda pagetab,x        ; get block start address (high byte)
          sta RAMPtrH          ; and store it 
          jsr testMem          ; execute test
          
          lda TestResult       ; TestResult?
          beq xlnkBlk1         ; 0 -> passed
          
          ; not passed:
          ldx index2           ; get bit pattern for thsi block 
          lda bittab,x         
          ora XLnk             ; update  
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk
          
xlnkBlk1  
         lda index2            ; is index already 0
         beq xlnkEnd           ; yes: then finished
         dec index2            ; no: decrement
         jmp testXlnk_Loop     ; and loop   
xlnkEnd
          pla                  ; restore variable TestResult
          sta TestResult
          rts

;===================================================
; printXlnk
; prints all cross link status
;===================================================
printXlnk 
!ifdef  RAM {
          lda #$06
} else { 
          lda #$05
}
          sta index2          ; initialize index variable
          tax
          lda bittab,x
          sta BitPattern      ; initialize bit pattern 
          ; for index2...
printXa                       ; set cursor position
          lda #titlerow+1     
          clc
          adc index2
          tax
          ldy #crosscol
          jsr posCursor
          
          lda BitPattern      ; get bit BitPattern
          and RAMfound        ; is the RAMblock detected?
          beq printXf         ; no -> skip     

          and StickyXLnk      ; has this block ever been bad?
          beq printXb         ; no  
          jsr setRed          ; former/present errors are marked red
          jmp printXc
printXb   jsr setBlue         ; no cross links: it's blue
printXc   lda BitPattern      ; BitPattern again
          and XLnk            ; is it cross linked in this test?
          beq printXd         ; no
          ; it is cross linked
          lda #<txtyes        ; print "YES"
          ldy #>txtyes
          jmp printXe
          
          ; not cross linked
printXd   lda #<txtno         ; print "NO"    
          ldy #>txtno       

printXe   jsr printStr        ; now, print 
          
printXf   dec index2          ; decrement index variable
          clc
          lda BitPattern      ; load bit pattern
          ror                 ; shift right
          sta BitPattern      ; store it 
          bne printXa         ; for loop
;End
          rts 
 

; ==== main ====
main       lda #$00               ;reset test flags
           sta XLnk
           sta BlockOk
           sta StickyBad
           sta StickyXLnk
           sta Result
           sta CountL             ; set CountL/H to 0
           sta CountH
           
           jsr clearScreen        ; clear screen and print table
           jsr setBlk             ; black cursor
           
           ldx #0                 ; print program title
           ldy #1
           clc
           jsr posCursor
           lda #<title
           ldy #>title
           jsr printStr
           ldx #1                 ; print program title
           ldy #6
           clc
           jsr posCursor
           lda #<title1
           ldy #>title1
           jsr printStr
           
           ;==== print table
                                   ; print table title
           ldx #titlerow
           ldy #0
           clc
           jsr posCursor
           lda #<tabtitle
           ldy #>tabtitle
           jsr printStr
                                  ; print ram block text

           ldx #titlerow+1
!ifdef RAM {
           lda #$07
} else {
           lda #$06
}
           sta index1
                                  ; print all RAM block names
print_tab_loop
           clc 
           lda #titlerow
           adc index1
           tax   
           ldy #0
           clc
           jsr posCursor
           
           lda index1
           tax
           dex
           lda expblk_hi,x
           tay
           lda expblk_lo,x  
           jsr printStr
           dec index1
           bne print_tab_loop
                                   ; print count text
           ldx #countrow
           ldy #0
           clc
           jsr posCursor
           lda #<counttxt
           ldy #>counttxt
           jsr printStr
                                   ; print result text
           ldx #resultrow
           ldy #0
           clc
           jsr posCursor
           lda #<resulttxt
           ldy #>resulttxt
           jsr printStr
           
           ldx #countrow+3        ; print legend line 1
           ldy #0
           clc
           jsr posCursor
           lda #<txtleg1
           ldy #>txtleg1
           jsr printStr
           
           ldx #countrow+5        ; print legend line 2
           ldy #0
           clc
           jsr posCursor
           lda #<txtleg2
           ldy #>txtleg2
           jsr printStr

           ldx #countrow+6        ; print legend line 3
           ldy #0
           clc
           jsr posCursor
           lda #<txtleg3
           ldy #>txtleg3
           jsr printStr
      
           ; detect RAM blocks 
           
!ifdef RAM {
           lda #$06
} else {
           lda #$05
}
           sta index1
           
           lda #0                ; reset the variable, that holds the found RAM blocks
           sta RAMfound

detect_loop
           lda index1            ; get block number
           sta TestBlk
           jsr detectBlock       ; detect RAM

           lda #titlerow+1       ; position cursor 
           clc
           adc index1
           tax
           ldy #foundcol
           clc
           jsr posCursor
           
           lda TestResult        ; TestResult != 0 => RAM not found
           bne RAMsrch1 
           
           jsr setBlue           ; blue           
           lda #<txtyes          ; RAM detected: print "yes"
           ldy #>txtyes          
           jmp RAMsrch2
           
RAMsrch1   jsr setRed
           lda #<txtno           ; RAM not detected => print "no"
           ldy #>txtno
           
RAMsrch2   jsr printStr
           dec index1
           bpl detect_loop

           ; ============================================
           ; here, everything is searched and printed 
           ; let the test begin!
           ; ============================================
           

test       
           lda #$00
           sta index1            ; initialize index variable          
          
test_Loop           
           jsr fillZero          ; fill all RAM with zero
           
           ldx index1
           lda bittab,x          ; get the bit pattern, that corresponds with the index1
           and RAMfound
           beq testblk1          ; block is not present. Skip!
           ; Block found
           ldy #testcol          ; position cursor in table (test column)
           lda #titlerow+1
           clc
           adc index1
           tax
           clc
           jsr posCursor
           
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           

           
           lda index1
           sta TestBlk           
           jsr testBlock         ; test block 
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           ldy #testcol          ; position cursor in table (test column)
           
           lda #titlerow+1
           clc
           adc index1
           tax
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk0a         ; is 0 -> passed 
           
           ; not passed:
           ldx index1
           lda bittab,x          ; get the bit pattern, that corresponds with the index1
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk0d
 
           ; passed
testblk0a  
           ldx index1
           lda bittab,x          ; get the bit pattern, that corresponds with the index1
           and StickyBad
           beq testblk0b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk0c
           
testblk0b  jsr setBlue           ; ok is blue in case always ok  
testblk0c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk0d  jsr printStr          ; print the result in table   


           
           jsr printXlnk         ; print all cross linked results

testblk1   


           inc index1
           lda index1
!ifdef RAM {
           cmp #$07
} else {
           cmp #$06
}                      
           beq testcount
           jmp test_Loop 

testcount  

           ; increment count
           ldy #countcol         ; position cursor in table (test column)
           ldx #countrow         ; and row of count
           clc
           jsr posCursor
           jsr setBlk            ; black color
           
           clc                   ; increment the counter
           lda #1
           adc CountL            ; lsb
           sta CountL
           lda #0
           adc CountH            ; msb
           sta CountH     
           
           ldx CountL            ; MSB is in .A, LSB -> .X
           jsr PrtFix            ; print as decimal number
           
           lda #<txttrail        ; set pointer to trailing spaces
           ldy #>txttrail          
           jsr printStr          ; print the trailing spaces (in case of overflow)
           
           lda Result
           ora StickyBad
           ora StickyXLnk
           sta Result
           
           ldy #countcol         ; position cursor in table (count column)
           ldx #resultrow        ; and row of result
           clc
           jsr posCursor
           
           lda Result
           bne RAMbad
           
           jsr setBlue
           lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
           jsr printStr          ; print ok
           jmp testend
RAMbad           
           jsr setRed
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad          
           jsr printStr          ; print bad
testend           
           jmp test              ; repeat the test
           
;end        rts        