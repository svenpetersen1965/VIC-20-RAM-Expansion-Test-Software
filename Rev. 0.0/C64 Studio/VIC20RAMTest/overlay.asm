; *********************************************
; RAM test for the VIC-20 memory extensions
; by Sven Petersen 
; http://www.github.com/
; *********************************************
; revision history:
; Rev. 0.00: initial release
; 
; assembler used: C64 Studio
; *********************************************


; ROM=1           ; options:
                ;          in ROM (auto start cartridge@ $A000)
                ;          in RAM (sys 4864 = $1300)
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
RAMfound    = $be       ; flags for found RAM blocks (bit set = found)
TestResult  = $bf       ; temporary test result storage
RAMPtrL     = $fb       ; pointer to RAM block (LSB)
RAMPtrH     = $fc       ; pointer to RAM block (MSB)         
NumPages    = $fd       ; number of pages
RAMfill     = $fe       ; fill value/test value

;===============================================


           
; Texts
title      !text "VIC-20 RAM EXPANSION", 0
title1     !text "TEST V0.0",0
exp3k      !text "3K@0400",0
expblk1    !text "8K@2000",0       
expblk2    !text "8K@4000",0       
expblk3    !text "8K@6000",0 
!ifdef RAM {      
expblk5    !text "8K@A000",0       
}
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
countrow   =11              ; row of "COUNT:"
countcol   =7               ; column of the count
resultrow  =12
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
fillZero   lda #$00                 ; set LSB of RAMPtrH
           sta RAMPtrL
           sta RAMfill              ; set RAMfill = $00    
           
           lda #$01
           and RAMfound
           beq fillZero1
           lda #$04                 ; pointer to $0400
           sta RAMPtrH
           lda #12                  ; 3k are 12 pages
           sta NumPages
           jsr fillMem

fillZero1  lda #$20                 ; 8k are 32 pages
           sta NumPages

           lda #$02
           and RAMfound
           beq fillZero2
           
           lda #$20                 ; pointer to $2000
           sta RAMPtrH              
           jsr fillMem
           
fillZero2  lda #$04
           and RAMfound
           beq fillZero3
           
           lda #$40                 ; pointer to $4000
           sta RAMPtrH
           jsr fillMem

fillZero3  lda #$08
           and RAMfound
           beq fillZero4
           
           lda #$60                 ; pointer to $6000
           sta RAMPtrH
           jsr fillMem

fillZero4  
!ifdef RAM {
           lda #$10
           and RAMfound
           beq fillZero5
           lda #$A0                 ; pointer to $8000
           sta RAMPtrH
           jsr fillMem
fillZero5  
}         
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
; block number (0..4) in TestBlk           
; 0: 0400-0fff
; 1: 2000-3fff
; 2: 4000-5fff
; 3: 6000-7fff
; 4: a000-bfff (not ROM version)
;===================================================

detectBlock 
           lda #1               ; only one page tested
           sta NumPages
          
           lda #$00
           sta RAMPtrL
           
           ; initialize RAMPtrH and Bit Pattern depending on 
           ; the variabkle TestBlk
           cmp TestBlk            ; is it block 0
           bne detBlock1           
           
           ; 3k€0400
           lda #$01        
           sta BitPattern
           lda #$04
           sta RAMPtrH
           jmp detBlockSt
           
detBlock1  lda #$01
           cmp TestBlk            ; is it block 1
           bne detBlock2
           
           ; 8k@2000
           lda #$02
           sta BitPattern
           lda #$20
           sta RAMPtrH
           jmp detBlockSt
 
detBlock2  lda #$02
           cmp TestBlk            ; is it block 2
           bne detBlock3
           
           ; 8k@4000
           lda #$04
           sta BitPattern
           lda #$40
           sta RAMPtrH
           jmp detBlockSt
           
detBlock3  lda #$03
           cmp TestBlk            ; is it block 3
           bne detBlock4
           
           ; 8k@6000
           lda #$08
           sta BitPattern
           lda #$60
           sta RAMPtrH
           jmp detBlockSt

           ; 8k@a000
detBlock4  lda #$04
           cmp TestBlk            ; is it block 4
           bne detBlockEnd
           
           lda #$10
           sta BitPattern
           lda #$A0
           sta RAMPtrH
           jmp detBlockSt
           
           ;start the actual test
detBlockSt                    
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
; block number (0..4) in TestBlk           
; 0: 0400-0fff
; 1: 2000-3fff
; 2: 4000-5fff
; 3: 6000-7fff
; 4: a000-bfff (not ROM version)
;===================================================
testBlock  ; setting up the test:
           ; block address, size and a bit patterns
           ; depending on the variable TestBlk
           
           lda #$00
           cmp TestBlk            ; is it block 0
           bne testBlock1
           
           ; 3k€0400
           lda #$01
           sta BitPattern
           lda #12                ; yes
           sta NumPages           ; 12 pages
           lda #$04
           sta RAMPtrH
           jmp testBlockSt
           
testBlock1 lda #$01
           cmp TestBlk            ; is it block 1
           bne testBlock2
           
           ; 8k@2000
           lda #$02
           sta BitPattern
           lda #$20               ; yes
           sta NumPages           ; 32 pages
           lda #$20
           sta RAMPtrH
           jmp testBlockSt
 
testBlock2 lda #$02
           cmp TestBlk            ; is it block 2
           bne testBlock3
           
           ; 8k@4000
           lda #$04
           sta BitPattern
           lda #$20               ; yes
           sta NumPages           ; 32 pages
           lda #$40
           sta RAMPtrH
           jmp testBlockSt
           
testBlock3 lda #$03
           cmp TestBlk            ; is it block 3
           bne testBlock4
           
           ; 8k@6000
           lda #$08
           sta BitPattern
           lda #$20               ; yes
           sta NumPages           ; 32 pages
           lda #$60
           sta RAMPtrH
           jmp testBlockSt

           ; 8k@a000
testBlock4 lda #$04
           cmp TestBlk            ; is it block 4
           bne testBlock5
           
           lda #$10
           sta BitPattern
           lda #$20               ; yes
           sta NumPages           ; 32 pages
           lda #$A0
           sta RAMPtrH
           jmp testBlockSt
testBlock5                   
           jmp testBlockEnd 

           ; execute the actual test
testBlockSt lda #$00
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
            
testBlockErr nop                 ; no further action, yet

testBlockEnd rts

;===================================================
; testXlnk
; tests all "other" blocks if they are fillZero
; excluded block in TestBlk
;===================================================
testXlnk
          lda TestResult       ; save variable TestResult
          pha 
          
          lda #$00             ; set RAMPtrL for all test
          sta RAMPtrL
          sta XLnk             ; reset crolls link flags
          
          ; Block 0
          lda TestBlk          ; is block 0 excluded
          beq xlnkBlk1         ; yes: skip block 0
  
          lda #%00000001       ; bit#0
          and RAMfound         ; is block 0 detected?
          beq xlnkBlk1         ; no: skip block 0
          
          lda #$00      
          sta RAMfill          ; test for #$00
          lda #12                
          sta NumPages         ; 12 pages
          lda #$04             ; block start @ 0400
          sta RAMPtrH 
          jsr testMem          ; execute test
          lda TestResult       ; TestResult?
          beq xlnkBlk1         ; 0 -> passed
          ; not passed:
          lda #%00000001       ; set bit#0
          ora XLnk             ; in 
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk
          
xlnkBlk1  lda TestBlk          ; is block 1 excluded
          cmp #1                   
          beq xlnkBlk2         ; yes: skip block 1
  
          lda #$02             ; bit#1
          and RAMfound         ; is block 1 detected?
          beq xlnkBlk2         ; no: skip block 1
          
          lda #$00      
          sta RAMfill          ; test for #$00
          lda #$20                
          sta NumPages         ; 32 pages
          lda #$20             ; block start @ 2000
          sta RAMPtrH 
          jsr testMem          ; execute test
          lda TestResult       ; TestResult?
          beq xlnkBlk2         ; 0 -> passed
          ; not passed:
          lda #%00000010       ; set bit#1
          ora XLnk             ; in 
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk

xlnkBlk2  lda TestBlk          ; is block 2 excluded
          cmp #2                   
          beq xlnkBlk3         ; yes: skip block 2
  
          lda #%00000100       ; bit#2
          and RAMfound         ; is block 2 detected?
          beq xlnkBlk3         ; no: skip block 2
          
          lda #$00      
          sta RAMfill          ; test for #$00
          lda #$20                
          sta NumPages         ; 32 pages
          lda #$40             ; block start @ 4000
          sta RAMPtrH 
          jsr testMem          ; execute test
          lda TestResult       ; TestResult?
          beq xlnkBlk3         ; 0 -> passed
          ; not passed:
          lda #%00000100       ; set bit#2
          ora XLnk             ; in 
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk          

xlnkBlk3  lda TestBlk          ; is block 3 excluded
          cmp #3                   
          beq xlnkBlk4         ; yes: skip block 3
  
          lda #%00001000       ; bit#3
          and RAMfound         ; is block 3 detected?
          beq xlnkBlk4         ; no: skip block 3
          
          lda #$00      
          sta RAMfill          ; test for #$00
          lda #$20                
          sta NumPages         ; 32 pages
          lda #$60             ; block start @ 6000
          sta RAMPtrH 
          jsr testMem          ; execute test
          lda TestResult       ; TestResult?
          beq xlnkBlk4         ; 0 -> passed
          ; not passed:
          lda #%00001000       ; set bit#3
          ora XLnk             ; in 
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk          
xlnkBlk4  lda TestBlk          ; is block 4 excluded
          cmp #4                   
          beq xlnkEnd          ; yes: skip block 4
  
          lda #%00010000       ; bit#4
          and RAMfound         ; is block 4 detected?
          beq xlnkEnd          ; no: skip block 4
          
          lda #$00      
          sta RAMfill          ; test for #$00
          lda #$20                
          sta NumPages         ; 32 pages
          lda #$a0             ; block start @ a000
          sta RAMPtrH 
          jsr testMem          ; execute test
          lda TestResult       ; TestResult?
          beq xlnkEnd          ; 0 -> passed
          ; not passed:
          lda #%00010000       ; set bit#4
          ora XLnk             ; in 
          sta XLnk             ; cross link flags
          ora StickyXLnk
          sta StickyXLnk       ; set bits in sticky XLnk          
xlnkEnd
          pla                  ; restore variable TestResult
          sta TestResult
          rts

;===================================================
; printXlnk
; prints all cross link status
;===================================================
printXlnk lda #$04
          sta index1          ; initialize index variable
          lda #%00010000       
          sta BitPattern      ; initialize bit pattern 
          ; for index1...
printXa                       ; set cursor position
          lda #titlerow+1     
          clc
          adc index1
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
          
printXf   dec index1          ; decrement index variable
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
           ldy #0
           clc
           jsr posCursor
           lda #<exp3k
           ldy #>exp3k
           jsr printStr
                                   ; print blk1 text
           ldx #titlerow+2
           ldy #0
           clc
           jsr posCursor
           lda #<expblk1
           ldy #>expblk1
           jsr printStr
                                   ; print blk2 text
           ldx #titlerow+3
           ldy #0
           clc
           jsr posCursor
           lda #<expblk2
           ldy #>expblk2
           jsr printStr
           
                                   ; print blk3 text
           ldx #titlerow+4
           ldy #0
           clc
           jsr posCursor
           lda #<expblk3
           ldy #>expblk3
           jsr printStr

!ifdef RAM {           
                                   ; print blk5 text
           ldx #titlerow+5
           ldy #0
           clc
           jsr posCursor
           lda #<expblk5
           ldy #>expblk5
           jsr printStr
}           
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
           lda #0                ; reset the variable, that holds the found RAM blocks
           sta RAMfound
           
           lda #0                ; detect 3k@0400 
           sta TestBlk
           jsr detectBlock       ; detect RAM
           
           ldx #titlerow+1       ; position cursor (3k)
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
           
           lda #1                ; detect 8k@2000 
           sta TestBlk
           jsr detectBlock       ; detect RAM
           
           ldx #titlerow+2       ; position cursor ($2000)
           ldy #foundcol 
           clc
           jsr posCursor
           
           lda TestResult
           bne RAMsrch3          ; TestResult != 0 => RAM not found
            
           jsr setBlue
           lda #<txtyes          ; RAM detected: print "yes"
           ldy #>txtyes          
           jmp RAMsrch4
           
RAMsrch3   jsr setRed
           lda #<txtno           ; RAM not detected => print "no"
           ldy #>txtno
           
RAMsrch4   jsr printStr
            
           lda #2                ; detect 8k@4000 
           sta TestBlk
           jsr detectBlock       ; detect RAM
           
           ldx #titlerow+3       ; position cursor ($4000)
           ldy #foundcol
           clc
           jsr posCursor
           
           lda TestResult        ; TestResult != 0 => RAM not found
           bne RAMsrch5 
           
           jsr setBlue
           lda #<txtyes          ; RAM detected: print "yes"
           ldy #>txtyes          
           jmp RAMsrch6
           
RAMsrch5   jsr setRed
           lda #<txtno           ; RAM not detected => print "no"
           ldy #>txtno
           
           
RAMsrch6   jsr printStr
           
           lda #3                ; detect 8k@6000 
           sta TestBlk
           jsr detectBlock       ; detect RAM
           
           ldx #titlerow+4       ; position cursor ($6000)
           ldy #foundcol
           clc
           jsr posCursor
           
           lda TestResult        ; TestResult != 0 => RAM not found
           bne RAMsrch7 
           
           jsr setBlue
           lda #<txtyes          ; RAM detected: print "yes"
           ldy #>txtyes          
           jmp RAMsrch8
           
RAMsrch7   jsr setRed
           lda #<txtno           ; RAM not detected => print "no"
           ldy #>txtno
           
RAMsrch8   jsr printStr
!ifdef RAM {
           
           lda #4                ; detect 8k@A000 
           sta TestBlk
           jsr detectBlock       ; detect RAM
           
           ldx #titlerow+5       ; position cursor ($A000)
           ldy #foundcol
           clc
           jsr posCursor
           
           lda TestResult        ; TestResult != 0 => RAM not found
           bne RAMsrch9 
           
           jsr setBlue           ; blue           
           lda #<txtyes          ; RAM detected: print "yes"
           ldy #>txtyes          
           jmp RAMsrch10
           
RAMsrch9   jsr setRed
           lda #<txtno           ; RAM not detected => print "no"
           ldy #>txtno
RAMsrch10   jsr printStr
}  
           ; ====
           ; here, everything was searched and printed 

           ; ==== let the test begin!
test       jsr fillZero          ; fill all RAM with zero
           
           lda #%00000001        ; bit#0 for testing RAMfound
           and RAMfound
           beq testblk1          ; block 0 is not present. Skip!
           ; Block 0 found
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+1
           clc
           jsr posCursor
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           
           lda #$00
           sta TestBlk           
           jsr testBlock         ; test block 0
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+1
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk0a         ; is 0 -> passed 
           
           ; not passed:
           lda #%00000001        
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk0d
 
           ; passed
testblk0a  lda #%00000001        ; test bit in StickyBad
           and StickyBad
           beq testblk0b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk0c
           
testblk0b  jsr setBlue           ; ok is blue in case always ok  
testblk0c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk0d  jsr printStr          ; print the result in table   

           jsr printXlnk         ; print all cross linked results

testblk1   jsr fillZero          ; fill all RAM with zero
           
           lda #%00000010        ; bit#2 for testing RAMfound
           and RAMfound
           beq testblk2          ; block 1 is not present. Skip!
           ; Block 1 found
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+2
           clc
           jsr posCursor
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           
           lda #$01
           sta TestBlk           
           jsr testBlock         ; test block 1
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+2
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk1a         ; is 0 -> passed 
           
           ; not passed:
           lda #%00000010        
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk1d
 
           ; passed
testblk1a  lda #%00000010        ; test bit in StickyBad
           and StickyBad
           beq testblk1b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk1c
           
testblk1b  jsr setBlue           ; ok is blue in case always ok  
testblk1c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk1d  jsr printStr          ; print the result in table   

           jsr printXlnk         ; print all cross linked results   

testblk2   jsr fillZero          ; fill all RAM with zero
           
           lda #%00000100        ; bit#2 for testing RAMfound
           and RAMfound
           beq testblk3          ; block 2 is not present. Skip!
           ; Block 2 found
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+3
           clc
           jsr posCursor
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           
           lda #$02
           sta TestBlk           
           jsr testBlock         ; test block 2
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+3
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk2a         ; is 0 -> passed 
           
           ; not passed:
           lda #%00000100        
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk2d
 
           ; passed
testblk2a  lda #%00000100        ; test bit in StickyBad
           and StickyBad
           beq testblk2b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk2c
           
testblk2b  jsr setBlue           ; ok is blue in case always ok  
testblk2c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk2d  jsr printStr          ; print the result in table   

           jsr printXlnk         ; print all cross linked results   

testblk3   jsr fillZero          ; fill all RAM with zero
           
           lda #%00001000        ; bit#2 for testing RAMfound
           and RAMfound
           beq testblk4          ; block 3 is not present. Skip!
           ; Block 3 found
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+4
           clc
           jsr posCursor
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           
           lda #$03
           sta TestBlk           
           jsr testBlock         ; test block 3
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+4
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk3a         ; is 0 -> passed 
           
           ; not passed:
           lda #%00001000        
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk3d
 
           ; passed
testblk3a  lda #%00001000        ; test bit in StickyBad
           and StickyBad
           beq testblk3b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk3c
           
testblk3b  jsr setBlue           ; ok is blue in case always ok  
testblk3c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk3d  jsr printStr          ; print the result in table   

           jsr printXlnk         ; print all cross linked results   
testblk4   
!ifdef RAM {           
           jsr fillZero
           lda #%00010000        ; bit#4 for testing RAMfound
           and RAMfound
           beq testcount         ; block 4 is not present. Skip!
           ; Block 4 found
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+5
           clc
           jsr posCursor
           jsr setBlk            ; set color black
           
           lda #<txtrun
           ldy #>txtrun
           jsr printStr          ; print "RUN"
           
           lda #$04
           sta TestBlk           
           jsr testBlock         ; test block 4
           
           jsr testXlnk          ; test other block for corruption/cross linked
           
           ldy #testcol          ; position cursor in table (test column)
           ldx #titlerow+5
           clc
           jsr posCursor
           
           lda TestResult        ; if TestResult
           beq testblk4a         ; is 0 -> passed 
           
           ; not passed:
           lda #%00010000        
           ora StickyBad
           sta StickyBad         ; set bit in StickyBad
           
           jsr setRed            ; errors are red 
           lda #<txtbad          ; set pointer to "BAD"
           ldy #>txtbad
           jmp testblk4d
 
           ; passed
testblk4a  lda #%00010000        ; test bit in StickyBad
           and StickyBad
           beq testblk4b
           jsr setRed            ; former errors (sticky!) are red 
           jmp testblk4c
           
testblk4b  jsr setBlue           ; ok is blue in case always ok  
testblk4c  lda #<txtok           ; set pointer to "OK"
           ldy #>txtok          
testblk4d  jsr printStr          ; print the result in table   

           jsr printXlnk         ; print all cross linked results   
}

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
           lda #<txtbad           ; set pointer to "BAD"
           ldy #>txtbad          
           jsr printStr          ; print bad
testend           
           jmp test
           
;end        rts        