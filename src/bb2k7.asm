;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $c000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $c000
banks 1
.endro

; this demo uses no RAM! (apart from stack)

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 1.00,"Bock's Birthday 2007","Happy birthday Bock!","Maxim"

.bank 0 slot 0
.org $0000

.include "Phantasy Star decompressors.inc"

;==============================================================
; Boot section
;==============================================================
.org $0000
.section "Boot section" force
  di              ; disable interrupts
  im 1            ; Interrupt mode 1
  jp main         ; jump to main program
.ends

;==============================================================
; VBlank handler
;==============================================================
.org $0038
.section "VBlank handler" force
  ; do nothing much in here
  push af
    in a,($bf)
  pop af
  ei
  reti
.ends

;==============================================================
; Pause button handler
;==============================================================
.org $0066
.section "Pause button handler" force
  ; Do nothing
  retn
.ends

;==============================================================
; Main program
;==============================================================
.section "Main program" free
main:
  ld sp, $dff0

  ; Initialise VDP
  call DefaultInitialiseVDP
  call ClearVRAM

  call NoSprites ; they mess things up

loop:
  ; Load tiles
  ld de,$4000
  ld hl,bockstiles
  call LoadTiles4BitRLE

  ; Load tilemap
  ld hl,bockstilemap
  ld de,$3800 | $4000
  call LoadTilemapToVRAM

  call ZoomAnimate

  ; Load tiles
  ld de,$4000
  ld hl,birthdaytiles
  call LoadTiles4BitRLE

  ; Load tilemap
  ld hl,birthdaytilemap
  ld de,$3800 | $4000
  call LoadTilemapToVRAM

  call ZoomAnimate

  ; Load tiles
  ld de,$4000
  ld hl,z2007tiles
  call LoadTiles4BitRLE

  ; Load tilemap
  ld hl,z2007tilemap
  ld de,$3800 | $4000
  call LoadTilemapToVRAM

  call ZoomAnimate

  ; test card section
  ld de,$4000
  ld hl,testcardtiles
  call LoadTiles4BitRLE

  ld hl,testcardtilemap
  ld de,$3800 | $4000
  call LoadTilemapToVRAM

  ; load first palette
  ld hl,$c000                     ; palette index 0 write address
  call VRAMToHL
  ld hl,testcardpalettes
  ld bc,11
  call WriteToVRAM

  call ScreenOn

  ld a,60*2
  call WaitAFrames

  ld hl,$c000                     ; palette index 0 write address
  call VRAMToHL
  ld hl,testcardpalettes+11
  ld bc,11
  call WriteToVRAM

  call WaitForButton
  call ScreenOff

  jp loop
.ends

.section "Wait a frames" free
WaitAFrames:
  push bc
    ei
    ld b,a
  -:halt
    djnz -
    di
  pop bc
  ret
.ends

.section "Zoom animate" free
ZoomAnimate:
  ; load first palette
  ld hl,$c000                     ; palette index 0 write address
  call VRAMToHL
  ld hl,z2007palettes             ; data
  ld bc,16
  call WriteToVRAM

  call ScreenOn

  ; animate
  ld hl,z2007palettes+16
  ld b,3
--:
  ld a,6
  call WaitAFrames
  push hl
    ld hl,$c000                     ; palette index 0 write address
    call VRAMToHL
  pop hl
  push bc
    ; hl = source, will be incremented
    ld bc,16
    call WriteToVRAM
  pop bc
  djnz --

  ld a,60*2
  call WaitAFrames

  call ScreenOff

  ret
.ends

;==============================================================
; Data
;==============================================================
.section "data 1" superfree
bockstiles:
.incbin "bocks (tiles).pscompr"
.ends
.section "data 2" superfree
bockstilemap:
.incbin "bocks (tile numbers).pscompr"
.ends
.section "data 3" superfree
birthdaytiles:
.incbin "birthday (tiles).pscompr"
.ends
.section "data 4" superfree
birthdaytilemap:
.incbin "birthday (tile numbers).pscompr"
.ends
.section "data 5" superfree
z2007tiles:
.incbin "2007 (tiles).pscompr"
.ends
.section "data 6" superfree
z2007tilemap:
.incbin "2007 (tile numbers).pscompr"
.ends
.section "data 7" superfree
z2007palettes:
.define B $00
.define W $3f
.db B B B B W W W W W W B B W W B B ; large
.db B W W B B W W B W W W W B B B B
.db B B B B B B B B W W W W W W W W
.db B B W W B B W W B W B W B W B W ; small
.undef B
.undef W
.ends

.section "data 8" superfree
  testcardtiles:
  .incbin "testcard (tiles).pscompr"
.ends
.section "data 9" superfree
testcardtilemap:
.incbin "testcard (tile numbers).pscompr"
.ends
.section "data 10" superfree
testcardpalettes:
.db $00 $00 $00 $00 $00 $00 $00 $3f $00 $00 $00 ; before
.db $00 $15 $30 $0C $3C $03 $33 $3f $0F $2A $3F ; after
.ends


;==============================================================
; Set up VDP registers (default values)
;==============================================================
; Call DefaultInitialiseVDP to set up VDP to default values.
; Also defines NameTableAddress, SpriteTableAddress and SpriteSet
; which can be used after this code in the source file.
; To change the values used, copy and paste the modified data
; and code into the main source. Data is commented to help.
;==============================================================
.section "Initialise VDP to defaults" free
DefaultInitialiseVDP:
    push hl
    push bc
        ld hl,_Data
        ld b,_End-_Data
        ld c,$bf
        otir
    pop bc
    pop hl
    ret

.define SpriteSet           0       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

_Data:
    .db %00000100,$80
    ;    |||||||`- Disable synch
    ;    ||||||`-- Enable extra height modes
    ;    |||||`--- SMS mode instead of SG
    ;    ||||`---- Shift sprites left 8 pixels
    ;    |||`----- Enable line interrupts
    ;    ||`------ Blank leftmost column for scrolling
    ;    |`------- Fix top 2 rows during horizontal scrolling
    ;    `-------- Fix right 8 columns during vertical scrolling
    .db %10000100,$81
    ;     |||| |`- Zoomed sprites -> 16x16 pixels
    ;     |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;     |||`---- 30 row/240 line mode
    ;     ||`----- 28 row/224 line mode
    ;     |`------ Enable VBlank interrupts
    ;     `------- Enable display
    .db (NameTableAddress>>10) |%11110001,$82
    .db (SpriteTableAddress>>7)|%10000001,$85
    .db (SpriteSet<<2)         |%11111011,$86
    .db $f|$f0,$87
    ;    `-------- Border palette colour (sprite palette)
    .db $00,$88
    ;    ``------- Horizontal scroll
    .db $00,$89
    ;    ``------- Vertical scroll
    .db $ff,$8a
    ;    ``------- Line interrupt spacing ($ff to disable)
_End:
.ends

;==============================================================
; Clear VRAM
;==============================================================
; Sets all of VRAM to zero
;==============================================================
.section "Clear VRAM" free
ClearVRAM:
  push af
  push hl
    ld hl,$4000
    call VRAMToHL
    ; Output 16KB of zeroes
    ld hl, $4000    ; Counter for 16KB of VRAM
  -:ld a,$00        ; Value to write
    out ($be),a ; Output to VRAM address, which is auto-incremented after each write
    dec hl
    ld a,h
    or l
    jp nz,-
  pop hl
  pop af
  ret
.ends

;==============================================================
; VRAM to HL
;==============================================================
; Sets VRAM write address to hl
;==============================================================
.section "VRAM to HL" free
VRAMToHL:
  push af
    ld a,l
    out ($bf),a
    ld a,h
    out ($bf),a
  pop af
  ret
.ends

;==============================================================
; VRAM writer
;==============================================================
; Writes BC bytes from HL to VRAM
; Clobbers HL, BC, A
;==============================================================
.section "Raw VRAM writer" free
WriteToVRAM:
-:ld a,(hl)
  out ($be),a
  inc hl
  dec bc
  ld a,c
  or b
  jp nz,-
  ret
.ends

;==============================================================
; Sprite disabler
;==============================================================
; Sets sprite 1 to y=208
; Clobbers HL, A
;==============================================================
.section "No sprites" free
NoSprites:
  ld hl,SpriteTableAddress | $4000
  call VRAMToHL
  ld a,208
  out ($be),a
  ret
.ends

;==============================================================
; Wait for button press
;==============================================================
; Clobbers A
; Not very efficient, I'm aiming for simplicity here
;==============================================================
.section "Wait for button press" free
WaitForButton:
-:in a,$dc ; get input
  cpl      ; invert bits
  or a     ; test bits
  jr nz,-  ; wait for no button press
-:in a,$dc ; get input
  cpl      ; invert bits
  or a     ; see if any are set
  jr z,-
  ret
.ends

.section "Screen on/off" free
ScreenOn:
  ld a,$e4
  jr +
ScreenOff:
  ld a,$a4
+:out ($bf),a
  ld a,$81
  out ($bf),a
  ret
.ends
