;************************************************************
;
; Crates 3D v1.0
; ==============
; for Ion on the TI-83/TI-83+
;
; by Badja
; 3 July 2001
;
; http://badja.calc.org
; badja@calc.org
;
; You may modify this source code for personal use only.
; You may NOT distribute the modified source or program file.
;
;************************************************************


      .nolist
      #include "ion.inc"
      .list

      #define DefaultSpriteHeight   10                ; for sprite clip routine
      #define NO_MOD_AX                               ;
      #define GRAPH_MEM             plotsscreen       ;

      #define viewWidth             15                ; width of view
      #define viewHeight            18                ; height of view

      #define pushing               asm_flag1_0       ; is player pressing the push key?
      #define keyDelay              asm_flag1_1       ; typematic key delay?

      #define yPos                  saferam3          ; y-position of player
      #define xPos                  yPos+1            ; x-position of player
      #define zPos                  xPos+1            ; z-position of player
      #define yFinish               zPos+1            ; y-position of finish
      #define xFinish               yFinish+1         ; x-position of finish
      #define zFinish               xFinish+1         ; z-position of finish
      #define numLevels             zFinish+1         ; number of levels in current level set
      #define levelSet              numLevels+1       ; address of current level set data
      #define angle                 levelSet+2        ; current viewing angle
      #define detectPtr1            angle+1           ; address of next entry in program table
      #define detectPtr2            detectPtr1+2      ; address of current level set program

      #define pos                   yPos              ; xPos and yPos
      #define finish                yFinish           ; xFinish and yFinish


#ifdef TI83P
      .org  progstart-2
      .db   $BB,$6D
#else
      .org  progstart
#endif
      ret
      jr    nc,start
      .db   "Crates 3D",0
start:
      ld    hl,(vat)                ; check if any level sets exist
      ld    ix,strDetect
      call  ionDetect
      ret   nz

      set   textwrite,(iy+sgrflags) ; write all text to graph buffer
      jr    skipDetect
detect:
      ld    ix,strDetect            ; detect next level set
      call  ionDetect
      jr    z,skipDetect
      ld    hl,(vat)
      jr    detect
skipDetect:
      ld    (detectPtr1),de         ; save program pointers
      ld    (detectPtr2),hl
      push  de
      push  hl
      ld    de,yPos                 ; load title-screen level variables
      ld    hl,titlePos
      ld    bc,6
      ldir
      ld    a,b                     ; set viewing angle to 0
      ld    (angle),a
      ld    de,saferam1             ; load title-screen level data
      ld    hl,titleLevel
      call  DispRLE
      call  modifyDrawMap
      call  drawMap                 ; draw title-screen level
      ld    bc,0                    ; draw title-screen text
      ld    hl,strAlpha
      call  putString
      ld    bc,256*12+4
      ld    hl,strURL
      call  putString
      ld    bc,256*18
      ld    hl,strClear
      call  putString
      ld    bc,256*24
      ld    hl,str2nd
      call  putString
      ld    a,5
      ld    hl,256*6+4
      ld    (pencol),hl
      bcall(_vputmap)
      pop   hl
      bcall(_vputs)
      push  hl
      call  ionFastCopy
      pop   de
      pop   hl
      call  delay
menuLoop:
      call  resetKeyport
      ld    a,$df
      out   (1),a
      in    a,(1)
      cp    127               ; ALPHA
      jr    z,detect          ; change level set
      call  resetKeyport
      ld    a,$bf
      out   (1),a
      in    a,(1)
      cp    223               ; 2nd
      jr    z,play            ; play current level set
      call  resetKeyport
      ld    a,$fd
      out   (1),a
      in    a,(1)
      cp    191               ; CLEAR
      ret   z                 ; exit game
      jr    menuLoop

play:
      ex    de,hl
      ld    a,(hl)            ; read number of levels
      ld    (numLevels),a
      inc   hl
      ld    (levelSet),hl     ; save address of level set data

playLevel:
      ld    hl,(levelSet)     ; load chosen level
      ld    a,(hl)
      inc   hl
      inc   a
      ld    b,a
findLevel:
      push  bc
      ld    de,yPos           ; load level variables
      ld    bc,6
      ldir
      ld    de,saferam1
      call  DispRLE           ; load level data
      pop   bc
      djnz  findLevel         ; repeat until at chosen level

      xor   a                 ; set viewing angle to 0
      ld    (angle),a
      call  modifyDrawMap     ; modify the drawMap routine for current viewing angle
      call  drawScreen        ; draw the level
gameLoop:
      res   pushing,(iy+asm_flag1)  ; reset pushing flag
      ld    hl,angle
      call  resetKeyport
      ld    a,$bf
      out   (1),a
      in    a,(1)
      cp    247               ; WINDOW
      jp    z,rotateRight     ; rotate viewing angle right
      cp    239               ; Y=
      jp    z,rotateLeft      ; rotate viewing angle left
      cp    223               ; 2nd
      jr    nz,noPushKey
      set   pushing,(iy+asm_flag1)  ; set pushing flag
noPushKey:
      cp    127               ; DEL
      jp    z,startAgain1     ; restart level
      ld    hl,(pos)
      ld    d,h
      ld    e,l               ; DE = player position, HL = player position
      call  resetKeyport
      ld    a,$fe
      out   (1),a
      in    a,(1)
      ld    b,a
      ld    a,(angle)         ; check current viewing angle
      and   %00000011
      cp    3
      jr    z,angle3Move
      cp    2
      jr    z,angle2Move
      or    a
      jr    nz,angle1Move

      bit   0,b
      jr    z,down
      bit   1,b
      jr    z,left
      bit   2,b
      jr    z,right
      bit   3,b
      jr    z,up
      jr    doneMove
angle1Move:
      bit   0,b
      jr    z,left
      bit   1,b
      jr    z,up
      bit   2,b
      jr    z,down
      bit   3,b
      jr    z,right
      jr    doneMove
angle2Move:
      bit   0,b
      jr    z,up
      bit   1,b
      jr    z,right
      bit   2,b
      jr    z,left
      bit   3,b
      jr    z,down
      jr    doneMove
angle3Move:
      bit   0,b
      jr    z,right
      bit   1,b
      jr    z,down
      bit   2,b
      jr    z,up
      bit   3,b
      jr    z,left
doneMove:
      set   keyDelay,(iy+asm_flag1) ; set key delay flag
      call  resetKeyport
      ld    a,$fd
      out   (1),a
      in    a,(1)
      cp    253               ; plus
      jp    z,nextLevel       ; go to next level
      cp    251               ; minus
      jp    z,previousLevel   ; go to previous level
      cp    191               ; CLEAR
      jr    nz,doneKeys
      ld    de,(detectPtr1)   ; restore program pointers
      ld    hl,(detectPtr2)
      jp    z,skipDetect      ; go back to title screen
doneKeys:
      jp    gameLoop

up:
      dec   e           ; move player up
      dec   l
      dec   l
      jr    handleMove
down:
      inc   e           ; move player down
      inc   l
      inc   l
      jr    handleMove
left:
      dec   d           ; move player left
      dec   h
      dec   h
      jr    handleMove
right:
      inc   d           ; move player right
      inc   h
      inc   h
handleMove:
      ; DE = player's destination
      bit   pushing,(iy+asm_flag1)
      push  de
      jr    nz,handlePush     ; player is pressing push key
pushFailed:
      pop   de
      ld    a,d               ; check that destination is not off-map
      cp    32
      jr    nc,doneKeys
      ld    a,e
      cp    24
      jr    nc,doneKeys
      call  lookupCell        ; get byte for destination cell
      ld    a,(zPos)          ; check if move is legal
      ld    c,a
      neg
      add   a,7
      ld    b,a
      ld    a,(hl)
      jr    z,topLevel
      jr    nc,veryTopLevel
moveLoop:
      rla
      djnz  moveLoop
      jr    nc,noBlockAbove
      rla
      jr    c,doneKeys
      jr    noBlockInFront
noBlockAbove:
topLevel:
      rla
      jr    c,stepUp
noBlockInFront:
veryTopLevel:
      rla
      jr    c,stepAcross
      ld    b,a
      ld    a,c
      cp    1
      jr    z,doneKeys
      rl    b
      jr    c,stepDown
      jr    doneKeys

stepUp:
      push  bc          ; check that no block exists above player
      push  de
      ld    de,(pos)
      call  lookupCell
      pop   de
      pop   bc
      ld    a,(hl)
      rra
      rra
      ld    b,c
stepUpLoop:
      rra
      djnz  stepUpLoop
      jr    c,doneKeys
      ld    a,c
      inc   a
      jr    doneHeight
stepAcross:
      ld    a,c
      jr    doneHeight
stepDown:
      ld    a,c
      dec   a
doneHeight:
      ld    (zPos),a    ; update player position
      ld    (pos),de
      call  drawScreen  ; draw the level
      jr    checkFinish


handlePush:
      ; DE = player's destination, HL = block's destination
      ld    a,h               ; check that block's destination is not off-map
      cp    32
      jr    nc,toPushFailed
      ld    a,l
      cp    24
      jr    nc,toPushFailed
      ld    a,(xFinish)       ; check that block being pushed is not the finish
      cp    d
      jr    nz,notFinish
      ld    a,(yFinish)
      cp    e
      jr    nz,notFinish
      ld    a,(zFinish)
      ld    b,a
      ld    a,(zPos)
      cp    b
      jr    nz,notFinish
toPushFailed:
      jp    pushFailed
notFinish:
      push  hl
      call  lookupCell        ; check if push is legal
      ld    a,(zPos)
      ld    b,a
      ld    a,(hl)
      pop   hl
pushLoop1:
      rra
      djnz  pushLoop1
      jr    nc,toPushFailed
      rra
      jr    nc,toPushFailed
      ex    de,hl
      push  hl
      call  lookupCell
      ld    a,(zPos)
      inc   a
      ld    b,a
      ld    a,(hl)
      pop   hl
pushLoop2:
      rra
      djnz  pushLoop2
      jr    c,toPushFailed
      pop   af

      ld    (pos),hl    ; update player position
      ex    de,hl
      ; DE = player's destination, HL = block's destination
      push  hl
      call  lookupCell  ; remove block from source
      ld    a,(zPos)
      ld    b,a
      ld    a,1
pushLoop3:
      sla   a
      djnz  pushLoop3
      ld    b,a
      cpl
      and   (hl)
      ld    (hl),a
      pop   de
      push  bc
      call  lookupCell  ; place block at destination
      pop   bc
      ld    c,(hl)
fallLoop:               ; make block fall
      ld    a,b
      or    c
      ld    (hl),a
      push  bc
      push  hl
      call  drawScreen
      pop   hl
      pop   bc
      srl   b
      jr    c,fallOff   ; block fell off level
      ld    a,c
      and   b
      jr    nz,checkFinish
      jr    fallLoop
fallOff:
      ld    a,c         ; remove the block
      or    b
      ld    (hl),a
      call  drawScreen

checkFinish:
      bit   keyDelay,(iy+asm_flag1) ; delay if necessary
      call  nz,delay
      res   keyDelay,(iy+asm_flag1) ; turn off key delay
      ld    de,(pos)                ; check if player is at finish
      ld    hl,(finish)
      ld    a,d
      cp    h
      jr    nz,notAtFinish
      ld    a,e
      cp    l
      jr    nz,notAtFinish
      ld    a,(zPos)
      dec   a
      ld    b,a
      ld    a,(zFinish)
      cp    b
      jr    z,nextLevel             ; if so, go to next level
notAtFinish:
      jr    doneRedraw


rotateRight:
      dec   (hl)              ; decrease viewing angle
      jr    redraw
rotateLeft:
      inc   (hl)              ; incease viewing angle
redraw:
      call  modifyDrawMap     ; modify drawMap for this viewing angle
      call  drawScreen        ; draw the level
      call  delay
doneRedraw:
      jp    gameLoop


nextLevel:
      ld    hl,(levelSet)     ; go to next level
      ld    a,(numLevels)
      ld    b,a
      ld    a,(hl)
      inc   a
      cp    b
      jr    nz,notLastLevel
      xor   a
notLastLevel:
      jr    startAgain

previousLevel:
      ld    hl,(levelSet)     ; go to previous level
      ld    a,(numLevels)
      ld    b,a
      ld    a,(hl)
      cp    0
      jr    nz,notFirstLevel
      ld    a,b
notFirstLevel:
      dec   a
startAgain:
      ld    (hl),a
startAgain1:
      call  delay
      jp    playLevel


modifyDrawMap:
      ld    bc,6        ; modify the drawMap routine for the current viewing angle
      ld    de,set6
      ld    a,(angle)
      and   %00000011
      cp    3
      jr    z,angle3
      cp    2
      jr    z,angle2
      or    a
      jr    nz,angle1
angle0:
      call  setSub1
      call  setSub3
      call  setSub5
      ld    hl,(2*viewWidth+1)+32
      ld    (set13+1),hl
      ret
angle1:
      call  setSub2
      call  setSub3
      call  setSub6
      ld    hl,(2*viewHeight+1)-32
      ld    (set13+1),hl
      ret
angle2:
      call  setSub1
      call  setSub4
      call  setSub6
      ld    hl,-(2*viewWidth+1)-32
      ld    (set13+1),hl
      ret
angle3:
      call  setSub2
      call  setSub4
      call  setSub5
      ld    hl,-(2*viewHeight+1)+32
      ld    (set13+1),hl
      ret

setSub1:
      ld    a,viewWidth+1
      ld    (set1+1),a
      ld    a,viewWidth
      ld    (set2+1),a
      ld    a,viewHeight+1
      ld    (set3+1),a
      ld    a,viewHeight
      ld    (set4+1),a
      ld    hl,256*((4*viewWidth)-(2*viewHeight)+44) + (-(2*viewWidth)-(3*viewHeight)+41)
      ld    (set5+1),hl
      ld    hl,nextCode1
      ldir
      ld    a,4*(2*viewWidth+1)+2
      ld    (set10+1),a
      ld    a,2*(2*viewWidth+1)-3
      ld    (set11+1),a
      ld    a,2*viewWidth+1
      ld    (set12+1),a
      ret

setSub2:
      ld    a,viewHeight+1
      ld    (set1+1),a
      ld    a,viewHeight
      ld    (set2+1),a
      ld    a,viewWidth+1
      ld    (set3+1),a
      ld    a,viewWidth
      ld    (set4+1),a
      ld    hl,256*((4*viewWidth)-(2*viewHeight)+44) + (-(2*viewWidth)-(3*viewHeight)+41)
      ld    (set5+1),hl
      ld    hl,nextCode2
      ldir
      ld    a,-2*(2*viewHeight+1)-4
      ld    (set10+1),a
      ld    a,3*(2*viewHeight+1)-2
      ld    (set11+1),a
      ld    a,2*viewHeight+1
      ld    (set12+1),a
      ret

setSub3:
      ld    a,$d6             ; sub n
      ld    (set1),a
      ld    a,$c6             ; add a,n
      ld    (set2),a
      ld    hl,$152b          ; dec hl, dec d
      ld    (set7),hl
      ld    a,$c6             ; add a,n
      ld    (set12),a
      ret

setSub4:
      ld    a,$c6             ; add a,n
      ld    (set1),a
      ld    a,$d6             ; sub n
      ld    (set2),a
      ld    hl,$1423          ; inc hl, inc d
      ld    (set7),hl
      ld    a,$d6             ; sub n
      ld    (set12),a
      ret

setSub5:
      ld    a,$c6             ; add a,n
      ld    (set3),a
      ld    a,$d6             ; sub n
      ld    (set4),a
      ld    a,$1c             ; inc e
      ld    (set8),a
      ld    a,24
      ld    (set9+1),a
      ret

setSub6:
      ld    a,$d6             ; sub n
      ld    (set3),a
      ld    a,$c6             ; add a,n
      ld    (set4),a
      ld    a,$1d             ; dec e
      ld    (set8),a
      ld    a,-1
      ld    (set9+1),a
      ret

nextCode1:
      ld    a,b               ; 6-byte code patch
      sub   4
      ld    b,a
      inc   c
      inc   c

nextCode2:
      inc   b                 ; 6-byte code patch
      inc   b
      inc   c
      inc   c
      inc   c
      nop


drawMap:
      bcall(_cleargbuf)
      ld    hl,(yPos)

      ld    a,h
set1:
      sub   viewWidth+1       ; self-modified
      ld    (setRowEnd+1),a
      ld    a,h
set2:
      add   a,viewWidth       ; self-modified
      ld    d,a

      ld    a,l
set3:
      add   a,viewHeight+1    ; self-modified
      ld    (setMapEnd+1),a
      ld    a,l
set4:
      sub   viewHeight        ; self-modified
      ld    e,a

      call  lookupCell

set5:
      ld    bc,256*((4*viewWidth)-(2*viewHeight)+44) + (-(2*viewWidth)-(3*viewHeight)+41) ; self-modified

drawRow:
      ld    a,d
      cp    32
      jr    nc,doneCell
      ld    a,e
      cp    24
      jr    nc,doneCell
      ld    a,(xPos)
      cp    d
      jr    nz,noPlayer
      ld    a,(yPos)
      cp    e
      jr    z,drawPlayerCell

noPlayer:
      ; draw cell without player
      ld    a,(hl)
      cp    0
      jr    z,doneCell
      push  bc
      push  de
      push  hl
      ld    h,a
      ld    a,(yFinish)
      cp    e
      ld    e,-1
      jr    nz,noFinish1
      ld    a,(xFinish)
      cp    d
      jr    nz,noFinish1
      ld    a,(zFinish)
      ld    e,a
noFinish1:
      ld    a,h
      ld    d,0
cellLoop1:
      rr    a                 ; can't use RRA - need to check Z flag
      push  af
      jr    nc,noBlock1
      ld    hl,sprBlock
      ld    a,d
      cp    e
      jr    nz,noFinishHere1
      ld    hl,sprBlack
noFinishHere1:
      push  bc
      push  de
      call  PutSprClp
      pop   de
      pop   bc
noBlock1:
      ld    a,c
      sub   4
      ld    c,a
      pop   af
      jr    z,noMoreCrates
      inc   d
      bit   3,d
      jr    z,cellLoop1
noMoreCrates:
      pop   hl
      pop   de
      pop   bc

doneCell:
set6:
      ld    a,b                     ; self-modified
      sub   4                       ;
      ld    b,a                     ;
      inc   c                       ;
      inc   c                       ;
set7:
      dec   hl                      ; self-modified
      dec   d                       ;
      ld    a,d
setRowEnd:
      cp    0                       ; self-modified by drawMap
      jr    nz,drawRow

set8:
      inc   e                       ; self-modified
      ld    a,e
setMapEnd:
      cp    0                       ; self-modified by drawMap
      ret   z
set9:
      cp    24                      ; self-modified
      ret   z
      ld    a,b
set10:
      add   a,4*(2*viewWidth+1)+2   ; self-modified
      ld    b,a
      ld    a,c
set11:
      sub   2*(2*viewWidth+1)-3     ; self-modified
      ld    c,a
      ld    a,d
set12:
      add   a,2*viewWidth+1         ; self-modified
      ld    d,a
      push  bc
set13:
      ld    bc,(2*viewWidth+1)+32   ; self-modified
      add   hl,bc
      pop   bc
      jp    drawRow

      ; draw cell with player
drawPlayerCell:
      ld    a,(hl)
      push  bc
      push  de
      push  hl
      ld    h,a
      ld    a,(yFinish)
      cp    e
      ld    e,-1
      jr    nz,noFinish2
      ld    a,(xFinish)
      cp    d
      jr    nz,noFinish2
      ld    a,(zFinish)
      ld    e,a
noFinish2:
      ld    a,h
      ld    d,0
cellLoop2:
      rra
      push  af
      push  bc
      push  de
      jr    nc,noBlock2
      ld    hl,sprBlock
      ld    a,d
      cp    e
      jr    nz,noFinishHere2
      ld    hl,sprBlack
noFinishHere2:
      call  PutSprClp
      jr    noPlayerHere
noBlock2:
      ld    a,(zPos)
      cp    d
      jr    nz,noPlayerHere
      ld    hl,sprBlack
      call  PutSprClp
noPlayerHere:
      pop   de
      pop   bc
      ld    a,c
      sub   4
      ld    c,a
      pop   af
      inc   d
      bit   3,d
      jr    z,cellLoop2
      xor   a
      bit   1,d
      jr    z,cellLoop2
      pop   hl
      pop   de
      pop   bc
      jr    doneCell


lookupCell:                   ; returns cell (A) in position (DE)
      ld    h,0
      bit   7,e
      jr    z,notNegative1
      ld    h,255
notNegative1:
      ld    l,e
      add   hl,hl
      add   hl,hl
      add   hl,hl
      add   hl,hl
      add   hl,hl
      ld    bc,saferam1
      add   hl,bc
      ld    b,0
      bit   7,d
      jr    z,notNegative2
      ld    b,255
notNegative2:
      ld    c,d
      add   hl,bc
      ret


drawScreen:                   ; draws the level and level number
      call  drawMap
      ld    hl,(levelSet)
      ld    a,(hl)
      inc   a
      ld    hl,0
      ld    (pencol),hl
      bcall(_setxxop1)
      ld    a,2
      bcall(_dispop1a)
      call  ionFastCopy
      ret


resetKeyport:
      ld    a,$ff             ; reset keyport
      out   (1),a
      ret


putString:
      ld    (pencol),bc
      bcall(_vputs)
      ret


delay:
      ld    bc,$6000
delayLoop:
      dec   bc
      ld    a,b
      or    c
      jr    nz,delayLoop
      ret


      #include spr30ax.inc    ; sprite clip routine
      #include rle.asm        ; RLE decompression routine


titlePos:                     ; title-screen level variables
.db $09,$11,$01,$09,$0F,$04

titleLevel:                   ; title-screen data
.db $91,$00,$00,$91,$00,$25,$20,$20,$20,$00,$20,$20,$20,$00,$20,$20
.db $30,$00,$20,$20,$20,$00,$20,$20,$20,$00,$20,$20,$20,$91,$00,$09
.db $20,$00,$00,$00,$20,$00,$20,$00,$20,$00,$20,$00,$00,$20,$00,$00
.db $20,$00,$00,$00,$20,$91,$00,$0B,$20,$00,$00,$00,$20,$20,$00,$00
.db $20,$20,$20,$00,$00,$20,$00,$00,$20,$20,$20,$00,$20,$20,$20,$91
.db $00,$09,$20,$00,$00,$00,$20,$00,$20,$00,$20,$00,$20,$00,$00,$20
.db $00,$00,$20,$91,$00,$05,$20,$91,$00,$09,$20,$20,$20,$00,$20,$00
.db $20,$00,$20,$00,$20,$00,$00,$20,$00,$00,$20,$20,$20,$00,$20,$20
.db $20,$91,$00,$35,$11,$15,$15,$0A,$00,$1F,$11,$11,$0E,$91,$00,$00
.db $91,$00,$06


sprBlock:         ; normal block sprite (masked)
.db %11110011
.db %11000011
.db %00000001
.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %10000000
.db %11000011
.db %11001111

.db %00001100
.db %00110100
.db %11000010
.db %11000011
.db %11101101
.db %11111011
.db %11110101
.db %01111011
.db %00111100
.db %00110000

sprBlack:         ; player/finish block sprite (masked)
.db %11110011
.db %11000011
.db %00000001
.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %10000000
.db %11000011
.db %11001111

.db %00001100
.db %00111100
.db %11111110
.db %10111101
.db %11010011
.db %11101111
.db %11101111
.db %01101111
.db %00111100
.db %00110000


strDetect:
.db   "C3D",0
strAlpha:
.db   "ALPHA: Change level set",0
strURL:
.db   "badja.calc.org",0
strClear:
.db   "CLEAR: Quit",0
str2nd:
.db   "2nd: Play",0

.end
