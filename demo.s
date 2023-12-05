.segment "HEADER"
  ;.byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
  counter: .res 1
  x_pos: .res 1
  y_pos: .res 1
  x_tile: .res 1
  y_tile: .res 1
  facing: .res 1
  direction: .res 1
  sprite_action: .res 1
  take_damage: .res 1
  controller1: .res 1
  controller2: .res 1
  on_ground: .res 1
  jump_frames: .res 1
  walking_step: .res 1

; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit $2002
  bpl vblankwait2


main:
load_palettes:
  lda $2002 ;reads from the CPU-RAM PPU address register to reset it
  lda #$3f  ;loads the higher byte of the PPU address register of the palettes in a (we want to write in $3f00 of the PPU since it is the address where the palettes of the PPU are stored)
  sta $2006 ;store what's in a (higher byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  lda #$00  ;loads the lower byte of the PPU address register in a
  sta $2006 ;store what's in a (lower byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  ldx #$00  ;AFTER THIS, THE PPU-RAM GRAPHICS POINTER WILL BE POINTING TO THE MEMORY LOCATION THAT CONTAINS THE SPRITES, NOW WE NEED TO TRANSFER SPRITES FROM THE CPU-ROM TO THE PPU-RAM
            ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT

; NO NEED TO MODIFY THIS LOOP SUBROUTINE, IT ALWAYS LOADS THE SAME AMOUNT OF PALETTE REGISTER. TO MODIFY PALETTES, REFER TO THE PALETTE SECTION
@loop: 
  lda palettes, x   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  sta $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  inx
  cpx #$20
  bne @loop


; displaySprites:
;   ldx #$00
; displaySpritesLoop:
;   lda sprites, x 	; oLad the hello message into SPR-RAM one by one, the pointer is increased every time a byte is written. Sprites are referenced by using the third byte of the 4-byte arrays in "hello"
;   sta $0200, x
;   inx
;   cpx #$48           ;ATTENTION: if you add more letters, you must increase this number by 4 per each additional letter. This is the limit for the sprite memory copy routine
;   bne displaySpritesLoop

constants:
  button_A = %10000000
  button_B = %01000000
  button_Start = %00010000
  button_Select = %00100000
  button_Up = %00001000
  button_down = %00000100
  button_left = %00000010
  button_right = %00000001

lda #$50
sta x_pos
lda #$20
sta y_pos
lda #$01
sta direction
lda #$00
sta take_damage
lda #$03
sta sprite_action


LoadBackground:
    LDA $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
    ldx #$00

Loop:
    lda background, x ; Loop que va a ayudar a cargar el fondo de pantalla
    sta $2007
    inx
    cpx #$00
    bne Loop
    ldx $00
Loop02:
    lda background02, x
    sta $2007
    inx
    cpx #$00
    bne Loop02
    ldx $00
Loop03:
    lda background03, x 
    sta $2007
    inx
    cpx #$00
    bne Loop03
    ldx $00
Loop04:
    lda background04, x 
    sta $2007
    inx
    cpx #$00
    bne Loop04


    
enable_rendering: ; DO NOT MODIFY THIS
  lda #%10010000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  ; use it to animate sprites statically
  jmp forever

; nmi:  ;WHENEVER AN NMI INTERRUPT OCCURS, THE PROGRAM JUMPS HERE (60fps)
;   ldx #$00 	; Set SPR-RAM address to 0
;   stx $2003 ;Sets the PPU-RAM pointer to $2003 to start receiving sprite information saved under the tag "hello"B



nmi:
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  lda #$00
  sta $2005

  jsr readcontroller1 ;reads both controllers
  jsr manageinput1  ;decifers controller 1 inputs
  jsr checkIfOnPlatform
  jsr checkIfOnGround
  jsr gravity
  jsr action_decider

rti



counter_section:
  inc counter
  ldx #$15
  cpx counter
  bne endCounterSection
  lda #$00
  sta counter
  ldx walking_step
  inx
  stx walking_step
  cpx #$04
  bne endCounterSection
  lda #$00
  sta walking_step
  endCounterSection:
  rts

walking_animation:
  ldx walking_step
  cpx #$00
  beq walk1
  cpx #$01
  beq stand
  cpx #$02
  beq walk2
  jmp stand

  walk1:
  lda #$01
  sta sprite_action
  rts

  walk2:
  lda #$02
  sta sprite_action
  rts

  stand:
  lda #$00
  sta sprite_action
  rts



gravity:
  lda on_ground
  cmp #$01
  beq noGravity
  lda y_pos
  clc
  adc #$03
  sta y_pos
  noGravity:
    rts

checkIfOnPlatform:
  lda x_pos
  cmp #$38
  bmi notOnPlatform
  cmp #$C0
  bpl notOnPlatform
  lda y_pos
  cmp #$AB
  bpl notOnPlatform ; branch if the register is larger than the one i'm comparing it to
  cmp #$A8
  bmi notOnPlatform ; branch if the register is smaller than the one i'm comparing it to
  lda #$01
  sta on_ground
  lda #$00
  sta jump_frames
  jmp endPlatformCheck
  notOnPlatform:
    lda #$00
    sta on_ground
  endPlatformCheck:
  rts

  

checkIfOnGround:
  lda y_pos
  cmp #$D3
  bpl notOnGround
  cmp #$D0
  bmi notOnGround
  lda #$01
  sta on_ground
  lda #$00
  sta jump_frames
  notOnGround:
  lda on_ground
  cmp #$01
  beq endGroundCheck
  lda #$03
  sta sprite_action
  endGroundCheck:
  rts

jump:
  lda y_pos
  clc
  sbc #$06
  sta y_pos
  inc jump_frames
  rts

readcontroller1:
  lda #1
  sta controller1

  sta $4016
  lda #0
  sta $4016

  read_loop:
    lda $4017
    lsr a
    rol controller2

    lda $4016
    lsr a
    rol controller1
    bcc read_loop

rts

manageinput1:
  lda #$00
  sta take_damage

  lda controller1
  cmp #$00
  beq noinput

  lda controller1
  and #button_right
  cmp #button_right
  bne notRight
  ;if right pressed
  jsr counter_section
  jsr walking_animation
  lda #01
  sta direction
  lda x_pos
  clc
  adc #$03
  tax
  cpx #$f3
  bcs notRight
  sta x_pos

  notRight:

  lda controller1
  and #button_left
  cmp #button_left
  bne notLeft
  ;if left pressed
  jsr counter_section
  jsr walking_animation
  lda #00
  sta direction
  lda x_pos
  clc
  sec
  sbc #$03
  bcc notLeft
  sta x_pos

  notLeft:

  lda controller1
  and #button_A
  cmp #button_A
  bne notA
  ; if A pressed
  lda jump_frames
  cmp #$10
  bpl notA
  jsr jump
  jmp endAcheck
  notA:
    lda #$11
    sta jump_frames
  endAcheck:

  lda controller1
  and #button_Select
  cmp #button_Select
  bne notSelect
  ;if select pressed
  lda #$01
  sta take_damage
  lda #$04
  sta sprite_action  

  notSelect:

  lda controller1
  and #button_Start
  cmp #button_Start
  bne notStart
  ;if start pressed
  lda #$01
  sta take_damage

  notStart:

  lda controller1
  and #button_B
  cmp #button_B
  bne notB
  ;if B pressed
  lda #$05
  sta sprite_action

  notB:

  rts
  noinput:
  lda #$00
  sta sprite_action
  ;if no button is pressed

rts



action_decider:
  lda x_pos
  sta x_tile
  lda y_pos
  sta y_tile
  ldx sprite_action
  cpx #$00
  beq display_base_kitty
  cpx #$01
  beq display_walking_kitty1
  cpx #$02
  beq display_walking_kitty2
  cpx #$03
  beq display_jumping_kitty
  cpx #$04
  beq display_gameover_kitty
  cpx #$05
  beq display_attack_kitty



display_base_kitty:
  ldx #$00
  jmp display_single_kitty

display_walking_kitty1:
  ldx #$08
  jmp display_single_kitty

display_walking_kitty2:
  ldx #$10
  jmp display_single_kitty
 
display_jumping_kitty:
  ldx #$18
  jmp display_single_kitty 

display_gameover_kitty:
  ldx #$20
  jmp display_single_kitty
  
display_attack_kitty:
  ldx #$28
  jmp display_single_kitty




display_scratch_sprites: 
  beq scratch
  rts
  scratch:
    lda facing
    cmp #$00
    beq left_scratch
    bne flip_scratch
    left_scratch:
      lda x_pos
      sec
      sbc #$08
      sta x_tile
      jmp load_scratch_bytes
    flip_scratch:
      lda x_tile
      clc
      adc #$08
      sta x_tile
      jmp load_scratch_bytes
    load_scratch_bytes:
    ldy #$30
    lda y_pos
    sta $0200, x
    inx
    lda sprites, y
    sta $0200, x
    inx
    iny
    lda facing
    sta $0200, x
    inx
    lda x_tile
    sta $0200, x
    inx

    lda y_pos
    clc
    adc #$08
    sta $0200, x
    inx
    lda sprites, y
    sta $0200, x
    inx
    iny
    lda facing
    sta $0200, x
    inx
    lda x_tile
    sta $0200, x
    inx
  rts


delete_scratch_sprites:
  bne delete_scratch
  rts    
  delete_scratch:
    lda #$00
    inx
    sta $0200, x
    inx
    inx
    inx
    inx
    sta $0200, x
  rts




direction_decider:
  beq left
  bne right

  left:
    ldy #$00
    lda take_damage
    cmp #$00
    beq store_left_attribute
    iny
    store_left_attribute:
      sty facing
      rts

  right:
    ldy #$40
    lda take_damage
    cmp #$00
    beq store_right_attribute
    iny
    store_right_attribute:
      sty facing

      txa
      clc
      adc #$04
      tax
      rts



display_single_kitty:
  ldy direction
  cpy #$00
  jsr direction_decider
  ldy #$00
  

set_sprite_tileattr_loop:
  iny
  lda sprites, x
  sta $0200, y
  inx
  iny
  lda facing
  sta $0200, y
  iny
  iny
  cpy #$10
  bne set_sprite_tileattr_loop

  ldx #$00
  ldy #$00

set_sprite_xy_loop:

  cpy #$00
  beq load_tiles
  jsr not_first_tile

  load_tiles:
    lda y_tile
    sta $0200, x
    inx
    inx
    inx
    lda x_tile
    sta $0200, x
    inx
    iny
    ;do a clear carry before???
    cpy #$04
    bne set_sprite_xy_loop


  lda sprite_action
  cmp #$05
  jsr display_scratch_sprites
  jsr delete_scratch_sprites
  rts




not_first_tile:
  cpy #$01
  beq right_tiles

  cpy #$03
  beq right_tiles

  cpy #$02
  beq lower_tiles

  right_tiles:
    lda x_tile
    clc
    adc #$08
    sta x_tile
    rts

  lower_tiles:
    lda y_tile
    clc

    adc #$08
    sta y_tile

    lda x_tile
    sec
    sbc #$08
    sta x_tile
    rts




palettes: 
;Background color
  .byte $31, $27, $17, $38
  .byte $31, $37, $27, $17
  .byte $31, $0F, $19, $29
  .byte $31, $28, $19, $29

;Sprite Palette 
  .byte $31, $0f, $28, $35 ;light-blue (transparent), black, golden-yellow, light-pink
  .byte $31, $06, $28, $35 ;light-blue (transparent), dark-red, golden-yellow, light-pink
  .byte $31, $31, $31, $31
  .byte $31, $31, $31, $31

  background:
	.byte $00,$00,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$35,$37,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$25,$36,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
   
  background02:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05
	.byte $06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09
	.byte $0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

  background03:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07
	.byte $08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09
	.byte $0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$05,$09,$18,$26,$26,$26,$26,$26,$26,$26
	.byte $16,$16,$16,$16,$16,$16,$16,$17,$0a,$06,$00,$00,$00,$00,$00,$00

  background04:
	.byte $00,$00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$00,$00,$00
	.byte $24,$21,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07
	.byte $08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$22,$13
	.byte $23,$00,$05,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09,$07,$09
	.byte $0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$08,$0a,$06,$00,$23
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
	.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
	.byte $fb,$3b,$00,$00,$00,$00,$00,$00,$8f,$0b,$02,$80,$00,$0a,$00,$00
	.byte $00,$08,$a0,$00,$80,$a0,$a0,$a3,$08,$0a,$00,$00,$08,$0a,$0a,$02
	.byte $aa,$aa,$00,$00,$00,$00,$00,$00,$02,$26,$00,$00,$00,$00,$00,$00
	.byte $20,$00,$00,$00,$00,$00,$00,$80,$05,$05,$05,$05,$05,$05,$05,$05



sprites:
  .byte $01 ;left base kitty
  .byte $02
  .byte $11
  .byte $12

  ;flip is $40

  .byte $02 ;right base kitty
  .byte $01
  .byte $12
  .byte $11

  .byte $03 ;left walking kitty 1
  .byte $04
  .byte $13
  .byte $14

  .byte $04 ;right walking kitty 1
  .byte $03
  .byte $14
  .byte $13

  .byte $03 ;left walking kitty 2 (uses the 'just bottom half')
  .byte $04
  .byte $15
  .byte $16

  .byte $04 ;right walking kitty 2 (uses the 'just bottom half')
  .byte $03
  .byte $16
  .byte $15

  .byte $07 ;left jumping kitty
  .byte $08
  .byte $17
  .byte $18

  .byte $08 ;right jumping kitty
  .byte $07
  .byte $18
  .byte $17

  .byte $09 ;left game-over kitty
  .byte $0A
  .byte $19
  .byte $1A

  .byte $0A ;right game-over kitty
  .byte $09
  .byte $1A
  .byte $19

  .byte $0C ;left attack kitty
  .byte $0D
  .byte $1C
  .byte $1D

  .byte $0D ;right attack kitty
  .byte $0C
  .byte $1D
  .byte $1C

  .byte $0B ;left scratch
  .byte $1B



; Character memory
.segment "CHARS"
.incbin "background_and_kittiesSprites2.chr"