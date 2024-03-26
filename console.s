.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
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

.segment "ZEROPAGE"
  nmi_lock:       .res 1 ; prevents NMI re-entry
  nmi_count:      .res 1 ; is incremented every NMI
  nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
  nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
  scroll_x:       .res 1 ; x scroll position
  scroll_y:       .res 1 ; y scroll position
  scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
  cursor_ptr:     .res 2 ;    
  temp:           .res 2 ; temporary variable
  temp2:          .res 1
  temp3:          .res 1

.segment "PROG_RAM"
  counter:        .res 1
  print_char:     .res 1 ;
  back_char:      .res 1 ;
  key_state:      .res 9 ;
  last_key_state: .res 9 ;


; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

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
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop

fill_background: ; with spaces
  lda $2002 ; reset latch
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  ; $20 over entire nametable which is a space
  lda #$20
  ldy #30 ; 30 rows
  :
    ldx #32 ; 32 columns
    :
      sta $2007
      dex
      bne :-
    dey
    bne :--
  ; set all attributes to 0
  lda #0
  ldx #64 ; 64 bytes
  :
    sta $2007
    dex
    bne :-

ldx #$80
stx cursor_ptr+0
ldx #$20
stx cursor_ptr+1

ready_bg_for_rendering:
  lda $2002 ; reset ppu latch
  lda #$00 ; Reset background scroll position
  sta $2005
  sta $2005

enable_rendering:
  lda #%10000000	  ; Enable NMI
  sta $2000
  lda #%00001010	; Enable Sprites
  sta $2001

forever:
  vblankwait3:
  bit $2002
  bpl vblankwait3

  jsr readkb

  ldx #$00
  stx counter
  ldx #$ff
  _outer:
    inx
    cpx #$09
    bpl _end

    ldy #$00
    lda key_state, x
    _inner:
      ror A
      bcc _endcompare
        sty temp
        stx temp2
        sta temp3

        iny
        lda last_key_state, x
        :
          dey
          bmi :+
          ror A
          jmp :-
        :

        bcs :+
          ldx counter
          ldy charsetmap, x
          sty print_char
          ldy #$01
          sty nmi_ready
          jmp _end
        :

        ldy temp
        ldx temp2
        lda temp3
      _endcompare:
      sty temp
      ldy counter
      iny
      sty counter
      ldy temp

      iny
      cpy #$08
      bmi _inner
      bpl _outer
  _end:

  ldx #$08
  :
      lda key_state, x
      sta last_key_state, x
      dex
      bpl :-

  ;ldx #$00
  ;stx nmi_ready
  jmp forever

readkb:
;Reads the keyboard matrix state.
  lda #$05
  sta $4016          ;reset keyboard
  nop
  nop
  nop
  nop
  nop
  nop                ;wait for keyboard to get ready
  
;Read all 9 rows in column 0 and 1:
  ldx #$00           ;loop counter
@loop_r:

;Read column 0 keys:
  lda #$04           ;select colum 0 (from second lap: also select next row)
  sta $4016
  ldy #$0A           ;loop counter
@wait1:
  dey
  bne @wait1
  nop
  nop                ;wait to give time to scan all keys
  lda $4017          ;read key state of selected row and column
  lsr a              ;right shift to get key state into low nibble
  and #$0F           ;clear high nibble
  sta key_state+0,x  ;save column 0 key states in RAM

;Read column 1 keys:
  lda #$06           ;select colum 1
  sta $4016
  ldy #$0A           ;loop counter
@wait2:
  dey
  bne @wait2
  nop
  nop                ;wait to give time to scan all keys
  lda $4017          ;read key status of selected row and column
  rol a
  rol a
  rol a              ;rotate left to get key status to high nibble
  and #$F0           ;clear low nibble
  ora key_state+0,x  ;merge A with column 0 key status
  eor #$FF           ;invert key states so that 0=unpressed
  ldy #$08           ;loop counter for 8 bits
@store:
  asl a              ;shift bit 7 left into carry
  ror key_state+0,x  ;store key state bit to RAM from carry
  dey
  bne @store         ;loop for storing all 8 bits in RAM
  
  inx
  cpx #$09
  bne @loop_r        ;loop for all 9 rows
  
  rts

nmi:
  ; save registers
  pha
  txa
  pha
  tya
  pha
  ; prevent NMI re-entry
  lda nmi_lock
  beq :+
    jmp @nmi_end
  :
  lda #1
  sta nmi_lock
  ; increment frame counter
  inc nmi_count
  ;
  lda nmi_ready
  bne :+ ; nmi_ready == 0 not ready to update PPU
    jmp @ppu_update_end
  :
  cmp #2 ; nmi_ready == 2 turns rendering off
  bne :+
    lda #%00000000
    sta $2001
    ldx #0
    stx nmi_ready
    jmp @ppu_update_end
  :

  ;
  ; CODE HERE
  ; 

  lda print_char
  cmp #$48
  beq  :+++
    ; Increment cursor position
    inc cursor_ptr+0 ; increment low byte
    bne :+ ; 255+1 rolls over to 0, so increment high byte too:
      inc cursor_ptr+1
    :

    ; Determine if cursor position has wrapped (16 bit subtraction with branch)
    sec           ; initialize carry flag to 1 for subtraction
    lda cursor_ptr+0
    sbc #$a0      ; subtract the lowest byte first
    sta temp+0
    lda cursor_ptr+1
    sbc #$23      ; then subtract the next byte using the carry output from the previous byte
    sta temp+1
    bmi :+
      ldx #$80
      stx cursor_ptr+0
      ldx #$20
      stx cursor_ptr+1
    :
  :
  bne :+
    lda #$20
    sta print_char
    lda #$48
    sta back_char
  :
  
  ; Render next character in sequence to cursor position
  bit $2002 ; reset latch
  ldx cursor_ptr+1 
  stx $2006
  ldx cursor_ptr+0
  stx $2006

  lda print_char
  sta $2007

  lda back_char
  cmp #$48
  bne :+++
    lda cursor_ptr+0
    bne :+
      dec cursor_ptr+1
    :
    dec cursor_ptr+0

    ; Determine if cursor position has wrapped (16 bit subtraction with branch)
    sec           ; initialize carry flag to 1 for subtraction
    lda cursor_ptr+0
    sbc #$80      ; subtract the lowest byte first
    sta temp+0
    lda cursor_ptr+1
    sbc #$20      ; then subtract the next byte using the carry output from the previous byte
    sta temp+1
    bpl :+
      ldx #$80
      stx cursor_ptr+0
      ldx #$20
      stx cursor_ptr+1
    :

    lda #0
    sta back_char
  :

  ;
  ; END CODE HERE
  ;

  ; Ensure NMI enabled
  lda #%10000000	  
  sta $2000
  ; reset bg scroll position
  lda #$00 ; Reset background scroll position
  sta $2005
  sta $2005
  ; enable rendering
  lda #%00001010
  sta $2001
  ; flag PPU update complete
  ldx #0
  stx nmi_ready
  @ppu_update_end:
  ; if this engine had music/sound, this would be a good place to play it
  ; unlock re-entry flag
  lda #0
  sta nmi_lock
  @nmi_end:
  ; restore registers and return
  pla
  tay
  pla
  tax
  pla
  rti

charsetmap:
  .byte $48, $1c, $20, $20, $1d, $1b, $20, $20
  .byte $1e, $2d, $2f, $1f, $3b, $3a, $00, $20
  .byte $30, $10, $2c, $2e, $0b, $0c, $0f, $20
  .byte $38, $39, $0e, $0d, $0a, $15, $09, $20
  .byte $36, $37, $16, $02, $08, $07, $19, $20
  .byte $34, $35, $03, $06, $04, $12, $14, $20
  .byte $33, $05, $1a, $18, $01, $13, $17, $40
  .byte $32, $31, $20, $20, $20, $11, $20, $41
  .byte $20, $48, $20, $4a, $48, $55, $4b, $48

palettes:
  ; Background Palette
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

  ; Sprite Palette
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

; Character memory
.segment "CHARS"
  .byte %00111000
  .byte %01000100
  .byte %01010100
  .byte %01011100
  .byte %01011000
  .byte %01000000
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00101000
  .byte %01000100
  .byte %01000100
  .byte %01111100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %01000000
  .byte %01000000
  .byte %01111000
  .byte %01000000
  .byte %01000000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %01000000
  .byte %01000000
  .byte %01111000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111100
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01001100
  .byte %01000100
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01111100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000100
  .byte %00000100
  .byte %00000100
  .byte %00000100
  .byte %00000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01001000
  .byte %01010000
  .byte %01100000
  .byte %01010000
  .byte %01001000
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01101100
  .byte %01010100
  .byte %01010100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %01100100
  .byte %01010100
  .byte %01001100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01010100
  .byte %01001000
  .byte %00110100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %01010000
  .byte %01001000
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000000
  .byte %00111000
  .byte %00000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00101000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01010100
  .byte %01010100
  .byte %01101100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %00101000
  .byte %00010000
  .byte %00101000
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000100
  .byte %01000100
  .byte %00101000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %01000000
  .byte %00100000
  .byte %00010000
  .byte %00001000
  .byte %00000100
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %00001100
  .byte %00001100
  .byte %00001100
  .byte %00001100
  .byte %00001100
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00101000
  .byte %01000100
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %11111110
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00101000
  .byte %00101000
  .byte %00101000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00101000
  .byte %00101000
  .byte %01111100
  .byte %00101000
  .byte %01111100
  .byte %00101000
  .byte %00101000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00111100
  .byte %01010000
  .byte %00111000
  .byte %00010100
  .byte %01111000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01100000
  .byte %01100100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01001100
  .byte %00001100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00100000
  .byte %01010000
  .byte %01010000
  .byte %00100000
  .byte %01010100
  .byte %01001000
  .byte %00110100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %00100000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00001000
  .byte %00000100
  .byte %00000100
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %01010100
  .byte %00111000
  .byte %00010000
  .byte %00111000
  .byte %01010100
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00010000
  .byte %00010000
  .byte %01111100
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00010000
  .byte %00100000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %01111100
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01001100
  .byte %01010100
  .byte %01100100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00110000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %00000100
  .byte %00011000
  .byte %00100000
  .byte %01000000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %00000100
  .byte %00001000
  .byte %00011000
  .byte %00000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00001000
  .byte %00011000
  .byte %00101000
  .byte %01001000
  .byte %01111100
  .byte %00001000
  .byte %00001000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %01000000
  .byte %01111000
  .byte %00000100
  .byte %00000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00011100
  .byte %00100000
  .byte %01000000
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111100
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %00100000
  .byte %00100000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %00111100
  .byte %00000100
  .byte %00001000
  .byte %01110000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte %00010000
  .byte %00010000
  .byte %00100000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %00100000
  .byte %00010000
  .byte %00001000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01111100
  .byte %00000000
  .byte %01111100
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00100000
  .byte %00010000
  .byte %00001000
  .byte %00000100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00111000
  .byte %01000100
  .byte %00001000
  .byte %00010000
  .byte %00010000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110111
  .byte %11101111
  .byte %10010011
  .byte %00000001
  .byte %00000011
  .byte %00000011
  .byte %10000001
  .byte %10010011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110111
  .byte %11101111
  .byte %10010011
  .byte %01111101
  .byte %01111011
  .byte %01111011
  .byte %10101101
  .byte %10010011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111111
  .byte %10011111
  .byte %10001111
  .byte %10000111
  .byte %10010011
  .byte %10111101
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000001
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11101111
  .byte %11010111
  .byte %10101011
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111101
  .byte %11111011
  .byte %01110111
  .byte %10101111
  .byte %11011111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000001
  .byte %00000011
  .byte %00000101
  .byte %11001001
  .byte %01010001
  .byte %00100001
  .byte %00100001
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110001
  .byte %11111001
  .byte %10000001
  .byte %01110011
  .byte %01100001
  .byte %11110011
  .byte %00000011
  .byte %10111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11100111
  .byte %00011111
  .byte %11111111
  .byte %00011111
  .byte %11001111
  .byte %11101111
  .byte %11110001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %00000001
  .byte %10111111
  .byte %11011111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %10101011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %01101101
  .byte %10101011
  .byte %11000111
  .byte %11101111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11000111
  .byte %10101011
  .byte %01101101
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000001
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11011101
  .byte %10011101
  .byte %00000001
  .byte %10011111
  .byte %11011111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte %00000011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00110111
  .byte %11100111
  .byte %11000111
  .byte %10000001
  .byte %11000111
  .byte %11100111
  .byte %11110111
  .byte %00001001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11011001
  .byte %11001111
  .byte %11000111
  .byte %00000011
  .byte %11000111
  .byte %11001111
  .byte %11011111
  .byte %00100001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111101
  .byte %11101101
  .byte %11101111
  .byte %00000001
  .byte %10000011
  .byte %11000111
  .byte %11101101
  .byte %11111101
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111101
  .byte %11101101
  .byte %11000111
  .byte %10000011
  .byte %00000001
  .byte %11101111
  .byte %11101101
  .byte %11111101
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %00000001
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11110111
  .byte %11111011
  .byte %00000001
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte %01010101
  .byte %10101011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %10000011
  .byte %01111101
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %00000001
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %00000011
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %00000001
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte %11111101
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11000111
  .byte %10000011
  .byte %00000001
  .byte %10000011
  .byte %11000111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000001
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11010111
  .byte %11010111
  .byte %00010001
  .byte %11111111
  .byte %00010001
  .byte %11010111
  .byte %11010111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000001
  .byte %11111101
  .byte %11111101
  .byte %11001101
  .byte %11001101
  .byte %11111101
  .byte %11111101
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte %01111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00100000
  .byte %00010000
  .byte %00001000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111000
  .byte %00000100
  .byte %00111100
  .byte %01000100
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000000
  .byte %01000000
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111100
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000100
  .byte %00000100
  .byte %00111100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111000
  .byte %01000100
  .byte %01111100
  .byte %01000000
  .byte %00111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00011000
  .byte %00100100
  .byte %00100000
  .byte %01111000
  .byte %00100000
  .byte %00100000
  .byte %00100000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %00111100
  .byte %00000100
  .byte %00111000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000000
  .byte %01000000
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00000000
  .byte %00110000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00001000
  .byte %00000000
  .byte %00011000
  .byte %00001000
  .byte %00001000
  .byte %00001000
  .byte %01001000
  .byte %00110000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01000000
  .byte %01000000
  .byte %01000100
  .byte %01001000
  .byte %01110000
  .byte %01001000
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00110000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01101100
  .byte %01010100
  .byte %01010100
  .byte %01010100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01111000
  .byte %01000100
  .byte %01000100
  .byte %01111000
  .byte %01000000
  .byte %01000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111100
  .byte %01000100
  .byte %01000100
  .byte %00111100
  .byte %00000100
  .byte %00000100
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01011100
  .byte %01100000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %00111100
  .byte %01000000
  .byte %00111000
  .byte %00000100
  .byte %01111000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00100000
  .byte %00100000
  .byte %01111000
  .byte %00100000
  .byte %00100000
  .byte %00100100
  .byte %00011000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %01001100
  .byte %00110100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00101000
  .byte %00010000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01000100
  .byte %01000100
  .byte %01010100
  .byte %01010100
  .byte %01101100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01000100
  .byte %00101000
  .byte %00010000
  .byte %00101000
  .byte %01000100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01000100
  .byte %01000100
  .byte %01000100
  .byte %00111100
  .byte %00000100
  .byte %00111000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %00000000
  .byte %01111100
  .byte %00001000
  .byte %00010000
  .byte %00100000
  .byte %01111100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00011100
  .byte %00110000
  .byte %00110000
  .byte %01100000
  .byte %00110000
  .byte %00110000
  .byte %00011100
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01110000
  .byte %00011000
  .byte %00011000
  .byte %00001100
  .byte %00011000
  .byte %00011000
  .byte %01110000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00110100
  .byte %01011000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %00000000
  .byte %01010100
  .byte %00101000
  .byte %01010100
  .byte %00101000
  .byte %01010100
  .byte %00000000
  .byte %00000000
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10101011
  .byte %10100011
  .byte %10100111
  .byte %10111111
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %10111011
  .byte %10000011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000011
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10110011
  .byte %10111011
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10000011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10110111
  .byte %10101111
  .byte %10011111
  .byte %10101111
  .byte %10110111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10010011
  .byte %10101011
  .byte %10101011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10011011
  .byte %10101011
  .byte %10110011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10101011
  .byte %10110111
  .byte %11001011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10101111
  .byte %10110111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111111
  .byte %11000111
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10101011
  .byte %10101011
  .byte %10010011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %10111111
  .byte %11011111
  .byte %11101111
  .byte %11110111
  .byte %11111011
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11010111
  .byte %11010111
  .byte %11010111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11010111
  .byte %11010111
  .byte %10000011
  .byte %11010111
  .byte %10000011
  .byte %11010111
  .byte %11010111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11000011
  .byte %10101111
  .byte %11000111
  .byte %11101011
  .byte %10000111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10011111
  .byte %10011011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10110011
  .byte %11110011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11011111
  .byte %10101111
  .byte %10101111
  .byte %11011111
  .byte %10101011
  .byte %10110111
  .byte %11001011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11011111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11110111
  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %10101011
  .byte %11000111
  .byte %11101111
  .byte %11000111
  .byte %10101011
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11101111
  .byte %11101111
  .byte %10000011
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11101111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %10000011
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10110011
  .byte %10101011
  .byte %10011011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11001111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %11111011
  .byte %11100111
  .byte %11011111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11111011
  .byte %11110111
  .byte %11100111
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110111
  .byte %11100111
  .byte %11010111
  .byte %10110111
  .byte %10000011
  .byte %11110111
  .byte %11110111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10111111
  .byte %10000111
  .byte %11111011
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11100011
  .byte %11011111
  .byte %10111111
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %11011111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %11000011
  .byte %11111011
  .byte %11110111
  .byte %10001111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte %11101111
  .byte %11101111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %11011111
  .byte %11101111
  .byte %11110111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10000011
  .byte %11111111
  .byte %10000011
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11011111
  .byte %11101111
  .byte %11110111
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %11110111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10101011
  .byte %10100011
  .byte %10100111
  .byte %10111111
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %10111011
  .byte %10000011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000011
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10110011
  .byte %10111011
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10000011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10110111
  .byte %10101111
  .byte %10011111
  .byte %10101111
  .byte %10110111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10010011
  .byte %10101011
  .byte %10101011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10011011
  .byte %10101011
  .byte %10110011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10101011
  .byte %10110111
  .byte %11001011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10101111
  .byte %10110111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000111
  .byte %10111011
  .byte %10111111
  .byte %11000111
  .byte %11111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10101011
  .byte %10101011
  .byte %10010011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11111011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10111111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10011111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %10111111
  .byte %11011111
  .byte %11101111
  .byte %11110111
  .byte %11111011
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10000011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %11110011
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %00000001
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11011111
  .byte %11101111
  .byte %11110111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000111
  .byte %11111011
  .byte %11000011
  .byte %10111011
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111011
  .byte %11111011
  .byte %11000011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000111
  .byte %10111011
  .byte %10000011
  .byte %10111111
  .byte %11000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11100111
  .byte %11011011
  .byte %11011111
  .byte %10000111
  .byte %11011111
  .byte %11011111
  .byte %11011111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %11000011
  .byte %11111011
  .byte %11000111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111111
  .byte %10111111
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11111111
  .byte %11001111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11110111
  .byte %11111111
  .byte %11100111
  .byte %11110111
  .byte %11110111
  .byte %11110111
  .byte %10110111
  .byte %11001111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10111111
  .byte %10111111
  .byte %10111011
  .byte %10110111
  .byte %10001111
  .byte %10110111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11001111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10010011
  .byte %10101011
  .byte %10101011
  .byte %10101011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10000111
  .byte %10111011
  .byte %10111011
  .byte %10000111
  .byte %10111111
  .byte %10111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %10111011
  .byte %10111011
  .byte %11000011
  .byte %11111011
  .byte %11111011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10100011
  .byte %10011111
  .byte %10111111
  .byte %10111111
  .byte %10111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %10111111
  .byte %11000111
  .byte %11111011
  .byte %10000111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11011111
  .byte %11011111
  .byte %10000111
  .byte %11011111
  .byte %11011111
  .byte %11011011
  .byte %11100111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %10110011
  .byte %11001011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111011
  .byte %10111011
  .byte %10101011
  .byte %10101011
  .byte %10010011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111011
  .byte %11010111
  .byte %11101111
  .byte %11010111
  .byte %10111011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10111011
  .byte %10111011
  .byte %10111011
  .byte %11000011
  .byte %11111011
  .byte %11000111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %11111111
  .byte %10000011
  .byte %11110111
  .byte %11101111
  .byte %11011111
  .byte %10000011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11100011
  .byte %11001111
  .byte %11001111
  .byte %10011111
  .byte %11001111
  .byte %11001111
  .byte %11100011
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte %11101111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %10001111
  .byte %11100111
  .byte %11100111
  .byte %11110011
  .byte %11100111
  .byte %11100111
  .byte %10001111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11001011
  .byte %10100111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111
  .byte %10101011
  .byte %11010111
  .byte %10101011
  .byte %11010111
  .byte %10101011
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00
