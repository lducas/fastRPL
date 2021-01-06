;;;;;;;;;;;;;;;;;;;;;;;TEXT;;;;;;;;;;;;;;;;;;;;;;;;;;;;

guillemet:
	inc de
	push de
	ld a,22h
	ld bc,(script_lenght)
	ex de,hl
	cpir
	dec hl
	ld (read_address),hl
;	dec hl
;	xor a
;	ld (hl),a
	jp read_loop





disp:
	ld de,disp_txt_buffer

	xor a
	ld (disp_zero),a
	pop hl
	ld a,' '
	bit 7,h
	jr z,Num2Dec
	ld de,0
	ex de,hl
	ld a,'-'
	or a	
	sbc hl,de
	ld de,disp_txt_buffer+1
	
Num2Dec:
	ld 	(de),a
	ld	bc,-10000
	call	Num1
	ld	bc,-1000
	call	Num1
	ld	bc,-100
	call	Num1
	ld	c,-10
	call	Num1
	ld 	a,1
	ld	(disp_zero),a
	ld	c,-1
	call	Num1
	xor	a
	ld	(de),a
	
	ld 	hl,disp_txt_buffer
	push hl
	jr text
	
Num1:
	ld	a,'0'-1
Num2:
	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc

	ld	(de),a
	push 	hl 
	ld 	hl,disp_zero
	xor	'0'
	or	(hl)
	ld	(hl),a
	pop	hl
	ret	z
	
	inc	de
	ret

disp_txt_buffer:
	.db "-01234",0
disp_zero:
	.db 0




text:	
	res textInverse, (IY + textflags)
	res fracDrawLFont, (IY + fontFlags)
	pop de
	pop hl
	ld a,l

	bit 0,h
	jr z,text_not_reversed
	set textInverse, (IY +textFlags)
	neg
text_not_reversed:
	ld h,a
	ld (text_next_line_X+1),a
	pop bc

	bit 0,b
	jr z,text_not_big
	set fracDrawLFont, (IY +fontFlags)
	ld a,c
	neg
	ld c,a
text_not_big:

	ld l,c
	ld (pencol),hl
	ex de,hl

text_loop:
	LD A,(HL) 					; get a character of string name
	INC HL

	cp $F2						; end of string?
	jr z,text_next_line

	cp $22
	jp z,read_loop


	push bc		 				; yes --->
	bcall(_VPutMap)					; display one character of string
	pop bc
	JR NC,text_loop 			; display rest of string IF FITS
	
	dec hl
	dec hl
	
text_next_line:
	inc hl
	ld de,(pencol)
	ld a,6
	add a,d
	ld d,a
	add a,-95
	jp c,read_loop
text_next_line_X:
	ld a,0
	ld e,a
	ld (pencol),de
	
	JR text_loop
	
		

;;;;;;;;;;;;;;;;;;;;;;;GRAPH;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pixel:
	add a,-093h
	ld b,a
	add a,a
	add a,b
	ld b,a
	ld c,0
	ld hl,table_pxl	
	add hl,bc
	ld a,(hl)
	ld (pixel_OP),a
	inc hl
	ld a,(hl)
	ld (pixel_OP+1),a
	inc hl
	ld a,(hl)
	ld (pixel_OP+2),a

	pop hl
	ld h,0
	ld a,l
	pop de
	cp 64
	jp nc,read_loop
	
	add hl,hl
	add hl,hl
	push hl
	pop bc
	add hl,hl
	add hl,bc
	ld bc,plotsscreen
	add hl,bc

	ld a,e
	cp 96
	jp nc,read_loop
	ld b,a
	sra a
	sra a
	sra a

	ld d,0
	ld e,a
	add hl,de
	

	ld a,%00000111
	and b
	ld b,a
	ld a,1
	inc b

pixel_mask:
	rrc a
	djnz pixel_mask

pixel_OP:
	nop
	nop
	nop

	ld (hl),a
	jp read_loop	



pixel_get:
	and (hl)
	ld l,a
	ld h,0
	push hl
	jp bool



table_pxl:
	or (hl)
	nop
	nop
	cpl
	and (hl)
	xor (hl)
	nop
	nop
	jp pixel_get

graph_fast_copy:
	call graph_fast_copy_call
	jp read_loop
graph_fast_copy_call:
 	di
 	ld a,80h
 	out ($10),a
 	ld hl,gbuf-12-(-(12*64)+1)
 	ld a,20h
 	ld c,a
 	inc hl                ; X
 	dec hl               ; X
fastCopyAgain1:
 	ld b,64
 	inc c
 	ld de,-(12*64)+1
 	out ($10),a
 	add hl,de
 	ld de,10             ; X
fastCopyLoop1:
 	add hl,de           ; X
 	inc hl                ; X
 	inc hl                ; X
 	inc de               ; X
 	ld a,(hl)
 	out ($11),a
 	dec de              ; X
 	djnz fastCopyLoop1
 	ld a,c
 	cp 44
 	jr nz,fastCopyAgain1
	ret	


efface_ecran:
	ld hl,plotsscreen
	xor a
	ld (hl),a
	ld de,plotsscreen+1
	ld bc,12*64-1
	ldir
	jp read_loop



putsprite:
	pop hl
	pop bc
	pop de
	
	ld a,d
	inc a
	cp 2
	jp p,read_loop


	ld a,b
	ld b,e
	
	inc a
	cp 2
	jp p,read_loop
	
	

PutSprClpXOR:   XOR  A
__XChange_1:    LD   DE, DefaultXSpriteHeight     ; D = 0, E = Height

                OR   C                            ; If C < 0
                JP   M, _SCX_NoBotClp             ; No bottom clip.

                LD   A, $3F                       ; Is C is offscreen?
                SUB  C
		jp c,read_loop

__XChange_2:    CP   DefaultXSpriteHeight-1       ; If C + 7 < 64
                JR   NC, _SCX_NoVertClp           ; No vertical clip.
                INC  A
                LD   E, A
                JR   _SCX_NoVertClp               ; Height = 64 - C

_SCX_NoBotClp:
__XChange_3:    CP   -(DefaultXSpriteHeight-1)    ; Is C is offscreen?
               	jp c,read_loop

                ADD  A, E                         ; Find how many lines
                LD   C, A                         ; to actually draw
                SUB  E

                NEG
                LD   E, A
                ADD  HL, DE                       ; Move HL down
                LD   E, C                         ; by -C lines
                LD   C, D

_SCX_NoVertClp: PUSH HL                           ; IX -> Sprite
                POP  IX

                LD   A, $77                       ; OP code for
                LD   (_SCX_OPchg_1), A            ;   LD   (HL), A
                LD   (_SCX_OPchg_2), A

                XOR  A                            ; Is B > 0?
                OR   B
                JP   M, _SCX_NoRightClp

                CP   89                           ; Is B < 89?
                JR   C, _SCX_ClpDone
                CP   96
               	jp  nc,read_loop


                LD   HL, _SCX_OPchg_1             ; Modify LD to NOP
                JR   _SCX_ClpModify

_SCX_NoRightClp:CP   -7                           ; Is B is offscreen?
               	jp c,read_loop
                LD   HL, _SCX_OPchg_2             ; Modify LD to NOP
_SCX_ClpModify: LD   (HL), D

_SCX_ClpDone:   LD   B, D
                LD   H, B
                LD   L, C
                ADD  HL, BC                       ; HL = Y * 12
                ADD  HL, BC
                ADD  HL, HL
                ADD  HL, HL

                LD   C, A                         ; HL = Y*12 + X/8
                SRA  C
                SRA  C
                SRA  C
                INC  C

                ADD  HL, BC
                LD   BC, GRAPH_MEM
                ADD  HL, BC

                LD   B, E                         ; B = number of rows

                CPL
                AND  %00000111                    ; find number of
                LD   E, A                         ; instructions to jump
                ADD  A, E
                ADD  A, E
                LD   (_SCX_OPchg_3 + 1), A        ; 3 * (7 - number)

                LD   DE, 13

_SCX_LineLoop:  LD   C, (IX)
                XOR  A
_SCX_OPchg_3:   JR   _SCX_OPchg_3                 ; modify

                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA

                XOR  (HL)                         ; XOR with background
_SCX_OPchg_1:   LD   (HL), A                      ; Write
                DEC  HL                           ; HL -> next 8 pixels

                LD   A, C
                XOR  (HL)                         ; XOR with background
_SCX_OPchg_2:   LD   (HL), A                      ; Write
                ADD  HL, DE                       ; HL -> next row

		inc ix
                DJNZ _SCX_LineLoop
                jp read_loop

draw_line:
	pop de
	pop hl
	ld a,%00111111
	and e
	ld e,a
	ld (penrow),a

	ld a,%01111111
	and l
	ld d,a
	ld (pencol),a


	pop bc
	pop hl
	ld a,%00111111
	and c
	ld c,a

	ld a,%01111111
	and l
	ld b,a
	
	di	
	call Line_in_buffer
	ei
	jp read_loop

;--------------------= Line_in_buffer =----------------------
; Author:       Akambi Fagbohoun                            *
; Email:        Mcbeach1@hotmail.com                        *
; Site:         http://ProgramCenter.paxl.org               *
; Version:      1.0                                         *
; Platform:     TI-OS                                       *
; Date:         08/09/2004                                  *
;                                                           *
; entré:        b=x1 ; c=y1 ; d=x2 ; e=y2                   *
; sortie:       ligne de cordonn‚e b,c,d,e dans le buffer   *
; registres d‚truits:       b,c,d,e,h,l                     *
;------------------------------------------------------------
                                                            ;
Line_in_buffer:                                             ;
        ld a,b                                              ;
        cp d                                                ;
        jr c,noexchange                                     ;
        ld b,d     ; ‚change bc et de si b > d              ;
        ld d,a                                              ;
        ld a,c                                              ;
        ld c,e                                              ;
        ld e,a                                              ;
noexchange:                                                 ;
        ld a,d                                              ;
        ld l,a                                              ;
        sub b                                               ;
        ld d,a    ; calcule xdiff                           ;                   
        ld a,e                                              ;
        ld h,a                                              ;
        sub c     ; calcule ydiff                           ;  
        ld e,a                                              ;
        cp 64                                               ;
        jr c,no_negate                                      ;
        neg            ; valeur absolu de a                 ;
no_negate:                                                  ;
        cp d                                                ;
        jr nc,y_sup_to_x                                    ;
        ld h,0                                              ;
x_sup_to_y:                                                 ;
        call putpoint                                       ;
        ld a,e                                              ;
        cp 64                                               ;
        jr c,no_negate1                                     ;
        neg                                                 ;
no_negate1:                                                 ;
        add a,h                                             ;
        cp d                                                ;
        jr c,no_inc_y                                       ;
        ld h,a                                              ;
        ld a,e                                              ;
        cp 64                                               ;
        jr c,no_negate2                                     ;
        dec c                                               ;
        dec c                                               ;
no_negate2:                                                 ;
        inc c                                               ;
        ld a,h                                              ;
        sub d                                               ;
no_inc_y:                                                   ;
        ld h,a                                              ;
        inc b                                               ;
        ld a,b                                              ;
        cp l                                                ;
        jr z,x_sup_to_y                                     ;
        ret nc                                              ;
        jr x_sup_to_y                                       ;
y_sup_to_x:                                                 ;
        ld l,0                                              ;
y_sup_to_x1:                                                ;
        call putpoint                                       ;
        push de                                             ;
        ld a,e                                              ;
        cp 64                                               ;
        jr c,no_negate3                                     ;
        neg                                                 ;
        ld e,a                                              ;
no_negate3:                                                 ;
        ld a,d                                              ;
        add a,l                                             ;
        cp e                                                ;
        jr c,no_inc_x                                       ;
        inc b                                               ;
        sub e                                               ;
no_inc_x:                                                   ;
        ld l,a                                              ;
        pop de                                              ;
        ld a,e                                              ;
        cp 64                                               ;
        jr c,no_negate4                                     ;
        dec c                                               ;
        dec c                                               ;
no_negate4:                                                 ;
        inc c                                               ;
        ld a,c                                              ;
        cp h                                                ;
        jr z,y_sup_to_x1                                    ;
        bit 7,e                                             ;
        jr z,no_negate5                                     ;
        ret c                                               ;
        jr y_sup_to_x1                                      ;
no_negate5:                                                 ;
        ret nc                                              ;
        jr y_sup_to_x1                                      ;
                                                            ;
;;;;;;;;;;                                                  ;
                                                            ;
putpoint:                                                   ;
                                                            ;
        push hl                                             ;
        push bc                                             ;
                                                            ;
        ld a,b                                              ;
        ld b,0                                              ;
        sla c                                               ;
        sla c                                               ;
        rl b                                                ;
        ld h,b                                              ;
        ld l,c                                              ;
        add hl,hl                                           ;
        add hl,bc                                           ;
        ld bc,plotsscreen                                   ;
        add hl,bc                                           ;
                                                            ;
                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;
;;; pointe sur l'octet qui contient le bit de départ ;;;    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;
                                                            ;
        ld c,a                                              ;
        ld b,0                                              ;
        srl c                                               ;
        srl c                                               ;
        srl c                                               ;
        add hl,bc                                           ;
        and 00000111b                                       ;
        ld b,a                                              ;
        ld a,%10000000                                      ;
        jr z,putpoint1                                      ;
putpoint2:                                                  ;
        rrca                                                ;
        djnz putpoint2                                      ;
putpoint1:                                                  ;                                                            ;
        ld b,(hl)                                           ;
        or b                                                ;
        ld (hl),a                                           ;
                                                            ;
        pop bc                                              ;
        pop hl                                              ;
        ret                                                 ;
                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;************************************************************
