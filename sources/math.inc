racine_carre:
	pop hl
	
Sqr16:	ld de,$0040
	ld a,l
	ld l,h
	ld h,d
	or a
	ld b,8
Sqr16_Loop:
	sbc hl,de
	jr nc,Sqr16_Skip
	add hl,de
Sqr16_Skip:
	ccf
	rl d
	add a,a
	adc hl,hl
	add a,a
	adc hl,hl
	djnz Sqr16_Loop

	ld e,d
	ld d,0
	push de
	jp read_loop

		


addition:
	pop hl
	pop de
	add hl,de
	push hl
	jp read_loop

soustraction:
	pop hl
	pop de
	or a
	sbc hl,de
	push hl
	jp read_loop


carre:
	pop hl
	push hl
	push hl
	
multiplication:
	pop bc
	pop de

Mult16:	ld a,b
	ld b,16
Mult16_Loop:
	add hl,hl
	sla c
	rla
	jr nc,Mult16_NoAdd
	add hl,de
Mult16_NoAdd:
	djnz Mult16_Loop


	push hl
	jp read_loop


division:

	pop bc
	pop de

	ld a,e
	or d
	jp z,err_divide_by_0

	xor a


	bit 7,b
	jr z,bc_not_neg
	ld hl,0
	xor a
	sbc hl,bc
	push hl
	pop bc
	ld a,1
bc_not_neg:

	bit 7,b
	jr z,de_not_neg
	ld hl,0
	xor a
	sbc hl,de
	push hl
	pop de
	xor 1
de_not_neg:


	push af
	call div16
	pop af
	or a
	jr z,res_not_neg

	ld hl,0
	sbc hl,de
	push hl
	jp read_loop


res_not_neg:
	push bc
	jp read_loop

Div16:	bit 7,d
	jp z,Div15
	ld h,b
	ld l,c
	ld bc,1
	or a
	sbc hl,de
	ret nc
	add hl,de
	dec bc
	ret


;Divide 16-bit value by a 15-bit value (with 16-bit result)
;In: Divide BC by divider DE
;Out: BC = result, HL = rest
;
Div15:	ld a,b
	ld hl,0
	ld b,16
Div15_Loop:
	rl c
	rla
	adc hl,hl
	sbc hl,de
Div15_Null:
	ccf
	jr nc,Div15_Negative
Div15_Positive:
	djnz Div15_Loop
	rl c
	rla
	ld b,a
	ret
Div15_Restore:
	rl c
	rla
	adc hl,hl
	and a
	adc hl,de
	jr c,Div15_Positive
	jr z,Div15_Null
Div15_Negative:
	djnz Div15_Restore
	rl c
	rla
	add hl,de
	ld b,a
	ret




minus:
	pop de
	ld hl,0
	sbc hl,de
	push hl
	jp read_loop



egal:
	pop hl
	pop de
	or a
	sbc hl,de
	ld a,l
	ld de,0
	or h
	jr z,comp_vrai
	push de
	jp read_loop
comp_vrai:
	inc de
	push de
	jp read_loop


superieur:
	pop hl
	pop de
	jr inf_sup
		

inferieur:
	pop de
	pop hl
inf_sup:
	ld bc,$8000
	add hl,bc
	ex de,hl
	add hl,bc
	
	or a
	sbc hl,de
	ld de,0
	jr c,comp_vrai
	push de
	jp read_loop	


random:
 	ld hl,(randData)
 	ld a,r
 	ld d,a
 	ld e,(hl)
 	add hl,de
 	add a,l
 	xor h
 	ld (randData),hl
 	ld l,a

	push hl



Modulo:					;Modulo from HL by DE
	pop hl
	pop de
	
	ld a,e
	or d
	jp z,err_divide_by_0
	
	ld b,0
	or a
modulo_Loop1:
	inc b
	rl e
	rl d
	jr nc,modulo_Loop1

modulo_Loop2:
	rr d
	rr e
	or a
	sbc hl,de
	jr nc,modulo_Loop2_not_neg

	add hl,de

modulo_Loop2_not_neg:
	or a
	djnz modulo_Loop2

	push hl

	jp read_loop	


randData:
	.db $7A,$B6