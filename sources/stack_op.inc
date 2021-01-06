store:
	inc de
	ld (read_address),de
	ld a,(de)
	add a,-ta
	jp nc,err_syntax
	cp 28+5+26
	jp p,err_syntax

	sla a
	ld e,a
	ld d,0
	ld hl,var_table
	add hl,de
	pop de
	
	ld (hl),e
	inc hl
	ld (hl),d
	
	jp read_loop




	
push_nb:
	add a,-t0
	ld l,a
	ld h,0
push_nb_loop:
	inc de
	ld a,(de)
	add a,-t0
	jr nc,push_nb_end
	cp 10
	jp nc,push_nb_end
	add hl,hl
	ld b,h
	add a,l
	ld c,a
	add hl,hl
	add hl,hl
	add hl,bc
	jr push_nb_loop


push_nb_end:
	dec de
	ld (read_address),de
	push hl
	jp read_loop
	
push_var:
	add a,-ta
	sla a
	ld e,a
	ld d,0
	ld hl,var_table
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	jp read_loop

duplicate:
	pop hl
	push hl
	push hl
	jp read_loop

drop:
	pop hl
	jp read_loop	
	
swap12:
	pop hl
	pop de
	push hl
	push de
	jp read_loop	
	

