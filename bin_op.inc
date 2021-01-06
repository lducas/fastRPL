
ou:
	pop hl
	pop de
	ld a,h
	or d
	ld d,a
	ld a,l
	or e
	ld e,a
	push de
	jp read_loop
 




xou:
	pop hl
	pop de
	ld a,h
	xor d
	ld d,a
	ld a,l
	xor e
	ld e,a
	push de
	jp read_loop
 


et:
	pop hl
	pop de
	ld a,h
	and d
	ld d,a
	ld a,l
	and e
	ld e,a
	push de
	jp read_loop
	
non:
	pop de
	ld a,e
	or d
	jp z,non_true
	ld de,0
	push de
	jp read_loop
non_true:
	ld de,1
	push de
	jp read_loop

 
highB:
	pop af
	ld l,a
	push hl

lowB:
	pop hl
	xor a
	ld h,a
	push hl
	jp read_loop

decalR:
	pop hl
	srl h
	rr l
	push hl
	jp read_loop

decalL:
	pop hl
	sla l
	rl h
	push hl
	jp read_loop

bool:
	pop de
	ld a,e
	or d
	jp nz,bool_true
	push de
	jp read_loop
bool_true:
	ld de,1
	push de
	jp read_loop
