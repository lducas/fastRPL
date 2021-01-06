load_lib:
	ld hl,8
	add hl,de
	ld (read_address),hl
	
	ex de,hl
	bcall(_Mov9ToOP1)
	ld a,$05
	ld (op1),a
	bcall(_chkFindSym)
	jp c,err_not_found
	xor a
	or b
	jp nz,err_not_found
	inc de
	inc de

	push de
	pop ix

	ld a,lib_token
	cp (ix)
	jp nz,err_not_found
	inc ix
	ld a,lib_token2
	cp (ix)
	jp nz,err_not_found

	ld b,(ix)
	inc ix


load_lib_loop:
	push ix
	push de
	ld l,(ix)
	ld h,0
	add hl,hl
	ld de,tab_funct
	add hl,de
	pop de
	push hl
	ld h,(ix+2)
	ld l,(ix+1)
	add hl,de
	pop ix
	ld (ix+1),h
	ld (ix+0),l
	pop ix
	ld hl,11
	ex de,hl
	add ix,de
	ex de,hl
	djnz load_lib_loop

	jp read_loop
