callfunc:
	pop hl
	inc de
	push de
	push hl

goto:
	pop de
	dec de
	ld hl,(script_address)			;
	xor a					;carry=0
	sbc hl,de				;Vérifier que le curseur de lecture est bien dans le script 
	ld bc,(script_lenght)			;en cour d'execution
	add hl,bc				;
	jp nc,err_out_of_script			;
	ld (read_address),de
 	
	jp read_loop	

pointeur:	
	inc de
	ld a,(de)
	ld ix,lbl_table-3
pointeur_loop:	
	inc ix
	inc ix
	inc ix
	cp (ix+0)
	jr nz,pointeur_loop 
	ld l,(ix+1)
	ld h,(ix+2)
	push hl
	dec de	
		
jump_1:
	inc de
	ld (read_address),de
	jp read_loop	

push_address:
	push de
	jp read_loop	




si:
	pop hl
	ld a,h
	or l	
	jp nz,read_loop
	ld a,$F2
	ex de,hl
	ld bc,(script_lenght)
	cpir
	dec hl
	ld (read_address),hl
	jp read_loop


wait:
	pop bc
	ld b,c
	ei
wait_loop:
	halt
	halt
	djnz wait_loop
	di
	jp read_loop