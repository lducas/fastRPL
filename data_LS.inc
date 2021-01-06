load_str:
	pop hl
	ld a,l
	dec a
	bit 7,a
	jr z,load_str_not0
	ld a,9
load_str_not0:
     	ld (strngnameLD+2),a
     	     
     	bcall(_zeroop1)    ; load var name
     	ld hl,strngnameLD  ;
     	ld de,op1          ;
     	ld bc,3            ;
     	ldir               ;
	bcall(_chkfindsym) ; look it up
	jp c,var_not_found
	xor a
	or b
	jp nz,var_not_found

	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	push hl
	push bc
	jp read_loop


load_pic:
	pop hl
	ld a,l
	dec a
	bit 7,a
	jr z,load_pic_not0
	ld a,9
load_pic_not0:
     	ld (picnameLD+2),a
     	     
     	bcall(_zeroop1)    ; load var name
     	ld hl,picnameLD  ;
     	ld de,op1          ;
     	ld bc,5            ;
     	ldir               ;
	bcall(_chkfindsym) ; look it up
	jr c,pic_not_found
	xor a
	or b
	jr nz,pic_not_found
	inc de
	inc de
	
	push de
	
	jp read_loop
	
pic_not_found:
	ld hl,0
	push hl
	jp read_loop
	
	
read_byte:
	pop ix
	ld h,(ix+0)
	ld l,(ix+1)
	push hl

	jp read_loop
	
write_byte:
	pop ix
	pop hl
	ld (ix+0),h
	ld (ix+1),l

	jp read_loop


load_prgm:
	pop hl
	dec hl
	bcall(_Mov9ToOP1)
	ld a,$05
	ld (op1),a
	bcall(_chkFindSym)
	jr c,var_not_found
	xor a
	or b
	jr nz,var_not_found

	push de
	pop ix
	ld c,(ix+0)
	ld b,(ix+1)
	inc bc
	inc ix
	inc ix
	push ix
	push bc	

	jp read_loop


	
search_2bytes:
	pop de
	pop bc
	pop hl
search_2bytes_loop:
	ld a,d
	cpir
	ld a,b
	or c
	jr z,search_2bytes_end
	ld a,e
	cp (hl)
	jr nz,search_2bytes_loop
	dec hl
	push hl
	jp read_loop
	

search_2bytes_end:
	push bc
	jp read_loop


screen_address:
	ld hl,plotsscreen
	push hl
	jp read_loop

	
copy_data_down:
	pop bc
	pop de
	pop hl
	ldir
	jp read_loop
	
copy_data_up:
	pop bc
	pop de
	pop hl
	lddr
	jp read_loop
	


var_not_found:
	ld hl,0
	push hl
	push hl
	jp read_loop

strngnameLD:
     .db $04,$AA,$00		  ; object type strng, var type strng, var Str1

picnameLD:
     .db $07,$60,$00,$00,$00  ; object type strng, var type strng, var Str1
