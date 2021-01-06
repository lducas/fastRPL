.nolist 
    #include "ion.inc" ; 
	#include "directin.inc"

	.list 
#ifdef TI83P
	.org	progstart-2
	.dw	$6DBB
#else
	.org	progstart
#endif


 #define ProgObj 05h

 #define script_address 	saferam1
 #define read_address 		saferam1+2
 #define script_end 		saferam1+4
 #define script_lenght 		saferam1+6
 #define top_line		saferam1+8
 #define cursor_address		saferam1+10
 #define cursor_line		saferam1+12
 #define cursor_row		saferam1+14
 #define line_address		saferam1+16
 #define cursor_onoff		saferam1+18
 #define top_line_address	saferam1+20
 
 #define mode_key		saferam1+22
 #define char_list_choose	saferam1+24
 #define menu_cursor_line	saferam1+26
 #define menu_address		saferam1+28
 
 #define ta			041h
 #define tadd    		70h
 #define t0       		30h
 #define tdisp			0deh
 #define tstore  		04h
 #define tlbl  			0D6h
 #define DefaultXSpriteHeight	8
 #define strngobj  		04h
 #define stack_safe_buffer 	16
 #define stack_max_size 	256*2
 #define _JErrorNo	 	44D7h


 #define fontFlags		50
 #define fracDrawLFont		2
 #define EditProg		4A32h
 #define CloseProg		4A35h
 #define RclN			4addh
 #define convdim00		4b46h
 #define DisableApd		4C84h
 
 
 #define c_nextline		$F2
 #define c_ifthen		$87


start: 
	jr real_start
.dw (table_token-1024)-start+2
real_start:
	di
	set textWrite, (IY + sGrFlags)
	res textEraseBelow, (IY + textFlags)
	res indicRun, (IY+indicFlags)
	bcall(DisableApd)

	call efface_ecran

     bcall(_zeroop1)    ; load var name
     ld hl,strngname   ;
     ld de,op1         ;
     ld bc,3           ;
     ldir              ;
     bcall(_chkfindsym); look it up
     ret c             ; return if it is not found
	ld hl,2
	add hl,de
	ld de,prgm_name+1
	ld bc,8
	ldir

	ld hl,prgm_name
	bcall(_Mov9ToOP1)
	bcall(_chkFindSym)
	push af
	xor a
	or b	
	jp nz,pop_and_quit
	pop af
	call c,create_new_prog	
	ld a,1


	push de
	pop ix
	ld c,(ix+0)
	ld b,(ix+1)
	inc bc
	ld (script_lenght),bc


	bcall(4A32h)		;_editprog
	ld hl,(iMathPtr1)
	inc hl
	inc hl
	inc hl
	ld (cursor_address),hl
	ld (script_address),hl
	ld (line_address),hl
	ld (top_line_address),hl
	ld de,0
	ld b,10
	call boucle_ecran
	ld hl,1
	ld (cursor_row),hl	
	dec hl
	ld (top_line),hl
	ld (cursor_line),hl
	ld (cursor_onoff),hl
	ld (mode_key),hl
	bcall(_getCSC)
	bcall(4addh)
	bcall(convdim00)
	dec de
	ld hl,(script_lenght)	
	bcall(_cphlde)
	jr nc,goto_at_start
	ld de,0
goto_at_start:
	inc de
goto_at_start_loop:
	push de
	call key_right_call
	pop de
	dec de
	ld a,e
	or d
	jr nz,goto_at_start_loop
	jp key_left
	
key_loop:
	ld b,5
	ei
key_loop_wait:
	halt
	djnz key_loop_wait
	di
	call refresh_line
	call graph_fast_copy
	bcall(_getCSC)
	or a
	jr z,key_loop

	cp $38
	jp z,del_char

	cp $37
	jr z,quit

	cp $36
	jp z,key_2nd
	
	cp $28
	jp z,menu_comp_exec

	cp $30
	jp z,key_alpha

	cp $20
	jp z,insert_sprite

	cp $17
	jp z,insert_matrix
	
	cp $30
	jp m,key_loop_not_special
	
	cp $36
	jp m,menu
	
key_loop_not_special:
	dec a
	jp z,key_down_no_call
	dec a
	jr z,key_left
	dec a
	jr z,key_right
	dec a
	jp z,key_up
	sub 5
	jp z,key_Enter
	jp key_any 

	
quit:	
	res textWrite, (IY + sGrFlags)
	set indicRun, (IY+indicFlags)
	ei
;	bcall(enableAPD)		;enable APD
	bcall(4A35h)		;_closeprog
	ret

	

key_right:
	ld hl,key_loop
	push hl
key_right_call:
	ld hl,(cursor_address)
	ld de,(imathptr2)
	dec de
	dec de
	bcall(_cphlde)
	ret z
	
	ld a,c_nextline
	cp (hl)
	jr z,key_down_1

	ld a,c_ifthen
	cp (hl)
	jr nz,key_right_not_ifthen
	inc hl
	inc hl
	push hl
	ld hl,cursor_row
	inc (hl)
	inc (hl)
	pop hl
key_right_not_ifthen:

	inc hl
	ld (cursor_address),hl
	ld hl,cursor_row
	inc (hl)
	call refresh_line
	ret

key_left:
	ld hl,(cursor_address)
	dec hl
	ld a,c_nextline
	cp (hl)
	jp z,key_up_1

	ld a,$7e
	cp (hl)
	jr nz,key_left_not_ifthen
	dec hl
	dec hl
	push hl
	ld hl,cursor_row
	dec (hl)
	dec (hl)
	pop hl
key_left_not_ifthen:


	ld (cursor_address),hl
	ld hl,cursor_row
	dec (hl)
	call refresh_line
	jp key_loop


key_down_no_call:
	ld hl,key_loop
	push hl
	jr key_down
	
key_down_1:
	ld a,1
	ld (cursor_row),a

key_down:

	ld a,(cursor_row)
	push af
	xor a
	ld (cursor_row),a
	call refresh_line
	ld hl,(cursor_address)
	ld a,c_nextline
	ld bc,255
	cpir

	ld de,(imathptr2)
	dec de
	bcall(_cphlde)	
	jp p,no_more_down
	ld (line_address),hl


	pop af
	ld b,0
	ld c,a
	ld a,c_nextline
	cpir
	dec hl

	ld (cursor_address),hl


	ld de,(line_address)
	xor a
	sbc hl,de
	ld a,l
	inc a
	ld (cursor_row),a
	ld hl,(cursor_line)
	inc hl
	ld (cursor_line),hl
	ld de,(top_line)
	ld a,10
	or a
	sbc hl,de
	cp l
	jr z,screen_down
	call refresh_line
	ret	
	
screen_down:
	ld hl,top_line
	inc (hl)	
	ld hl,(top_line_address)
	ld a,c_nextline
	ld bc,255
	cpir
	ld (top_line_address),hl
	push hl
	call efface_ecran
	pop hl
	ld de,0
	ld b,10
	call boucle_ecran
	ret
	
	
no_more_down:
	pop af	
	ld (cursor_row),a
	ret

	
key_up_1:
	ld a,-1
	ld (cursor_row),a
key_up:	


	ld hl,(line_address)
	ld de,(script_address)
	xor a
	sbc hl,de
	jp nz,key_up_not_begining
	ld a,1
	ld (cursor_row),a
	ld hl,(script_address)
	ld (cursor_address),hl
	jp key_loop
		
key_up_not_begining:
	ld a,(cursor_row)
	push af
	xor a
	ld (cursor_row),a
	call refresh_line
	ld hl,(line_address)
	dec hl
	dec hl
	ld a,c_nextline
	ld bc,255
	cpdr
	inc hl
	inc hl
	ld (line_address),hl
	pop af
	ld b,0
	ld c,a
	ld a,c_nextline
	cpir
	dec hl
	ld (cursor_address),hl
	ld de,(line_address)
	xor a
	sbc hl,de
	ld a,l
	inc a
	ld (cursor_row),a


	ld hl,(cursor_line)
	dec hl
	ld (cursor_line),hl
	inc hl
	ld de,(top_line)
	or a
	sbc hl,de
	jr z,screen_up
	
	call refresh_line
	jp key_loop	
	
screen_up:
	ld hl,top_line
	dec (hl)	
	ld hl,(cursor_address)
	ld (top_line_address),hl
	push hl
	call efface_ecran
	pop hl
	ld de,0
	ld b,10
	call boucle_ecran 
	jp key_loop

key_special:
	add a,$c2
	call inser_char
	jp key_right
	
boucle_ecran:	
	ld c,0
	ld (pencol),de

	push bc
	push de
	call boucle_ligne_cursor
	ld de,(imathptr2)
	bcall(_cphlde)
	jp p,boucle_ecran_EOF
	
	pop de
	pop bc
	
	push hl
	ld hl,256*6
	add hl,de
	ex de,hl
	pop hl
	djnz boucle_ecran

boucle_ecran_aff_mode:
	ret
	

boucle_ecran_EOF:
	pop de
	pop de
	jr boucle_ecran_aff_mode
	
		
refresh_line:
	ld hl,(cursor_line)
	ld de,(top_line)
	xor a
	sbc hl,de	
	ld a,l
	push af
	call efface_ligne
	ld a,(cursor_row)
	ld c,a
	inc b
	pop af
	sla a
	ld b,a
	sla a
	add a,b
	ld (penrow),a
	xor a
	ld (pencol),a
	ld hl,(line_address)
	call boucle_ligne_cursor

	set textInverse, (IY + textFlags)
	ld a,(mode_key)
	ld l,a
	ld h,0
	ld de,modes_txt
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,de
	ld a,81
	ld (pencol),a
	ld a,56
	ld (penrow),a
	bcall(_VPuts)

	res textInverse, (IY + textFlags)


	ret
	


boucle_ligne_cursor:
	res fracDrawLFont, (IY + fontFlags)
	res textInverse, (IY + textFlags)

;	ld a,':'
;	bcall(_VPutMap)					; display one character of string
boucle_ligne_cursor2:
	dec c
	call z,affiche_curs
	LD A,(HL) 			; get a character of string name
	INC HL

	cp c_nextline 				; end of string?
	ret z


	cp $80
	jp p,boucle_ligne_cursor_token

	or a
	call z,zero_char


	push bc		 		; yes --->
	bcall(_VPutMap)					; display one character of string
	pop bc
	JR NC,boucle_ligne_cursor2 		; display rest of string IF FITS

	ld a,88
	ld (pencol),a
	ld a,' '
	bcall(_VPutMap) 			; display one character of string
	ld a,'.'
	bcall(_VPutMap) 			; display one character of string
	ld a,'.'
	bcall(_VPutMap) 			; display one character of string
	ld a,'.'
	bcall(_VPutMap) 			; display one character of string
	ld a,c_nextline
	ld bc,255
	cpir






	RET

zero_char:
	ld a,$b9
	ret

boucle_ligne_cursor_token:
	push af
	push hl
	push de
	push bc
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,table_token-$400
	add hl,de
	bcall(_vputs)
	pop bc
	pop de
	pop hl
	pop af
	cp c_ifthen
	jr nz,boucle_ligne_cursor2
	ld a,$7e
	ld (hl),a
	inc hl
	ld (hl),a
	dec hl
	
	jr boucle_ligne_cursor2
	

affiche_curs:
	ld a,(cursor_onoff)
	inc a
	ld (cursor_onoff),a
	bit 1,a
	jr z,affiche_curs_on
	ld a,' '
	bcall(_VPutMap)
	ld a,' '
	bcall(_VPutMap)
	ret



affiche_curs_on:	
	ld a,'|'
	bcall(_VPutMap)
	ret

get_ligne_address:
	ld b,a
	sla a
	sla a
	sla a
	add a,b
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,plotsscreen
	add hl,de
	ret

efface_ligne:
	call get_ligne_address
	ld de,plotsscreen
	ld a,0
	ld (hl),a
	push hl
	pop de
	inc de
	ld bc,72
	ldir
	ret
	
efface_ecran:
	ld hl,plotsscreen
	ld a,0
	ld (hl),a
	ld (hl),a
	ld de,plotsscreen+1
	ld bc,12*64
	ldir
	ret

graph_fast_copy:
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


key_any:
	dec a
	ld e,a
	ld d,0
	ld a,(mode_key)
	sla a
	sla a
	sla a
	ld b,a
	sla a
	sla a
	add a,b
	add a,e
	ld e,a
	
	ld hl,char_maj
	
	add hl,de
	ld a,(hl)
	call inser_char
	jp key_right	



key_enter:
	ld a,c_nextline
	call inser_char

	call efface_ecran
	ld hl,(top_line_address)
	ld de,0
	ld b,10
	call boucle_ecran 
	jp key_right

inser_char:
	push af
	ld hl,(iMathPtr2)
	ld de,(iMathPtr3)
	or a
	sbc hl,de
	ret z
	ld hl,(imathptr1)
	inc (hl)
	jr nz,inser_char_size_NC
	inc hl
	inc (hl)

inser_char_size_NC:	
	ld hl,(iMathPtr2)
	push hl
	inc hl
	push hl
	ld (iMathPtr2),hl
	ld de,(cursor_address)
	or a
	sbc hl,de
	push hl
	pop bc
;	dec bc
	pop de
	pop hl
	lddr
	pop af
	ld hl,(cursor_address)
	ld (hl),a
	ret

del_char:
	ld hl,(iMathPtr2)
	ld de,(cursor_address)
	dec hl
	dec hl
	or a
	sbc hl,de
	jp z,key_loop

	inc hl
	push hl
	pop bc
	
	ld hl,(iMathPtr2)
	dec hl
	ld (iMathPtr2),hl	
	
	ld hl,(cursor_address)
	ld de,(cursor_address)	

	inc hl	
	ldir

	ld hl,(imathptr1)
	dec (hl)
	inc (hl)
	jr nz,del_char_size_NC
	inc hl
	dec (hl)
	dec hl
del_char_size_NC:	
	dec (hl)

	call efface_ecran
	ld hl,(top_line_address)
	ld de,0
	ld b,10
	call boucle_ecran 

	jp key_loop	
	

key_2nd:
	ld a,(mode_key)
	cp 2
	jr z,set_maj
	ld a,2
	ld (mode_key),a
	jp key_loop

key_alpha:
	ld a,(mode_key)
	cp 1
	jr z,set_maj
	ld a,1
	ld (mode_key),a
	jp key_loop

set_maj:
	xor a
	ld (mode_key),a
	jp key_loop



create_new_prog:
	ld hl,3
	bcall(4339h)
	push de
	pop hl
	inc hl
	inc hl
	ld a,c_nextline
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	ret


menu_comp_exec:
	ld hl,menu_comp
	jr menu_affiche

menu:

	add a,-$31

	ld b,a
	ld a,(mode_key)
	ld c,a
	sla a
	sla a
	add a,c
	add a,b
	
	sla a
	sla a
	ld b,a
	sla a
	add a,b
	ld e,a
	ld d,0
	ld hl,menu_deb
	add hl,de


menu_affiche:
	ld (menu_address),hl
	push hl
	call efface_ecran
	pop hl

	ld b,10	
	ld de,8
	
	
menu_affiche_loop:
	ld (pencol),de
	push bc
	push hl
	push de
	ld a,'9'+1
	sub b
	bcall(_vputmap)
	ld a,':'
	bcall(_vputmap)
	

	ld a,(hl)
	cp $80
	jp m,menu_affiche_not_token
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,table_token-$400
	add hl,de
	bcall(_vputs)
menu_affiche_not_token_end:
	pop de
	pop hl
	ld a,6
	add a,d
	ld d,a
	inc hl
	pop bc
	djnz menu_affiche_loop

	ld a,0
	ld (menu_cursor_line),a
	ld c,0
	jp menu_down_up

menu_affiche_not_token:
	bcall(_vputmap)
	jr menu_affiche_not_token_end
	

menu_key_loop:
	ei
	ld b,8
menu_key_loop_wait:
	halt
	djnz menu_key_loop_wait
	di
	bcall(_getCSC)
	or a
	jr z,menu_key_loop
	ld c,1
	cp 1
	jp z,menu_down_up
	cp 4
	ld c,-1
	jr z,menu_down_up
	
	ld d,0
	cp 21h
	jr z,select_number_d
	inc d
	cp 22h
	jr z,select_number_d
	inc d
	cp 1Ah
	jr z,select_number_d
	inc d
	cp 12h
	jr z,select_number_d
	inc d
	cp 23h
	jr z,select_number_d
	inc d
	cp 1Bh
	jr z,select_number_d
	inc d
	cp 13h
	jr z,select_number_d
	inc d
	cp 24h
	jr z,select_number_d
	inc d
	cp 1Ch
	jr z,select_number_d
	inc d
	cp 14h
	jr z,select_number_d

	
	
	
	
	
	cp 9
	jr nz,menu_no_selection



	ld a,(menu_cursor_line)
selected_number:
	ld e,a
	ld d,0
	ld hl,(menu_address)
	add hl,de
	ld a,(hl)

	cp c_ifthen
	jp z,add_2byte
	call inser_char

add_2byte_end:
	call efface_ecran
	ld hl,(top_line_address)
	ld de,0
	ld b,10
	call boucle_ecran
	jp key_right
	

menu_no_selection:
	call efface_ecran
	ld hl,(top_line_address)
	ld de,0
	ld b,10
	call boucle_ecran
	jp key_loop

select_number_d:	
	ld a,d
	jr selected_number


add_2byte:
	call inser_char
	call inser_char
	call inser_char

	jr add_2byte_end

menu_down_up:
	push bc
	ld a,(menu_cursor_line)
	sla a
	ld b,a
	sla a
	add a,b
	ld (penrow),a
	xor a
	ld (pencol),a
	ld a,$06
	bcall(_vputmap)
	pop bc
	ld a,(menu_cursor_line)

	add a,c
	ld e,a
	ld d,0
	ld hl,(menu_address)
	add hl,de
	xor a
	bit 7,e
	jr nz,menu_zero
	
	cp (hl)
	jr z,menu_zero

	ld a,e

menu_zero:
		
	ld (menu_cursor_line),a
	sla a
	ld b,a
	sla a
	add a,b
	ld (penrow),a
	xor a
	ld (pencol),a
	ld a,$05
	bcall(_vputmap)
	call graph_fast_copy
	jp menu_key_loop



pop_and_quit:
	pop hl
	ret


insert_sprite:
     	     
     	bcall(_zeroop1)    	; load var name
     	ld hl,picnameLD)	;
     	ld de,op1          	;
     	ld bc,5            	;
     	ldir               	;
	bcall(_chkfindsym) 	; look it up

	inc de
	inc de
	
	ld hl,plotsscreen
	ex de,hl
	ld bc,768
	ldir
	ld bc,0


insert_sprite_loop:
	ld hl,cursor_picture
	push bc
	call PutSprClpXOR
	call graph_fast_copy
	ld b,4
	ei
insert_sprite_loop_wait:
	halt
	djnz insert_sprite_loop_wait	

	pop bc
	push bc
	ld hl,cursor_picture
	call PutSprClpXOR
	call graph_fast_copy
	halt
	halt
	di	
	bcall(_getCSC)
	pop bc
	ld hl,1
	dec a
	jp z,key_sprite
	ld hl,-256*8
	dec a
	jr z,key_sprite
	ld hl,256*8
	dec a
	jr z,key_sprite
	ld hl,-1
	dec a
	jp z,key_sprite
	sub 5
	jp z,key_sprite_Enter
	cp -9
	jr z,insert_sprite_loop

	call efface_ecran
	jp menu_no_selection
	
key_sprite:
	add hl,bc
	push hl
	pop bc
	jr insert_sprite_loop
	

key_sprite_Enter:
	ld l,c
	ld h,0
	add hl,hl
	add hl,hl
	push hl
	add hl,hl
	pop de
	add hl,de
	ld d,0
	ld e,b
	sra e
	sra e
	sra e
	add hl,de
	ld de,plotsscreen
	add hl,de
	ld b,8

key_sprite_Enter_loop:
	push hl
	push bc
	ld a,(hl)
	call inser_char
	call key_right_call
	pop bc
	pop hl
	
	ld de,12
	add hl,de
	djnz key_sprite_Enter_loop


	ld a,$F2
	call inser_char

	call efface_ecran
	ld hl,(top_line_address)
	ld de,0
	ld b,10
	call boucle_ecran
	jp key_right
	


insert_matrix:
	call efface_ecran
	ld hl,matrix_choice_txt
	ld a,1
	ld (pencol),a
	ld a,1
	ld (penrow),a
	bcall(_VPuts)
 	call graph_fast_copy


insert_matrix_key_loop:
	ei
	ld b,8
insert_matrix_key_loop_wait:
	halt
	djnz insert_matrix_key_loop_wait
	di
	bcall(_getCSC)


	or a
	jr z,insert_matrix_key_loop

	ld d,$9C
	ld hl,insert_matrix_4
	cp 22h
	jr z,insert_matrix_next

	ld d,$9E
	ld hl,insert_matrix_loop8
	cp 1Ah
	jr z,insert_matrix_next

	ld d,$9D
	ld hl,insert_matrix_loop16
	cp 12h
	jr z,insert_matrix_next


	jp key_right 


insert_matrix_next:
	ld a,d
	push hl
	call inser_char
	ld hl,matAName
	bcall(_Mov9ToOP1)
	bcall(_findsym)
	pop hl
	jp c,key_right
	ld a,b
	or a
	jp nz,key_right

	push hl
	push de
	call key_right_call
	pop ix
	ld a,(ix+1)
	push ix
	call inser_char
	call key_right_call
	pop ix
	ld a,(ix+0)
	push ix
	call inser_char
	call key_right_call
	pop ix

	ld a,(ix+0)
	ld e,a
	xor a
	ld d,a
	ld hl,0
	ld b,(ix+1)
insert_matrix_mult:
	add hl,de
	djnz insert_matrix_mult

	push hl
	
	push ix	
	pop de
	ld bc,257
	bcall(4609h)

	pop bc
	ret 		;(jp déguisé)

insert_matrix_4:
	inc bc
	
	srl b
	rr c
	
	
insert_matrix_loop4:
	push bc
	ld de,OP1
	ld bc,9
	ldir
	push hl
	call OP1toHL
	ld a,l
	and %00001111
	rlca
	rlca
	rlca
	rlca
	pop hl
	push af

	ld de,OP1
	ld bc,9
	ldir
	push hl

	call OP1toHL
	ld a,%00001111
	and l
	pop hl
	pop de
	or d


	push hl
	call inser_char
	call key_right_call
	pop hl
	pop bc
	dec bc
	ld a,c
	or b
	jr nz,insert_matrix_loop4

	jp key_right


insert_matrix_loop8:
	push bc
	ld de,OP1
	ld bc,9
	ldir
	push hl
	call OP1toHL
	ld a,l
	call inser_char
	call key_right_call
	pop hl
	pop bc
	dec bc
	ld a,c
	or b
	jr nz,insert_matrix_loop8

	jp key_right


	
insert_matrix_loop16:
	push bc
	ld de,OP1
	ld bc,9
	ldir
	push hl
	call OP1toHL
	ld a,h
	push af
	ld a,l
	call inser_char
	call key_right_call
	pop af
	call inser_char
	call key_right_call

	pop hl
	pop bc
	dec bc
	ld a,c
	or b
	jr nz,insert_matrix_loop16


	jp key_right



MatAName:
.DB $02,$5C,$00,0,0



OP1toHL:
	ld ix,OP1+1
	ld a,(ix)
	and %00000111
	ld b,a
	inc b
	
	ld hl,0
OP1toHL_boucle:
	add hl,hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de

	inc ix
	ld a,(ix)
	and %11110000
	rrca
	rrca
	rrca
	rrca
	ld e,a
	ld d,0
	add hl,de
	
	dec b
	ret z

	add hl,hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de

	ld a,(ix)
	and %00001111
	ld d,0
	ld e,a
	add hl,de
	
	
	djnz OP1toHL_boucle
	ret




PutSprClpXOR:   XOR  A
__XChange_1:    LD   DE, DefaultXSpriteHeight     ; D = 0, E = Height

                OR   C                            ; If C < 0
                JP   M, _SCX_NoBotClp             ; No bottom clip.

                LD   A, $3F                       ; Is C is offscreen?
                SUB  C
                RET  C

__XChange_2:    CP   DefaultXSpriteHeight-1       ; If C + 7 < 64
                JR   NC, _SCX_NoVertClp           ; No vertical clip.
                INC  A
                LD   E, A
                JR   _SCX_NoVertClp               ; Height = 64 - C

_SCX_NoBotClp:
__XChange_3:    CP   -(DefaultXSpriteHeight-1)    ; Is C is offscreen?
                RET  C

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
                RET  NC

                LD   HL, _SCX_OPchg_1             ; Modify LD to NOP
                JR   _SCX_ClpModify

_SCX_NoRightClp:CP   -7                           ; Is B is offscreen?
                RET  C

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

                INC  IX                           ; Increment to next data
                DJNZ _SCX_LineLoop
                RET

	

char_maj:
	.db $22,"WRMH  ?",$5b
	.db "VQLG  :ZUPKFC  YTOJEBX XSNIDA",0,0
char_min:
	.db $22,"wrmh  ",$b9,$5b
	.db "vqlg  :zupkfc  ytojebx xsnida",0,0
char_num:
	.db "+-*/^  ",$1a
	.db "369)]} .258(",$c1 ,"{ 0147,%X  "
	.db $1C,"$",$23,$12,$10   


	
		
prgm_name:
	.db ProgObj,"AAAAAAAA",0
strngname:
     .db $04,$AA,$00  ; object type strng, var type strng, var Str1
picnameLD:
     .db $07,$60,$09,$00,$00  ; Pic0

table_token:			;
	.db "Label: ",0			;$80
	.db " Goto",0,0,0		;$81
	.db " call",0,0,0		;$82
	.db $C5," ",0,0,0,0,0,0		;$83 (Rho : pointer)
	.db " End",0,0,0,0		;$84
	.db " Stop",0,0,0		;$85
	.db " If",0,0,0,0,0		;$86
	.db "If-Then",0			;$87
	.db " Wait",0,0,0	
	.db "#Input",0,0
	.db "Swap1-2",0			;$8A
	.db "Swap1-N",0			;
	.db "lastKEY",0			;$8C
	.db "isKEYon",0			;$8D
	.db "#Rand",0,0,0
	.db "Mod",0,0,0,0,0
	.db "PutSprit"	 		;$90 
	.db " Xor ",0,0,0		;$91
	.db " Print",0,0		;$92
	.db "Pxl-ON",0,0		;$93
	.db "Pxl-OFF",0			;$94
	.db "Pxl-CHA",0			;$95
	.db "Pxl-Get",0			;$96
	.db "ClrDraw",0			;$97
	.db "Refresh",0			;$98
	.db "Pen-Pos",0			;$99
	.db "#Print",0,0		;$9A
	.db $C5,"Screen",0
	.db "Matrix4",0
	.db "Matrx16",0
	.db "Matrix8",0	
	.db "Line",0,0,0,0
	.db " High-B",0			;$A0
	.db " Low-B",0,0		;$A1
	.db " And",0,0,0,0		;$A2
	.db " Or",0,0,0,0,0		;$A3
	.db " Not",0,0,0,0		;$A4
	.db $05,0,0,0,0,0,0,0		;$A5  decal bin ->
	.db $CF,0,0,0,0,0,0,0		;$A6  decal bin <-
	.db " Bool ",0,0		;$A7
	.db "RamRead",0			;$A8
	.db "RamWrit",0			;$A9
	.db "Copy",$1F,0,0,0	
	.db "Str",0,0,0,0,0	
	.db "Pic",0,0,0,0,0	
	.db "prgm",0,0,0,0
	.db "Lib:",0,0,0,0	
	.db "Search",$1F,0

	.db $05,"Matrix",0			;$B0
	.db "Matrix",$05,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0		;$B0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	
	.db $b9,0,0,0,0,0,0,0		;$C0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0		;$C8
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0

	.db $b9,0,0,0,0,0,0,0		;$D0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0		;$D8
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0

	.db $b9,0,0,0,0,0,0,0		;$E0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0		;$E8
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	
	.db $b9,0,0,0,0,0,0,0		;$F0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0	
	.db $b9,0,0,0,0,0,0,0
	.db $b9,0,0,0,0,0,0,0
menu_deb:

menu_out:
	.db $90,$92,$93,$94,$95,$97,$98,$9A,$9B,$9F,0,0
Menu_in:
	.db $8C,$8D,$96,$99,$B0,$B1,$89,0,0,0,0,0
menu_binary:
	.db $A2,$A3,$91,$A4,$A0,$A1,$A5,$A6,$A7,0,0,0	
menu_stack:
	.db $1E,$1F,$8A,$A8,$A9,$AA,$AB,$AC,$AD,$AF,0,0
menu_prog:
	.db $86,$81,$82,$80,$83,$84,$24,$85,$88,$AE,0,0

menu_A1:
	.db $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,0,0,0,0
	.db $B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,0,0,0,0

	.db $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,0,0,0,0
	.db $C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,0,0,0,0

	.db $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,0,0,0,0
	.db $D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,0,0,0,0

	.db $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,0,0,0,0
	.db $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,0,0,0,0

	.db $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,0,0,0,0
	.db $F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF,0,0,0,0


menu_comp:
	.db $3D
	.db $3C
	.db $3E
	.db $8E
	.db $8F	
	
	.db 0
	.db $18
	.db $17
	.db $19
	.db 0
	.db 0
	.db 0


modes_txt:
 	.db " A-Z   ",0
 	.db " a-z   ",0
 	.db " 0-9   ",0

matrix_choice_txt:
	.db "1: 4bits        2:"
	.db " 8bits        3: 16bits",0


cursor_picture:
	.db %11000011
	.db %10000001
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %10000001
	.db %11000011


.end