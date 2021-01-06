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
 #define stack_init_height 	saferam1+8
 #define stack_init_pos 	saferam1+10
 
 #define var_table 		saferam1+20
 
 #define nb_lbl 		saferam1+160
 #define lbl_table 		saferam1+162
 #define nextvar  		saferam1+260
 
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
 #define _StoN			4ACBh	

 #define fontFlags		50
 #define fracDrawLFont		2
 #define lib_token		$FE
 #define lib_token2		$BB
 





start: 
	jr flag_setting
	jp read_loop
flag_setting:
	di
	set textWrite, (IY + sGrFlags)
	set textEraseBelow, (IY + textFlags)


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
	ret c	
	ld a,1


	push de
	pop ix
	ld c,(ix+0)
	ld b,(ix+1)
	inc bc
	ld (script_lenght),bc
		
	inc de
;	inc de
	ld (script_address),de
;	inc de
	ld (read_address),de

	ex de,hl
	ld ix,lbl_table
	ld de,nb_lbl
	
lbl_search:
	ld a,$80
	cpir
	jr nz,lbl_search_ended

	ld a,(hl)
	ld (ix+0),a
	inc hl
	inc ix
	ld (ix+0),l
	inc ix
	ld (ix+0),h
	inc ix
	ex de,hl
	inc (hl)
	ex de,hl
	jr lbl_search



lbl_search_ended:
	ld hl,0
	add hl,sp
	ld (stack_init_pos),hl
	
	ld b,stack_safe_buffer
stack_safe_buffer_fill:
	push bc
	djnz stack_safe_buffer_fill


	ld hl,0
	xor a
	sbc hl,sp
	push bc
	ld (stack_init_height),hl


	
read_loop:
	ld a,reset
	out (1),a
	ld a,group2
	out (1),a
	in a,(1)
	cp %10111110				;CLEAR + ENTER
	jp z,err_break

	ld hl,(stack_init_height)
	add hl,sp
	jp c,err_stack_low
	ld de,stack_max_size
	add hl,de
	jp nc,err_stack_overflow
	


	ld de,(read_address)			;
	inc de					;Avance le curseur de lecture
	ld (read_address),de			;


	ld a,(de)	
	
	ld c,a					;
	ld b,0					;
	sla c					;
	rl b					;
	ld ix,tab_funct				;hl= adresse de la fonction correspondant au caractère lu
	add ix,bc				;
	ld h,(ix+1)				;
	ld l,(ix+0)				;
	
	
	
	jp (hl)					;saut a la fonction
					
	


quit:
	ld hl,(stack_init_height)
	pop bc
	add hl,sp
	ld a,h
	or l
	jp nz,warning_stack
	
	ld hl,(stack_init_pos)
	ld sp,hl
	call reset_flags

	ld hl,1
	bcall(_setXXXXop2)
	bcall(_Op2toOp1)

	bcall(4ACBh)
	
	ret
reset_flags:
	res textWrite, (IY + sGrFlags)
	res textInverse, (IY + textflags)
	res fracDrawLFont, (IY + fontFlags)
	ei


	ret

	#include "math.inc"
	#include "stack_op.inc"
	#include "out.inc"
	#include "control.inc"
	#include "bin_op.inc"
	#include "in.inc"
	#include "error.inc"
	#include "data_LS.inc"
	#include "lib.inc"
	#include "matrice.inc"
prgm_name:
	.db ProgObj,"AAAAAAAA",0
strngname:
     .db $04,$AA,$00  ; object type strng, var type strng, var Str1





tab_funct:
.dw err_ut	; token 00h
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	;  
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw racine_carre	; token 10h
.dw err_ut	; 
.dw carre	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw minus	; 
.dw err_ut	; 
.dw store	; 
.dw err_ut	; 
.dw duplicate	; 
.dw drop	; 
.dw read_loop	; 	token 20h
.dw err_ut	; 
.dw guillemet	; 
.dw err_ut	; 
.dw push_address	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw multiplication	; 2A
.dw addition		; 
.dw read_loop		; 
.dw soustraction	; 
.dw err_ut	; 
.dw division	; 
.dw push_nb	; 0 	token 30h
.dw push_nb	; 1
.dw push_nb	; 2
.dw push_nb	; 3
.dw push_nb	; 4
.dw push_nb	; 5
.dw push_nb	; 6
.dw push_nb	; 7
.dw push_nb	; 8
.dw push_nb	; 9
.dw err_ut	; 
.dw err_ut	; 
.dw inferieur	; <=
.dw egal	; =
.dw superieur	; >=
.dw err_ut	; 
.dw err_ut	; 	token $40 
.dw push_var	; 
.dw push_var	; 
.dw push_var	; 
.dw push_var	; 
.dw push_var
.dw push_var
.dw push_var
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var	; 	token 50h
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw load_prgm	; 
.dw err_ut	; 	token 60h
.dw push_var	; 
.dw push_var	; 
.dw push_var	; 
.dw push_var	; 
.dw push_var
.dw push_var
.dw push_var
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var	; 	token 70h
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw push_var 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw jump_1	; 	token 80h
.dw goto	; 
.dw callfunc	; 
.dw pointeur	; 
.dw read_loop	; 
.dw quit	; 
.dw si		; 
.dw err_ut	; 
.dw wait	; 
.dw input_nb	; 
.dw swap12	; 
.dw err_ut	; 
.dw lastkey	; 
.dw isKEYon	; 
.dw random	; 
.dw modulo	; 
.dw putsprite	; 	token 90h
.dw Xou		; 
.dw text	; 
.dw pixel	; text(
.dw pixel	; 
.dw pixel	; 
.dw pixel	; 
.dw efface_ecran; 
.dw graph_fast_copy	; 
.dw Pen_Pos	; 
.dw Disp	; 
.dw screen_address	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw draw_line	; 
.dw highB	; 	token A0h
.dw lowB	;
.dw et		;
.dw ou		;
.dw non
.dw decalR	; 
.dw decalL	; 
.dw Bool	; 
.dw read_byte	; 
.dw write_byte	; 
.dw copy_data_down
.dw load_str	; Str
.dw load_pic	; 
.dw load_prgm	; 
.dw load_lib	; 
.dw search_2bytes	; 
.dw put_in_matrice	; 	token $B0
.dw get_in_matrice	; 	
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	;
.dw err_ut	; 	token C0h
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 	token D0h
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 	token E0h
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 	token F0h
.dw err_ut	; 
.dw read_loop	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 
.dw err_ut	; 



.end