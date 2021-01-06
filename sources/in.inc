Pen_pos:
	ld hl,(pencol)
	ld e,h
	xor a
	ld h,a
	ld d,a
	push hl
	push de
	jp read_loop
	


isKEYon:
	pop hl
	ld de,tableau_isKEYon-20
	add hl,hl
	add hl,de
	ld de,0
	ld a,reset
	out (1),a
	ld a,(hl)
	out (1),a
	in a,(1)
	inc hl
	cpl
	and (hl)
	jp nz,comp_vrai
	push de
	jp read_loop


lastkey:
	bcall(_getCSC)
	ld e,a
	ld d,0
	ld hl,tableau_lastkey
	add hl,de
	ld a,(hl)
	ld e,a
	push de
	jp read_loop
	
	

tableau_lastkey:
	.db 0,34,24,26,18,0,0,0,0,105,95,85,75,65,55,45
	.db 0,104,94,84,74,64,54,44,0,103,93,83,73,63,53,43
	.db 33,102,92,82,72,62,52,42,32,0,91,81,71,61,51,41
	.db 31,15,14,13,12,11,21,22,23
	

tableau_isKEYon:
.db group7,%00011111	;10 	ligne 1
.db group7,%00010000	;11	Y=
.db group7,%00001000	;12	Window
.db group7,%00000100	;13	Zoom
.db group7,%00000010	;14	Trace
.db group7,%00000001	;15	Graph
.db group1,0		;16
.db group1,0		;17
.db group1,0		;18
.db group1,0		;19

.db group7,%11100000	;20	2eme ligne (2nd+Mode+Del)
.db group7,%00100000	;21	2nd
.db group7,%01000000	;22	mode
.db group7,%10000000	;23	del
.db group1,%00000010	;24	[<]
.db group1,%00001000	;25	[^]
.db group1,%00000100	;26	[>]
.db group1,%00000001	;27	[v]

.db group1,%00000110	;28	[<] ou [>]
.db group1,%00001001	;29	[^] ou [v]
.db group1,%00001111	;30	[ < > ^ v ]

.db group6,%10000000	;31	Alpha
.db group5,%10000000	;32	X,T,Theta,n
.db group4,%10000000	;33	stat
.db group1,%00000001	;34	[v]

.db group1,0		;35
.db group1,0		;36
.db group1,0		;37
.db group1,0		;38
.db group1,0		;39
.db group1,0		;40

.db group6,%01000000	;41	Math
.db group5,%01000000	;42	Apps
.db group4,%01000000	;43	Prgm
.db group3,%01000000	;44	Vars
.db group2,%01000000	;45	Clear

.db group1,0		;46
.db group1,0		;47
.db group1,0		;48
.db group1,0		;49
.db group1,0		;50



.db group6,%00100000	;51	x-1
.db group5,%00100000	;52	sin
.db group4,%00100000	;53	cos
.db group3,%00100000	;54	tan
.db group2,%00100000	;55	^

.db group1,0		;56
.db group1,0		;57
.db group1,0		;58
.db group1,0		;59
.db group1,0		;60


.db group6,%00010000	;61	x²
.db group5,%00010000	;62	,
.db group4,%00010000	;63	(
.db group3,%00010000	;64	)
.db group2,%00010000	;65	/

.db group1,0		;66
.db group1,0		;67
.db group1,0		;68
.db group1,0		;69
.db group1,0		;70


.db group6,%00001000	;71	log
.db group5,%00001000	;72	7
.db group4,%00001000	;73	8
.db group3,%00001000	;74	9
.db group2,%00001000	;75	*

.db group1,0		;76
.db group1,0		;77
.db group1,0		;78
.db group1,0		;79
.db group1,0		;80

.db group6,%00000100	;81	ln
.db group5,%00000100	;82	4
.db group4,%00000100	;83	5
.db group3,%00000100	;84	6
.db group2,%00000100	;85	-

.db group1,0		;86
.db group1,0		;87
.db group1,0		;88
.db group1,0		;89
.db group1,0		;90

.db group6,%00000010	;91	Sto->
.db group5,%00000010	;92	1
.db group4,%00000010	;93	2
.db group3,%00000010	;94	3
.db group2,%00000010	;95	+

.db group1,0		;96
.db group1,0		;97
.db group1,0		;98
.db group1,0		;99
.db group1,0		;100

.db group6,%00000001	;101	
.db group5,%00000001	;102	0
.db group4,%00000001	;103	.
.db group3,%00000001	;104	(-)
.db group2,%00000001	;105	ENTER





input_nb:
        bcall(4570h)		;runindicoff
	res textInverse, (IY + textflags)
	res fracDrawLFont, (IY + fontFlags)
	ld a,$06
	ld (updateactiveedit_big+1),a

	pop hl
	ld a,l

	bit 0,h
	jr z,input_not_reversed
	set textInverse, (IY +textFlags)
	neg
input_not_reversed:
	ld h,a
	ld (text_next_line_X+1),a
	pop bc

	bit 0,b
	jr z,input_not_big
	set fracDrawLFont, (IY +fontFlags)
	ld a,' '
	ld (updateactiveedit_big+1),a
	ld a,c
	neg
	ld c,a
input_not_big:
	






        ld a,2
        add a,h 
	ld (tableedit+1),a

        ld a,2
        add a,c
	ld (tableedit),a
	xor a
	ld (tableedit+2),a
        ld hl,value
        ld (hl),a
        ld bc,5
        ld de,value+1
        ldir
        ld a,255
        ld (key_press),a	
	call updateactiveedit
        call graph_fast_copy_call
  
touchedialog:
        ld a,0ffh
        out (1),a
        ld a,0fdh
        out (1),a
        in a,(1)
        rrca
        jp nc,valid_enter       ; touche [ENTER]
        rlca
        rlca
        rlca
        jp c,no_undo 
 	xor a
	ld (tableedit+2),a
        ld hl,value
        ld a,06h
        ld (hl),a
        ld bc,4
        ld de,value+1
        ldir
        ld a,255
        ld (key_press),a
        call updateactiveedit
        call graph_fast_copy
        jr touchedialog
no_undo:
        ld a,0ffh
        out (1),a
        ld a,0bfh
        out (1),a
        in a,(1)
        rlca
        jr c,no_delvarletter       ; touche [DEL]
        call delvarletter
        jr touchedialog

no_delvarletter:
        bcall(_getkey)
        cp 8Eh
        jr nz,no_0
        ld a,48
        ld (key_press),a
no_0:
        cp 8Fh
        jp nz,no_1
        ld a,49
        ld (key_press),a
no_1:
        cp $90
        jr nz,no_2
        ld a,50
        ld (key_press),a
no_2:
        cp $91
        jr nz,no_3
        ld a,51
        ld (key_press),a
no_3:
        cp $92
        jr nz,no_4
        ld a,52
        ld (key_press),a
no_4:
        cp $93
        jr nz,no_5
        ld a,53
        ld (key_press),a
no_5:
        cp $94
        jr nz,no_6
        ld a,54
        ld (key_press),a
no_6:
        cp $95
        jr nz,no_7
        ld a,55
        ld (key_press),a
no_7:
        cp $96
        jr nz,no_8
        ld a,56
        ld (key_press),a
no_8:
        cp $97
        jr nz,no_9
        ld a,57
        ld (key_press),a
no_9:
        ld a,(key_press)
        cp 255
        call nz,storevalue
        jp touchedialog

delvarletter:

        ld hl,tableedit+2
        ld a,(hl)
        or a
        jr z,noefface
        ld c,a
        dec (hl)
	ld hl,value
        ld b,0
        add hl,bc
        dec hl
        ld a,0h
        ld (hl),a
        call updateactiveedit
	ei
	ld b,10
delvarletter_delay:
	halt
	djnz delvarletter_delay

noefface:
        jp graph_fast_copy_call
        

storevalue:

        ld hl,tableedit+2
        ld a,(hl)
	cp 5
        jr z,maxvalue
        ld c,(hl)
        inc (hl)
	ld hl,value
        ld b,0
        add hl,bc
        ld a,(key_press)
        ld (hl),a
        call updateactiveedit

maxvalue:
        ld a,255
        ld (key_press),a
        jp graph_fast_copy_call

updateactiveedit:

        ld hl,tableedit
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (pencol),de
	ld hl,value
        bcall(_vputs)
       	ld a,'|'
	bcall(_VPutMap)
updateactiveedit_big:
	ld a,$06
	bcall(_VPutMap)
        
        ret


; les données suivantes font parties de la fonction

key_press:
.db 255
tableedit:
; x,y,position du curseur
.db 0,0,0
value:
.db 0,0,0,0,0,0
valid_enter:

        ld hl,tableedit
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (pencol),de
	ld hl,value
        bcall(_vputs)
       	ld a,' '
	bcall(_VPutMap)
       	ld a,' '
	bcall(_VPutMap)
        call graph_fast_copy_call

        


        ld hl,0
        ld de,value
        ld a,(de)
        or a
        jr z,end_calculate_number
        cp 06h
        jr z,end_calculate_number
        sub 48
        ld l,a
        ld h,0
calculate_number:
        inc de
        ld a,(de)
        or a
        jr z,end_calculate_number
        cp 06h
        jr z,end_calculate_number
        sub 48        
	add hl,hl
	ld b,h
	add a,l
	ld c,a
	add hl,hl
	add hl,hl
	add hl,bc
        jr calculate_number

end_calculate_number:
	push hl
;        bcall(_disphl)
;        bcall(_getkey)
	jp read_loop
