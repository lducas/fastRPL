;--------------------------= put_in_matrice =---------------------------*
; Author:       Akambi Fagbohoun                                        *
; Email:        Mcbeach1@hotmail.com                                    *
; Version:      1.0                                                     *
; Platform:     TI-OS                                                   *
; Date:         23/02/2005                                              *
;                                                                       *
; entré:        pile = N,Y,X,p et p est le dernier ‚l‚ment de la pile   *                    
; sortie:       pile = 0 , error  et si pile = 1 no error               *
; registres d‚truits:       all register                                *
;-----------------------------------------------------------------------*

put_in_matrice:

        pop ix            ; ix = adresse de la matrice
        ld a,(ix+0)
        and %11111100
        cp %10011100
        jr z,label_is_ptrmatrice

        pop hl
        pop hl
        pop hl
        jp err_Invalid_Dim

label_is_ptrmatrice:

        pop hl            ; l = x
        ld a,h
        or a
        jr nz,poppile2
        or l
        jr z,poppile2     ; si x=0 quitte la routine doc x>0

        ld b,(ix+2)       ; (ix+2) = largeur de la matrice
        inc b
        cp b
        jr c,x_is_not_sup_to_largeur  ; si x < largeur de la matrice, comme x>0
                                      ; largeur de la matrice > 0
poppile2:
        pop hl
poppile1:
        pop hl            ; ‚quilibrage de la pile...
        jp err_Invalid_Dim

x_is_not_sup_to_largeur:

        dec l              ;  car le premier la premier colonne est 0
        
        pop de            ; e = y
        ld a,d
        or a
        jr nz,poppile1
        or e
        jr z,poppile1

        ld c,(ix+1)       ; (ix+1) = hauteur de la matrice
        inc c
        cp c
        jr nc,poppile1    ; si y sup‚rieur … hauteur quitte la routine
                          ; comme avant, hauteur de la matrice > 0

        dec e
        jr z,y_ispointed


        ld a,e
        dec b
        ld e,b

        add hl,de
        dec a
        jr z,y_ispointed
multhl_by_a:
        add hl,de
        dec a
        jr nz,multhl_by_a

y_ispointed:

	ld a,(ix+0)
	rrca
	jr nc,no_matrice_16bit
        add hl,hl              ; car les donn‚es sont des mots
	jr matrice_8bit
no_matrice_16bit:
	rrca
	jr c,matrice_8bit

	srl h
	rr l
	ld c,1
	jr c,matrice_8bit
	ld c,0

matrice_8bit:
        ld de,3
        add hl,de              ; skip les 3 premiers octets
        push ix
        pop de
        add hl,de
        pop de

        ld a,(ix+0)
        rrca
        jr c,bit16

	rrca
	jr c,matrice_bit8
	ld a,e
	and %11110000
        jp nz,err_Invalid_Dim

	ld a,c
	or a
	jr nz,matrice_bit4
	sla e
	sla e
	sla e
	sla e
	ld a,(hl)
	and %00001111
	or e
	ld (hl),a
	jp read_loop

matrice_bit4:

	ld a,e
	and %00001111
	ld e,a
	ld a,(hl)
	and %11110000
	or e
	ld (hl),a
	jp read_loop

bit16:
        ld (hl),e
        inc hl
        ld (hl),d
	jp read_loop
matrice_bit8:
        ld (hl),e
	jp read_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;************************************************************************



;--------------------------= get_in_matrice =----------------------------*
; Author:       Akambi Fagbohoun                                        *
; Email:        Mcbeach1@hotmail.com                                    *
; Version:      1.0                                                     *
; Platform:     TI-OS                                                   *
; Date:         23/02/2005                                              *
;                                                                       *
; entré:        pile = Y,X,p et p est le dernier ‚l‚ment de la pile   *                    *
; sortie:       pile = 0 , error  et si pile = 1 no error               *
; registres d‚truits:       all register                                *
;-----------------------------------------------------------------------*

get_in_matrice:

        pop ix            ; ix = adresse de la matrice
        ld a,(ix+0)
        and %11111100
        cp %10011100
        jr z,label_is_ptrmatrice_get

        pop hl
        pop hl
        jp err_Invalid_Dim

label_is_ptrmatrice_get:

        pop hl            ; l = x
        ld a,h
        or a
        jr nz,poppile1_get
        or l
        jr z,poppile1_get     ; si x=0 quitte la routine doc x>0

        ld b,(ix+2)       ; (ix+2) = largeur de la matrice
        inc b
        cp b
        jr c,x_is_not_sup_to_largeur_get  ; si x < largeur de la matrice, comme x>0
                                      ; largeur de la matrice > 0
poppile1_get:
        pop hl            ; ‚quilibrage de la pile...
poppile0_get:
        jp err_Invalid_Dim

x_is_not_sup_to_largeur_get:

        dec l              ;  car le premier la premier colonne est 0
        
        pop de            ; e = y
        ld a,d
        or a
        jr nz,poppile0_get
        or e
        jr z,poppile0_get

        ld c,(ix+1)       ; (ix+1) = hauteur de la matrice
        inc c
        cp c
        jr nc,poppile0_get    ; si y sup‚rieur … hauteur quitte la routine
                          ; comme avant, hauteur de la matrice > 0

        dec e
        jr z,y_ispointed_get


        ld a,e
        dec b
        ld e,b

        add hl,de
        dec a
        jr z,y_ispointed_get
multhl_by_a_get:
        add hl,de
        dec a
        jr nz,multhl_by_a_get

y_ispointed_get:

	ld a,(ix+0)
	rrca
	jr nc,no_matrice_16bit_get
        add hl,hl              ; car les donn‚es sont des mots
	jr matrice_8bit_get
no_matrice_16bit_get:
	rrca
	jr c,matrice_8bit_get

	srl h
	rr l
	ld c,1
	jr c,matrice_8bit_get
	ld c,0

matrice_8bit_get:
        ld de,3
        add hl,de              ; skip les 3 premiers octets
        push ix
        pop de
        add hl,de

        ld a,(ix+0)
        rrca
        jr c,bit16_get

	rrca
	jr c,matrice_bit8_get

	ld a,(hl)
	ld e,a
	
	ld a,c
	or a
	jr nz,matrice_bit4_get
	srl e
	srl e
	srl e
	srl e
	ld d,0
        push de   ; le stockage s'est mal d‚roul‚
	jp read_loop

matrice_bit4_get:

	ld a,e
	and %00001111
	ld e,a
	ld d,0
        push de   ; le stockage s'est mal d‚roul‚
	jp read_loop

bit16_get:
        ld e,(hl)
        inc hl
        ld d,(hl)
        push de                ; le stockage c'est bien d‚roul,
	jp read_loop
matrice_bit8_get:
	ld d,0
        ld e,(hl)
        push de                ; le stockage c'est bien d‚roul‚
	jp read_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;************************************************************************
