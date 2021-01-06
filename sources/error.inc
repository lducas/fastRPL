err_Syntax:
	ld hl,err_syntax_mess
	jr error

err_Invalid_Dim:
	ld hl,err_Invalid_Dim_mess
	jr error

err_Break:
	ld hl,err_break_mess
	jr error

err_out_of_script:
	ld hl,err_out_of_script_mess
	jr error

err_divide_by_0:
	ld hl,err_divide_by_0_mess
	jr error


err_stack_low:
	ld hl,err_stack_low_mess
	jr error

err_not_found:
	ld hl,err_not_found_mess
	jr error

err_stack_overflow:
	ld hl,err_stack_overflow_mess
	jr error

err_ut:
err_unknown_token:
	ld a,(de)
	ld (err_unknown_token_mess_tok),a
	ld hl,err_unknown_token_mess
	jr error


warning_stack:
	ld hl,warning_stack_mess
	jr error
	
error:


	push hl
	call reset_flags
	bcall(_clrLCDFull)
	pop hl
	ld de,1
	ld (penrow),de
	ld (pencol),de
	ld (currow),de
	ld (curcol),de
	bcall(_vputs)
	ld hl,(read_address)
	dec hl
	ld de,(script_address)
	or a
	sbc hl,de
	bcall(_setXXXXop2)
	bcall(_Op2toOp1)
	bcall(4ACBh)

	ld hl,(stack_init_pos)
	ld sp,hl
	ld a,255

	bcall(_getCSC)
	ret 
	
err_Invalid_Dim_mess:
.db "ERROR : Invalid Dim Or Address",0	
err_out_of_script_mess:	
.db "ERR0R : Address 0ut 0f Script",0
err_stack_low_mess:	
.db "ERR0R : Stack is Empty",0
err_stack_overflow_mess:	
.db "ERR0R : Stack is Full",0
err_not_found_mess:
.db "ERROR : Not Found",0

err_divide_by_0_mess:
.db "ERROR : division by 0",0

err_unknown_token_mess:
.db "ERR0R : unknown token : "
err_unknown_token_mess_tok:
.db 0,0,0
warning_stack_mess:	
.db "WARNING : Stack not at 0",0

err_break_mess:
.db "ERROR : Break",0

err_Syntax_mess:
.db "ERROR : Syntax",0
