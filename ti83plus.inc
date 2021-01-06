;======================================================================
; Include File for the TI-83 Plus
; Last Updated 02/20/2002 
;
; Copyright (c) 1999, 2001, 2002 Texas Instruments: The Licensed Materials are
; copyrighted by TI. LICENSEE agrees that it will 
; not delete the copyright notice, trademarks or
; protective notices from any copy made by LICENSEE.
;
; Warranty: TI does not warrant that the Licensed Materials will
; be free from errors or will meet your specific requirements.
; The Licensed Materials are made available "AS IS" to LICENSEE.
;
; Limitations: TI MAKES NO WARRANTY OR CONDITION, EITHER EXPRESS
; OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE,
; REGARDING THE LICENSED MATERIALS.  IN NO EVENT SHALL
; TI OR ITS SUPPLIERS BE LIABLE FOR ANY INDIRECT, INCIDENTAL
; OR CONS=ENTIAL DAMAGES, LOSS OF PROFITS, LOSS OF USE OR DATA, 
; OR INTERRUPTION OF BUSINESS, WHETHER THE ALLEGED DAMAGES ARE
; LABELED IN TORT, CONTRACT OR INDEMNITY.
;
;
;======================================================================
;           System Variable Equates
;======================================================================
;       Entry Points : RclSysTok, StoSysTok
;
XSCLt		=  2
YSCLt		=  3
XMINt		=  0Ah
XMAXt		=  0Bh
YMINt		=  0Ch
YMAXt		=  0Dh
TMINt		=  0Eh
TMAXt		=  0Fh
THETMINt	=  10h
THETMAXt	=  11h
TBLMINt		=  1Ah
PLOTSTARTt	=  1Bh
NMAXt		=  1Dh
NMINt		=  1Fh
TBLSTEPt	=  21h
TSTEPt		=  22h
THETSTEPt	=  23h
DELTAXt		=  26h
DELTAYt		=  27h
XFACTt		=  28h
YFACTt		=  29h
FINNt		=  2Bh
FINIt		=  2Ch
FINPVt		=  2Dh
FINPMTt		=  2Eh
FINFVt		=  2Fh
FINPYt		=  30h
FINCYt		=  31h
PLOTSTEPt	=  34h
XRESt		=  36h
;

;======================================================================
;           Run indicators
;======================================================================
;
busyPause	=	10101010b
busyNormal	=	11110000b
;
;======================================================================
; Macro to call base code from an app,
;======================================================================
;
rBR_CALL	=	28h		; NEEDED FOR APPS
BRT_JUMP0	=	50h
;
B_CALL macro   label
	     RST     rBR_CALL
	     DW      _&label
       .endm
;
B_JUMP macro   label
	     CALL    BRT_JUMP0	     ; fixed location on page 0
	     DW      _&label
       .endm

;======================================================================
;           Common subroutine RST numbers  
;======================================================================
;
rOP1TOOP2	=  08h
rFINDSYM	=  10h
rPUSHREALO1	=  18h
rMOV9TOOP1	=  20h
rFPADD		=  30h
;

;SYMBOLIC_DEBUG_START
;======================================================================
;           Entry points  
;======================================================================
;
_AbsO1O2Cp	= 410Eh
_AbsO1PAbsO2	= 405Ah
_ACos		= 40DEh
_ACosH		= 40F0h
_ACosRad	= 40D2h
_AdrLEle	= 462Dh
_AdrMEle	= 4609h
_AdrMRow	= 4606h
_AllEq		= 4876h
_AllocFPS	= 43A5h
_AllocFPS1	= 43A8h
_Angle		= 4102h
_AnsName	= 4B52h
_ApdSetup	= 4C93h
_AppGetCalc	= 4C78h
_AppGetCbl	= 4C75h
_AppInit	= 404Bh  ;***
_Arc_Unarc	= 4FD8h
_ASin		= 40E4h
_ASinH		= 40EDh
_ASinRad	= 40DBh
_ATan		= 40E1h
_ATan2		= 40E7h
_ATan2Rad	= 40D8h
_ATanH		= 40EAh
_ATanRad	= 40D5h

_BinOPExec	= 4663h
_Bit_VertSplit	= 4FA8h
_BufClr		= 5074h
_BufCpy		= 5071h

_CAbs		= 4E97h
_CAdd		= 4E88h
_CanAlphIns	= 4C69h
_CDiv		= 4E94h
_CDivByReal	= 4EBBh
_CEtoX		= 4EA9h
_CFrac		= 4EC1h
_CheckSplitFlag	= 49F0h
_Chk_Batt_Low	= 50B3h  ;***
_ChkFindSym	= 42F1h
_CIntgr		= 4EC4h
_CircCmd	= 47D4h
_CkInt		= 4234h
_CkOdd		= 4237h
_CkOP1C0	= 4225h
_CkOP1Cplx	= 40FCh
_CkOP1FP0	= 4228h
_CkOP1Pos	= 4258h
_CkOP1Real	= 40FFh
_CkOP2FP0	= 422Bh
_CkOP2Pos	= 4255h
_CkOP2Real	= 42DFh
_CkPosInt	= 4231h
_CkValidNum	= 4270h
_CleanAll	= 4A50h
_ClearRect	= 4D5Ch
_ClearRow	= 4CEDh
_CLine		= 4798h
_CLineS		= 479Bh
_CLN		= 4EA0h
_CLog		= 4EA3h
_CloseEditBuf	= 48D3h  ;***
_CloseEditBufNoR	= 476Eh
_CloseEditEqu	= 496Ch  ;***
_CloseProg	= 4A35h
_ClrGraphRef	= 4A38h
_ClrLCD		= 4543h
_ClrLCDFull	= 4540h
_ClrLp		= 41D1h
_ClrOP1S	= 425Eh
_ClrOP2S	= 425Bh
_ClrScrn	= 4549h
_ClrScrnFull	= 4546h
_ClrTxtShd	= 454Ch
_CMltByReal	= 4EB8h
_CmpSyms	= 4A4Ah
_CMult		= 4E8Eh
_Conj		= 4EB5h
_ConvDim	= 4B43h
_ConvDim00	= 4B46h
_ConvLcToLr	= 4A23h
_ConvLrToLc	= 4A56h
_ConvOP1	= 4AEFh
_COP1Set0	= 4105h
_Cos		= 40C0h
_CosH		= 40CCh
_CpHLDE		= 400Ch
_CPoint		= 4DC8h
_CPointS	= 47F5h
_CpOP1OP2	= 4111h
_CpOP4OP3	= 4108h
_CpyO1ToFPS1	= 445Ch
_CpyO1ToFPS2	= 446Bh
_CpyO1ToFPS3	= 4477h
_CpyO1ToFPS4	= 4489h
_CpyO1ToFPS5	= 4483h
_CpyO1ToFPS6	= 447Dh
_CpyO1ToFPS7	= 4480h
_CpyO1ToFPST	= 444Ah
_CpyO2ToFPS1	= 4459h
_CpyO2ToFPS2	= 4462h
_CpyO2ToFPS3	= 4474h
_CpyO2ToFPS4	= 4486h
_CpyO2ToFPST	= 4444h
_CpyO3ToFPS1	= 4453h
_CpyO3ToFPS2	= 4465h
_CpyO3ToFPST	= 4441h
_CpyO5ToFPS1	= 4456h
_CpyO5ToFPS3	= 4471h
_CpyO6ToFPS2	= 4468h
_CpyO6ToFPST	= 4447h
_CpyStack	= 4429h
_CpyTo1FPS1	= 4432h
_CpyTo1FPS10	= 43F3h
_CpyTo1FPS11	= 43D8h
_CpyTo1FPS2	= 443Bh
_CpyTo1FPS3	= 4408h
_CpyTo1FPS4	= 440Eh
_CpyTo1FPS5	= 43DEh
_CpyTo1FPS6	= 43E4h
_CpyTo1FPS7	= 43EAh
_CpyTo1FPS8	= 43EDh
_CpyTo1FPS9	= 43F6h
_CpyTo1FPST	= 4423h
_CpyTo2FPS1	= 442Fh
_CpyTo2FPS2	= 4438h
_CpyTo2FPS3	= 4402h
_CpyTo2FPS4	= 43F9h
_CpyTo2FPS5	= 43DBh
_CpyTo2FPS6	= 43E1h
_CpyTo2FPS7	= 43E7h
_CpyTo2FPS8	= 43F0h
_CpyTo2FPST	= 4420h
_CpyTo3FPS1	= 442Ch
_CpyTo3FPS2	= 4411h
_CpyTo3FPST	= 441Dh
_CpyTo4FPST	= 441Ah
_CpyTo5FPST	= 4414h
_CpyTo6FPS2	= 43FFh
_CpyTo6FPS3	= 43FCh
_CpyTo6FPST	= 4417h
_CpyToFPS1	= 445Fh
_CpyToFPS2	= 446Eh
_CpyToFPS3	= 447Ah
_CpyToFPST	= 444Dh
_CpyToStack	= 4450h
_Create0Equ	= 432Ah
_CreateAppVar	= 4E6Ah
_CreateCList	= 431Bh
_CreateCplx	= 430Ch
_CreateEqu	= 4330h
_CreatePair	= 4B0Dh
_CreatePict	= 4333h
_CreateProg	= 4339h
_CreateProtProg	= 4E6Dh
_CreateReal	= 430Fh
_CreateRList	= 4315h
_CreateRMat	= 4321h
_CreateStrng	= 4327h
_CRecip		= 4E91h
_CSqRoot	= 4E9Dh
_CSquare	= 4E8Bh
_CSub		= 4E85h
_CTenX		= 4EA6h
_CTrunc		= 4EBEh
_Cube		= 407Bh
_CursorOff	= 45BEh
_CursorOn	= 45C4h
_CXrootY	= 4EACh
_CYtoX		= 4EB2h

_DarkLine	= 47DDh
_DarkPnt	= 47F2h
_DataSize	= 436Ch
_DataSizeA	= 4369h
_DeallocFPS	= 439Fh
_DeallocFPS1	= 43A2h
_DecO1Exp	= 4267h
_DelListEl	= 4A2Fh
_DelMem		= 4357h
_DelRes		= 4A20h
_DelVar		= 4351h
_DelVarArc	= 4FC6h
_DelVarNoArc	= 4FC9h
_DisableApd	= 4C84h
_Disp		= 4F45h
_DispDone	= 45B5h
_DispEOL	= 45A6h
_DispHL		= 4507h
_DisplayImage	= 4D9Bh
_DispMenuTitle	= 5065h
_DispOP1A	= 4BF7h
_DivHLBy10	= 400Fh
_DivHLByA	= 4012h
_DrawCirc2	= 4C66h
_DrawCmd	= 48C1h
_DrawRectBorder	= 4D7Dh
_DrawRectBorderClear	= 4D8Ch
_DToR		= 4075h

_EditProg	= 4A32h
_EnableApd	= 4C87h
_EnoughMem	= 42FDh
_EOP1NotReal	= 4279h
_Equ_or_NewEqu	= 42C4h
_EraseEOL	= 4552h
_EraseRectBorder	= 4D86h
_ErrArgument	= 44ADh
_ErrBadGuess	= 44CBh
_ErrBreak	= 44BFh
_ErrD_OP1_0	= 42D3h
_ErrD_OP1_LE_0	= 42D0h
_ErrD_OP1Not_R	= 42CAh
_ErrD_OP1NotPos	= 42C7h
_ErrD_OP1NotPosInt	= 42CDh
_ErrDataType	= 44AAh
_ErrDimension	= 44B3h
_ErrDimMismatch	= 44B0h
_ErrDivBy0	= 4498h
_ErrDomain	= 449Eh
_ErrIncrement	= 44A1h
_ErrInvalid	= 44BCh
_ErrIterations	= 44C8h
_ErrLinkXmit	= 44D4h
_ErrMemory	= 44B9h
_ErrNon_Real	= 44A4h
_ErrNonReal	= 4A8Ch
_ErrNotEnoughMem	= 448Ch
_ErrOverflow	= 4495h
_ErrSignChange	= 44C5h
_ErrSingularMat	= 449Bh
_ErrStat	= 44C2h
_ErrStatPlot	= 44D1h
_ErrSyntax	= 44A7h
_ErrTolTooSmall	= 44CEh
_ErrUndefined	= 44B6h
_EToX		= 40B4h
_Exch9		= 43D5h
_ExLp		= 4222h
_ExpToHex	= 424Fh

_Factorial	= 4B85h
_FillRect	= 4D62h
_FillRectPattern	= 4D89h
_Find_Parse_Formula	= 4AF2h
_FindAlphaDn	= 4A47h
_FindAlphaUp	= 4A44h
_FindApp		= 4C4Eh
_FindAppDn		= 4C4Bh
_FindAppNumPages	= 509Bh
_FindAppUp		= 4C48h
_FindSym	= 42F4h
_FiveExec	= 467Eh
_FixTempCnt	= 4A3Bh
_FlashToRam	= 5017h
_ForceFullScreen	= 508Fh
_FormBase	= 50AAh
_FormDCplx	= 4996h
_FormEReal	= 4990h
_FormReal	= 4999h
_FourExec	= 467Bh
_FPAdd		= 4072h
_FPDiv		= 4099h
_FPMult		= 4084h
_FPRecip	= 4096h
_FPSquare	= 4081h
_FPSub		= 406Fh
_Frac		= 4093h

_Get_Tok_Strng	= 4594h  ;***
_GetBaseVer	= 4C6Fh
_GetCSC		= 4018h
_GetKey		= 4972h
_GetLToOP1	= 4636h
_GetMToOP1	= 4615h
_GetSysInfo 	= 50DDh  ; v. 1.13+ only
_GetTokLen	= 4591h  ;***
_GrBufClr	= 4BD0h
_GrBufCpy	= 486Ah
_GrphCirc	= 47D7h

_HLTimes9	= 40F9h
_HorizCmd	= 48A6h
_HTimesL	= 4276h

_IBounds	= 4C60h
_IBoundsFull	= 4D98h
_ILine		= 47E0h
_IncLstSize	= 4A29h
_InsertList	= 4A2Ch
_InsertMem	= 42F7h
_Int		= 40A5h
_Intgr		= 405Dh
_InvCmd		= 48C7h
_InvertRect	= 4D5Fh
_InvOP1S	= 408Dh
_InvOP1SC	= 408Ah
_InvOP2S	= 4090h
_InvSub		= 4063h
_IOffset	= 4C63h
_IPoint		= 47E3h
_IsA2ByteTok	= 42A3h  ;***
_IsEditEmpty	= 492Dh  ;***

_JError		= 44D7h
_JErrorNo	= 4000h
_JForceCmdNoChar	= 4027h
_JForceGraphKey	= 5005h
_JForceGraphNoKey	= 5002h

_KeyToString	= 45CAh

_LCD_BUSY	= 4051h  ;***
_LCD_DRIVERON	equ 4978h  ;***
_LdHLind	equ 4009h  ;***
_LineCmd	= 48ACh
_LnX		= 40ABh
_Load_SFont	= 4783h
_LoadCIndPaged	= 501Dh
_LoadDEIndPaged	= 501Ah
_LoadPattern	= 4CB1h
_LogX		= 40AEh

_Max		= 4057h
_MemChk		= 42E5h
_MemClear	= 4C30h
_MemSet		= 4C33h
_Min		= 4054h
_Minus1		= 406Ch
_Mov10B		= 415Ch
_Mov18B		= 47DAh
_Mov7B		= 4168h
_Mov8B		= 4165h
_Mov9B		= 415Fh
_Mov9OP1OP2	= 417Dh
_Mov9OP2Cp	= 410Bh
_Mov9ToOP1	= 417Ah
_Mov9ToOP2	= 4180h
_MovFrOP1	= 4183h

_NewLine	= 452Eh  ;***
_NZIf83Plus	= 50E0h  ; v. 1.13+ only

_OneVar		= 4BA3h
_OP1ExOP2	= 421Fh
_OP1ExOP3	= 4219h
_OP1ExOP4	= 421Ch
_OP1ExOP5	= 420Dh
_OP1ExOP6	= 4210h
_OP1ExpToDec	= 4252h
_OP1Set0	= 41BFh
_OP1Set1	= 419Bh
_OP1Set2	= 41A7h
_OP1Set3	= 41A1h
_OP1Set4	= 419Eh
_OP1ToOP2	= 412Fh
_OP1ToOP3	= 4123h
_OP1ToOP4	= 4117h
_OP1ToOP5	= 4153h
_OP1ToOP6	= 4150h
_OP2ExOP4	= 4213h
_OP2ExOP5	= 4216h
_OP2ExOP6	= 4207h
_OP2Set0	= 41BCh
_OP2Set1	= 41ADh
_OP2Set2	= 41AAh
_OP2Set3	= 4198h
_OP2Set4	= 4195h
_OP2Set5	= 418Fh
_OP2Set60	= 4AB0h
_OP2Set8	= 418Ch
_OP2SetA	= 4192h
_OP2ToOP1	= 4156h
_OP2ToOP3	= 416Eh
_OP2ToOP4	= 411Ah
_OP2ToOP5	= 414Ah
_OP2ToOP6	= 414Dh
_OP3Set0	= 41B9h
_OP3Set1	= 4189h
_OP3Set2	= 41A4h
_OP3ToOP1	= 413Eh
_OP3ToOP2	= 4120h
_OP3ToOP4	= 4114h
_OP3ToOP5	= 4147h
_OP4Set0	= 41B6h
_OP4Set1	= 4186h
_OP4ToOP1	= 4138h
_OP4ToOP2	= 411Dh
_OP4ToOP3	= 4171h
_OP4ToOP5	= 4144h
_OP4ToOP6	= 4177h
_OP5ExOP6	= 420Ah
_OP5Set0	= 41B3h
_OP5ToOP1	= 413Bh
_OP5ToOP2	= 4126h
_OP5ToOP3	= 4174h
_OP5ToOP4	= 412Ch
_OP5ToOP6	= 4129h
_OP6ToOP1	= 4135h
_OP6ToOP2	= 4132h
_OP6ToOP5	= 4141h
_OutputExpr	= 4BB2h

_PagedGet	= 5023h
_ParseInp	= 4A9Bh
_PDspGrph	= 48A3h
_PixelTest	= 48B5h
_Plus1		= 4069h
_PointCmd	= 48B2h
_PointOn	= 4C39h
_PopMCplxO1	= 436Fh  ;***
_PopOP1		= 437Eh
_PopOP3		= 437Bh
_PopOP5		= 4378h
_PopReal	= 4393h
_PopRealO1	= 4390h
_PopRealO2	= 438Dh
_PopRealO3	= 438Ah
_PopRealO4	= 4387h
_PopRealO5	= 4384h
_PopRealO6	= 4381h
_PosNo0Int	= 422Eh
_PToR		= 40F3h
_PushMCplxO1	= 43CFh  ;***
_PushMCplxO3	= 43C6h  ;***
_PushOP1	= 43C9h
_PushOP3	= 43C3h
_PushOP5	= 43C0h
_PushReal	= 43BDh
_PushRealO1	= 43BAh
_PushRealO2	= 43B7h
_PushRealO3	= 43B4h
_PushRealO4	= 43B1h
_PushRealO5	= 43AEh
_PushRealO6	= 43ABh
_PutAway	= 4039h  ;***
_PutC		= 4504h
_PutMap		= 4501h
_PutPS		= 4510h
_PutPSB		= 450dh  ;***
_PutS		= 450Ah
_PutTokString	= 4960h
_PutToL		= 4645h
_PutToMat	= 461Eh

_RandInit	= 4B7Fh
_Random		= 4B79h
_Rcl_StatVar	= 42DCh
_RclAns		= 4AD7h
_RclGDB2	= 47D1h
_RclN		= 4ADDh
_RclSysTok	= 4AE6h
_RclVarSym	= 4AE3h
_RclX		= 4AE0h
_RclY		= 4ADAh
_Rec1stByte	= 4EFAh
_Rec1stByteNC	= 4EFDh
_RecAByteIO	= 4F03h
_RedimMat	= 4A26h
_Regraph	= 488Eh
_ReleaseBuffer	= 4771h  ;***
_ReloadAppEntryVecs	= 4C36h
_RestoreDisp	= 4870h
_RName		= 427Fh
_RndGuard	= 409Fh
_RnFx		= 40A2h
_Round		= 40A8h
_RToD		= 4078h
_RToP		= 40F6h
_RunIndicOff	= 4570h
_RunIndicOn	= 456Dh

_SaveDisp	= 4C7Bh
_SendAByte	= 4EE5h
_SendVarCmd	= 4A14h ;***
_SetAllPlots	= 4FCCh
_SetExSpeed 	= 50BFh ; v. 1.13+ only
_SetFuncM	= 4840h
_SetNorm_Vals	= 49FCh
_SetParM	= 4849h
_SetPolM	= 4846h
_SetSeqM	= 4843h
_SetTblGraphDraw	= 4C00h
_SetupPagedPtr	= 5020h
_SetXXOP1	= 478Ch
_SetXXOP2	= 478Fh
_SetXXXXOP2	= 4792h
_SFont_Len	= 4786h
_Sin		= 40BDh
_SinCosRad	= 40BAh
_SinH		= 40CFh
_SinHCosH	= 40C6h
_SqRoot		= 409Ch
_SrchVLstDn	= 4F12h
_SrchVLstUp	= 4F0Fh
_SStringLength	= 4CB4h
_StMatEl	= 4AE9h
_StoAns		= 4ABFh
_StoGDB2	= 47CEh
_StoN		= 4ACBh
_StoOther	= 4AD4h
_StoR		= 4AC5h
_StoRand	= 4B7Ch
_StoSysTok	= 4ABCh
_StoT		= 4ACEh
_StoTheta	= 4AC2h
_StoX		= 4AD1h
_StoY		= 4AC8h
_StrCopy	= 44E3h
_StrLength	= 4C3Fh   

_Tan		= 40C3h
_TanH		= 40C9h
_TanLnF		= 48BBh
_TenX		= 40B7h
_ThetaName	= 427Ch
_ThreeExec	= 4675h
_Times2		= 4066h
_TimesPt5	= 407Eh
_TName		= 428Eh
_ToFrac		= 4657h
_Trunc		= 4060h
_TwoVarSet	= 50D4h  ; v. 1.13+ only

_UCLineS	= 4795h
_UnLineCmd	= 48AFh
_UnOPExec	= 4672h

_VertCmd	= 48A9h
_VPutMap	= 455Eh
_VPutS		= 4561h
_VPutSN		= 4564h
_VtoWHLDE	= 47FBh


_XftoI		= 4804h
_Xitof		= 47FEh
_XName		= 4288h
_XRootY		= 479Eh

_YftoI		= 4801h
_YName		= 428Bh
_YToX		= 47A1h

_Zero16D	= 41B0h
_ZeroOP		= 41CEh
_ZeroOP1	= 41C5h
_ZeroOP2	= 41C8h
_ZeroOP3	= 41CBh
_ZmDecml	= 484Fh
_ZmFit		= 485Bh
_ZmInt		= 484Ch
_ZmPrev		= 4852h
_ZmSquare	= 485Eh
_ZmStats	= 47A4h
_ZmTrig		= 4861h
_ZmUsr		= 4855h
_ZooDefault	= 4867h
;
;======================================================================
;           System-defined RAM Variable Address Equates
;======================================================================
;

ramStart             =  8000h
appData              =  8000h
ramCode              =  8100h
ramCodeEnd           =  822Fh
baseAppBrTab         =  8230h
bootTemp             =  8251h
appSearchPage        =  82A3h
tempSwapArea         =  82A5h
appID                =  838Dh
ramReturnData        =  83EDh
arcInfo              =  83EEh
savedArcInfo         =  8406h
appInfo              =  8432h
appBank_jump         =  843Ch
appPage              =  843Eh
kbdScanCode          =  843Fh
kbdKey               =  8444h
kbdGetKy             =  8445h
keyExtend            =  8446h
contrast             =  8447h
apdSubTimer          =  8448h
apdTimer             =  8449h
curTime              =  844Ah
curRow               =  844Bh
curCol               =  844Ch
curOffset            =  844Dh
curUnder             =  844Eh
curY                 =  844Fh
curType              =  8450h
curXRow              =  8451h
prevDData            =  8452h
lFont_record         =  845Ah
sFont_record         =  8462h
tokVarPtr            =  846Ah
tokLen               =  846Ch
indicMem             =  846Eh
indicCounter         =  8476h
indicBusy            =  8477h
OP1                  =  8478h
OP1M                 =  847Ah
OP2                  =  8483h
OP2M                 =  8485h
OP2EXT               =  848Ch
OP3                  =  848Eh
OP3M                 =  8490h
OP4                  =  8499h
OP4M                 =  849Bh
OP5                  =  84A4h
OP5M                 =  84A6h
OP6                  =  84AFh
OP6M                 =  84B1h
OP6EXT               =  84B8h
progToEdit           =  84BFh
nameBuff             =  84C7h
equ_edit_save        =  84D2h
iMathPtr1            =  84D3h
iMathPtr2            =  84D5h
iMathPtr3            =  84D7h
iMathPtr4            =  84D9h
iMathPtr5            =  84DBh
chkDelPtr1           =  84DDh
chkDelPtr2           =  84DFh
insDelPtr            =  84E1h
upDownPtr            =  84E3h
fOutDat              =  84E5h
asm_data_ptr1        =  84EBh
asm_data_ptr2        =  84EDh
asm_sym_ptr1         =  84EFh
asm_sym_ptr2         =  84F1h
asm_ram              =  84F3h
asm_ind_call         =  8507h
textShadow           =  8508h
textShadCur          =  8588h
textShadTop          =  858Ah
textShadAlph         =  858Bh
textShadIns          =  858Ch
cxMain               =  858Dh
cxPPutAway           =  858Fh
cxPutAway            =  8591h
cxRedisp             =  8593h
cxErrorEP            =  8595h
cxSizeWind           =  8597h
cxPage               =  8599h
cxCurApp             =  859Ah
cxPrev               =  859Bh
monQH                =  85AAh
monQT                =  85ABh
monQueue             =  85ACh
onSP                 =  85BCh
promptRow            =  85C0h
promptCol            =  85C1h
promptIns            =  85C2h
promptShift          =  85C3h
promptRet            =  85C4h
promptValid          =  85C6h
promptTop            =  85C8h
promptCursor         =  85CAh
promptTail           =  85CCh
promptBtm            =  85CEh
varType              =  85D0h
varCurrent           =  85D1h
varClass             =  85D9h
menuActive           =  85DCh
menuAppDepth         =  85DDh
MenuCurrent          =  85DEh
ProgCurrent          =  85E8h
userMenuSA           =  85FEh
ioPrompt             =  865Fh
dImageWidth          =  8660h
RectFillPHeight      equ  8660h
RectFillPWidth       equ  8661h
RectFillPattern      equ  8662h
ioFlag               =  8670h
sndRecState          =  8672h
ioErrState           =  8673h
header               =  8674h
ioData               =  867Dh
ioNewData            =  8689h
bakHeader            =  868Bh
penCol               =  86D7h
penRow               =  86D8h
rclQueue             =  86D9h
rclQueueEnd          =  86DBh
errNo                =  86DDh
errSP                =  86DEh
errOffset            =  86E0h
saveSScreen          =  86ECh
bstCounter           =  89EEh
flags                =  89F0h
statVars             =  8A3Ah
anovaf_vars          =  8C17h
infVars              =  8C4Dh
curGStyle            =  8D17h
curGY                =  8D18h
curGX                =  8D19h
curGY2               =  8D1Ah
curGX2               =  8D1Bh
freeSaveY            =  8D1Ch
freeSaveX            =  8D1Dh
XOffset              =  8DA1h
YOffset              =  8DA2h
lcdTallP             =  8DA3h
pixWideP             =  8DA4h
pixWide_m_1          =  8DA5h
pixWide_m_2          =  8DA6h
lastEntryPTR         =  8DA7h
lastEntryStk         =  8DA9h
numLastEntries       =  8E29h
currLastEntry        =  8E2Ah
curInc               =  8E67h
uXmin                =  8E7Eh
uXmax                =  8E87h
uXscl                =  8E90h
uYmin                =  8E99h
uYmax                =  8EA2h
uYscl                =  8EABh
uThetMin             =  8EB4h
uThetMax             =  8EBDh
uThetStep            =  8EC6h
uTmin                =  8ECFh
uTmax                =  8ED8h
uTStep               =  8EE1h
uPlotStart           =  8EEAh
unMax                =  8EF3h
uu0                  =  8EFCh
uv0                  =  8F05h
unMin                =  8F0Eh
uu02                 =  8F17h
uv02                 =  8F20h
uw0                  =  8F29h
uPlotStep            =  8F32h
uXres                =  8F3Bh
uw02                 =  8F44h
Xmin                 =  8F50h
Xmax                 =  8F59h
Xscl                 =  8F62h
Ymin                 =  8F6Bh
Ymax                 =  8F74h
Yscl                 =  8F7Dh
ThetaMin             =  8F86h
ThetaMax             =  8F8Fh
ThetaStep            =  8F98h
TminPar              =  8FA1h
TmaxPar              =  8FAAh
Tstep                =  8FB3h
PlotStart            =  8FBCh
nMax                 =  8FC5h
u0                   =  8FCEh
v0                   =  8FD7h
nMin                 =  8FE0h
u02                  =  8FE9h
v02                  =  8FF2h
w0                   =  8FFBh
PlotStep             =  9004h
XresO                =  900Dh
w02                  =  9016h
un1                  =  901Fh
un2                  =  9028h
vn1                  =  9031h
vn2                  =  903Ah
wn1                  =  9043h
wn2                  =  904Ch
fin_N                =  9055h
fin_I                =  905Eh
fin_PV               =  9067h
fin_PMT              =  9070h
fin_FV               =  9079h
fin_PY               =  9082h
fin_CY               =  908Bh
cal_N                =  9094h
cal_I                =  909Dh
cal_PV               =  90A6h
cal_PMT              =  90AFh
cal_FV               =  90B8h
cal_PY               =  90C1h
smallEditRAM         =  90D3h
XFact                =  913Fh
YFact                =  9148h
Xres_int             =  9151h
deltaX               =  9152h
deltaY               =  915Bh
shortX               =  9164h
shortY               =  916Dh
lower                =  9176h
upper                =  917Fh
XOutSym              =  918Ch
XOutDat              =  918Eh
YOutSym              =  9190h
YOutDat              =  9192h
inputSym             =  9194h
inputDat             =  9196h
prevData             =  9198h
TblMin               =  92B3h
TblStep              =  92BCh
P1Type		     =  92C9h
SavX1List	     =  92CAh
SavY1List	     =  92CFh
SavF1List	     =  92D4h
P1FrqOnOff	     =  92D9h
P2Type		     =  92DAh
SavX2List	     =  92DBh
SavY2List	     =  92E0h
SavF2List	     =  92E5h
P2FrqOnOff	     =  92EAh
P3Type		     =  92EBh	
SavX3List	     =  92ECh
SavY3List	     =  92F1h
SavF3List	     =  92F6h
P3FrqOnOff	     =  92FBh
plotSScreen          =  9340h
seed1                =  9640h
seed2                =  9649h
cmdShadow            =  966Eh
cmdShadCur           =  96EEh
cmdShadAlph          =  96F0h
cmdShadIns           =  96F1h
cmdCursor            =  96F2h
editTop              =  96F4h
editCursor           =  96F6h
editTail             =  96F8h
editBtm              =  96FAh
editSym              =  9706h
editDat              =  9708h
winTop               =  97A5h
winBtm               =  97A6h
winLeftEdge          =  97A7h
winLeft              =  97A8h
winAbove             =  97AAh
winRow               =  97ACh
winCol               =  97AEh
fmtDigits            =  97B0h
fmtString            =  97B1h
fmtConv              =  97F2h
fmtLeft              =  9804h
fmtIndex             =  9806h
fmtMatSym            =  9808h
fmtMatMem            =  980Ah
EQS                  =  980Ch
tSymPtr1             =  9818h
tSymPtr2             =  981Ah
chkDelPtr3           =  981Ch
chkDelPtr4           =  981Eh
tempMem              =  9820h
fpBase               =  9822h
FPS                  =  9824h
OPBase               =  9826h
OPS                  =  9828h
pTempCnt             =  982Ah
cleanTmp             =  982Ch
pTemp                =  982Eh
progPtr              =  9830h
newDataPtr           =  9832h
pagedCount           =  9834h
pagedPN              =  9835h
pagedGetPtr          =  9836h
pagedPutPtr          =  9838h
pagedBuf             =  983Ah
appErr1              =  984Dh
appErr2              =  985Ah
flashByte1           =  9867h
flashByte2           =  9868h
freeArcBlock         =  9869h
arcPage              =  986Bh
arcPtr               =  986Ch
appRawKeyHandle      =  9870h
appBackUpScreen      =  9872h
customHeight         =  9B72h
localLanguage        =  9B73h
cursorHookPtr        =  9B7Ch
rawKeyHookPtr        =  9B84h
getKeyHookPtr        =  9B88h
fontHookPtr          =  9B9Ch
restartClr           =  9BD0h
localTokStr          =  9D65h
keyForStr            =  9D76h
keyToStrRam          =  9D77h
sedMonSp             =  9D88h
bpSave               =  9D8Ah
userMem              =  9D95h
symTable             =  0FE66h

;SYMBOLIC_DEBUG_END
;
;======================================================================
;		Language localization equates
;======================================================================
LANG_NEUTRAL                       equ   00h
LANG_DANISH                        equ   06h
LANG_DUTCH                         equ   13h
LANG_ENGLISH                       equ   09h
LANG_FINNISH                       equ   0bh
LANG_FRENCH                        equ   0ch
LANG_GERMAN                        equ   07h
LANG_HUNGARIAN                     equ   0eh
LANG_ITALIAN                       equ   10h
LANG_NORWEGIAN                     equ   14h
LANG_POLISH                        equ   15h
LANG_PORTUGUESE                    equ   16h
LANG_SPANISH                       equ   0ah
LANG_SWEDISH                       equ   1dh

SUBLANG_NEUTRAL                    equ   00h   ; language neutral
SUBLANG_DUTCH                    equ   01h   ; Dutch
SUBLANG_ENGLISH_US               equ   01h   ; English (USA)
SUBLANG_FRENCH                   equ   01h   ; French
SUBLANG_GERMAN                   equ   01h   ; German
SUBLANG_ITALIAN                  equ   01h   ; Italian
SUBLANG_NORWEGIAN_BOKMAL         equ   01h   ; Norwegian (Bokmal)
SUBLANG_PORTUGUESE               equ   02h   ; Portuguese
SUBLANG_SPANISH                  equ   01h   ; Spanish (Castilian)


;======================================================================
;           Error handler equates/macros 
;======================================================================
;
APP_PUSH_ERRORH =	59h
APP_POP_ERRORH	=	5Ch
;
AppOnErr   macro   handaddr
	LD	HL,handaddr		;hl -> stack handler address
	CALL	APP_PUSH_ERRORH 	;establish error handler context
	.endm

AppOffErr  macro
	CALL	APP_POP_ERRORH		;remove error handler context
	.endm
;
;======================================================================
;           System and State Flags
;======================================================================
;

trigFlags	=  0		;Trigonometry mode settings
trigDeg 	=    2	; 1=degrees, 0=radians

kbdFlags	=  0		;Keyboard scan
kbdSCR		=    3	; 1=scan code ready
kbdKeyPress	=    4	; 1=key has been pressed

doneFlags	=  0		;display "Done"
donePrgm	=    5	; 1=display "Done" after prgm

ioDelFlag	=  0
inDelete	=    0	;1 = DELETE SCREEN 

;----------------------------------------------------------------------
editFlags	=  1
editOpen	=    2	; 1=edit buffer is open

monFlags	=  1		;monitor flags
monAbandon	=    4	; 1=don't start any long process
				; in put away (#715)
;----------------------------------------------------------------------
plotFlags	=  2		;plot generation flags
plotLoc 	=    1	; 0=bkup & display, 1=display only
plotDisp	=    2	; 1=plot is in display, 0=text in display


grfModeFlags	=  2		;graph mode settings
grfFuncM	=    4	; 1=function graph
grfPolarM	=    5	; 1=polar graph
grfParamM	=    6	; 1=parametric graph
grfRecurM       =    7        ; 1=RECURSION graph

graphFlags	=  3
graphDraw	=    0	; 0=graph is valid, 1=redraw graph

grfDBFlags	=  4
grfDot		=    0	; 0=line, 1=dot
grfSimul	=    1	; 0=sequential, 1=simultaneous
grfGrid 	=    2	; 0=no grid, 1=grid
grfPolar	=    3	; 0=rectangular, 1=polar coordinates
grfNoCoord	=    4	; 0=display coordinates, 1=off
grfNoAxis	=    5	; 0=axis, 1=no axis
grfLabel	=    6	; 0=off, 1=axis label

textFlags	=  5		;Text output flags
textEraseBelow	=    1	; 1=erase line below small char
textScrolled	=    2	; 1=screen scrolled
textInverse	=    3	; 1=display inverse bit-map
textInsMode	=    4	; 0=overstrike, 1=insert mode

ParsFlag2	=  7		;PARSER flags
numOP1		=    0	; 1=RESULT IN OP1, 0=NO RESULT

newDispF        =  8		;Derivative mode flags
preClrForMode   =    0	; 1=HELP BLINK ON MODE SCREEN

apdFlags	=  8		;Automatic power-down
apdAble 	=    2	; 1=APD enabled
apdRunning	=    3	; 1=APD clock running


web_err_mask    =  60h

onFlags 	=  9		;on key flags
onRunning	=  3		; 1 = calculator is running
onInterrupt	=    4	; 1=on key interrupt request

statFlags	=  9		;statistics flags
statsValid	=    6	; 1=stats are valid
statANSDISP	=  7		; 1=display stat results


fmtFlags	=  10		;numeric format flags
fmtExponent	=    0	; 1=show exponent, 0=no exponent
fmtEng		=    1	; 1=engineering notion, 0=scientific
fmtHex		=    2	; 1=hexadecimal
fmtOct		=    3	; 1=octal
fmtBin		=    4	; 1=binary
;
numMode         =  10
fmtReal         =    5
fmtRect         =    6
fmtPolar        =    7

realMode        =    5
rectMode        =    6
polarMode       =    7
;					;   if Hex and Oct both = 1
;					;   then Bin=0 means >Frac
;					;	 Bin=1 means >DMS
fmtBaseMask     =  00011100B		; mask to base flags
fmtBaseShift    =  2			; offset to base flags
;
;       CHECK IF THESE ARE USED BY NUMFORM,
;
;               =  6
;               =  7

fmtOverride	=  11		;copy of fmtFlags with conversion override

fmtEditFlags	=  12		;numeric editing flags
fmtEdit 	=    0	; 1=format number for editing

curFlags	=  12		;Cursor
curAble 	=    2	; 1=cursor flash is enabled
curOn		=    3	; 1=cursor is showing
curLock 	=    4	; 1=cursor is locked off

appFlags	=  13		;application flags
appWantIntrpt	=    0		; 1=want ON key interrupts
appTextSave	=    1		; 1=save characters in textShadow
appAutoScroll	=    2		; 1=auto-scroll text on last line
appMenus	=    3		; 1=process keys that bring up menus
					; 0=check Lock menu flag
appLockMenus	=    4		; 1=ignore menu keys
					; 0=switch to home screen and bring up menu
appCurGraphic	=    5		; 1=graphic cursor
appCurWord	=    6		; 1=text cursor covers entire word
appExit 	=    7		; 1=application handles [EXIT] key itself

appWantIntrptF	=	1<<appWantIntrpt
appTextSaveF	=	1<<appTextSave
appAutoScrollF	=	1<<appAutoScroll
appMenusF	=	1<<appMenus
appLockMenusF	=	1<<appLockMenus
appCurGraphicF	=	1<<appCurGraphic
appCurWordF	=	1<<appCurWord
appExitF	=	1<<appExit


seqFlags        =  15              ; Sequential Graph flags
webMode         =    0             ; 0 = NORMAL SEQ MODE, 1 = WEB MODE
webVert         =    1             ;
sequv           =    2             ; U vs V
seqvw           =    3             ; V vs W
sequw           =    4             ; U vs W


promptFlags	=  17		;prompt line flags
promptEdit	=    0	; 1=editing in prompt buffer

indicFlags	=  18		;Indicator flags
indicRun	=    0	; 1=run indicator ON
indicInUse	=    1	; indicator save area in use=1, free=0
indicOnly	=    2	; interrupt handler only checks run indicator

shiftFlags	=  18		;[2nd] and [ALPHA] flags
shift2nd	=    3	; 1=[2nd] has been pressed
shiftAlpha	=    4	; 1=[ALPHA] has been pressed
shiftLwrAlph	=    5	; 1=lower case, 0=upper case
shiftALock	=    6	; 1=alpha lock has been pressed
shiftKeepAlph	=    7	; 1=cannot cancel alpha shift


tblFlags        =  19		;table flags.
autoFill        =    4	; 1=prompt, 0=fillAuto
autoCalc        =    5	; 1=prompt, 0=CalcAuto
reTable         =    6	; 0=table is ok, 1=must recompute table.

sGrFlags	=  20
grfSplit        =    0	; 1=Split Graph, 0=Normal
vertSplit       =    1	; 1=Vertical (left-right) Split
grfSChanged     =    2	; 1=Graph just changed Split <-> normal
grfSplitOverride =   3	; 1 = ignore graph split flag if set
write_on_graph  =    4	; 1 = TEXT OR = WRITING TO GRAPH SCREEN
g_style_active  =    5	; 1 = GRAPH STYLES ARE ENABLED, USE THEM
cmp_mod_box     =    6	; 1 = DOING MOD BOX PLOT COMPUTATION
textWrite       =    7	; 1 = Small font writes to buffer
							; 0 = Small font writes to display
;

newIndicFlags   =  21
extraIndic      =    0
saIndic         =    1

newFlags2	    =  22
noRestores	    =  5

smartFlags      =  23
;
;----------------------------------------------------------------------
; Note: Fix these Equates if smartFlags are moved                            
;----------------------------------------------------------------------
;
smarter_mask    =  3
smarter_test    =  1
smartGraph      =  0
smartGraph_inv  =  1                                                 

more_Flags	= 26

No_Del_Stat	= 2
;----------------------------------------------------------------------
;           Available for ASM programming
;----------------------------------------------------------------------

asm_Flag1       =  33          ; ASM CODING
asm_Flag2       =  34          ; ASM CODING
asm_Flag3       =  35          ; ASM CODING

;----------------------------------------------------------------------
;
getSendFlg	=  36
comFailed	=  1	      ; 1 = Get/Send Communication Failed
;
appLwrCaseFlag  =  36
lwrCaseActive   =  3
;
apiFlg3		=  42 
;
apiFlg4		=  43
fullScrnDraw	=    2	; DRAW INTO LAST ROW/COL OF SCREEN

groupFlags	=  38
inGroup		=  1		;1 = IN GROUP CONTEXT


xapFlag0        =  46      ; external app flags
xapFlag1        =  47
xapFlag2        =  48
xapFlag3        =  49

fontFlags	=  50
fracDrawLFont	=    2
fracTallLFont	=    3
customFont	=    7  

plotFlag3	=  60  
bufferOnly	=    0
useFastCirc	=    4

;
varTypeMask	=  1FH             
varGraphRef	=    6

;
;======================================================================
;           Character Font equates  
;======================================================================
;
; Large font equates
;
LrecurN         =     001h
LrecurU 	=	002h
LrecurV 	=	003h
LrecurW		=	004h
Lconvert	=	005h
LsqUp		=	006h
LsqDown		=	007h
Lintegral	=	008h
Lcross		=	009h
LboxIcon 	=     00Ah
LcrossIcon 	=     00Bh
LdotIcon 	=     00Ch
LsubT   	=     00Dh		;small capital T for parametric mode.
LcubeR  	=     00Eh		;slightly different 3 for cubed root.
LhexF		=	00Fh
Lroot		=	010h
Linverse	=	011h
Lsquare		=	012h
Langle		=	013h
Ldegree		=	014h
Lradian		=	015h
Ltranspose	=	016h
LLE		=	017h
LNE		=	018h
LGE		=	019h
Lneg		=	01Ah
Lexponent	=	01Bh
Lstore		=	01Ch
Lten		=	01Dh
LupArrow	=	01Eh
LdownArrow	=	01Fh
Lspace		=	020h
Lexclam		=	021h
Lquote		=	022h
Lpound		=	023h
Lfourth		=     024h
Lpercent	=	025h
Lampersand	=	026h
Lapostrophe	=	027h
LlParen		=	028h
LrParen		=	029h
Lasterisk	=	02Ah
LplusSign	=	02Bh
Lcomma		=	02Ch
Ldash		=	02Dh
Lperiod		=	02Eh
Lslash		=	02Fh
L0		=	030h
L1		=	031h
L2		=	032h
L3		=	033h
L4		=	034h
L5		=	035h
L6		=	036h
L7		=	037h
L8		=	038h
L9		=	039h
Lcolon		=	03Ah
Lsemicolon	=	03Bh
LLT		=	03Ch
LEQ		=	03Dh
LGT		=	03Eh
Lquestion	=	03Fh
LatSign		=	040h
LcapA	=	041h
LcapB	=	042h
LcapC	=	043h
LcapD	=	044h
LcapE	=	045h
LcapF	=	046h
LcapG	=	047h
LcapH	=	048h
LcapI	=	049h
LcapJ	=	04Ah
LcapK	=	04Bh
LcapL	=	04Ch
LcapM	=	04Dh
LcapN	=	04Eh
LcapO	=	04Fh
LcapP	=	050h
LcapQ	=	051h
LcapR	=	052h
LcapS	=	053h
LcapT	=	054h
LcapU	=	055h
LcapV	=	056h
LcapW	=	057h
LcapX	=	058h
LcapY	=	059h
LcapZ	=	05Ah
Ltheta  =     05Bh
Lbackslash	=	05Ch
LrBrack		=	05Dh
Lcaret		=	05Eh
Lunderscore	=	05Fh
Lbackquote	=	060h
La	=	061h
Lb	=	062h
Lc	=	063h
Ld	=	064h
Le	=	065h
Lf	=	066h
Lg	=	067h
Lh	=	068h
Li	=	069h
Lj	=	06Ah
Lk	=	06Bh
Ll	=	06Ch
Lm	=	06Dh
Ln	=	06Eh
Lo	=	06Fh
Lp	=	070h
Lq	=	071h
Lr	=	072h
Ls	=	073h
Lt	=	074h
Lu	=	075h
Lv	=	076h
Lw	=	077h
Lx	=	078h
Ly	=	079h
Lz	=	07Ah
LlBrace	=	07Bh
Lbar	=	07Ch
LrBrace	=	07Dh
Ltilde	=	07Eh
LinvEQ	=	07Fh
Lsub0	=	080h
Lsub1	=	081h
Lsub2	=	082h
Lsub3	=	083h
Lsub4	=	084h
Lsub5	=	085h
Lsub6	=	086h
Lsub7	=	087h
Lsub8	=	088h
Lsub9	=	089h
LcapAAcute	=	08Ah
LcapAGrave	=	08Bh
LcapACaret	=	08Ch
LcapADier	=	08Dh
LaAcute		=	08Eh
LaGrave		=	08Fh
LaCaret		=	090h
LaDier		=	091h
LcapEAcute	=	092h
LcapEGrave	=	093h
LcapECaret	=	094h
LcapEDier	=	095h
LeAcute		=	096h
LeGrave		=	097h
LeCaret		=	098h
LeDier		=	099h
LcapIAcute	=	09Ah
LcapIGrave	=	09Bh
LcapICaret	=	09Ch
LcapIDier	=	09Dh
LiAcute		=	09Eh
LiGrave		=	09Fh
LiCaret		=	0A0h
LiDier		=	0A1h
LcapOAcute	=	0A2h
LcapOGrave	=	0A3h
LcapOCaret	=	0A4h
LcapODier	=	0A5h
LoAcute		=	0A6h
LoGrave		=	0A7h
LoCaret		=	0A8h
LoDier		=	0A9h
LcapUAcute	=	0AAh
LcapUGrave	=	0ABh
LcapUCaret	=	0ACh
LcapUDier	=	0ADh
LuAcute		=	0AEh
LuGrave		=	0AFh
LuCaret		=	0B0h
LuDier		=	0B1h
LcapCCed	=	0B2h
LcCed		=	0B3h
LcapNTilde	=	0B4h
LnTilde		=	0B5h
Laccent		=	0B6h
Lgrave		=	0B7h
Ldieresis	=	0B8h
LquesDown	=	0B9h
LexclamDown	=	0BAh
Lalpha		=	0BBh
Lbeta		=	0BCh
Lgamma		=	0BDh
LcapDelta	=	0BEh
Ldelta		=	0BFh
Lepsilon	=	0C0h
LlBrack 	=     0C1h
Llambda		=	0C2h
Lmu		=	0C3h
Lpi		=	0C4h
Lrho		=	0C5h
LcapSigma	=	0C6h
Lsigma		=	0C7h
Ltau		=	0C8h
Lphi		=	0C9h
LcapOmega	=	0CAh
LxMean		=	0CBh
LyMean		=	0CCh
LsupX		=	0CDh
Lellipsis	=	0CEh
Lleft		=	0CFh
Lblock		=	0D0h
Lper		=	0D1h
Lhyphen		=	0D2h
Larea		=	0D3h
Ltemp		=	0D4h
Lcube		=	0D5h
Lenter		=	0D6h
LimagI		=	0D7h
Lphat		=	0D8h
Lchi		=	0D9h
LstatF		=     0DAh
Llne		=	0DBh
LlistL		=	0DCh
LfinanN 	=	0DDh
L2_r_paren 	=	0DEh
LblockArrow     =     0DFh
LcurO   	=     0E0h
LcurO2  	=     0E1h
LcurOcapA       =     0E2h
LcurOa  	=     0E3h
LcurI   	=     0E4h
LcurI2  	=     0E5h
LcurIcapA       =     0E6h
LcurIa  	=     0E7h
LGline          =     0E8h    ; = 0
LGthick         =     0E9h    ; = 1
LGabove         =     0EAh    ; = 2
LGbelow         =     0EBh    ; = 3
LGpath          =     0ECh    ; = 4
LGanimate       =     0EDh    ; = 5
LGdot           =     0EEh    ; = 6
LUpBlk          =     0EFh    ;Up arrow and Block in solver
LDnBlk          =     0F0h    ;Down arrow and Block in solver
LcurFull        =     0F1h    ;note: must be last char (PutMap checks)

;Small font equates
;
SrecurN         equ     001h		
SrecurU		equ	002h
SrecurV		equ	003h
SrecurW		equ	004h
Sconvert	equ	005h
SFourSpaces 	equ 	006h
SsqDown		equ	007h
Sintegral	equ	008h
Scross		equ	009h
SboxIcon 	equ	00Ah
ScrossIcon 	equ	00Bh
SdotIcon 	equ    	00Ch
SsubT   	equ     00Dh
ScubeR  	equ     00Eh
ShexF		equ	00Fh
Sroot		equ	010h
Sinverse	equ	011h
Ssquare		equ	012h
Sangle		equ	013h
Sdegree		equ	014h
Sradian		equ	015h
Stranspose	equ	016h
SLE		equ	017h
SNE		equ	018h
SGE		equ	019h
Sneg		equ	01Ah
Sexponent	equ	01Bh
Sstore		equ	01Ch
Sten		equ	01Dh
SupArrow	equ	01Eh
SdownArrow	equ	01Fh
Sspace		equ	020h
Sexclam		equ	021h
Squote		equ	022h
Spound		equ	023h
Sdollar		equ	024h
Spercent	equ	025h
Sampersand	equ	026h
Sapostrophe	equ	027h
SlParen		equ	028h
SrParen		equ	029h
Sasterisk	equ	02Ah
SplusSign	equ	02Bh
Scomma		equ	02Ch
Sdash		equ	02Dh
Speriod		equ	02Eh
Sslash		equ	02Fh
S0		equ	030h
S1		equ	031h
S2		equ	032h
S3		equ	033h
S4		equ	034h
S5		equ	035h
S6		equ	036h
S7		equ	037h
S8		equ	038h
S9		equ	039h
Scolon		equ	03Ah
Ssemicolon	equ	03Bh
SLT		equ	03Ch
SEQ		equ	03Dh
SGT		equ	03Eh
Squestion	equ	03Fh
SatSign		equ	040h
ScapA		equ	041h
ScapB		equ	042h
ScapC		equ	043h
ScapD		equ	044h
ScapE		equ	045h
ScapF		equ	046h
ScapG		equ	047h
ScapH		equ	048h
ScapI		equ	049h
ScapJ		equ	04Ah
ScapK		equ	04Bh
ScapL		equ	04Ch
ScapM		equ	04Dh
ScapN		equ	04Eh
ScapO		equ	04Fh
ScapP		equ	050h
ScapQ		equ	051h
ScapR		equ	052h
ScapS		equ	053h
ScapT		equ	054h
ScapU		equ	055h
ScapV		equ	056h
ScapW		equ	057h
ScapX		equ	058h
ScapY		equ	059h
ScapZ		equ	05Ah
Stheta  	equ     05Bh
Sbackslash	equ	05Ch
SrBrack		equ	05Dh
Scaret		equ	05Eh
Sunderscore	equ	05Fh
Sbackquote	equ	060h
SmallA		equ	061h
SmallB		equ	062h
SmallC		equ	063h
SmallD		equ	064h
SmallE		equ	065h
SmallF		equ	066h
SmallG		equ	067h
SmallH		equ	068h
SmallI		equ	069h
SmallJ		equ	06Ah
SmallK		equ	06Bh
SmallL		equ	06Ch
SmallM		equ	06Dh
SmallN		equ	06Eh
SmallO		equ	06Fh
SmallP		equ	070h
SmallQ		equ	071h
SmallR		equ	072h
SmallS		equ	073h
SmallT		equ	074h
SmallU		equ	075h
SmallV		equ	076h
SmallW		equ	077h
SmallX		equ	078h
SmallY		equ	079h
SmallZ		equ	07Ah
SlBrace		equ	07Bh
Sbar		equ	07Ch
SrBrace		equ	07Dh
Stilde		equ	07Eh
SinvEQ		equ	07Fh
Ssub0		equ	080h
Ssub1		equ	081h
Ssub2		equ	082h
Ssub3		equ	083h
Ssub4		equ	084h
Ssub5		equ	085h
Ssub6		equ	086h
Ssub7		equ	087h
Ssub8		equ	088h
Ssub9		equ	089h
ScapAAcute	equ	08Ah
ScapAGrave	equ	08Bh
ScapACaret	equ	08Ch
ScapADier	equ	08Dh
SaAcute		equ	08Eh
SaGrave		equ	08Fh
SaCaret		equ	090h
SaDier		equ	091h
ScapEGrave	equ	092h
ScapEAcute	equ	093h
ScapECaret	equ	094h
ScapEDier	equ	095h
SeAcute		equ	096h
SeGrave		equ	097h
SeCaret		equ	098h
SeDier		equ	099h
ScapIAcute	equ	09Ah
ScapIGrave	equ	09Bh
ScapICaret	equ	09Ch
ScapIDier	equ	09Dh
SiAcute		equ	09Eh
SiGrave		equ	09Fh
SiCaret		equ	0A0h
SiDier		equ	0A1h
ScapOAcute	equ	0A2h
ScapOGrave	equ	0A3h
ScapOCaret	equ	0A4h
ScapODier	equ	0A5h
SoAcute		equ	0A6h
SoGrave		equ	0A7h
SoCaret		equ	0A8h
SoDier		equ	0A9h
ScapUAcute	equ	0AAh
ScapUGrave	equ	0ABh
ScapUCaret	equ	0ACh
ScapUDier	equ	0ADh
SuAcute		equ	0AEh
SuGrave		equ	0AFh
SuCaret		equ	0B0h
SuDier		equ	0B1h
ScapCCed	equ	0B2h
ScCed		equ	0B3h
ScapNTilde	equ	0B4h
SnTilde		equ	0B5h
Saccent		equ	0B6h
Sgrave		equ	0B7h
Sdieresis	equ	0B8h
SquesDown	equ	0B9h
SexclamDown	equ	0BAh
Salpha		equ	0BBh
Sbeta		equ	0BCh
Sgamma		equ	0BDh
ScapDelta	equ	0BEh
Sdelta		equ	0BFh
Sepsilon	equ	0C0h
SlBrack 	equ     0C1h
Slambda		equ	0C2h
Smu		equ	0C3h
Spi		equ	0C4h
Srho		equ	0C5h
ScapSigma	equ	0C6h
Ssigma		equ	0C7h
Stau		equ	0C8h
Sphi		equ	0C9h
ScapOmega	equ	0CAh
SxMean		equ	0CBh
SyMean		equ	0CCh
SsupX		equ	0CDh
Sellipsis	equ	0CEh
Sleft		equ	0CFh
Sblock		equ	0D0h
Sper		equ	0D1h
Shyphen		equ	0D2h
Sarea		equ	0D3h
Stemp		equ	0D4h
Scube		equ	0D5h
Senter		equ	0D6h
SimagI		equ	0D7h
Sphat		equ	0D8h
Schi		equ	0D9h
SstatF  	equ     0DAh
Slne		equ	0DBh
SlistL		equ	0DCh
SfinanN 	equ	0DDh
S2_r_paren 	equ	0DEh
SnarrowCapE     equ     0DFh
SListLock 	equ	0E0h
Sscatter1 	equ	0E1h                    
Sscatter2 	equ	0E2h                    
Sxyline1  	equ	0E3h                    
Sxyline2  	equ	0E4h
Sboxplot1 	equ	0E5h
Sboxplot2 	equ	0E6h
Shist1    	equ	0E7h
Shist2    	equ	0E8h
SmodBox1  	equ	0E9h
SmodBox2  	equ	0EAh
Snormal1  	equ	0EBh
Snormal2  	equ	0ECh
;
;======================================================================
;           Keypress Equates
;======================================================================
;           Keyboard key names
;
;
kRight          =       001h
kLeft           =       002h
kUp             =       003h
kDown           =       004h
kEnter          =       005h
kAlphaEnter     =       006h
kAlphaUp        =       007h
kAlphaDown      =       008h
kClear          =       009h
kDel            =       00Ah
kIns            =       00Bh
kRecall         =       00Ch
kLastEnt        =       00Dh
kBOL            =       00Eh
kEOL            =       00Fh
;
kSelAll         =       010h
kUnselAll       =       011h
kLtoTI82        =       012h
kBackup         =       013h
kRecieve        =       014h
kLnkQuit        =       015h
kTrans          =       016h
kRename         =       017h
kOverw          =       018h
kOmit           =       019h
kCont           =       01Ah
kSendID         =       01Bh
kSendSW         =       01Ch
kYes            =       01Dh
kNoWay          =       01Eh
kvSendType      =       01Fh
kOverWAll       =       020h
;
kNo             =       025h
kKReset         =       026h
kApp            =       027h
;
kDoug           =       028h
kListflag       =       029h
menuStart       =       02Bh
;
kAreYouSure     =       02Bh
kAppsMenu       =       02Ch
kPrgm           =       02Dh
kZoom           =       02Eh
kDraw           =       02Fh
kSPlot          =       030h
kStat           =       031h
kMath           =       032h
kTest           =       033h
kChar           =       034h
kVars           =       035h
kMem            =       036h
kMatrix         =       037h
kDist           =       038h
kAngle          =       039h
kList           =       03Ah
kCalc           =       03Bh
kFin            =       03Ch
;
menuEnd         =       kFin
;
kCatalog        =       03Eh
kInputDone      =       03Fh
kOff            =       kInputDone
;
kQuit           =       040h
appStart        =       kQuit

;
kLinkIO         =       041h
kMatrixEd       =       042h
kStatEd         =       043h
kGraph          =       044h
kMode           =       045h
kPrgmEd         =       046h        ; PROGRAM EDIT
kPrgmCr         =       047h        ; PROGRAM CREATE
kWindow         =       048h        ; RANGE EDITOR
kYequ           =       049h        ; =ATION EDITOR
kTable          =       04Ah        ; TABLE EDITOR
kTblSet         =       04Bh        ; TABLE SET
kChkRAM         =       04Ch        ; CHECK RAM
kDelMem         =       04Dh        ; DELETE MEM
kResetMem       =       04Eh        ; RESET MEM
kResetDef	=	  04Fh        ; RESET DEFAULT
kPrgmInput      =       050h        ; PROGRAM INPUT
kZFactEd        =       051h        ; ZOOM FACTOR EDITOR
kError          =       052h        ; ERROR
kSolveTVM       =       053h        ; TVM SOLVER
kSolveRoot	=	  054h        ; SOLVE EDITOR
kStatP          =       055h        ; stat plot
kInfStat 	=	  056h	      ; Inferential Statistic
kFormat         =       057h        ; FORMAT
kExtApps        =       058h        ; External Applications.     NEW
kNewApps        =       059h        ; New Apps for Cerberus.
;
append          =       kNewApps
;
echoStart1      =       05Ah
;
kTrace          =       05Ah
kZFit           =       05Bh
kZIn            =       05Ch
kZOut           =       05Dh
kZPrev          =       05Eh
kBox            =       05Fh
kDecml          =       060h
kSetZm          =       061h
kSquar          =       062h
kStd            =       063h
kTrig           =       064h
kUsrZm          =       065h
kZSto           =       066h
kZInt           =       067h
kZStat          =       068h
;
echoStart2      =       069h
;
kSelect         =       069h
kCircl          =       06Ah
kClDrw          =       06Bh
kLine           =       06Ch
kPen            =       06Dh
kPtChg          =       06Eh
kPtOff          =       06Fh
kPtOn           =       070h
kVert           =       071h
kHoriz          =       072h
kText           =       073h
kTanLn          =       074h
;
kEval           =       075h
kInters         =       076h
kDYDX           =       077h
kFnIntg         =       078h
kRootG          =       079h
kDYDT           =       07Ah
kDXDT           =       07Bh
kDRDo           =       07Ch
KGFMin          =       07Dh
KGFMax          =       07Eh
;
;
EchoStart       =       07Fh
;
kListName       =       07Fh
kAdd            =       080h
kSub            =       081h
kMul            =       082h
kDiv            =       083h
kExpon          =       084h
kLParen         =       085h
kRParen         =       086h
kLBrack         =       087h
kRBrack         =       088h
kShade          =       089h
kStore          =       08Ah
kComma          =       08Bh
kChs            =       08Ch
kDecPnt         =       08Dh
k0              =       08Eh
k1              =       08Fh
k2              =       090h
k3              =       091h
k4              =       092h
k5              =       093h
k6              =       094h
k7              =       095h
k8              =       096h
k9              =       097h
kEE             =       098h
kSpace          =       099h
kCapA           =       09Ah
kCapB           =       09Bh
kCapC           =       09Ch
kCapD           =       09Dh
kCapE           =       09Eh
kCapF           =       09Fh
kCapG           =       0A0h
kCapH           =       0A1h
kCapI           =       0A2h
kCapJ           =       0A3h
kCapK           =       0A4h
kCapL           =       0A5h
kCapM           =       0A6h
kCapN           =       0A7h
kCapO           =       0A8h
kCapP           =       0A9h
kCapQ           =       0AAh
kCapR           =       0ABh
kCapS           =       0ACh
kCapT           =       0ADh
kCapU           =       0AEh
kCapV           =       0AFh
kCapW           =       0B0h
kCapX           =       0B1h
kCapY           =       0B2h
kCapZ           =       0B3h
kVarx           =       0B4h
kPi             =       0B5h
kInv            =       0B6h
kSin            =       0B7h
kASin           =       0B8h
kCos            =       0B9h
kACos           =       0BAh
kTan            =       0BBh
kATan           =       0BCh
kSquare         =       0BDh
kSqrt           =       0BEh
kLn             =       0BFh
kExp            =       0C0h
kLog            =       0C1h
kALog           =       0C2h
kToABC          =       0C3h
;
kClrTbl         =       0C4h
;
kAns            =       0C5h
kColon          =       0C6h
;
kNDeriv         =       0C7h
kFnInt          =       0C8h
kRoot           =       0C9h
;
kQuest          =       0CAh
kQuote          =       0CBh
kTheta          =       0CCh
kIf             =       0CDh
kThen           =       0CEh
kElse           =       0CFh
kFor            =       0D0h
kWhile          =       0D1h
kRepeat         =       0D2h
kEnd            =       0D3h
kPause          =       0D4h
kLbl            =       0D5h
kGoto           =       0D6h
kISG            =       0D7h
kDSL            =       0D8h
kMenu           =       0D9h
kExec           =       0DAh
kReturn         =       0DBh
kStop           =       0DCh
kInput          =       0DDh
kPrompt         =       0DEh
kDisp           =       0DFh
kDispG          =       0E0h
kDispT          =       0E1h
kOutput         =       0E2h
kGetKey         =       0E3h
kClrHome        =       0E4h
kPrtScr         =       0E5h
kSinH           =       0E6h
kCosH           =       0E7h
kTanH           =       0E8h
kASinH          =       0E9h
kACosH          =       0EAh
kATanH          =       0EBh
kLBrace         =       0ECh
kRBrace         =       0EDh
kI              =       0EEh
kCONSTeA        =       0EFh
kPlot3          =       0F0h
kFMin           =       0F1h
kFMax           =       0F2h
kL1A            =       0F3h
kL2A            =       0F4h
kL3A            =       0F5h
kL4A            =       0F6h
kL5A            =       0F7h
kL6A            =       0F8h
kunA            =       0F9h
kvnA            =       0FAh
kwnA            =       0FBh
;
;======================================================================
;       THIS KEY MEANS THAT IT IS A 2 BYTE KEYCODE
;       THERE ARE 2 OF THESE KEYS; BE CAREFUL WITH USAGE
;======================================================================
;
kExtendEcho2    =       0FCh
;
;======================================================================;
;       THIS KEY MEANS THAT THE KEY PRESS IS ONE THAT ECHOS
;       INTO A BUFFER, AND IT IS A 2 BYTE KEY CODE, GO LOOK AT
;       (EXTECHO) FOR THE KEY VALUE 
;======================================================================
;
kExtendEcho     =       0FEh
;

kE1BT           =       0

kDrawInv        =       kE1BT
kDrawF          =       kE1BT+1
kPixelOn        =       kE1BT+2
kPixelOff       =       kE1BT+3
kPxlTest        =       kE1BT+4
kRCGDB          =       kE1BT+5
kRCPic          =       kE1BT+6
kSTGDB          =       kE1BT+7
kSTPic          =       kE1BT+8
kAbs            =       kE1BT+9
kTEqu           =       kE1BT+10    ; ==
kTNoteQ         =       kE1BT+11    ; <>
kTGT            =       kE1BT+12    ; >
kTGTE           =       kE1BT+13    ; > =
kTLT            =       kE1BT+14    ; <
kTLTE           =       kE1BT+15    ; < =

kAnd            =       kE1BT+16
kOr             =       kE1BT+17
kXor            =       kE1BT+18
kNot            =       kE1BT+19

kLR1            =       kE1BT+20

kXRoot          =       kE1BT+21
kCube           =       kE1BT+22
kCbRt           =       kE1BT+23    ; Cube ROOT
kToDec          =       kE1BT+24
;
kCubicR         =       kE1BT+25
kQuartR         =       kE1BT+26
;
kPlot1          =       kE1BT+27
kPlot2          =       kE1BT+28
;

kRound          =       kE1BT+29
kIPart          =       kE1BT+30
kFPart          =       kE1BT+31
kInt            =       kE1BT+32

kRand           =       kE1BT+33
kNPR            =       kE1BT+34
kNCR            =       kE1BT+35
kXFactorial     =       kE1BT+36

kRad            =       kE1BT+37
kDegr           =       kE1BT+38    ; DEGREES CONV
kAPost          =       kE1BT+39
kToDMS          =       kE1BT+40
kRToPo          =       kE1BT+41    ; R
kRToPr          =       kE1BT+42
kPToRx          =       kE1BT+43
kPToRy          =       kE1BT+44

kRowSwap        =       kE1BT+45
kRowPlus        =       kE1BT+46
kTimRow         =       kE1BT+47
kTRowP          =       kE1BT+48

kSortA          =       kE1BT+49
kSortD          =       kE1BT+50
kSeq            =       kE1BT+51

kMin            =       kE1BT+52
kMax            =       kE1BT+53
kMean           =       kE1BT+54
kMedian         =       kE1BT+55
kSum            =       kE1BT+56
kProd           =       kE1BT+57

kDet            =       kE1BT+58
kTransp         =       kE1BT+59
kDim            =       kE1BT+60
kFill           =       kE1BT+61
kIdent          =       kE1BT+62
kRandm          =       kE1BT+63
kAug            =       kE1BT+64

kOneVar         =       kE1BT+65
kTwoVar         =       kE1BT+66
kLR             =       kE1BT+67
kLRExp          =       kE1BT+68
kLRLn           =       kE1BT+69
kLRPwr          =       kE1BT+70
kMedMed         =       kE1BT+71
kQuad           =       kE1BT+72
kClrLst         =       kE1BT+73
kHist           =       kE1BT+74
kxyLine         =       kE1BT+75
kScatter        =       kE1BT+76

kmRad           =       kE1BT+77
kmDeg           =       kE1BT+78
kmNormF         =       kE1BT+79
kmSci           =       kE1BT+80
kmEng           =       kE1BT+81
kmFloat         =       kE1BT+82

kFix            =       kE1BT+83
kSplitOn        =       kE1BT+84
kFullScreen     =       kE1BT+85
kStndrd         =       kE1BT+86
kParam          =       kE1BT+87
kPolar          =       kE1BT+88
kSeqG           =       kE1BT+89
kAFillOn        =       kE1BT+90
kAFillOff       =       kE1BT+91
kACalcOn        =       kE1BT+92
kACalcOff       =       kE1BT+93
kFNOn           =       kE1BT+94
kFNOff          =       kE1BT+95

kPlotsOn        =       kE1BT+96
kPlotsOff       =       kE1BT+97

kPixelChg       =       kE1BT+98

kSendMBL        =       kE1BT+99
kRecvMBL        =       kE1BT+100

kBoxPlot        =       kE1BT+101
kBoxIcon        =       kE1BT+102
kCrossIcon      =       kE1BT+103
kDotIcon        =       kE1BT+104

kE2BT           =       kE1BT+105

kSeqential      =       kE2BT
kSimulG         =       kE2BT+1
kPolarG         =       kE2BT+2
kRectG          =       kE2BT+3
kCoordOn        =       kE2BT+4
kCoordOff       =       kE2BT+5
kDrawLine       =       kE2BT+6
kDrawDot        =       kE2BT+7
kAxisOn         =       kE2BT+8
kAxisOff        =       kE2BT+9
kGridOn         =       kE2BT+10
kGridOff        =       kE2BT+11
kLblOn          =       kE2BT+12
kLblOff         =       kE2BT+13

kL1             =       kE2BT+14
kL2             =       kE2BT+15
kL3             =       kE2BT+16
kL4             =       kE2BT+17
kL5             =       kE2BT+18
kL6             =       kE2BT+19

;
;======================================================================
;           These keys are layed on top of existing keys to
;           enable localization in the inferential stats editor
;======================================================================
;
kinfData        =       kL1
kinfStats       =       kL1+1
kinfYes         =       kL1+2
kinfNo          =       kL1+3
kinfCalc        =       kL1+4
kinfDraw        =       kL1+5
kinfAlt1ne      =       kL1+6
kinfAlt1lt      =       kL1+7
kinfAlt1gt      =       kL1+8
kinfAlt2ne      =       kL1+9
kinfAlt2lt      =       kL1+10
kinfAlt2gt      =       kL1+11
kinfAlt3ne      =       kL1+12
kinfAlt3lt      =       kL1+13
kinfAlt3gt      =       kL1+14
kinfAlt4ne      =       kL1+15
kinfAlt4lt      =       kL1+16
kinfAlt4gt      =       kL1+17
kinfAlt5ne      =       kL1+18
kinfAlt5lt      =       kL1+19
kinfAlt5gt      =       kL1+20
kinfAlt6ne      =       kL1+21
kinfAlt6lt      =       kL1+22
kinfAlt6gt      =       kL1+23
;
;
kMatA           =       kE2BT+20
kMatB           =       kE2BT+21
kMatC           =       kE2BT+22
kMatD           =       kE2BT+23
kMatE           =       kE2BT+24

kXmin           =       kE2BT+25
kXmax           =       kE2BT+26
kXscl           =       kE2BT+27
kYmin           =       kE2BT+28
kYmax           =       kE2BT+29
kYscl           =       kE2BT+30
kTmin           =       kE2BT+31
kTmax           =       kE2BT+32
kTStep          =       kE2BT+33
kOmin           =       kE2BT+34
kOmax           =       kE2BT+35
kOStep          =       kE2BT+36
ku0             =       kE2BT+37
kv0             =       kE2BT+38
knMin           =       kE2BT+39
knMax           =       kE2BT+40
kDeltaY         =       kE2BT+41
kDeltaX         =       kE2BT+42

kZXmin          =       kE2BT+43
kZXmax          =       kE2BT+44
kZXscl          =       kE2BT+45
kZYmin          =       kE2BT+46
kZYmax          =       kE2BT+47
kZYscl          =       kE2BT+48
kZTmin          =       kE2BT+49
kZTmax          =       kE2BT+50
kZTStep         =       kE2BT+51
kZOmin          =       kE2BT+52
kZOmax          =       kE2BT+53
kZOStep         =       kE2BT+54
kZu0            =       kE2BT+55
kZv0            =       kE2BT+56
kZnMin          =       kE2BT+57
kZnMax          =       kE2BT+58

kDelLast        =       kE2BT+59
kSinReg         =       kE2BT+60
kConstE         =       kE2BT+61

kPic1           =       kE2BT+62
kPic2           =       kE2BT+63
kPic3           =       kE2BT+64

kDelVar         =       kE2BT+65
kGetCalc        =       kE2BT+66
kRealM          =       kE2BT+67
kPolarM         =       kE2BT+68
kRectM          =       kE2BT+69
kuv             =       kE2BT+70    ; U vs V
kvw             =       kE2BT+71    ; V vs W
kuw             =       kE2BT+72    ; U vs W
kFinPMTend      =       kE2BT+73
kFinPMTbeg      =       kE2BT+74
;
kGraphStyle     =       kE2BT+75
;
kExprOn         =       kE2BT+76
kExprOff        =       kE2BT+77
kStatA          =       kE2BT+78
kStatB          =       kE2BT+79
kStatC          =       kE2BT+80
kCorr           =       kE2BT+81
kStatD          =       kE2BT+82
kStatE          =       kE2BT+83
kRegEq          =       kE2BT+84
kMinX           =       kE2BT+85
kQ1             =       kE2BT+86
kMD             =       kE2BT+87
kQ3             =       kE2BT+88
kMaxX           =       kE2BT+89
kStatX1         =       kE2BT+90
kStatY1         =       kE2BT+91
kStatX2         =       kE2BT+92
kStatY2         =       kE2BT+93
kStatX3         =       kE2BT+94
kStatY3         =       kE2BT+95
kTblMin         =       kE2BT+96
kTblStep        =       kE2BT+97
kSetupLst       =       kE2BT+98
kClrAllLst      =       kE2BT+99
kLogistic       =       kE2BT+100
kZTest          =       kE2BT+101
kTTest          =       kE2BT+102
k2SampZTest     =       kE2BT+103
k2SampTTest     =       kE2BT+104
k1PropZTest     =       kE2BT+105
k2PropZTest     =       kE2BT+106
kChiTest        =       kE2BT+107
k2SampFTest     =       kE2BT+108
kZIntVal        =       kE2BT+109
kTIntVal        =       kE2BT+110
k2SampTInt      =       kE2BT+111
k2SampZInt      =       kE2BT+112
k1PropZInt      =       kE2BT+113
k2PropZInt      =       kE2BT+114
kDNormal        =       kE2BT+115
kInvNorm        =       kE2BT+116
kDT             =       kE2BT+117
kChi            =       kE2BT+118
kDF             =       kE2BT+119
kBinPDF         =       kE2BT+120
kBinCDF         =       kE2BT+121
kPoiPDF         =       kE2BT+122
kPoiCDF         =       kE2BT+123
kun             =       kE2BT+124
kvn             =       kE2BT+125
kwn             =       kE2BT+126
kRecn           =       kE2BT+127
kPlotStart      =       kE2BT+128
kZPlotStart     =       kE2BT+129   ; recursion n
kXFact          =       kE2BT+130   ; PlotStart
kYFact          =       kE2BT+131   ; ZPlotStart
kANOVA          =       kE2BT+132   ; XFact
kMaxY           =       kE2BT+133   ; YFact
kWebOn          =       kE2BT+134   ; MinY
kWebOff         =       kE2BT+135   ; MaxY
kTblInput       =       kE2BT+136   ; WEB ON
kGeoPDF         =       kE2BT+137   ; WEB OFF
kGeoCDF         =       kE2BT+138   ; WEB OFF
kShadeNorm      =       kE2BT+139
kShadeT         =       kE2BT+140
kShadeChi       =       kE2BT+141
kShadeF         =       kE2BT+142
kPlotStep       =       kE2BT+143
kZPlotStep      =       kE2BT+144
kLinRegtTest    =       kE2BT+145
KMGT		=	  kE2BT+146   ; VERT SPLIT
kSelectA        =       kE2BT+147
kZFitA          =       kE2BT+148
kE2BT_End       =       kZFitA
;
;
;======================================================================
;           More 2 Byte Keys 
;======================================================================

kE2BT2		=       0

kGDB1		=       kE2BT2
kGDB2		=       kE2BT2+1
kGDB3		=       kE2BT2+2
kY1		=       kE2BT2+3
kY2		=       kE2BT2+4
kY3		=       kE2BT2+5
kY4		=       kE2BT2+6
kY5		=       kE2BT2+7
kY6		=       kE2BT2+8
kY7		=       kE2BT2+9
kY8		=       kE2BT2+10
kY9		=       kE2BT2+11
kY0		=       kE2BT2+12
kX1T		=       kE2BT2+13
kY1T		=       kE2BT2+14
kX2T		=       kE2BT2+15
kY2T		=       kE2BT2+16
kX3T		=       kE2BT2+17
kY3T		=       kE2BT2+18
kX4T		=       kE2BT2+19
kY4T		=       kE2BT2+20
kX5T		=       kE2BT2+21
kY5T		=       kE2BT2+22
kX6T		=       kE2BT2+23
kY6T		=       kE2BT2+24
kR1		=       kE2BT2+25
kR2		=       kE2BT2+26
kR3		=       kE2BT2+27
kR4		=       kE2BT2+28
kR5		=       kE2BT2+29
kR6		=       kE2BT2+30
kGDB4		=       kE2BT2+31
kGDB5		=       kE2BT2+32
kGDB6		=       kE2BT2+33
kPic4		=       kE2BT2+34
kPic5		=       kE2BT2+35
kPic6		=       kE2BT2+36
kGDB7		=       kE2BT2+37
kGDB8		=       kE2BT2+38
kGDB9		=       kE2BT2+39
kGDB0		=       kE2BT2+40
kPic7		=       kE2BT2+41
kPic8		=       kE2BT2+42
kPic9		=       kE2BT2+43
kPic0		=       kE2BT2+44
kStatN		=       kE2BT2+45
kXMean		=       kE2BT2+46
kConj		=       kE2BT2+47
kReal		=       kE2BT2+48
kFAngle		=       kE2BT2+49
kLCM		=       kE2BT2+50
kGCD		=       kE2BT2+51
kRandInt	=       kE2BT2+52
kRandNorm	=       kE2BT2+53
kToPolar	=       kE2BT2+54
kToRect		=       kE2BT2+55
kYMean		=       kE2BT2+56
kStdX		=       kE2BT2+57
kStdX1		=       kE2BT2+58
kw0		=       kE2BT2+59
kMatF		=       kE2BT2+60
kMatG		=       kE2BT2+61
kMatRH		=       kE2BT2+62
kMatI		=       kE2BT2+63
kMatJ		=       kE2BT2+64
kYMean1		=       kE2BT2+65
kStdY		=       kE2BT2+66
kStdY1		=       kE2BT2+67
kMatToLst	=       kE2BT2+68
kLstToMat	=       kE2BT2+69
kCumSum		=       kE2BT2+70
kDeltaLst	=       kE2BT2+71
kStdDev		=       kE2BT2+72
kVariance	=       kE2BT2+73
kLength		=       kE2BT2+74
kEquToStrng	=       kE2BT2+75
kStrngToEqu	=       kE2BT2+76
kExpr		=       kE2BT2+77
kSubStrng	=       kE2BT2+78
kInStrng	=       kE2BT2+79
kStr1		=       kE2BT2+80
kStr2		=       kE2BT2+81
kStr3		=       kE2BT2+82
kStr4           =       kE2BT2+83
kStr5           =       kE2BT2+84
kStr6           =       kE2BT2+85
kStr7           =       kE2BT2+86
kStr8           =       kE2BT2+87
kStr9           =       kE2BT2+88
kStr0           =       kE2BT2+89
kFinN           =       kE2BT2+90
kFinI           =       kE2BT2+91
kFinPV          =       kE2BT2+92
kFinPMT         =       kE2BT2+93
kFinFV          =       kE2BT2+94
kFinPY          =       kE2BT2+95
kFinCY          =       kE2BT2+96
kFinFPMT        =       kE2BT2+97
kFinFI          =       kE2BT2+98
kFinFPV         =       kE2BT2+99
kFinFN          =       kE2BT2+100
kFinFFV         =       kE2BT2+101
kFinNPV         =       kE2BT2+102
kFinIRR         =       kE2BT2+103
kFinBAL         =       kE2BT2+104
kFinPRN         =       kE2BT2+105
kFinINT         =       kE2BT2+106
kSumX           =       kE2BT2+107
kSumX2          =       kE2BT2+108
kFinToNom       =       kE2BT2+109
kFinToEff       =       kE2BT2+110
kFinDBD         =       kE2BT2+111
kStatVP         =       kE2BT2+112
kStatZ          =       kE2BT2+113
kStatT          =       kE2BT2+114
kStatChi        =       kE2BT2+115
kStatF          =       kE2BT2+116
kStatDF         =       kE2BT2+117
kStatPhat       =       kE2BT2+118
kStatPhat1      =       kE2BT2+119
kStatPhat2      =       kE2BT2+120
kStatMeanX1     =       kE2BT2+121
kStatMeanX2     =       kE2BT2+122
kStatStdX1      =       kE2BT2+123
kStatStdX2      =       kE2BT2+124
kStatStdXP      =       kE2BT2+125
kStatN1         =       kE2BT2+126
kStatN2         =       kE2BT2+127
kStatLower      =       kE2BT2+128
kStatUpper      =       kE2BT2+129
kuw0            =       kE2BT2+130
kImag           =       kE2BT2+131
kSumY           =       kE2BT2+132
kXres           =       kE2BT2+133
kStat_s         =       kE2BT2+134
kSumY2          =       kE2BT2+135
kSumXY          =       kE2BT2+136
kuXres          =       kE2BT2+137
kModBox		=       kE2BT2+138
kNormProb	=       kE2BT2+139
kNormalPDF      =       kE2BT2+140
kTPDF           =       kE2BT2+141
kChiPDF         =       kE2BT2+142
kFPDF           =       kE2BT2+143
kMinY           =       kE2BT2+144   ; MinY
kRandBin        =       kE2BT2+145
kRef            =       kE2BT2+146
kRRef           =       kE2BT2+147
kLRSqr          =       kE2BT2+148
kBRSqr          =       kE2BT2+149
kDiagOn         =       kE2BT2+150
kDiagOff        =       kE2BT2+151
kun1            =       kE2BT2+152   ; FOR RCL USE WHEN GOTTEN FROM 82
kvn1            =       kE2BT2+153   ; FOR RCL USE WHEN GOTTEN FROM 82
;
k83_00End       =	kvn1            ;end of original keys...
kArchive        =     k83_00End + 1
kUnarchive      =     k83_00End + 2
kAsm            =     k83_00End + 3   ; Asm(
kAsmPrgm        =     k83_00End + 4   ; AsmPrgm
kAsmComp        =     k83_00End + 5   ; AsmComp(
;
kcapAAcute	=       k83_00End + 6
kcapAGrave	=       k83_00End + 7
kcapACaret	=       k83_00End + 8
kcapADier	=       k83_00End + 9
kaAcute		=       k83_00End + 10
kaGrave		=       k83_00End + 11
kaCaret		=       k83_00End + 12
kaDier		=       k83_00End + 13
kcapEAcute	=       k83_00End + 14
kcapEGrave	=       k83_00End + 15
kcapECaret	=       k83_00End + 16
kcapEDier	=       k83_00End + 17
keAcute		=       k83_00End + 18
keGrave		=       k83_00End + 19
keCaret		=       k83_00End + 20
keDier		=       k83_00End + 21
kcapIAcute	=       k83_00End + 22
kcapIGrave	=       k83_00End + 23
kcapICaret	=       k83_00End + 24
kcapIDier	=       k83_00End + 25
kiAcute		=       k83_00End + 26
kiGrave		=       k83_00End + 27
kiCaret		=       k83_00End + 28
kiDier		=       k83_00End + 29
kcapOAcute	=       k83_00End + 30
kcapOGrave	=       k83_00End + 31
kcapOCaret	=       k83_00End + 32
kcapODier	=       k83_00End + 33
koAcute		=       k83_00End + 34
koGrave		=       k83_00End + 35
koCaret		=       k83_00End + 36
koDier		=       k83_00End + 37
kcapUAcute	=       k83_00End + 38
kcapUGrave	=       k83_00End + 39
kcapUCaret	=       k83_00End + 40
kcapUDier	=       k83_00End + 41
kuAcute		=       k83_00End + 42
kuGrave		=       k83_00End + 43
kuCaret		=       k83_00End + 44
kuDier		=       k83_00End + 45
kcapCCed	=       k83_00End + 46
kcCed		=       k83_00End + 47
kcapNTilde	=       k83_00End + 48
knTilde		=       k83_00End + 49
kaccent		=       k83_00End + 50
kgrave		=       k83_00End + 51
kdieresis	=       k83_00End + 52
kquesDown	=       k83_00End + 53
kexclamDown     =       k83_00End + 54
kalpha          =       k83_00End + 55
kbeta           =      k83_00End +  56
kgamma          =      k83_00End +  57
kcapDelta       =      k83_00End +  58
kdelta          =      k83_00End +  59
kepsilon        =      k83_00End +  60
klambda         =      k83_00End +  61
kmu             =      k83_00End +  62
kpi2            =      k83_00End +  63
krho            =      k83_00End +  64
kcapSigma       =      k83_00End +  65
ksigma          =      k83_00End +  66
ktau            =      k83_00End +  67
kphi            =      k83_00End +  68
kcapOmega       =      k83_00End +  69
kphat           =      k83_00End +  70
kchi2           =      k83_00End +  71
kstatF2         =      k83_00End +  72
kLa		=     k83_00End + 73
kLb		=     k83_00End + 74
kLc		=     k83_00End + 75
kLd		=     k83_00End + 76
kLe		=     k83_00End + 77
kLf		=     k83_00End + 78
kLg		=     k83_00End + 79
kLh		=     k83_00End + 80
kLi		=     k83_00End + 81
kLj		=     k83_00End + 82
kLk		=     k83_00End + 83
kLl		=     k83_00End + 84
kLm		=     k83_00End + 85
kLsmalln	=    k83_00End +  86
kLo		=    k83_00End +  87
kLp		=    k83_00End +  88
kLq		=    k83_00End +  89
kLsmallr	=    k83_00End +  90
kLs		=    k83_00End +  91
kLt		=    k83_00End +  92
kLu		=    k83_00End +  93
kLv		=    k83_00End +  94
kLw		=    k83_00End +  95
kLx		=    k83_00End +  96
kLy		=    k83_00End +  97
kLz		=    k83_00End +  98
kGarbageC	=  k83_00End +  99 ; GarbageCollect
;
kE2BT2_End	=     kGarbageC
;
;======================================================================
;           TI-83 Plus Context Equates
;======================================================================
;
cxCmd           =     kQuit                   ;home screen
cxMatEdit       =     kMatrixEd               ;matrix editor
cxPrgmEdit      =     kPrgmEd                 ;program editor
cxEquEdit       =     kYequ                   ;equation editor
cxGrRange       =     kWindow                 ;graph range editor
cxGrZfact       =     kZFactEd                ;graph zoom factors editor
cxGraph         =     kGraph                  ;graph mode
cxStatEdit      =     kStatEd                 ;statistics list editor
cxPrgmInput     =     kPrgmInput              ;programmed input
cxError         =     kError                  ;error handler
cxLinkIO        =     kLinkIO                 ;LINK I/O interface
cxMem           =     kResetMem               ;reset memory
cxDefMem        =     kResetDef               ;reset default
cxRAMApp        =     kChkRAM                 ;RAM usage screen
cxMode          =     kMode                   ;mode settings screen
cxErase         =     kDelMem                 ;memory erase
cxPrgmCreate    =     kPrgmCr                 ;PROGRAM CREATE
cxTableEditor   =     kTable                  ;TABLE EDITOR
cxTableSet      =     kTblSet                 ;TABLE SET UP
cxStatPlot      =     kStatP                  ;STAT PLOTS
cxInfStat	=	kInfStat		;Inferential Statistic
cxFormat        =     kFormat                 ;FORMAT CONTEXT
cxSolveTVM      =     kSolveTVM               ;Solve TVM
cxSolveRoot     =     kSolveRoot              ;Solve Root
lastOldApp      =     kExtApps                ;external applications
cxextapps       =     kExtApps
cxNewApps       =     kNewApps                ;new cerberus applications
cxGroup         =     cxNewApps+0             ;1st new app.
cxUnGroup       =     cxNewApps+1             ;2nd new app.
lastNewApp      =     cxUnGroup               ;last new app for this ver

;
;======================================================================
;           Scan Code Equates
;======================================================================;
skDown          =	01h
skLeft          =	02h
skRight         =	03h
skUp            =	04h
skEnter         =	09h
skAdd           =	0Ah
skSub           =	0Bh
skMul           =	0Ch
skDiv           =	0Dh
skPower         =	0Eh
skClear         =	0Fh
skChs           =	11h
sk3             =	12h
sk6             =	13h
sk9             =	14h
skRParen        =	15h
skTan           =	16h
skVars          =	17h
skDecPnt        =	19h
sk2             =	1Ah
sk5             =	1Bh
sk8             =	1Ch
skLParen        =	1Dh
skCos           =	1Eh
skPrgm          =	1Fh
skStat          =	20h
sk0             =	21h
sk1             =	22h
sk4             =	23h
sk7             =	24h
skComma         =	25h
skSin           =	26h
skMatrix        =	27h
skGraphvar      =	28h
skStore         =	2Ah
skLn            =	2Bh
skLog           =	2Ch
skSquare        =	2Dh
skRecip         =	2Eh
skMath          =	2Fh
skAlpha         =	30h
skGraph         =	31h
skTrace         =	32h
skZoom          =	33h
skWindow        =	34h
skYEqu          =	35h
sk2nd           =	36h
skMode          =	37h
skDel           =	38h
;
;======================================================================
;           Tokens
;======================================================================
;
EOSSTART        =       0
;
;
;   DISPLAY CONVERSIONS COME IMMEDIATELY BEFORE 'TSTORE'
;
DCONV   =     01h
;
tToDMS          =       DCONV       ; 01h
tToDEC          =       DCONV+1     ; 02h
tToAbc          =       DCONV+2     ; 03h    > A b/c
;
tStore          =       DCONV+3     ; 04h Lstore                01
;
tBoxPlot        =       05h
;
BRACKS          =       06h
;
tLBrack         =       BRACKS      ; 06h '['
tRBrack         =       BRACKS+1    ; 07h ']'
tLBrace         =       BRACKS+2    ; 08h '{'
tRBrace         =       BRACKS+3    ; 09h '}'
;
tPOST1          =       BRACKS+4
;
tFromRad        =       tPOST1       ; 0Ah Lradian
tFromDeg        =       tPOST1+1     ; 0Bh Ldegree
tRecip          =       tPOST1+2     ; 0Ch Linverse
tSqr            =       tPOST1+3     ; 0Dh Lsquare
tTrnspos        =       tPOST1+4     ; 0Eh Ltranspose
tCube           =       tPOST1+5     ; 0Fh '^3'
;
tLParen         =       10h         ; 10h '('
tRParen         =       11h         ; 11h ')'
;
;
IMUN            =       12h
;
tRound          =       IMUN        ; 12h 'round'
tPxTst          =       IMUN+1      ; 13h 'PXL-TEST'
tAug            =       IMUN+2      ; 14h 'aug'
tRowSwap        =       IMUN+3      ; 15h 'rSwap'
tRowPlus        =       IMUN+4      ; 16h 'rAdd'
tmRow           =       IMUN+5      ; 17h 'multR'
tmRowPlus       =       IMUN+6      ; 18h 'mRAdd'
tMax            =       IMUN+7      ; 19h 'max'
tMin            =       IMUN+8      ; 1Ah 'min'
tRToPr          =       IMUN+9      ; 1Bh 'R>Pr
tRToPo          =       IMUN+10     ; 1Ch 'R>Po
tPToRx          =       IMUN+11     ; 1Dh 'P>Rx
tPToRy          =       IMUN+12     ; 1Eh 'P>Ry
tMedian         =       IMUN+13     ; 1Fh 'MEDIAN
tRandM          =       IMUN+14     ; 20h 'randM'
tMean           =       IMUN+15     ; 21h
tRoot           =       IMUN+16     ; 22h 'ROOT'
tSeries         =       IMUN+17     ; 23h 'seq'
tFnInt          =       IMUN+18     ; 24h 'fnInt'
tNDeriv         =       IMUN+19     ; 25h 'fnIr'
tEvalF          =       IMUN+20     ; 26h
tFmin           =       IMUN+21     ; 27h
tFmax           =       IMUN+22     ; 28h
;
tEOSEL          =       IMUN+23
;
tSpace          =       tEOSEL       ; 29h   ' '
tString         =       tEOSEL+1     ; 2Ah  '"'
tComma          =       tEOSEL+2     ; 2Bh  ','
;
;
tii             =       2Ch         ; i
;
;======================================================================
;           Postfix Functions
;======================================================================
;
tPost           =       2Dh
;
tFact           =       tPost       ; 2Dh '!'
;
tCubicR         =       2Eh
tQuartR         =       2Fh

;======================================================================
;           Number Tokens 
;======================================================================
;
NUMS            =       30h
;
t0              =       NUMS        ; 30h
t1              =       NUMS+1      ; 31h
t2              =       NUMS+2      ; 32h
t3              =       NUMS+3      ; 33h
t4              =       NUMS+4      ; 34h
t5              =       NUMS+5      ; 35h
t6              =       NUMS+6      ; 36h
t7              =       NUMS+7      ; 37h
t8              =       NUMS+8      ; 38h
t9              =       NUMS+9      ; 39h
tDecPt          =       NUMS+10     ; 3Ah
tee             =       NUMS+11     ; 3Bh
;
;======================================================================
;            BINARY OP
;======================================================================
;
tOr             =       3Ch         ; 3Ch  '_or_'
tXor            =       3Dh         ; 3Dh
;
tColon          =       3Eh         ; 3Eh ':'
tEnter          =       3Fh         ; 3Fh  Lenter
;
tAnd            =       40h         ; 40h  '_and_'       
;
;======================================================================
;            LETTER TOKENS   
;======================================================================

;
LET             =       41h
;
tA              =       LET         ; 41h
tB              =       LET+1       ; 42h
tC              =       LET+2       ; 43h
tD              =       LET+3       ; 44h
tE              =       LET+4       ; 45h
tF              =       LET+5       ; 46h
tG              =       LET+6       ; 47h
tH              =       LET+7       ; 48h
tI              =       LET+8       ; 49h
tJ              =       LET+9       ; 4Ah
tK              =       LET+10      ; 4Bh
tL              =       LET+11      ; 4Ch
tM              =       LET+12      ; 4Dh
tN              =       LET+13      ; 4Eh
tO              =       LET+14      ; 4Fh
tP              =       LET+15      ; 50h
tQ              =       LET+16      ; 51h
tR              =       LET+17      ; 52h
tS              =       LET+18      ; 53h
tT              =       LET+19      ; 54h
tU              =       LET+20      ; 55h
tV              =       LET+21      ; 56h
tW              =       LET+22      ; 57h
tX              =       LET+23      ; 58h
tY              =       LET+24      ; 59h
tZ              =       LET+25      ; 5Ah
tTheta          =       LET+26      ; 5Bh
;
;======================================================================
;   THESE VAR TOKENS ARE 1ST OF A DOUBLE TOKEN 
;======================================================================

;
vToks   =     LET+27    ;
;
;       USER MATRIX TOKEN, 2ND TOKEN NEEDED FOR NAME
;
tVarMat         =       vToks       ; 5Ch
;
;       USER LIST TOKEN, 2ND TOKEN NEEDED FOR NAME
;
tVarLst         =       vToks+1     ; 5Dh
;
;       USER =ATION TOKEN, 2ND TOKEN NEEDED FOR NAME
;
tVarEqu         =       vToks+2     ; 5Eh
tProg           =       vToks+3     ; 5Fh
;
;       USER PICT TOKEN, 2ND TOKEN NEEDED FOR NAME
;
tVarPict        =       vToks+4     ; 60h
;
;       USER GDB TOKEN, 2ND TOKEN NEEDED FOR NAME
;
tVarGDB         =       vToks+5     ; 61h
tVarOut         =       vToks+6     ; 62h
tVarSys         =       vToks+7     ; 63h
;
;
;======================================================================
;            Mode Setting Commands
;======================================================================

;
MODESA          =       vToks+8     ; 64h
;
tRad            =       MODESA      ; 64h 'Radian'
tDeg            =       MODESA+1    ; 65h 'Degree'
tNormF          =       MODESA+2    ; 66h 'Normal'
tSci            =       MODESA+3    ; 67h 'Sci'
tEng            =       MODESA+4    ; 68h 'Eng'
tFloat          =       MODESA+5    ; 69h 'Float'
;
CMPS            =       6Ah
;
tEQ             =       CMPS        ; 6Ah '=='
tLT             =       CMPS+1      ; 6Bh '<'
tGT             =       CMPS+2      ; 6Ch '>'
tLE             =       CMPS+3      ; 6Dh LLE
tGE             =       CMPS+4      ; 6Eh LGE
tNE             =       CMPS+5      ; 6Fh LNE
;
;======================================================================
;            BINARY OP
;======================================================================
;
tAdd            =       70h         ; 70h '+'
tSub            =       71h         ; 71h '-'
tAns            =       72h         ; 72h
;
;======================================================================
;            Mode Setting Commands
;======================================================================
;
MODES           =       73h
;
tFix            =       MODES       ; 73h 'Fix_'
tSplitOn        =       MODES+1     ; 74h
tFullScreen     =       MODES+2     ; 75h
tStndrd         =       MODES+3     ; 76h 'Func'
tParam          =       MODES+4     ; 77h 'Param'
tPolar          =       MODES+5     ; 78h 'Pol'
tSeqG           =       MODES+6     ; 79h
tAFillOn        =       MODES+7     ; 7Ah 'AUTO FILL ON
tAFillOff       =       MODES+8     ; 7Bh
tACalcOn        =       MODES+9     ; 7Ch
tACalcOff       =       MODES+10    ; 7Dh 'AutoFill OFF
;
;       GRAPH FORMAT TOKENS ARE 2 BYTE TOKENS
;
tGFormat        =       MODES+11    ; 7Eh
;
tBoxIcon        =       7Fh
tCrossIcon      =       80h
tDotIcon        =       81h
;
;======================================================================
;            (More) BINARY OP
;======================================================================
;
tMul            =       82h         ; 82h  '*'
tDiv            =       83h         ; 83h  '/'
;
;======================================================================
;            SOME GRAPH COMMANDS
;======================================================================
;
GRCMDS          =       84h
;
tTrace          =       GRCMDS      ; 84h 'Trace'
tClDrw          =       GRCMDS+1    ; 85h 'ClDrw'
tZoomStd        =       GRCMDS+2    ; 86h 'ZStd'
tZoomtrg        =       GRCMDS+3    ; 87h 'Ztrg'
tZoomBox        =       GRCMDS+4    ; 88h 'ZBOX'
tZoomIn         =       GRCMDS+5    ; 89h 'ZIn'
tZoomOut        =       GRCMDS+6    ; 8Ah 'ZOut'
tZoomSqr        =       GRCMDS+7    ; 8Bh 'ZSqr'
tZoomInt        =       GRCMDS+8    ; 8Ch 'ZInt'
tZoomPrev       =       GRCMDS+9    ; 8Dh 'ZPrev'
tZoomDec        =       GRCMDS+10   ; 8Eh 'ZDecm'
tZoomStat       =       GRCMDS+11   ; 8Fh 'ZStat
tUsrZm          =       GRCMDS+12   ; 90h 'ZRcl'
tPrtScrn        =       GRCMDS+13   ; 91h 'PrtScrn'
tZoomSto        =       GRCMDS+14   ; 92h  ZOOM STORE
tText           =       GRCMDS+15   ; 93h
;
;======================================================================
;           BINARY OP  (Combination & Permutation)
;======================================================================
;
tnPr            =       GRCMDS+16   ; 94h '_nPr_'
tnCr            =       GRCMDS+17   ; 95h '_nCr_'
;
;======================================================================
;           MORE GRAPH COMMANDS
;======================================================================
;
tYOn            =       GRCMDS+18   ; 96h 'FnOn_'
tYOff           =       GRCMDS+19   ; 97h 'FnOff_'
tStPic          =       GRCMDS+20   ; 98h 'StPic_'
tRcPic          =       GRCMDS+21   ; 99h 'RcPic_'
tStoDB          =       GRCMDS+22   ; 9Ah 'StGDB_'
tRclDB          =       GRCMDS+23   ; 9Bh 'RcGDB_'
tLine           =       GRCMDS+24   ; 9Ch 'Line'
tVert           =       GRCMDS+25   ; 9Dh 'Vert_'
tPtOn           =       GRCMDS+26   ; 9Eh 'PtOn'
tPtOff          =       GRCMDS+27   ; 9Fh 'PtOff'
;
;======================================================================
;           TOKEN A0 CANNOT BE AN EOS FUNCTION
;           SINCE LOW MULT=A0 ALREADY
;======================================================================
;
tPtChg          =       GRCMDS+28   ; A0h 'PtChg'
tPXOn           =       GRCMDS+29   ; A1h
tPXOff          =       GRCMDS+30   ; A2h
tPXChg          =       GRCMDS+31   ; A3h
tShade          =       GRCMDS+32   ; A4h 'Shade'
tCircl          =       GRCMDS+33   ; A5h 'Circl'
tHorz           =       GRCMDS+34   ; A6h 'HORIZONTAL'
tTanLn          =       GRCMDS+35   ; A7h 'TanLn'
tDrInv          =       GRCMDS+36   ; A8h 'DrInv_'
tDrawF          =       GRCMDS+37   ; A9h 'DrawF_'
;
tVarStrng       =       0AAh
;
;======================================================================
;           Functions with No Arguments                                    
;======================================================================
;
NOARG   =     0ABh
;
tRand           =       NOARG       ; ABh 'rand'
tPi             =       NOARG+1     ; ACh  Lpi
tGetKey         =       NOARG+2     ; ADh 'getKy'
;
;
tAPost          =       tGetKey+1   ; APOSTROPHY
tQuest          =       tAPost+1    ; QUESTION MARK
;
;
UNARY   =     tQuest+1              ; B0h
;
tChs            =       UNARY       ; B0h
tInt            =       UNARY+1     ; B1h
tAbs            =       UNARY+2     ; B2h
tDet            =       UNARY+3     ; B3h
tIdent          =       UNARY+4     ; B4h
tDim            =       UNARY+5     ; B5h
tSum            =       UNARY+6     ; B6h
tProd           =       UNARY+7     ; B7h
tNot            =       UNARY+8     ; B8h
tIPart          =       UNARY+9     ; B9h
tFPart          =       UNARY+10    ; BAh
;
;
;======================================================================
;           NEW 2 BYTE TOKENS
;======================================================================
;
t2ByteTok       =       0BBh         ;
;
;
UNARYLR =     UNARY+12
;
tSqrt           =       UNARYLR     ; BCh
tCubRt          =       UNARYLR+1   ; BDh
tLn             =       UNARYLR+2   ; BEh
tExp            =       UNARYLR+3   ; BFh
tLog            =       UNARYLR+4   ; C0h
tALog           =       UNARYLR+5   ; C1h
tSin            =       UNARYLR+6   ; C2h
tASin           =       UNARYLR+7   ; C3h
tCos            =       UNARYLR+8   ; C4h
tACos           =       UNARYLR+9   ; C5h
tTan            =       UNARYLR+10  ; C6h
tATan           =       UNARYLR+11  ; C7h
tSinH           =       UNARYLR+12  ; C8h
tASinH          =       UNARYLR+13  ; C9h
tCosH           =       UNARYLR+14  ; CAh
tACosH          =       UNARYLR+15  ; CBh
tTanH           =       UNARYLR+16  ; CCh
tATanH          =       UNARYLR+17  ; CDh
; 
;======================================================================
;           SOME PROGRAMMING COMMANDS                                      
;======================================================================
;
PROGTOK =     UNARYLR+18
;
tIf             =       PROGTOK     ; CEh
tThen           =       PROGTOK+1   ; CFh
tElse           =       PROGTOK+2   ; D0h
tWhile          =       PROGTOK+3   ; D1h
tRepeat         =       PROGTOK+4   ; D2h
tFor            =       PROGTOK+5   ; D3h
tEnd            =       PROGTOK+6   ; D4h
tReturn         =       PROGTOK+7   ; D5h
tLbl            =       PROGTOK+8   ; D6h   'Lbl_'
tGoto           =       PROGTOK+9   ; D7h   'Goto_'
tPause          =       PROGTOK+10  ; D8h   'Pause_'
tStop           =       PROGTOK+11  ; D9h   'Stop'
tISG            =       PROGTOK+12  ; DAh   'IS>'
tDSL            =       PROGTOK+13  ; DBh   'DS<'
tInput          =       PROGTOK+14  ; DCh   'Input_'
tPrompt         =       PROGTOK+15  ; DDh   'Prompt_'
tDisp           =       PROGTOK+16  ; DEh   'Disp_'
tDispG          =       PROGTOK+17  ; DFh   'DispG'
;
tOutput         =       PROGTOK+18  ; E0h    'Outpt'
tClLCD          =       PROGTOK+19  ; E1h    'ClLCD'
tConst          =       PROGTOK+20  ; E2h    'Fill'
tSortA          =       PROGTOK+21  ; E3h    'sortA_'
tSortD          =       PROGTOK+22  ; E4h    'sortD_'
tDispTab        =       PROGTOK+23  ; E5h    'Disp Table
tMenu           =       PROGTOK+24  ; E6h    'Menu'
tSendMBL        =       PROGTOK+25  ; E7h    'SEND'
tGetMBL         =       PROGTOK+26  ; E8h    'GET'
;
;======================================================================
;           STAT PLOT COMMANDS                                             
;======================================================================
;
statPCmd        =       PROGTOK+27
;
tPlotOn         =       statPCmd    ; E9h     ' PLOTSON'
tPlotOff        =       statPCmd+1   ; EAh     ' PLOTSOFF
;
tListName       =       0EBh        ; LIST DESIGNATOR
;
tPlot1          =       0ECh
tPlot2          =       0EDh
tPlot3          =       0EEh
;
tUnused01       =       0EFh        ;        available?
;
tPower          =       0F0h        ;        '^'
tXRoot          =       0F1h        ;        LsupX,Lroot

STATCMD         =       0F2h
;
tOneVar         =       STATCMD     ; F2h    'OneVar_'
tTwoVar         =       STATCMD+1   ; F3h
tLR             =       STATCMD+2   ; F4h    'LinR(A+BX
tLRExp          =       STATCMD+3   ; F5h    'ExpR_'
tLRLn           =       STATCMD+4   ; F6h    'LnR_'
tLRPwr          =       STATCMD+5   ; F7h    'PwrR_'
tMedMed         =       STATCMD+6   ; F8h
tQuad           =       STATCMD+7   ; F9h
tClrLst         =       STATCMD+8   ; FAh    'CLEAR LIST
tClrTbl         =       STATCMD+9   ; FBh    CLEAR TABLE
tHist           =       STATCMD+10  ; FCh    'Hist_'
txyLine         =       STATCMD+11  ; FDh    'xyline_'
tScatter        =       STATCMD+12  ; FEh    'Scatter_'
tLR1            =       STATCMD+13  ; FFh    'LINR(AX+B

;       
;======================================================================
;           2ND HALF OF GRAPH FORMAT TOKENS
;======================================================================
;           Format settings commands
;
GFMT            =       0
;
tSeq            =       GFMT        ; 'SeqG'
tSimulG         =       GFMT+1      ; 'SimulG'
tPolarG         =       GFMT+2      ; 'PolarGC'
tRectG          =       GFMT+3      ; 'RectGC'
tCoordOn        =       GFMT+4      ; 'CoordOn'
tCoordOff       =       GFMT+5      ; 'CoordOff'
tDrawLine       =       GFMT+6      ; 'DrawLine'
tDrawDot        =       GFMT+7      ; 'DrawDot'
tAxisOn         =       GFMT+8      ; 'AxesOn'
tAxisOff        =       GFMT+9      ; 'AxesOff'
tGridOn         =       GFMT+10     ; 'GridOn'
tGridOff        =       GFMT+11     ; 'GridOff'
tLblOn          =       GFMT+12     ; 'LabelOn'
tLblOff         =       GFMT+13     ; 'LabelOff'
tWebOn          =       GFMT+14     ; 'WebOn'
tWebOff         =       GFMT+15     ; 'WebOFF'
tuv             =       GFMT+16     ; U vs V
tvw             =       GFMT+17     ; V vs W
tuw             =       GFMT+18     ; U vs W
;
;======================================================================
;           2ND HALF OF USER MATRIX TOKENS                                 
;======================================================================
;
tMatA           =       00h         ; MAT A
tMatB           =       01h         ; MAT B
tMatC           =       02h         ; MAT C
tMatD           =       03h         ; MAT D
tMatE           =       04h         ; MAT E
tMatF           =       05h         ; MAT F
tMatG           =       06h         ; MAT G
tMatH           =       07h         ; MAT H
tMatI           =       08h         ; MAT I
tMatJ           =       09h         ; MAT J
;      
;======================================================================
;           2ND HALF OF USER LIST TOKENS
;======================================================================
;
tL1             =       00h         ; LIST 1
tL2             =       01h         ; LIST 2
tL3             =       02h         ; LIST 3
tL4             =       03h         ; LIST 4
tL5             =       04h         ; LIST 5
tL6             =       05h         ; LIST 6

;
;======================================================================
;           2ND HALF OF USER =ATION TOKENS
;======================================================================
;
;       "Y" =ATIONS HAVE BIT 4 SET
;
tY1             =       10h         ; Y1
tY2             =       11h         ; Y2
tY3             =       12h         ; Y3
tY4             =       13h         ; Y4
tY5             =       14h         ; Y5
tY6             =       15h         ; Y6
tY7             =       16h         ; Y7
tY8             =       17h         ; Y8
tY9             =       18h         ; Y9
tY0             =       19h         ; Y0
;
;       PARAM =ATIONS HAVE BIT 5 SET
;
tX1T            =       20h         ; X1t
tY1T            =       21h         ; Y1t
tX2T            =       22h         ; X2t
tY2T            =       23h         ; Y2t
tX3T            =       24h         ; X3t
tY3T            =       25h         ; Y3t
tX4T            =       26h         ; X4t
tY4T            =       27h         ; Y4t
tX5T            =       28h         ; X5t
tY5T            =       29h         ; Y5t
tX6T            =       2Ah         ; X6t
tY6T            =       2Bh         ; Y6t
;
;       POLAR =ATIONS HAVE BIT 6 SET
;
tR1             =       40h         ; R1
tR2             =       41h         ; R2
tR3             =       42h         ; R3
tR4             =       43h         ; R4
tR5             =       44h         ; R5
tR6             =       45h         ; R6
;
;       RECURSION =ATIONS HAVE BIT 7 SET
;
tun             =       80h         ; Un
tvn             =       81h         ; Vn
twn             =       82h         ; Wn
;
;======================================================================
;           2ND HALF OF USER PICTURE TOKENS                                
;======================================================================
;
tPic1           =       00h         ; PIC1
tPic2           =       01h         ; PIC2
tPic3           =       02h         ; PIC3
tPic4           =       03h         ; PIC4
tPic5           =       04h         ; PIC5
tPic6           =       05h         ; PIC6
tPic7           =       06h         ; PIC7
tPic8           =       07h         ; PIC8
tPic9           =       08h         ; PIC9
tPic0           =       09h         ; PIC0
; 
;======================================================================
;           2ND HALF OF USER GRAPH DATABASE TOKENS                         
;======================================================================
;
tGDB1           =       00h         ; GDB1
tGDB2           =       01h         ; GDB2
tGDB3           =       02h         ; GDB3
tGDB4           =       03h         ; GDB4
tGDB5           =       04h         ; GDB5
tGDB6           =       05h         ; GDB6
tGDB7           =       06h         ; GDB7
tGDB8           =       07h         ; GDB8
tGDB9           =       08h         ; GDB9
tGDB0           =       09h         ; GDB0
;  
;======================================================================
;           2ND HALF OF STRING VARS                                        
;======================================================================
;
tStr1           =       00h
tStr2           =       01h
tStr3           =       02h
tStr4           =       03h
tStr5           =       04h
tStr6           =       05h
tStr7           =       06h
tStr8           =       07h
tStr9           =       08h
tStr0           =       09h

; 
;======================================================================
;           2ND HALF OF SYSTEM OUTPUT ONLY VARIABLES                       
;======================================================================
;
;
;   OPEN        =       00h
;
tRegEq          =       01h         ; REGRESSION =ATION
;
tStatN          =       02h         ; STATISTICS N
;
tXMean          =       03h         ; X MEAN
tSumX           =       04h         ; SUM(X)
tSumXSqr        =       05h         ; SUM(X^2)
tStdX           =       06h         ; STANDARD DEV X
tStdPX          =       07h         ; STANDARD DEV POP X
tMinX           =       08h         ; Min X VALUE
tMaxX           =       09h         ; Max X VALUE
;
tMinY           =       0Ah         ; Min Y VALUE
tMaxY           =       0Bh         ; Max Y VALUE
tYmean          =       0Ch         ; Y MEAN
tSumY           =       0Dh         ; SUM(Y)
tSumYSqr        =       0Eh         ; SUM(Y^2)
tStdY           =       0Fh         ; STANDARD DEV Y
tStdPY          =       10h         ; STANDARD DEV POP Y
;
tSumXY          =       11h         ; SUM(XY)
tCorr           =       12h         ; CORRELATION
tMedX           =       13h         ; MED(X)
tQ1             =       14h         ; 1ST QUADRANT OF X
tQ3             =       15h         ; 3RD QUADRANT OF X
tQuadA          =       16h         ; 1ST TERM OF QUAD POLY REG/ Y-INT
tQuadB          =       17h         ; 2ND TERM OF QUAD POLY REG/ SLOPE
tQuadC          =       18h         ; 3RD TERM OF QUAD POLY REG
tCubeD          =       19h         ; 4TH TERM OF CUBIC POLY REG
tQuartE         =       1Ah         ; 5TH TERM OF QUART POLY REG
tMedX1          =       1Bh         ; x1 FOR MED-MED
tMedX2          =       1Ch         ; x2 FOR MED-MED
tMedX3          =       1Dh         ; x3 FOR MED-MED
tMedY1          =       1Eh         ; y1 FOR MED-MED
tMedY2          =       1Fh         ; y2 FOR MED-MED
tMedY3          =       20h         ; y3 FOR MED-MED
;
tRecurn		=       21h		; RECURSION N
tStatP          =       22h
tStatZ          =       23h
tStatT          =       24h
tStatChi        =       25h
tStatF          =       26h
tStatDF         =       27h
tStatPhat       =       28h
tStatPhat1      =       29h
tStatPhat2      =       2Ah
tStatMeanX1     =       2Bh
tStatStdX1      =       2Ch
tStatN1         =       2Dh
tStatMeanX2     =       2Eh
tStatStdX2      =       2Fh
tStatN2         =       30h
tStatStdXP      =       31h
tStatLower      =       32h
tStatUpper      =       33h
tStat_s         =       34h
tLRSqr          =       35h         ; r^2
tBRSqr          =       36h         ; R^2
;
;======================================================================
;       These next tokens are only used to access the data
;       they are display only and the user cannot access them at all
;======================================================================
;
tF_DF    =     37h       ; ANOFAV FACTOR DF
tF_SS    =     38h       ; ANOFAV FACTOR SS
tF_MS    =     39h       ; ANOFAV FACTOR MS
tE_DF    =     3Ah       ; ANOFAV ERROR DF
tE_SS    =     3Bh       ; ANOFAV ERROR SS
tE_MS    =     3Ch       ; ANOFAV ERROR MS
;
;
;======================================================================
;           2ND HALF OF SYSTEM INPUT/OUTPUT VARIABLES
;======================================================================
;      SYSTEM VARIABLE =ATES
;
tuXscl       =  0
tuYscl       =  1
tXscl        =  2
tYscl        =  3
tRecuru0     =  4        ; U 1ST INITIAL COND
tRecurv0     =  5        ; V 1ST INITIAL COND
tun1         =  6        ; U(N-1)    ; NOT USED
tvn1         =  7        ; V(N-1)    ; NOT USED
tuRecuru0    =  8        ;
tuRecurv0    =  9        ;
;
tXmin        =  0Ah
tXmax        =  0Bh
tYmin        =  0Ch
tYmax        =  0Dh
tTmin        =  0Eh
tTmax        =  0Fh
tThetaMin    =  10h
tThetaMax    =  11h
tuXmin       =  12h
tuXmax       =  13h
tuYmin       =  14h
tuYmax       =  15h
tuThetMin    =  16h
tuThetMax    =  17h
tuTmin       =  18h
tuTmax       =  19h
tTblMin      =  1Ah
tPlotStart   =  1Bh
tuPlotStart  =  1Ch
tnMax        =  1Dh
tunMax       =  1Eh
tnMin        =  1Fh
tunMin       =  20h
;
tTblStep	=  21h
tTStep		=  22h
tThetaStep	=  23h
tuTStep		=  24h
tuThetStep	=  25h
;
tDeltaX		=  26h
tDeltaY		=  27h
;
tXFact		=  28h
tYFact		=  29h
;
tTblInput       =       2Ah
;
tFinN           =       2Bh
tFinI           =       2Ch
tFinPV          =       2Dh
tFinPMT         =       2Eh
tFinFV          =       2Fh
tFinPY          =       30h
tFinCY          =       31h
;
tRecurw0        =       32h         ; w0(1)
tuRecurw0       =       33h
;
tPlotStep       =       34h
tuPlotStep      =       35h
;
tXres           =       36h
tuXres          =       37h
;
tRecuru02       =       38h         ; u0(2)
tuRecuru02      =       39h
tRecurv02       =       3Ch         ; v0(2)
tuRecurv02      =       3Dh
tRecurw02       =       3Eh         ; w0(2)
tuRecurw02      =       3Fh
;
;======================================================================
;           2nd byte of t2ByteTok tokens
;======================================================================
;
tFinNPV         =       00h         ;
tFinIRR         =       01h         ;
tFinBAL         =       02h         ;
tFinPRN         =       03h         ;
tFinINT         =       04h         ;
tFinToNom       =       05h         ;
tFinToEff       =       06h         ;
tFinDBD         =       07h         ;
tLCM            =       08h         ;
tGCD            =       09h         ;
tRandInt        =       0Ah         ;
tRandBin        =       0Bh         ;
tSubStrng       =       0Ch         ;
tStdDev         =       0Dh         ;
tVariance       =       0Eh         ;
tInStrng        =       0Fh         ;
tDNormal        =       10h         ;
tInvNorm        =       11h         ;
tDT             =       12h         ;
tChI            =       13h         ;
tDF             =       14h         ;
tBINPDF         =       15h         ;
tBINCDF         =       16h         ;
tPOIPDF         =       17h         ;
tPOICDF         =       18h         ;
tGEOPDF         =       19h         ;
tGEOCDF         =       1Ah         ;
tNormalPDF      =       1Bh         ;
tTPDF           =       1Ch         ;
tChiPDF         =       1Dh         ;
tFPDF           =       1Eh         ;
tRandNorm       =       1Fh         ;
tFinFPMT        =       20h         ;
tFinFI          =       21h         ;
tFinFPV         =       22h         ;
tFinFN          =       23h         ;
tFinFFV         =       24h         ;
tConj           =       25h         ;
tReal           =       26h         ;
tImag           =       27h         ;
tAngle          =       28h         ;
tCumSum         =       29h         ;
tExpr           =       2Ah         ;
tLength         =       2Bh         ;
tDeltaLst       =       2Ch         ;
tRef            =       2Dh         ;
tRRef           =       2Eh         ;
tToRect         =       2Fh         ;
tToPolar        =       30h         ;
tConste         =       31h         ;
tSinReg         =       32h         ;
tLogistic       =       33h         ;
tLinRegTTest    =       34h         ;
tShadeNorm      =       35h         ;
tShadeT         =       36h         ;
tShadeChi       =       37h         ;
tShadeF         =       38h         ;
tMatToLst       =       39h         ;
tLstToMat       =       3Ah         ;
tZTest          =       3Bh         ;
tTTest          =       3Ch         ;
t2SampZTest     =       3Dh         ;
t1PropZTest     =       3Eh         ;
t2PropZTest     =       3Fh         ;
tChiTest        =       40h         ;
tZIntVal        =       41h         ;
t2SampZInt      =       42h         ;
t1PropZInt      =       43h         ;
t2PropZInt      =       44h         ;
tGraphStyle     =       45h         ;
t2SampTTest     =       46h         ;
t2SampFTest     =       47h         ;
tTIntVal        =       48h         ;
t2SampTInt      =       49h         ;
tSetupLst       =       4Ah         ;
tFinPMTend      =       4Bh         ;
tFinPMTbeg      =       4Ch         ;
tRealM          =       4Dh         ;
tPolarM         =       4Eh         ;
tRectM          =       4Fh         ;
tExprOn         =       50h         ;
tExprOff        =       51h         ;
tClrAllLst      =       52h         ;
tGetCalc        =       53h         ;
tDelVar         =       54h         ;
tEquToStrng     =       55h         ;
tStrngToEqu     =       56h         ;
tDelLast        =       57h         ;
tSelect         =       58h         ;
tANOVA          =       59h         ;
tModBox         =       5Ah         ;
tNormProb       =       5Bh         ;
;
;
tMGT            =       64h         ; VERTICAL SPLIT
tZFit           =       65h         ; ZOOM FIT
tDiag_on        =       66h         ; DIANOSTIC DISPLAY ON
tDiag_off       =       67h         ; DIANOSTIC DISPLAY OFF
tOkEnd2v0       =       67h         ;end of 2byte tokens for version 0.
tArchive        =       68h         ;archive
tUnarchive      =       69h         ;unarchive
tasm		=	  6Ah
tasmComp	=	  6Bh	      ; asm compile
tasmPrgm	=	  6Ch	      ; signifies a program is asm
tasmCmp 	=	  6Dh	      ; asm program is compiled
;
tLcapAAcute     =       6Eh
tLcapAGrave     =       6Fh
tLcapACaret     =       70h
tLcapADier      =       71h
tLaAcute        =       72h
tLaGrave        =       73h
tLaCaret        =       74h
tLaDier         =       75h
tLcapEAcute     =       76h
tLcapEGrave     =       77h
tLcapECaret     =       78h
tLcapEDier      =       79h
tLeAcute        =       7Ah
tLeGrave        =       7Bh
tLeCaret        =       7Ch
tLeDier         =       7Dh
;
tLcapIGrave     =       7Fh
tLcapICaret     =       80h
tLcapIDier      =       81h
tLiAcute        =       82h
tLiGrave        =       83h
tLiCaret        =       84h
tLiDier         =       85h
tLcapOAcute     =       86h
tLcapOGrave     =       87h
tLcapOCaret     =       88h
tLcapODier      =       89h
tLoAcute        =       8Ah
tLoGrave        =       8Bh
tLoCaret        =       8Ch
tLoDier         =       8Dh
tLcapUAcute     =       8Eh
tLcapUGrave     =       8Fh
tLcapUCaret     =       90h
tLcapUDier      =       91h
tLuAcute        =       92h
tLuGrave        =       93h
tLuCaret        =       94h
tLuDier         =       95h
tLcapCCed       =       96h
tLcCed          =       97h
tLcapNTilde     =       98h
tLnTilde        =       99h
tLaccent        =       9Ah
tLgrave         =       9Bh
tLdieresis      =       9Ch
tLquesDown      =       9Dh
tLexclamDown    =       9Eh
tLalpha         =       9Fh
tLbeta          =      0a0h
tLgamma         =      0a1h
tLcapDelta      =      0a2h
tLdelta         =      0a3h
tLepsilon       =      0a4h
tLlambda        =      0a5h
tLmu            =      0a6h
tLpi            =      0a7h
tLrho           =      0a8h
tLcapSigma      =      0a9h
;
;
tLphi           =      0aBh
tLcapOmega      =      0aCh
tLphat          =      0aDh
tLchi           =      0aEh
tLstatF         =      0aFh
;

tLa      =     0b0h
tLb      =     0b1h
tLc      =     0b2h
tLd      =     0b3h
tLsmalle =     0b4h
tLf      =     0b5h
tLsmallg =     0b6h
tLh      =     0b7h
tLi      =     0b8h
tLj      =     0b9h
tLk      =     0bAh
;

tLl      =     0bCh
tLm      =     0bDh
tLn      =     0bEh
tLo      =     0bFh
tLp      =     0c0h
tLq      =     0c1h
tLsmallr =     0c2h
tLs      =     0c3h
tLsmallt =     0c4h
tLu      =     0c5h
tLv      =     0c6h
tLw      =     0c7h
tLx      =     0c8h
tLy      =     0c9h
tLz      =     0cAh
tLsigma         =      0cbh
tLtau           =      0cch
tLcapIAcute     =      0cdh
;
;
tGarbagec =    0Ceh
LastToken       =      0Ceh         ;tLAST TOKEN IN THIS VERSION...
;
;======================================================================
;           Data Type Equates                                                    
;======================================================================
;
RealObj         =       0
ListObj         =       1
MatObj          =       2
EquObj          =       3
StrngObj        =       4
ProgObj         =       5
ProtProgObj     =       6
PictObj         =       7
GDBObj          =       8
UnknownObj      =       9
UnknownEquObj   =       0Ah
NewEquObj       =       0Bh
CplxObj         =       0Ch
CListObj        =       0Dh
UndefObj        =       0Eh
WindowObj       =       0Fh
ZStoObj         =       10h
TblRngObj       =       11h
LCDObj          =       12h
BackupObj       =       13h
AppObj		=	  14h		;application, only used in menus/link
AppVarObj	=	  15h		;application variable
TempProgObj     =       16h           ;program, home deletes when finished
GroupObj        =       17h           ;group.
; 
;======================================================================
;	    Parser Equates
;======================================================================
OPRand      =              tRand
OPProd      =              tProd
OPSum       =              tSum
OPAdd       =              tAdd
OPSub       =              tSub
OPMult      =              tMul
OPDiv       =              tDiv
OPPower     =              tPower
OPXRoot     =              tXRoot
OPEq        =              tEQ
OPNeq       =              tNE
OPRound2    =              tRound
OPAug       =              tAug
OPMax       =              tMax
OPMin       =              tMin
OPAnd       =              tAnd
OPOr        =              tOr
OPXor       =              tXor
OPNcr       =              tnCr
OPNpr       =              tnPr
OPLt        =              tLT
OPLe        =              tLE
OPGt        =              tGT
OPGe        =              tGE
;
OPLog       =              tLog
OPTenX      =              tALog
OPLn        =              tLn
OPEtoX      =              tExp
OPNot       =              tNot
OPSin       =              tSin
OPASin      =              tASin
OPCos       =              tCos
OPACos      =              tACos
OPTan       =              tTan
OPATan      =              tATan
OPSinH      =              tSinH
OPASinH     =              tASinH
OPCosH      =              tCosH
OPACosH     =              tACosH
OPTanH      =              tTanH
OPATanH     =              tATanH
OPInverse   =              tRecip
OPDet       =              tDet
OPSqroot    =              tSqrt
OPSquare    =              tSqr
OPNegate    =              tChs
OPIpart     =              tIPart
OPFpart     =              tFPart
OPIntgr     =              tInt
OPFact      =              tFact
OPAbs       =              tAbs
OPIdent     =              tIdent
OPTranspose =              tTrnspos
OPSortA     =              tSortA
OPSortD     =              tSortD
OPToDec     =              tToDEC
OPToFrac    =              tToAbc
OPRadd      =              tRowPlus
OPRmlt      =              tmRow
OPRswap     =              tRowSwap
OPMltRadd   =              tmRowPlus
OPRand1     =              tRandM
OPSolve     =              tRoot
OPDeriv81   =              tNDeriv
OPQuad      =              tFnInt
OPSeq       =              tSeries
OPDim       =              tDim
OPRad       =              tFromRad
OPDeg       =              tFromDeg
OPRtopr     =              tRToPr
OPRtopo     =              tRToPo
OPPtorx     =              tPToRx
OPPtory     =              tPToRy
OPCube      =              tCube
OPCbrt      =              tCubRt
OPFmin      =              tFmin
OPFmax      =              tFmax
OPNpv           =       tIf         ; immediate unary
OPIrr           =       tIf+1       ; immediate unary
OPBal           =       tIf+2       ; immediate unary
OPPrn           =       tIf+3       ; immediate unary
OPIntr          =       tIf+4       ; immediate unary
OPToNom         =       tIf+5       ; immediate unary
OPToEff         =       tIf+6       ; immediate unary
OPFinDbd           =       tIf+7       ; immediate unary
OPLcm           =       tIf+8       ; immediate unary
OPGcd           =       tIf+9       ; immediate unary
OPRandInt       =       tIf+10      ; immediate unary
OPRandBin        =      tIf+11      ; immediate unary
OPSubstr        =       tIf+12      ; immediate unary
OPstDev         =       tIf+13      ; immediate unary
OPVariance      =       tIf+14      ; immediate unary
OPInstr         =       tIf+15      ; immediate unary
OPNormal        =       tIf+16      ; immediate unary
OPINormal       =       tIf+17      ; immediate unary
OPDt            =       tIf+18      ; immediate unary
OPChi           =       tIf+19      ; immediate unary
OPDf            =       tIf+20      ; immediate unary
OPBinpdf        =       tIf+21      ; immediate unary
OPBincdf        =       tIf+22      ; immediate unary
OPPoipdf        =       tIf+23      ; immediate unary
OPPoicdf        =       tIf+24      ; immediate unary
OPGeopdf        =       tIf+25      ; immediate unary
OPGeocdf        =       tIf+26      ; immediate unary
OPNormalpdf     =       tIf+27      ; immediate unary
OPTpdf          =       tIf+28      ; immediate unary
OPChipdf        =       tIf+29      ; immediate unary
OPFpdf          =       tIf+30      ; immediate unary
OPRandNrm       =       tIf+31      ; immediate unary
OPConj           =       tZoomIn
OPReal           =       tZoomIn	   +1
OPImag           =       tZoomIn	   +2
OPAngle          =       tZoomIn	   +3
OPCumSum         =       tZoomIn	   +4
OPExpr           =       tZoomIn	   +5
OPLength         =       tZoomIn	   +6
OPDeltaLst       =       tZoomIn	   +7
OPRef            =       tZoomIn	   +8
OPRref           =       tZoomIn	   +9
OPConst          =       tZoomIn	   +10

;======================================================================
;           I/O equates                                                          
;======================================================================
;
D0D1_bits	=  03h
D0LD1L		=  03h
D0LD1H		=  01h
D0HD1L		=  02h
D0HD1H		=  00h
bport		=  0			; 4-BIT LINK PORT (I/O) 
;
;======================================================================
;	Interrupt equates
;======================================================================
;
iAll	= 1011b
intrptEnPort	equ 3 ; interrupt enable (in/out) 
;
;======================================================================
;	Memory paging equates
;======================================================================
;
memPageAPort	equ 6 ; memory page A (in/out)
;
;======================================================================
;	LCD Driver equates
;======================================================================
LCDinstPort	equ 10h
LCDdataPort equ 11h
;
;======================================================================
;           DEVICE CODES                                                       
;======================================================================
;
;
TI82DEV		=  82h
PC82DEV		=  02h
MAC82DEV	=  12h
;
TI83FDEV	=  73h
LINK83FDEV	=  23h
;
TI83DEV		=  83h
PC83DEV		=  03h
MAC83DEV	=  13h
;
TI85DEV		=  95h      ; DIFF THEN REAL 85 SO ME TALK
PC85DEV		=  05h
MAC85DEV	=  15h
;
TI73DEV		=  74h      ; Device x3 is always an 83
PC73DEV		=  07h
MAC73DEV	=  17h
;
TI83FDEV	=  73h
LINK73FDEV	=  23h
PC83FDEV	=  23h
;
;======================================================================
;           System Error Codes                                                  
;======================================================================
;
E_EDITF		=	7		;allow re-entering application
E_EDIT		=	1<<E_EDITF
E_Mask		=	7Fh

E_Overflow	=	1+E_EDIT
E_DivBy0	=	2+E_EDIT
E_SingularMat	=	3+E_EDIT
E_Domain	=	4+E_EDIT
E_Increment	=	5+E_EDIT
E_Break		=	6+E_EDIT
E_NonReal       =     7+E_EDIT
E_Syntax        =     8+E_EDIT
E_DataType      =     9+E_EDIT
E_Argument      =     10+E_EDIT
E_DimMismatch   =     11+E_EDIT
E_Dimension     =     12+E_EDIT
E_Undefined     =     13+E_EDIT
E_Memory        =     14+E_EDIT
E_Invalid       =     15+E_EDIT
E_IllegalNest   =     16+E_EDIT
E_Bound         =     17+E_EDIT
E_GraphRange    =     18+E_EDIT
E_Zoom          =     19+E_EDIT
E_Label         =     20
E_Stat          =     21
E_Solver        =     22+E_EDIT
E_Singularity   =     23+E_EDIT
E_SignChange    =     24+E_EDIT
E_Iterations    =     25+E_EDIT
E_BadGuess      =     26+E_EDIT
E_StatPlot      =     27
E_TolTooSmall   =     28+E_EDIT
E_Reserved      =     29+E_EDIT
E_Mode          =     30+E_EDIT
E_LnkErr        =     31+E_EDIT
E_LnkMemErr     =     32+E_EDIT
E_LnkTransErr   =     33+E_EDIT
E_LnkDupErr     =     34+E_EDIT
E_LnkMemFull    =     35+E_EDIT
E_Unknown       =     36+E_EDIT

E_Scale         =     37+E_EDIT
E_IdNotFound    =     38
E_NoMode        =     39+E_EDIT
E_Validation    =     40
E_Length        =     41+E_EDIT
E_Application   =     42+E_EDIT
E_AppErr1       =     43+E_EDIT
E_AppErr2       =     44+E_EDIT
E_ExpiredApp	=	45
E_BadAddr	=	46
E_Archived      =     47+E_EDIT
E_Version       =     48
E_ArchFull      =     49
E_Variable      =     50+E_EDIT
E_Duplicate     =     51+E_EDIT
HigErrNum       =     51


; obsolete error numbers34              ;first LINK error
E_LinkIOChkSum	=	34
E_LinkIOTimeOut	=	35
E_LinkIOBusy	=	36
E_LinkIOVer	=	37

;
;======================================================================
;           =ATES TO RAM LOCATIONS FOR STAT VARS 
;======================================================================
;
;
FPLEN	=	9		;Length of a floating-point number.
StatN   =     statVars
XMean   =     StatN      + FPLEN
SumX    =     XMean      + FPLEN
SumXSqr =     SumX       + FPLEN
StdX    =     SumXSqr    + FPLEN
StdPX   =     StdX       + FPLEN
MinX    =     StdPX      + FPLEN
MaxX    =     MinX       + FPLEN
MinY    =     MaxX       + FPLEN
MaxY    =     MinY       + FPLEN
YMean   =     MaxY       + FPLEN
SumY    =     YMean      + FPLEN
SumYSqr =     SumY       + FPLEN
StdY    =     SumYSqr    + FPLEN
StdPY   =     StdY       + FPLEN
SumXY   =     StdPY      + FPLEN
Corr    =     SumXY      + FPLEN
MedX    =     Corr       + FPLEN
Q1      =     MedX       + FPLEN
Q3      =     Q1         + FPLEN
QuadA   =     Q3         + FPLEN
QuadB   =     QuadA      + FPLEN
QuadC   =     QuadB      + FPLEN
CubeD   =     QuadC      + FPLEN
QuartE  =     CubeD      + FPLEN
MedX1   =     QuartE     + FPLEN
MedX2   =     MedX1      + FPLEN
MedX3   =     MedX2      + FPLEN
MedY1   =     MedX3      + FPLEN
MedY2   =     MedY1      + FPLEN
MedY3   =     MedY2      + FPLEN
PStat   =     MedY3      + 2*FPLEN
ZStat   =     PStat      + FPLEN
TStat   =     ZStat      + FPLEN
ChiStat =     TStat      + FPLEN
FStat   =     ChiStat    + FPLEN
DF      =     FStat      + FPLEN
Phat    =     DF         + FPLEN
Phat1   =     Phat       + FPLEN
Phat2   =     Phat1      + FPLEN
MeanX1  =     Phat2      + FPLEN
StdX1   =     MeanX1     + FPLEN
StatN1  =     StdX1      + FPLEN
MeanX2  =     StatN1     + FPLEN
StdX2   =     MeanX2     + FPLEN
StatN2  =     StdX2      + FPLEN
StdXP2  =     StatN2     + FPLEN
SLower  =     StdXP2     + FPLEN
SUpper  =     SLower     + FPLEN
SStat   =     SUpper     + FPLEN

F_DF	=     anovaf_vars
F_SS	=     F_DF + FPLEN
F_MS	=     F_SS + FPLEN
E_DF	=     F_MS + FPLEN
E_SS	=     E_DF + FPLEN
E_MS	=     E_SS + FPLEN


;======================================================================
;======================================================================
;  The following is a duplicate of all of the above equates
;  in lower case.  
;
;  It may be deleted if you choose case insensitive development
;  tools or choose to follow the above conventions.
;
;
;======================================================================
;
;======================================================================
;           system variable equates
;======================================================================
;       entry points : rclsystok, stosystok
;
xsclt		equ  2
ysclt		equ  3
xmint		equ  0ah
xmaxt		equ  0bh
ymint		equ  0ch
ymaxt		equ  0dh
tmint		equ  0eh
tmaxt		equ  0fh
thetmint	equ  10h
thetmaxt	equ  11h
tblmint		equ  1ah
plotstartt	equ  1bh
nmaxt		equ  1dh
nmint		equ  1fh
tblstept	equ  21h
tstept		equ  22h
thetstept	equ  23h
deltaxt		equ  26h
deltayt		equ  27h
xfactt		equ  28h
yfactt		equ  29h
finnt		equ  2bh
finit		equ  2ch
finpvt		equ  2dh
finpmtt		equ  2eh
finfvt		equ  2fh
finpyt		equ  30h
fincyt		equ  31h
plotstept	equ  34h
xrest		equ  36h
;

;======================================================================
;           run indicators
;======================================================================
;
busypause	equ	10101010b
busynormal	equ	11110000b
;
;======================================================================
; macro to call base code from an app,
;======================================================================
;
rbr_call	equ	28h		; needed for apps
brt_jump0	equ	50h
;
b_call macro   label
	     rst     rbr_call
	     dw      _&label
       .endm
;
b_jump macro   label
	     call    brt_jump0	     ; fixed location on page 0
	     dw      _&label
       .endm

;======================================================================
;           common subroutine rst numbers  
;======================================================================
;
rop1toop2	equ  08h
rfindsym	equ  10h
rpushrealo1	equ  18h
rmov9toop1	equ  20h
rfpadd		equ  30h
;

;symbolic_debug_start
;======================================================================
;           entry points  
;======================================================================
;
_abso1o2cp	equ 410eh
_abso1pabso2	equ 405ah
_acos		equ 40deh
_acosh		equ 40f0h
_acosrad	equ 40d2h
_adrlele	equ 462dh
_adrmele	equ 4609h
_adrmrow	equ 4606h
_alleq		equ 4876h
_allocfps	equ 43a5h
_allocfps1	equ 43a8h
_angle		equ 4102h
_ansname	equ 4b52h
_apdsetup	equ 4c93h
_appgetcalc	equ 4c78h
_appgetcbl	equ 4c75h
_appinit	equ 404bh
_arc_unarc	equ 4fd8h
_asin		equ 40e4h
_asinh		equ 40edh
_asinrad	equ 40dbh
_atan		equ 40e1h
_atan2		equ 40e7h
_atan2rad	equ 40d8h
_atanh		equ 40eah
_atanrad	equ 40d5h

_binopexec	equ 4663h
_bit_vertsplit	equ 4fa8h
_bufclr		equ 5074h
_bufcpy		equ 5071h

_cabs		equ 4e97h
_cadd		equ 4e88h
_canalphins	equ 4c69h
_cdiv		equ 4e94h
_cdivbyreal	equ 4ebbh
_cetox		equ 4ea9h
_cfrac		equ 4ec1h
_checksplitflag	equ 49f0h
_chk_batt_low	equ 50B3h
_chkfindsym	equ 42f1h
_cintgr		equ 4ec4h
_circcmd	equ 47d4h
_ckint		equ 4234h
_ckodd		equ 4237h
_ckop1c0	equ 4225h
_ckop1cplx	equ 40fch
_ckop1fp0	equ 4228h
_ckop1pos	equ 4258h
_ckop1real	equ 40ffh
_ckop2fp0	equ 422bh
_ckop2pos	equ 4255h
_ckop2real	equ 42dfh
_ckposint	equ 4231h
_ckvalidnum	equ 4270h
_cleanall	equ 4a50h
_clearrect	equ 4d5ch
_clearrow	equ 4cedh
_cline		equ 4798h
_clines		equ 479bh
_cln		equ 4ea0h
_clog		equ 4ea3h
_closeeditbuf	equ 48D3h
_closeeditbufnor	equ 476eh
_closeeditequ	equ 496Ch
_closeprog	equ 4a35h
_clrgraphref	equ 4a38h
_clrlcd		equ 4543h
_clrlcdfull	equ 4540h
_clrlp		equ 41d1h
_clrop1s	equ 425eh
_clrop2s	equ 425bh
_clrscrn	equ 4549h
_clrscrnfull	equ 4546h
_clrtxtshd	equ 454ch
_cmltbyreal	equ 4eb8h
_cmpsyms	equ 4a4ah
_cmult		equ 4e8eh
_conj		equ 4eb5h
_convdim	equ 4b43h
_convdim00	equ 4b46h
_convlctolr	equ 4a23h
_convlrtolc	equ 4a56h
_convop1	equ 4aefh
_cop1set0	equ 4105h
_cos		equ 40c0h
_cosh		equ 40cch
_cphlde		equ 400ch
_cpoint		equ 4dc8h
_cpoints	equ 47f5h
_cpop1op2	equ 4111h
_cpop4op3	equ 4108h
_cpyo1tofps1	equ 445ch
_cpyo1tofps2	equ 446bh
_cpyo1tofps3	equ 4477h
_cpyo1tofps4	equ 4489h
_cpyo1tofps5	equ 4483h
_cpyo1tofps6	equ 447dh
_cpyo1tofps7	equ 4480h
_cpyo1tofpst	equ 444ah
_cpyo2tofps1	equ 4459h
_cpyo2tofps2	equ 4462h
_cpyo2tofps3	equ 4474h
_cpyo2tofps4	equ 4486h
_cpyo2tofpst	equ 4444h
_cpyo3tofps1	equ 4453h
_cpyo3tofps2	equ 4465h
_cpyo3tofpst	equ 4441h
_cpyo5tofps1	equ 4456h
_cpyo5tofps3	equ 4471h
_cpyo6tofps2	equ 4468h
_cpyo6tofpst	equ 4447h
_cpystack	equ 4429h
_cpyto1fps1	equ 4432h
_cpyto1fps10	equ 43f3h
_cpyto1fps11	equ 43d8h
_cpyto1fps2	equ 443bh
_cpyto1fps3	equ 4408h
_cpyto1fps4	equ 440eh
_cpyto1fps5	equ 43deh
_cpyto1fps6	equ 43e4h
_cpyto1fps7	equ 43eah
_cpyto1fps8	equ 43edh
_cpyto1fps9	equ 43f6h
_cpyto1fpst	equ 4423h
_cpyto2fps1	equ 442fh
_cpyto2fps2	equ 4438h
_cpyto2fps3	equ 4402h
_cpyto2fps4	equ 43f9h
_cpyto2fps5	equ 43dbh
_cpyto2fps6	equ 43e1h
_cpyto2fps7	equ 43e7h
_cpyto2fps8	equ 43f0h
_cpyto2fpst	equ 4420h
_cpyto3fps1	equ 442ch
_cpyto3fps2	equ 4411h
_cpyto3fpst	equ 441dh
_cpyto4fpst	equ 441ah
_cpyto5fpst	equ 4414h
_cpyto6fps2	equ 43ffh
_cpyto6fps3	equ 43fch
_cpyto6fpst	equ 4417h
_cpytofps1	equ 445fh
_cpytofps2	equ 446eh
_cpytofps3	equ 447ah
_cpytofpst	equ 444dh
_cpytostack	equ 4450h
_create0equ	equ 432ah
_createappvar	equ 4e6ah
_createclist	equ 431bh
_createcplx	equ 430ch
_createequ	equ 4330h
_createpair	equ 4b0dh
_createpict	equ 4333h
_createprog	equ 4339h
_createprotprog	equ 4e6dh
_createreal	equ 430fh
_createrlist	equ 4315h
_creatermat	equ 4321h
_createstrng	equ 4327h
_crecip		equ 4e91h
_csqroot	equ 4e9dh
_csquare	equ 4e8bh
_csub		equ 4e85h
_ctenx		equ 4ea6h
_ctrunc		equ 4ebeh
_cube		equ 407bh
_cursoroff	equ 45beh
_cursoron	equ 45c4h
_cxrooty	equ 4each
_cytox		equ 4eb2h

_darkline	equ 47ddh
_darkpnt	equ 47f2h
_datasize	equ 436ch
_datasizea	equ 4369h
_deallocfps	equ 439fh
_deallocfps1	equ 43a2h
_deco1exp	equ 4267h
_dellistel	equ 4a2fh
_delmem		equ 4357h
_delres		equ 4a20h
_delvar		equ 4351h
_delvararc	equ 4fc6h
_delvarnoarc	equ 4fc9h
_disableapd	equ 4c84h
_disp		equ 4f45h
_dispdone	equ 45b5h
_dispeol	equ 45a6h
_disphl		equ 4507h
_displayimage	equ 4d9bh
_dispmenutitle	equ 5065h
_dispop1a	equ 4bf7h
_divhlby10	equ 400fh
_divhlbya	equ 4012h
_drawcirc2	equ 4c66h
_drawcmd	equ 48c1h
_drawrectborder	equ 4d7dh
_drawrectborderclear	equ 4d8ch
_dtor		equ 4075h

_editprog	equ 4a32h
_enableapd	equ 4c87h
_enoughmem	equ 42fdh
_eop1notreal	equ 4279h
_equ_or_newequ	equ 42c4h
_eraseeol	equ 4552h
_eraserectborder	equ 4d86h
_errargument	equ 44adh
_errbadguess	equ 44cbh
_errbreak	equ 44bfh
_errd_op1_0	equ 42d3h
_errd_op1_le_0	equ 42d0h
_errd_op1not_r	equ 42cah
_errd_op1notpos	equ 42c7h
_errd_op1notposint	equ 42cdh


_errdatatype	equ 44aah
_errdimension	equ 44b3h
_errdimmismatch	equ 44b0h
_errdivby0	equ 4498h
_errdomain	equ 449eh
_errincrement	equ 44a1h
_errinvalid	equ 44bch
_erriterations	equ 44c8h
_errlinkxmit	equ 44d4h
_errmemory	equ 44b9h
_errnon_real	equ 44a4h
_errnonreal	equ 4a8ch
_errnotenoughmem	equ 448ch
_erroverflow	equ 4495h
_errsignchange	equ 44c5h
_errsingularmat	equ 449bh
_errstat	equ 44c2h
_errstatplot	equ 44d1h
_errsyntax	equ 44a7h
_errtoltoosmall	equ 44ceh
_errundefined	equ 44b6h
_etox		equ 40b4h
_exch9		equ 43d5h
_exlp		equ 4222h
_exptohex	equ 424fh

_factorial	equ 4b85h
_fillrect	equ 4d62h
_fillrectpattern	equ 4d89h
_find_parse_formula	equ 4af2h
_findalphadn	equ 4a47h
_findalphaup	equ 4a44h
_findapp		equ 4c4eh
_findappdn		equ 4c4bh
_findappnumpages	equ 509bh
_findappup		equ 4c48h
_findsym	equ 42f4h
_fiveexec	equ 467eh
_fixtempcnt	equ 4a3bh
_flashtoram	equ 5017h
_forcefullscreen	equ 508fh
_formbase	equ 50aah
_formdcplx	equ 4996h
_formereal	equ 4990h
_formreal	equ 4999h
_fourexec	equ 467bh
_fpadd		equ 4072h
_fpdiv		equ 4099h
_fpmult		equ 4084h
_fprecip	equ 4096h
_fpsquare	equ 4081h
_fpsub		equ 406fh
_frac		equ 4093h

_get_tok_strng	= 4594h
_getbasever	equ 4c6fh
_getcsc		equ 4018h
_getkey		equ 4972h
_getltoop1	equ 4636h
_getmtoop1	equ 4615h
_getsysinfo equ 50ddh ; v.1.13+ only
_gettoklen	equ 4591h
_grbufclr	equ 4bd0h
_grbufcpy	equ 486ah
_grphcirc	equ 47d7h

_hltimes9	equ 40f9h
_horizcmd	equ 48a6h
_htimesl	equ 4276h

_ibounds	equ 4c60h
_iboundsfull	equ 4d98h
_iline		equ 47e0h
_inclstsize	equ 4a29h
_insertlist	equ 4a2ch
_insertmem	equ 42f7h
_int		equ 40a5h
_intgr		equ 405dh
_invcmd		equ 48c7h
_invertrect	equ 4d5fh
_invop1s	equ 408dh
_invop1sc	equ 408ah
_invop2s	equ 4090h
_invsub		equ 4063h
_ioffset	equ 4c63h
_ipoint		equ 47e3h
_isa2bytetok	equ 42A3h
_iseditempty	equ 492Dh

_jerror		equ 44d7h
_jerrorno	equ 4000h
_jforcecmdnochar	equ 4027h
_jforcegraphkey	equ 5005h
_jforcegraphnokey	equ 5002h

_keytostring	equ 45cah

_lcd_busy	equ 4051h
_lcd_driveron	equ 4978h
_ldhlind	equ 4009h
_linecmd	equ 48ach
_lnx		equ 40abh
_load_sfont	equ 4783h
_loadcindpaged	equ 501dh
_loaddeindpaged	equ 501ah
_loadpattern	equ 4cb1h
_logx		equ 40aeh

_max		equ 4057h
_memchk		equ 42e5h
_memclear	equ 4c30h
_memset		equ 4c33h
_min		equ 4054h
_minus1		equ 406ch
_mov10b		equ 415ch
_mov18b		equ 47dah
_mov7b		equ 4168h
_mov8b		equ 4165h
_mov9b		equ 415fh
_mov9op1op2	equ 417dh
_mov9op2cp	equ 410bh
_mov9toop1	equ 417ah
_mov9toop2	equ 4180h
_movfrop1	equ 4183h

_newline	equ 452eh
_nzif83plus	equ 50e0h  ; v. 1.13+ only

_onevar		equ 4ba3h
_op1exop2	equ 421fh
_op1exop3	equ 4219h
_op1exop4	equ 421ch
_op1exop5	equ 420dh
_op1exop6	equ 4210h
_op1exptodec	equ 4252h
_op1set0	equ 41bfh
_op1set1	equ 419bh
_op1set2	equ 41a7h
_op1set3	equ 41a1h
_op1set4	equ 419eh
_op1toop2	equ 412fh
_op1toop3	equ 4123h
_op1toop4	equ 4117h
_op1toop5	equ 4153h
_op1toop6	equ 4150h
_op2exop4	equ 4213h
_op2exop5	equ 4216h
_op2exop6	equ 4207h
_op2set0	equ 41bch
_op2set1	equ 41adh
_op2set2	equ 41aah
_op2set3	equ 4198h
_op2set4	equ 4195h
_op2set5	equ 418fh
_op2set60	equ 4ab0h
_op2set8	equ 418ch
_op2seta	equ 4192h
_op2toop1	equ 4156h
_op2toop3	equ 416eh
_op2toop4	equ 411ah
_op2toop5	equ 414ah
_op2toop6	equ 414dh
_op3set0	equ 41b9h
_op3set1	equ 4189h
_op3set2	equ 41a4h
_op3toop1	equ 413eh
_op3toop2	equ 4120h
_op3toop4	equ 4114h
_op3toop5	equ 4147h
_op4set0	equ 41b6h
_op4set1	equ 4186h
_op4toop1	equ 4138h
_op4toop2	equ 411dh
_op4toop3	equ 4171h
_op4toop5	equ 4144h
_op4toop6	equ 4177h
_op5exop6	equ 420ah
_op5set0	equ 41b3h
_op5toop1	equ 413bh
_op5toop2	equ 4126h
_op5toop3	equ 4174h
_op5toop4	equ 412ch
_op5toop6	equ 4129h
_op6toop1	equ 4135h
_op6toop2	equ 4132h
_op6toop5	equ 4141h
_outputexpr	equ 4bb2h

_pagedget	equ 5023h
_parseinp	equ 4a9bh
_pdspgrph	equ 48a3h
_pixeltest	equ 48b5h
_plus1		equ 4069h
_pointcmd	equ 48b2h
_pointon	equ 4c39h
_popmcplxo1	equ 436fh
_popop1		equ 437eh
_popop3		equ 437bh
_popop5		equ 4378h
_popreal	equ 4393h
_poprealo1	equ 4390h
_poprealo2	equ 438dh
_poprealo3	equ 438ah
_poprealo4	equ 4387h
_poprealo5	equ 4384h
_poprealo6	equ 4381h
_posno0int	equ 422eh
_ptor		equ 40f3h
_pushmcplxo1	equ 43cfh
_pushmcplxO3	equ 43C6h
_pushop1	equ 43c9h
_pushop3	equ 43c3h
_pushop5	equ 43c0h
_pushreal	equ 43bdh
_pushrealo1	equ 43bah
_pushrealo2	equ 43b7h
_pushrealo3	equ 43b4h
_pushrealo4	equ 43b1h
_pushrealo5	equ 43aeh
_pushrealo6	equ 43abh
_putaway	equ 4039h
_putc		equ 4504h
_putmap		equ 4501h
_putps		equ 4510h
_putpsb		equ 450dh
_puts		equ 450ah
_puttokstring	equ 4960h
_puttol		equ 4645h
_puttomat	equ 461eh

_randinit	equ 4b7fh
_random		equ 4b79h
_rcl_statvar	equ 42dch
_rclans		equ 4ad7h
_rclgdb2	equ 47d1h
_rcln		equ 4addh
_rclsystok	equ 4ae6h
_rclvarsym	equ 4ae3h
_rclx		equ 4ae0h
_rcly		equ 4adah
_rec1stbyte	equ 4efah
_rec1stbytenc	equ 4efdh
_recabyteio	equ 4f03h
_redimmat	equ 4a26h
_regraph	equ 488eh
_releasebuffer	equ 4771h
_reloadappentryvecs	equ 4c36h
_restoredisp	equ 4870h
_rname		equ 427fh
_rndguard	equ 409fh
_rnfx		equ 40a2h
_round		equ 40a8h
_rtod		equ 4078h
_rtop		equ 40f6h
_runindicoff	equ 4570h
_runindicon	equ 456dh

_savedisp	equ 4c7bh
_sendabyte	equ 4ee5h
_sendvarcmd	equ 4A14h
_setallplots	equ 4fcch
_setexspeed 	equ 50bfh ; v. 1.13+ only
_setfuncm	equ 4840h
_setnorm_vals	equ 49fch
_setparm	equ 4849h
_setpolm	equ 4846h
_setseqm	equ 4843h
_settblgraphdraw	equ 4c00h
_setuppagedptr	equ 5020h
_setxxop1	equ 478ch
_setxxop2	equ 478fh
_setxxxxop2	equ 4792h
_sfont_len	equ 4786h
_sin		equ 40bdh
_sincosrad	equ 40bah
_sinh		equ 40cfh
_sinhcosh	equ 40c6h
_sqroot		equ 409ch
_srchvlstdn	equ 4f12h
_srchvlstup	equ 4f0fh
_sstringlength	equ 4cb4h
_stmatel	equ 4ae9h
_stoans		equ 4abfh
_stogdb2	equ 47ceh
_ston		equ 4acbh
_stoother	equ 4ad4h
_stor		equ 4ac5h
_storand	equ 4b7ch
_stosystok	equ 4abch
_stot		equ 4aceh
_stotheta	equ 4ac2h
_stox		equ 4ad1h
_stoy		equ 4ac8h
_strcopy	equ 44e3h
_strlength	equ 4c3fh   

_tan		equ 40c3h
_tanh		equ 40c9h
_tanlnf		equ 48bbh
_tenx		equ 40b7h
_thetaname	equ 427ch
_threeexec	equ 4675h
_times2		equ 4066h
_timespt5	equ 407eh
_tname		equ 428eh
_tofrac		equ 4657h
_trunc		equ 4060h
_twovarset	equ 50D4h

_uclines	equ 4795h
_unlinecmd	equ 48afh
_unopexec	equ 4672h

_vertcmd	equ 48a9h
_vputmap	equ 455eh
_vputs		equ 4561h
_vputsn		equ 4564h
_vtowhlde	equ 47fbh


_xftoi		equ 4804h
_xitof		equ 47feh
_xname		equ 4288h
_xrooty		equ 479eh

_yftoi		equ 4801h
_yname		equ 428bh
_ytox		equ 47a1h

_zero16d	equ 41b0h
_zeroop		equ 41ceh
_zeroop1	equ 41c5h
_zeroop2	equ 41c8h
_zeroop3	equ 41cbh
_zmdecml	equ 484fh
_zmfit		equ 485bh
_zmint		equ 484ch
_zmprev		equ 4852h
_zmsquare	equ 485eh
_zmstats	equ 47a4h
_zmtrig		equ 4861h
_zmusr		equ 4855h
_zoodefault	equ 4867h
;
;======================================================================
;           system-defined ram variable address equates
;======================================================================
;

ramstart             equ  8000h
appdata              equ  8000h
ramcode              equ  8100h
ramcodeend           equ  822fh
baseappbrtab         equ  8230h
boottemp             equ  8251h
appsearchpage        equ  82a3h
tempswaparea         equ  82a5h
appid                equ  838dh
ramreturndata        equ  83edh
arcinfo              equ  83eeh
savedarcinfo         equ  8406h
appinfo              equ  8432h
appbank_jump         equ  843ch
apppage              equ  843eh
kbdscancode          equ  843fh
kbdkey               equ  8444h
kbdgetky             equ  8445h
keyextend            equ  8446h
contrast             equ  8447h
apdsubtimer          equ  8448h
apdtimer             equ  8449h
curtime              equ  844ah
currow               equ  844bh
curcol               equ  844ch
curoffset            equ  844dh
curunder             equ  844eh
cury                 equ  844fh
curtype              equ  8450h
curxrow              equ  8451h
prevddata            equ  8452h
lfont_record         equ  845ah
sfont_record         equ  8462h
tokvarptr            equ  846ah
toklen               equ  846ch
indicmem             equ  846eh
indiccounter         equ  8476h
indicbusy            equ  8477h
op1                  equ  8478h
op1m                 equ  847ah
op2                  equ  8483h
op2m                 equ  8485h
op2ext               equ  848ch
op3                  equ  848eh
op3m                 equ  8490h
op4                  equ  8499h
op4m                 equ  849bh
op5                  equ  84a4h
op5m                 equ  84a6h
op6                  equ  84afh
op6m                 equ  84b1h
op6ext               equ  84b8h
progtoedit           equ  84bfh
namebuff             equ  84c7h
equ_edit_save        equ  84d2h
imathptr1            equ  84d3h
imathptr2            equ  84d5h
imathptr3            equ  84d7h
imathptr4            equ  84d9h
imathptr5            equ  84dbh
chkdelptr1           equ  84ddh
chkdelptr2           equ  84dfh
insdelptr            equ  84e1h
updownptr            equ  84e3h
foutdat              equ  84e5h
asm_data_ptr1        equ  84ebh
asm_data_ptr2        equ  84edh
asm_sym_ptr1         equ  84efh
asm_sym_ptr2         equ  84f1h
asm_ram              equ  84f3h
asm_ind_call         equ  8507h
textshadow           equ  8508h
textshadcur          equ  8588h
textshadtop          equ  858ah
textshadalph         equ  858bh
textshadins          equ  858ch
cxmain               equ  858dh
cxpputaway           equ  858fh
cxputaway            equ  8591h
cxredisp             equ  8593h
cxerrorep            equ  8595h
cxsizewind           equ  8597h
cxpage               equ  8599h
cxcurapp             equ  859ah
cxprev               equ  859bh
monqh                equ  85aah
monqt                equ  85abh
monqueue             equ  85ach
onsp                 equ  85bch
promptrow            equ  85c0h
promptcol            equ  85c1h
promptins            equ  85c2h
promptshift          equ  85c3h
promptret            equ  85c4h
promptvalid          equ  85c6h
prompttop            equ  85c8h
promptcursor         equ  85cah
prompttail           equ  85cch
promptbtm            equ  85ceh
vartype              equ  85d0h
varcurrent           equ  85d1h
varclass             equ  85d9h
menuactive           equ  85dch
menuappdepth         equ  85ddh
menucurrent          equ  85deh
progcurrent          equ  85e8h
usermenusa           equ  85feh
ioprompt             equ  865fh
dimagewidth          equ  8660h
rectfillpheight      equ  8660h
rectfillpwidth       equ  8661h
rectfillpattern      equ  8662h
ioflag               equ  8670h
sndrecstate          equ  8672h
ioerrstate           equ  8673h
header               equ  8674h
iodata               equ  867dh
ionewdata            equ  8689h
bakheader            equ  868bh
pencol               equ  86d7h
penrow               equ  86d8h
rclqueue             equ  86d9h
rclqueueend          equ  86dbh
errno                equ  86ddh
errsp                equ  86deh
erroffset            equ  86e0h
savesscreen          equ  86ech
bstcounter           equ  89eeh
flags                equ  89f0h
statvars             equ  8a3ah
anovaf_vars          equ  8c17h
infvars              equ  8c4dh
curgstyle            equ  8d17h
curgy                equ  8d18h
curgx                equ  8d19h
curgy2               equ  8d1ah
curgx2               equ  8d1bh
freesavey            equ  8d1ch
freesavex            equ  8d1dh
xoffset              equ  8da1h
yoffset              equ  8da2h
lcdtallp             equ  8da3h
pixwidep             equ  8da4h
pixwide_m_1          equ  8da5h
pixwide_m_2          equ  8da6h
lastentryptr         equ  8da7h
lastentrystk         equ  8da9h
numlastentries       equ  8e29h
currlastentry        equ  8e2ah
curinc               equ  8e67h
uxmin                equ  8e7eh
uxmax                equ  8e87h
uxscl                equ  8e90h
uymin                equ  8e99h
uymax                equ  8ea2h
uyscl                equ  8eabh
uthetmin             equ  8eb4h
uthetmax             equ  8ebdh
uthetstep            equ  8ec6h
utmin                equ  8ecfh
utmax                equ  8ed8h
utstep               equ  8ee1h
uplotstart           equ  8eeah
unmax                equ  8ef3h
uu0                  equ  8efch
uv0                  equ  8f05h
unmin                equ  8f0eh
uu02                 equ  8f17h
uv02                 equ  8f20h
uw0                  equ  8f29h
uplotstep            equ  8f32h
uxres                equ  8f3bh
uw02                 equ  8f44h
xmin                 equ  8f50h
xmax                 equ  8f59h
xscl                 equ  8f62h
ymin                 equ  8f6bh
ymax                 equ  8f74h
yscl                 equ  8f7dh
thetamin             equ  8f86h
thetamax             equ  8f8fh
thetastep            equ  8f98h
tminpar              equ  8fa1h
tmaxpar              equ  8faah
tstep                equ  8fb3h
plotstart            equ  8fbch
nmax                 equ  8fc5h
u0                   equ  8fceh
v0                   equ  8fd7h
nmin                 equ  8fe0h
u02                  equ  8fe9h
v02                  equ  8ff2h
w0                   equ  8ffbh
plotstep             equ  9004h
xreso                equ  900dh
w02                  equ  9016h
un1                  equ  901fh
un2                  equ  9028h
vn1                  equ  9031h
vn2                  equ  903ah
wn1                  equ  9043h
wn2                  equ  904ch
fin_n                equ  9055h
fin_i                equ  905eh
fin_pv               equ  9067h
fin_pmt              equ  9070h
fin_fv               equ  9079h
fin_py               equ  9082h
fin_cy               equ  908bh
cal_n                equ  9094h
cal_i                equ  909dh
cal_pv               equ  90a6h
cal_pmt              equ  90afh
cal_fv               equ  90b8h
cal_py               equ  90c1h
smalleditram         equ  90d3h
xfact                equ  913fh
yfact                equ  9148h
xres_int             equ  9151h
deltax               equ  9152h
deltay               equ  915bh
shortx               equ  9164h
shorty               equ  916dh
lower                equ  9176h
upper                equ  917fh
xoutsym              equ  918ch
xoutdat              equ  918eh
youtsym              equ  9190h
youtdat              equ  9192h
inputsym             equ  9194h
inputdat             equ  9196h
prevdata             equ  9198h
tblmin               equ  92b3h
tblstep              equ  92bch
p1type		     =  92C9h
savx1list	     =  92CAh
savy1iist	     =  92CFh
savf1list	     =  92D4h
p1frqonoff	     =  92D9h
p2type		     =  92DAh
savx2list	     =  92DBh
savy2list	     =  92E0h
savf2list	     =  92E5h
p2frqonoff	     =  92EAh
p3type		     =  92EBh	
savx3list	     =  92ECh
savy3list	     =  92F1h
savf3list	     =  92F6h
p3frqonoff	     =  92FBh

plotsscreen          equ  9340h
seed1                equ  9640h
seed2                equ  9649h
cmdshadow            equ  966eh
cmdshadcur           equ  96eeh
cmdshadalph          equ  96f0h
cmdshadins           equ  96f1h
cmdcursor            equ  96f2h
edittop              equ  96f4h
editcursor           equ  96f6h
edittail             equ  96f8h
editbtm              equ  96fah
editsym              equ  9706h
editdat              equ  9708h
wintop               equ  97a5h
winbtm               equ  97a6h
winleftedge          equ  97a7h
winleft              equ  97a8h
winabove             equ  97aah
winrow               equ  97ach
wincol               equ  97aeh
fmtdigits            equ  97b0h
fmtstring            equ  97b1h
fmtconv              equ  97f2h
fmtleft              equ  9804h
fmtindex             equ  9806h
fmtmatsym            equ  9808h
fmtmatmem            equ  980ah
eqs                  equ  980ch
tsymptr1             equ  9818h
tsymptr2             equ  981ah
chkdelptr3           equ  981ch
chkdelptr4           equ  981eh
tempmem              equ  9820h
fpbase               equ  9822h
fps                  equ  9824h
opbase               equ  9826h
ops                  equ  9828h
ptempcnt             equ  982ah
cleantmp             equ  982ch
ptemp                equ  982eh
progptr              equ  9830h
newdataptr           equ  9832h
pagedcount           equ  9834h
pagedpn              equ  9835h
pagedgetptr          equ  9836h
pagedputptr          equ  9838h
pagedbuf             equ  983ah
apperr1              equ  984dh
apperr2              equ  985ah
flashbyte1           equ  9867h
flashbyte2           equ  9868h
freearcblock         equ  9869h
arcpage              equ  986bh
arcptr               equ  986ch
apprawkeyhandle      equ  9870h
appbackupscreen      equ  9872h
customheight         equ  9b72h
locallanguage        equ  9b73h
cursorhookptr        equ  9b7ch
rawkeyhookptr        equ  9b84h
getkeyhookptr        equ  9b88h
fonthookptr          equ  9b9ch
restartclr           equ  9bd0h
localtokstr          equ  9d65h
keyforstr            equ  9d76h
keytostrram          equ  9d77h
sedmonsp             equ  9d88h
bpsave               equ  9d8ah
usermem              equ  9d95h
symtable             equ  0fe66h

;symbolic_debug_end
;
;======================================================================
;		language localization equates
;======================================================================
lang_neutral                       equ   00h
lang_danish                        equ   06h
lang_dutch                         equ   13h
lang_english                       equ   09h
lang_finnish                       equ   0bh
lang_french                        equ   0ch
lang_german                        equ   07h
lang_hungarian                     equ   0eh
lang_italian                       equ   10h
lang_norwegian                     equ   14h
lang_polish                        equ   15h
lang_portuguese                    equ   16h
lang_spanish                       equ   0ah
lang_swedish                       equ   1dh

sublang_neutral                    equ   00h   ; language neutral
sublang_dutch                    equ   01h   ; dutch
sublang_english_us               equ   01h   ; english (usa)
sublang_french                   equ   01h   ; french
sublang_german                   equ   01h   ; german
sublang_italian                  equ   01h   ; italian
sublang_norwegian_bokmal         equ   01h   ; norwegian (bokmal)
sublang_portuguese               equ   02h   ; portuguese
sublang_spanish                  equ   01h   ; spanish (castilian)

;======================================================================
;           error handler equates/macros 
;======================================================================
;
app_push_errorh equ	59h
app_pop_errorh	equ	5ch
;
apponerr   macro   handaddr
	ld	hl,handaddr		;hl -> stack handler address
	call	app_push_errorh 	;establish error handler context
	.endm

appofferr  macro
	call	app_pop_errorh		;remove error handler context
	.endm
;
;======================================================================
;           system and state flags
;======================================================================
;

trigflags	equ  0		;trigonometry mode settings
trigdeg 	equ    2	; 1=degrees, 0=radians

kbdflags	equ  0		;keyboard scan
kbdscr		equ    3	; 1=scan code ready
kbdkeypress	equ    4	; 1=key has been pressed

doneflags	equ  0		;display "done"
doneprgm	equ    5	; 1=display "done" after prgm

iodelflag	equ  0
indelete	equ    0	;1 = delete screen 

;----------------------------------------------------------------------
editflags	equ  1
editopen	equ    2	; 1=edit buffer is open

monflags	equ  1		;monitor flags
monabandon	equ    4	; 1=don't start any long process
				; in put away (#715)
;----------------------------------------------------------------------
plotflags	equ  2		;plot generation flags
plotloc 	equ    1	; 0=bkup & display, 1=display only
plotdisp	equ    2	; 1=plot is in display, 0=text in display


grfmodeflags	equ  2		;graph mode settings
grffuncm	equ    4	; 1=function graph
grfpolarm	equ    5	; 1=polar graph
grfparamm	equ    6	; 1=parametric graph
grfrecurm       equ    7        ; 1=recursion graph

graphflags	equ  3
graphdraw	equ    0	; 0=graph is valid, 1=redraw graph

grfdbflags	equ  4
grfdot		equ    0	; 0=line, 1=dot
grfsimul	equ    1	; 0=sequential, 1=simultaneous
grfgrid 	equ    2	; 0=no grid, 1=grid
grfpolar	equ    3	; 0=rectangular, 1=polar coordinates
grfnocoord	equ    4	; 0=display coordinates, 1=off
grfnoaxis	equ    5	; 0=axis, 1=no axis
grflabel	equ    6	; 0=off, 1=axis label

textflags	equ  5		;text output flags
texterasebelow	equ    1	; 1=erase line below small char
textscrolled	equ    2	; 1=screen scrolled
textinverse	equ    3	; 1=display inverse bit-map
textinsmode	equ    4	; 0=overstrike, 1=insert mode

parsflag2	equ  7		;parser flags
numop1		equ    0	; 1=result in op1, 0=no result

newdispf        equ  8		;derivative mode flags
preclrformode   equ    0	; 1=help blink on mode screen

apdflags	equ  8		;automatic power-down
apdable 	equ    2	; 1=apd enabled
apdrunning	equ    3	; 1=apd clock running


web_err_mask    equ  60h

onflags 	equ  9		;on key flags
onrunning	equ  3		; 1 = calculator is running
oninterrupt	equ    4	; 1=on key interrupt request

statflags	equ  9		;statistics flags
statsvalid	equ    6	; 1=stats are valid
statansdisp	equ  7		; 1=display stat results

fmtflags	equ  10		;numeric format flags
fmtexponent	equ    0	; 1=show exponent, 0=no exponent
fmteng		equ    1	; 1=engineering notion, 0=scientific
fmthex		equ    2	; 1=hexadecimal
fmtoct		equ    3	; 1=octal
fmtbin		equ    4	; 1=binary
;
nummode         equ  10
fmtreal         equ    5
fmtrect         equ    6
fmtpolar        equ    7

realmode        equ    5
rectmode        equ    6
polarmode       equ    7
;					;   if hex and oct both = 1
;					;   then bin=0 means >frac
;					;	 bin=1 means >dms
fmtbasemask     equ  00011100b		; mask to base flags
fmtbaseshift    equ  2			; offset to base flags
;
;       check if these are used by numform,
;
;               equ  6
;               equ  7

fmtoverride	equ  11		;copy of fmtflags with conversion override

fmteditflags	equ  12		;numeric editing flags
fmtedit 	equ    0	; 1=format number for editing

curflags	equ  12		;cursor
curable 	equ    2	; 1=cursor flash is enabled
curon		equ    3	; 1=cursor is showing
curlock 	equ    4	; 1=cursor is locked off

appflags	equ  13		;application flags
appwantintrpt	equ    0		; 1=want on key interrupts
apptextsave	equ    1		; 1=save characters in textshadow
appautoscroll	equ    2		; 1=auto-scroll text on last line
appmenus	equ    3		; 1=process keys that bring up menus
					; 0=check lock menu flag
applockmenus	equ    4		; 1=ignore menu keys
					; 0=switch to home screen and bring up menu
appcurgraphic	equ    5		; 1=graphic cursor
appcurword	equ    6		; 1=text cursor covers entire word
appexit 	equ    7		; 1=application handles [exit] key itself

appwantintrptf	equ	1<<appwantintrpt
apptextsavef	equ	1<<apptextsave
appautoscrollf	equ	1<<appautoscroll
appmenusf	equ	1<<appmenus
applockmenusf	equ	1<<applockmenus
appcurgraphicf	equ	1<<appcurgraphic
appcurwordf	equ	1<<appcurword
appexitf	equ	1<<appexit


seqflags        equ  15              ; sequential graph flags
webmode         equ    0             ; 0 = normal seq mode, 1 = web mode
webvert         equ    1             ;
sequv           equ    2             ; u vs v
seqvw           equ    3             ; v vs w
sequw           equ    4             ; u vs w


promptflags	equ  17		;prompt line flags
promptedit	equ    0	; 1=editing in prompt buffer

indicflags	equ  18		;indicator flags
indicrun	equ    0	; 1=run indicator on
indicinuse	equ    1	; indicator save area in use=1, free=0
indiconly	equ    2	; interrupt handler only checks run indicator

shiftflags	equ  18		;[2nd] and [alpha] flags
shift2nd	equ    3	; 1=[2nd] has been pressed
shiftalpha	equ    4	; 1=[alpha] has been pressed
shiftlwralph	equ    5	; 1=lower case, 0=upper case
shiftalock	equ    6	; 1=alpha lock has been pressed
shiftkeepalph	equ    7	; 1=cannot cancel alpha shift


tblflags        equ  19		;table flags.
autofill        equ    4	; 1=prompt, 0=fillauto
autocalc        equ    5	; 1=prompt, 0=calcauto
retable         equ    6	; 0=table is ok, 1=must recompute table.

sgrflags	equ  20
grfsplit        equ    0	; 1=split graph, 0=normal
vertsplit       equ    1	; 1=vertical (left-right) split
grfschanged     equ    2	; 1=graph just changed split <-> normal
grfsplitoverride equ   3	; 1 = ignore graph split flag if set
write_on_graph  equ    4	; 1 = text or equ writing to graph screen
g_style_active  equ    5	; 1 = graph styles are enabled, use them
cmp_mod_box     equ    6	; 1 = doing mod box plot computation
textwrite       equ    7
;

newindicflags   equ  21
extraindic      equ    0
saindic         equ    1

newflags2	    equ  22
norestores	    equ  5

smartflags      equ  23
;
;----------------------------------------------------------------------
; note: fix these equates if smartflags are moved                            
;----------------------------------------------------------------------
;
smarter_mask    equ  3
smarter_test    equ  1
smartgraph      equ  0
smartgraph_inv  equ  1                                                 

more_flags	= 26

no_del_stat	= 2
;----------------------------------------------------------------------
;           available for asm programming
;----------------------------------------------------------------------

asm_flag1       equ  33          ; asm coding
asm_flag2       equ  34          ; asm coding
asm_flag3       equ  35          ; asm coding

;----------------------------------------------------------------------
;
getsendflg	equ  36
comfailed	equ  1	      ; 1 = get/send communication failed
;
applwrcaseflag  equ  36
lwrcaseactive   equ  3
;
apiflg3		equ  42 
;
apiflg4		equ  43
fullscrndraw	equ    2	; draw into last row/col of screen

groupflags	equ  38
ingroup		equ  1		;1 = in group context


xapflag0        equ  46      ; external app flags
xapflag1        equ  47
xapflag2        equ  48
xapflag3        equ  49

fontflags	equ  50
fracdrawlfont	equ    2
fractalllfont	equ    3
customfont	equ    7  

plotflag3	equ  60  
bufferonly	equ    0
usefastcirc	equ    4

;
vartypemask	equ  1fh             
vargraphref	equ    6

;
;======================================================================
;           character font equates  
;======================================================================
;
; large font equates

lrecurn         equ     001h
lrecuru 	equ	002h
lrecurv 	equ	003h
lrecurw		equ	004h
lconvert	equ	005h
lsqup		equ	006h
lsqdown		equ	007h
lintegral	equ	008h
lcross		equ	009h
lboxicon 	equ     00ah
lcrossicon 	equ     00bh
ldoticon 	equ     00ch
lsubt   	equ     00dh		;small capital t for parametric mode.
lcuber  	equ     00eh		;slightly different 3 for cubed root.
lhexf		equ	00fh
lroot		equ	010h
linverse	equ	011h
lsquare		equ	012h
langle		equ	013h
ldegree		equ	014h
lradian		equ	015h
ltranspose	equ	016h
lle		equ	017h
lne		equ	018h
lge		equ	019h
lneg		equ	01ah
lexponent	equ	01bh
lstore		equ	01ch
lten		equ	01dh
luparrow	equ	01eh
ldownarrow	equ	01fh
lspace		equ	020h
lexclam		equ	021h
lquote		equ	022h
lpound		equ	023h
lfourth		equ     024h
lpercent	equ	025h
lampersand	equ	026h
lapostrophe	equ	027h
llparen		equ	028h
lrparen		equ	029h
lasterisk	equ	02ah
lplussign	equ	02bh
lcomma		equ	02ch
ldash		equ	02dh
lperiod		equ	02eh
lslash		equ	02fh
l0		equ	030h
l1		equ	031h
l2		equ	032h
l3		equ	033h
l4		equ	034h
l5		equ	035h
l6		equ	036h
l7		equ	037h
l8		equ	038h
l9		equ	039h
lcolon		equ	03ah
lsemicolon	equ	03bh
llt		equ	03ch
leq		equ	03dh
lgt		equ	03eh
lquestion	equ	03fh
latsign		equ	040h
lcapa	equ	041h
lcapb	equ	042h
lcapc	equ	043h
lcapd	equ	044h
lcape	equ	045h
lcapf	equ	046h
lcapg	equ	047h
lcaph	equ	048h
lcapi	equ	049h
lcapj	equ	04ah
lcapk	equ	04bh
lcapl	equ	04ch
lcapm	equ	04dh
lcapn	equ	04eh
lcapo	equ	04fh
lcapp	equ	050h
lcapq	equ	051h
lcapr	equ	052h
lcaps	equ	053h
lcapt	equ	054h
lcapu	equ	055h
lcapv	equ	056h
lcapw	equ	057h
lcapx	equ	058h
lcapy	equ	059h
lcapz	equ	05ah
ltheta  equ     05bh
lbackslash	equ	05ch
lrbrack		equ	05dh
lcaret		equ	05eh
lunderscore	equ	05fh
lbackquote	equ	060h
la	equ	061h
lb	equ	062h
lc	equ	063h
ld	equ	064h
le	equ	065h
lf	equ	066h
lg	equ	067h
lh	equ	068h
li	equ	069h
lj	equ	06ah
lk	equ	06bh
ll	equ	06ch
lm	equ	06dh
ln	equ	06eh
lo	equ	06fh
lp	equ	070h
lq	equ	071h
lr	equ	072h
ls	equ	073h
lt	equ	074h
lu	equ	075h
lv	equ	076h
lw	equ	077h
lx	equ	078h
ly	equ	079h
lz	equ	07ah
llbrace	equ	07bh
lbar	equ	07ch
lrbrace	equ	07dh
ltilde	equ	07eh
linveq	equ	07fh
lsub0	equ	080h
lsub1	equ	081h
lsub2	equ	082h
lsub3	equ	083h
lsub4	equ	084h
lsub5	equ	085h
lsub6	equ	086h
lsub7	equ	087h
lsub8	equ	088h
lsub9	equ	089h
lcapaacute	equ	08ah
lcapagrave	equ	08bh
lcapacaret	equ	08ch
lcapadier	equ	08dh
laacute		equ	08eh
lagrave		equ	08fh
lacaret		equ	090h
ladier		equ	091h
lcapeacute	equ	092h
lcapegrave	equ	093h
lcapecaret	equ	094h
lcapedier	equ	095h
leacute		equ	096h
legrave		equ	097h
lecaret		equ	098h
ledier		equ	099h
lcapiacute	equ	09ah
lcapigrave	equ	09bh
lcapicaret	equ	09ch
lcapidier	equ	09dh
liacute		equ	09eh
ligrave		equ	09fh
licaret		equ	0a0h
lidier		equ	0a1h
lcapoacute	equ	0a2h
lcapograve	equ	0a3h
lcapocaret	equ	0a4h
lcapodier	equ	0a5h
loacute		equ	0a6h
lograve		equ	0a7h
locaret		equ	0a8h
lodier		equ	0a9h
lcapuacute	equ	0aah
lcapugrave	equ	0abh
lcapucaret	equ	0ach
lcapudier	equ	0adh
luacute		equ	0aeh
lugrave		equ	0afh
lucaret		equ	0b0h
ludier		equ	0b1h
lcapcced	equ	0b2h
lcced		equ	0b3h
lcapntilde	equ	0b4h
lntilde		equ	0b5h
laccent		equ	0b6h
lgrave		equ	0b7h
ldieresis	equ	0b8h
lquesdown	equ	0b9h
lexclamdown	equ	0bah
lalpha		equ	0bbh
lbeta		equ	0bch
lgamma		equ	0bdh
lcapdelta	equ	0beh
ldelta		equ	0bfh
lepsilon	equ	0c0h
llbrack 	equ     0c1h
llambda		equ	0c2h
lmu		equ	0c3h
lpi		equ	0c4h
lrho		equ	0c5h
lcapsigma	equ	0c6h
lsigma		equ	0c7h
ltau		equ	0c8h
lphi		equ	0c9h
lcapomega	equ	0cah
lxmean		equ	0cbh
lymean		equ	0cch
lsupx		equ	0cdh
lellipsis	equ	0ceh
lleft		equ	0cfh
lblock		equ	0d0h
lper		equ	0d1h
lhyphen		equ	0d2h
larea		equ	0d3h
ltemp		equ	0d4h
lcube		equ	0d5h
lenter		equ	0d6h
limagi		equ	0d7h
lphat		equ	0d8h
lchi		equ	0d9h
lstatf		equ     0dah
llne		equ	0dbh
llistl		equ	0dch
lfinann 	equ	0ddh
l2_r_paren 	equ	0deh
lblockarrow     equ     0dfh
lcuro   	equ     0e0h
lcuro2  	equ     0e1h
lcurocapa       equ     0e2h
lcuroa  	equ     0e3h
lcuri   	equ     0e4h
lcuri2  	equ     0e5h
lcuricapa       equ     0e6h
lcuria  	equ     0e7h
lgline          equ     0e8h    ; = 0
lgthick         equ     0e9h    ; = 1
lgabove         equ     0eah    ; = 2
lgbelow         equ     0ebh    ; = 3
lgpath          equ     0ech    ; = 4
lganimate       equ     0edh    ; = 5
lgdot           equ     0eeh    ; = 6
lupblk          equ     0efh    ;up arrow and block in solver
ldnblk          equ     0f0h    ;down arrow and block in solver
lcurfull        equ     0f1h    ;note: must be last char (putmap checks)
;
; small font equates
;
srecurn         equ     001h
srecuru		equ	002h
srecurv		equ	003h
srecurw		equ	004h
sconvert	equ	005h
sfourspaces 	equ 	006h
ssqdown		equ	007h
sintegral	equ	008h
scross		equ	009h
sboxicon 	equ	00ah
scrossicon 	equ	00bh
sdoticon 	equ    	00ch
ssubt   	equ     00dh
scuber  	equ     00eh
shexf		equ	00fh
sroot		equ	010h
sinverse	equ	011h
ssquare		equ	012h
sangle		equ	013h
sdegree		equ	014h
sradian		equ	015h
stranspose	equ	016h
sle		equ	017h
sne		equ	018h
sge		equ	019h
sneg		equ	01ah
sexponent	equ	01bh
sstore		equ	01ch
sten		equ	01dh
suparrow	equ	01eh
sdownarrow	equ	01fh
sspace		equ	020h
sexclam		equ	021h
squote		equ	022h
spound		equ	023h
sdollar		equ	024h
spercent	equ	025h
sampersand	equ	026h
sapostrophe	equ	027h
slparen		equ	028h
srparen		equ	029h
sasterisk	equ	02ah
splussign	equ	02bh
scomma		equ	02ch
sdash		equ	02dh
speriod		equ	02eh
sslash		equ	02fh
s0		equ	030h
s1		equ	031h
s2		equ	032h
s3		equ	033h
s4		equ	034h
s5		equ	035h
s6		equ	036h
s7		equ	037h
s8		equ	038h
s9		equ	039h
scolon		equ	03ah
ssemicolon	equ	03bh
slt		equ	03ch
seq		equ	03dh
sgt		equ	03eh
squestion	equ	03fh
satsign		equ	040h
scapa		equ	041h
scapb		equ	042h
scapc		equ	043h
scapd		equ	044h
scape		equ	045h
scapf		equ	046h
scapg		equ	047h
scaph		equ	048h
scapi		equ	049h
scapj		equ	04ah
scapk		equ	04bh
scapl		equ	04ch
scapm		equ	04dh
scapn		equ	04eh
scapo		equ	04fh
scapp		equ	050h
scapq		equ	051h
scapr		equ	052h
scaps		equ	053h
scapt		equ	054h
scapu		equ	055h
scapv		equ	056h
scapw		equ	057h
scapx		equ	058h
scapy		equ	059h
scapz		equ	05ah
stheta  	equ     05bh
sbackslash	equ	05ch
srbrack		equ	05dh
scaret		equ	05eh
sunderscore	equ	05fh
sbackquote	equ	060h
smalla		equ	061h
smallb		equ	062h
smallc		equ	063h
smalld		equ	064h
smalle		equ	065h
smallf		equ	066h
smallg		equ	067h
smallh		equ	068h
smalli		equ	069h
smallj		equ	06ah
smallk		equ	06bh
smalll		equ	06ch
smallm		equ	06dh
smalln		equ	06eh
smallo		equ	06fh
smallp		equ	070h
smallq		equ	071h
smallr		equ	072h
smalls		equ	073h
smallt		equ	074h
smallu		equ	075h
smallv		equ	076h
smallw		equ	077h
smallx		equ	078h
smally		equ	079h
smallz		equ	07ah
slbrace		equ	07bh
sbar		equ	07ch
srbrace		equ	07dh
stilde		equ	07eh
sinveq		equ	07fh
ssub0		equ	080h
ssub1		equ	081h
ssub2		equ	082h
ssub3		equ	083h
ssub4		equ	084h
ssub5		equ	085h
ssub6		equ	086h
ssub7		equ	087h
ssub8		equ	088h
ssub9		equ	089h
scapaacute	equ	08ah
scapagrave	equ	08bh
scapacaret	equ	08ch
scapadier	equ	08dh
saacute		equ	08eh
sagrave		equ	08fh
sacaret		equ	090h
sadier		equ	091h
scapegrave	equ	092h
scapeacute	equ	093h
scapecaret	equ	094h
scapedier	equ	095h
seacute		equ	096h
segrave		equ	097h
secaret		equ	098h
sedier		equ	099h
scapiacute	equ	09ah
scapigrave	equ	09bh
scapicaret	equ	09ch
scapidier	equ	09dh
siacute		equ	09eh
sigrave		equ	09fh
sicaret		equ	0a0h
sidier		equ	0a1h
scapoacute	equ	0a2h
scapograve	equ	0a3h
scapocaret	equ	0a4h
scapodier	equ	0a5h
soacute		equ	0a6h
sograve		equ	0a7h
socaret		equ	0a8h
sodier		equ	0a9h
scapuacute	equ	0aah
scapugrave	equ	0abh
scapucaret	equ	0ach
scapudier	equ	0adh
suacute		equ	0aeh
sugrave		equ	0afh
sucaret		equ	0b0h
sudier		equ	0b1h
scapcced	equ	0b2h
scced		equ	0b3h
scapntilde	equ	0b4h
sntilde		equ	0b5h
saccent		equ	0b6h
sgrave		equ	0b7h
sdieresis	equ	0b8h
squesdown	equ	0b9h
sexclamdown	equ	0bah
salpha		equ	0bbh
sbeta		equ	0bch
sgamma		equ	0bdh
scapdelta	equ	0beh
sdelta		equ	0bfh
sepsilon	equ	0c0h
slbrack 	equ     0c1h
slambda		equ	0c2h
smu		equ	0c3h
spi		equ	0c4h
srho		equ	0c5h
scapsigma	equ	0c6h
ssigma		equ	0c7h
stau		equ	0c8h
sphi		equ	0c9h
scapomega	equ	0cah
sxmean		equ	0cbh
symean		equ	0cch
ssupx		equ	0cdh
sellipsis	equ	0ceh
sleft		equ	0cfh
sblock		equ	0d0h
sper		equ	0d1h
shyphen		equ	0d2h
sarea		equ	0d3h
stemp		equ	0d4h
scube		equ	0d5h
senter		equ	0d6h
simagi		equ	0d7h
sphat		equ	0d8h
schi		equ	0d9h
sstatf  	equ     0dah
slne		equ	0dbh
slistl		equ	0dch
sfinann 	equ	0ddh
s2_r_paren 	equ	0deh
snarrowcape     equ     0dfh
slistlock 	equ	0e0h
sscatter1 	equ	0e1h                    
sscatter2 	equ	0e2h                    
sxyline1  	equ	0e3h                    
sxyline2  	equ	0e4h
sboxplot1 	equ	0e5h
sboxplot2 	equ	0e6h
shist1    	equ	0e7h
shist2    	equ	0e8h
smodbox1  	equ	0e9h
smodbox2  	equ	0eah
snormal1  	equ	0ebh
snormal2  	equ	0ech
;
;======================================================================
;           keypress equates
;======================================================================
;           keyboard key names
;
;
kright          equ       001h
kleft           equ       002h
kup             equ       003h
kdown           equ       004h
kenter          equ       005h
kalphaenter     equ       006h
kalphaup        equ       007h
kalphadown      equ       008h
kclear          equ       009h
kdel            equ       00ah
kins            equ       00bh
krecall         equ       00ch
klastent        equ       00dh
kbol            equ       00eh
keol            equ       00fh
;
kselall         equ       010h
kunselall       equ       011h
kltoti82        equ       012h
kbackup         equ       013h
krecieve        equ       014h
klnkquit        equ       015h
ktrans          equ       016h
krename         equ       017h
koverw          equ       018h
komit           equ       019h
kcont           equ       01ah
ksendid         equ       01bh
ksendsw         equ       01ch
kyes            equ       01dh
knoway          equ       01eh
kvsendtype      equ       01fh
koverwall       equ       020h
;
kno             equ       025h
kkreset         equ       026h
kapp            equ       027h
;
kdoug           equ       028h
klistflag       equ       029h
menustart       equ       02bh
;
kareyousure     equ       02bh
kappsmenu       equ       02ch
kprgm           equ       02dh
kzoom           equ       02eh
kdraw           equ       02fh
ksplot          equ       030h
kstat           equ       031h
kmath           equ       032h
ktest           equ       033h
kchar           equ       034h
kvars           equ       035h
kmem            equ       036h
kmatrix         equ       037h
kdist           equ       038h
kangle          equ       039h
klist           equ       03ah
kcalc           equ       03bh
kfin            equ       03ch
;
menuend         equ       kfin
;
kcatalog        equ       03eh
kinputdone      equ       03fh
koff            equ       kinputdone
;
kquit           equ       040h
appstart        equ       kquit

;
klinkio         equ       041h
kmatrixed       equ       042h
kstated         equ       043h
kgraph          equ       044h
kmode           equ       045h
kprgmed         equ       046h        ; program edit
kprgmcr         equ       047h        ; program create
kwindow         equ       048h        ; range editor
kyequ           equ       049h        ; equation editor
ktable          equ       04ah        ; table editor
ktblset         equ       04bh        ; table set
kchkram         equ       04ch        ; check ram
kdelmem         equ       04dh        ; delete mem
kresetmem       equ       04eh        ; reset mem
kresetdef	equ	  04fh        ; reset default
kprgminput      equ       050h        ; program input
kzfacted        equ       051h        ; zoom factor editor
kerror          equ       052h        ; error
ksolvetvm       equ       053h        ; tvm solver
ksolveroot	equ	  054h        ; solve editor
kstatp          equ       055h        ; stat plot
kinfstat 	equ	  056h	      ; inferential statistic
kformat         equ       057h        ; format
kextapps        equ       058h        ; external applications.     new
knewapps        equ       059h        ; new apps for cerberus.
;
append          equ       knewapps
;
echostart1      equ       05ah
;
ktrace          equ       05ah
kzfit           equ       05bh
kzin            equ       05ch
kzout           equ       05dh
kzprev          equ       05eh
kbox            equ       05fh
kdecml          equ       060h
ksetzm          equ       061h
ksquar          equ       062h
kstd            equ       063h
ktrig           equ       064h
kusrzm          equ       065h
kzsto           equ       066h
kzint           equ       067h
kzstat          equ       068h
;
echostart2      equ       069h
;
kselect         equ       069h
kcircl          equ       06ah
kcldrw          equ       06bh
kline           equ       06ch
kpen            equ       06dh
kptchg          equ       06eh
kptoff          equ       06fh
kpton           equ       070h
kvert           equ       071h
khoriz          equ       072h
ktext           equ       073h
ktanln          equ       074h
;
keval           equ       075h
kinters         equ       076h
kdydx           equ       077h
kfnintg         equ       078h
krootg          equ       079h
kdydt           equ       07ah
kdxdt           equ       07bh
kdrdo           equ       07ch
kgfmin          equ       07dh
kgfmax          equ       07eh
;
;
echostart       equ       07fh
;
klistname       equ       07fh
kadd            equ       080h
ksub            equ       081h
kmul            equ       082h
kdiv            equ       083h
kexpon          equ       084h
klparen         equ       085h
krparen         equ       086h
klbrack         equ       087h
krbrack         equ       088h
kshade          equ       089h
kstore          equ       08ah
kcomma          equ       08bh
kchs            equ       08ch
kdecpnt         equ       08dh
k0              equ       08eh
k1              equ       08fh
k2              equ       090h
k3              equ       091h
k4              equ       092h
k5              equ       093h
k6              equ       094h
k7              equ       095h
k8              equ       096h
k9              equ       097h
kee             equ       098h
kspace          equ       099h
kcapa           equ       09ah
kcapb           equ       09bh
kcapc           equ       09ch
kcapd           equ       09dh
kcape           equ       09eh
kcapf           equ       09fh
kcapg           equ       0a0h
kcaph           equ       0a1h
kcapi           equ       0a2h
kcapj           equ       0a3h
kcapk           equ       0a4h
kcapl           equ       0a5h
kcapm           equ       0a6h
kcapn           equ       0a7h
kcapo           equ       0a8h
kcapp           equ       0a9h
kcapq           equ       0aah
kcapr           equ       0abh
kcaps           equ       0ach
kcapt           equ       0adh
kcapu           equ       0aeh
kcapv           equ       0afh
kcapw           equ       0b0h
kcapx           equ       0b1h
kcapy           equ       0b2h
kcapz           equ       0b3h
kvarx           equ       0b4h
kpi             equ       0b5h
kinv            equ       0b6h
ksin            equ       0b7h
kasin           equ       0b8h
kcos            equ       0b9h
kacos           equ       0bah
ktan            equ       0bbh
katan           equ       0bch
ksquare         equ       0bdh
ksqrt           equ       0beh
kln             equ       0bfh
kexp            equ       0c0h
klog            equ       0c1h
kalog           equ       0c2h
ktoabc          equ       0c3h
;
kclrtbl         equ       0c4h
;
kans            equ       0c5h
kcolon          equ       0c6h
;
knderiv         equ       0c7h
kfnint          equ       0c8h
kroot           equ       0c9h
;
kquest          equ       0cah
kquote          equ       0cbh
ktheta          equ       0cch
kif             equ       0cdh
kthen           equ       0ceh
kelse           equ       0cfh
kfor            equ       0d0h
kwhile          equ       0d1h
krepeat         equ       0d2h
kend            equ       0d3h
kpause          equ       0d4h
klbl            equ       0d5h
kgoto           equ       0d6h
kisg            equ       0d7h
kdsl            equ       0d8h
kmenu           equ       0d9h
kexec           equ       0dah
kreturn         equ       0dbh
kstop           equ       0dch
kinput          equ       0ddh
kprompt         equ       0deh
kdisp           equ       0dfh
kdispg          equ       0e0h
kdispt          equ       0e1h
koutput         equ       0e2h
kgetkey         equ       0e3h
kclrhome        equ       0e4h
kprtscr         equ       0e5h
ksinh           equ       0e6h
kcosh           equ       0e7h
ktanh           equ       0e8h
kasinh          equ       0e9h
kacosh          equ       0eah
katanh          equ       0ebh
klbrace         equ       0ech
krbrace         equ       0edh
ki              equ       0eeh
kconstea        equ       0efh
kplot3          equ       0f0h
kfmin           equ       0f1h
kfmax           equ       0f2h
kl1a            equ       0f3h
kl2a            equ       0f4h
kl3a            equ       0f5h
kl4a            equ       0f6h
kl5a            equ       0f7h
kl6a            equ       0f8h
kuna            equ       0f9h
kvna            equ       0fah
kwna            equ       0fbh
;
;======================================================================
;       this key means that it is a 2 byte keycode
;       there are 2 of these keys; be careful with usage
;======================================================================
;
kextendecho2    equ       0fch
;
;======================================================================;
;       this key means that the key press is one that echos
;       into a buffer, and it is a 2 byte key code, go look at
;       (extecho) for the key value 
;======================================================================
;
kextendecho     equ       0feh
;

ke1bt           equ       0

kdrawinv        equ       ke1bt
kdrawf          equ       ke1bt+1
kpixelon        equ       ke1bt+2
kpixeloff       equ       ke1bt+3
kpxltest        equ       ke1bt+4
krcgdb          equ       ke1bt+5
krcpic          equ       ke1bt+6
kstgdb          equ       ke1bt+7
kstpic          equ       ke1bt+8
kabs            equ       ke1bt+9
ktequ           equ       ke1bt+10    ; ==
ktnoteq         equ       ke1bt+11    ; <>
ktgt            equ       ke1bt+12    ; >
ktgte           equ       ke1bt+13    ; > =
ktlt            equ       ke1bt+14    ; <
ktlte           equ       ke1bt+15    ; < =

kand            equ       ke1bt+16
kor             equ       ke1bt+17
kxor            equ       ke1bt+18
knot            equ       ke1bt+19

klr1            equ       ke1bt+20

kxroot          equ       ke1bt+21
kcube           equ       ke1bt+22
kcbrt           equ       ke1bt+23    ; cube root
ktodec          equ       ke1bt+24
;
kcubicr         equ       ke1bt+25
kquartr         equ       ke1bt+26
;
kplot1          equ       ke1bt+27
kplot2          equ       ke1bt+28
;

kround          equ       ke1bt+29
kipart          equ       ke1bt+30
kfpart          equ       ke1bt+31
kint            equ       ke1bt+32

krand           equ       ke1bt+33
knpr            equ       ke1bt+34
kncr            equ       ke1bt+35
kxfactorial     equ       ke1bt+36

krad            equ       ke1bt+37
kdegr           equ       ke1bt+38    ; degrees conv
kapost          equ       ke1bt+39
ktodms          equ       ke1bt+40
krtopo          equ       ke1bt+41    ; r
krtopr          equ       ke1bt+42
kptorx          equ       ke1bt+43
kptory          equ       ke1bt+44

krowswap        equ       ke1bt+45
krowplus        equ       ke1bt+46
ktimrow         equ       ke1bt+47
ktrowp          equ       ke1bt+48

ksorta          equ       ke1bt+49
ksortd          equ       ke1bt+50
kseq            equ       ke1bt+51

kmin            equ       ke1bt+52
kmax            equ       ke1bt+53
kmean           equ       ke1bt+54
kmedian         equ       ke1bt+55
ksum            equ       ke1bt+56
kprod           equ       ke1bt+57

kdet            equ       ke1bt+58
ktransp         equ       ke1bt+59
kdim            equ       ke1bt+60
kfill           equ       ke1bt+61
kident          equ       ke1bt+62
krandm          equ       ke1bt+63
kaug            equ       ke1bt+64

konevar         equ       ke1bt+65
ktwovar         equ       ke1bt+66
klr             equ       ke1bt+67
klrexp          equ       ke1bt+68
klrln           equ       ke1bt+69
klrpwr          equ       ke1bt+70
kmedmed         equ       ke1bt+71
kquad           equ       ke1bt+72
kclrlst         equ       ke1bt+73
khist           equ       ke1bt+74
kxyline         equ       ke1bt+75
kscatter        equ       ke1bt+76

kmrad           equ       ke1bt+77
kmdeg           equ       ke1bt+78
kmnormf         equ       ke1bt+79
kmsci           equ       ke1bt+80
kmeng           equ       ke1bt+81
kmfloat         equ       ke1bt+82

kfix            equ       ke1bt+83
kspliton        equ       ke1bt+84
kfullscreen     equ       ke1bt+85
kstndrd         equ       ke1bt+86
kparam          equ       ke1bt+87
kpolar          equ       ke1bt+88
kseqg           equ       ke1bt+89
kafillon        equ       ke1bt+90
kafilloff       equ       ke1bt+91
kacalcon        equ       ke1bt+92
kacalcoff       equ       ke1bt+93
kfnon           equ       ke1bt+94
kfnoff          equ       ke1bt+95

kplotson        equ       ke1bt+96
kplotsoff       equ       ke1bt+97

kpixelchg       equ       ke1bt+98

ksendmbl        equ       ke1bt+99
krecvmbl        equ       ke1bt+100

kboxplot        equ       ke1bt+101
kboxicon        equ       ke1bt+102
kcrossicon      equ       ke1bt+103
kdoticon        equ       ke1bt+104

ke2bt           equ       ke1bt+105

kseqential      equ       ke2bt
ksimulg         equ       ke2bt+1
kpolarg         equ       ke2bt+2
krectg          equ       ke2bt+3
kcoordon        equ       ke2bt+4
kcoordoff       equ       ke2bt+5
kdrawline       equ       ke2bt+6
kdrawdot        equ       ke2bt+7
kaxison         equ       ke2bt+8
kaxisoff        equ       ke2bt+9
kgridon         equ       ke2bt+10
kgridoff        equ       ke2bt+11
klblon          equ       ke2bt+12
klbloff         equ       ke2bt+13

kl1             equ       ke2bt+14
kl2             equ       ke2bt+15
kl3             equ       ke2bt+16
kl4             equ       ke2bt+17
kl5             equ       ke2bt+18
kl6             equ       ke2bt+19

;
;======================================================================
;           these keys are layed on top of existing keys to
;           enable localization in the inferential stats editor
;======================================================================
;
kinfdata        equ       kl1
kinfstats       equ       kl1+1
kinfyes         equ       kl1+2
kinfno          equ       kl1+3
kinfcalc        equ       kl1+4
kinfdraw        equ       kl1+5
kinfalt1ne      equ       kl1+6
kinfalt1lt      equ       kl1+7
kinfalt1gt      equ       kl1+8
kinfalt2ne      equ       kl1+9
kinfalt2lt      equ       kl1+10
kinfalt2gt      equ       kl1+11
kinfalt3ne      equ       kl1+12
kinfalt3lt      equ       kl1+13
kinfalt3gt      equ       kl1+14
kinfalt4ne      equ       kl1+15
kinfalt4lt      equ       kl1+16
kinfalt4gt      equ       kl1+17
kinfalt5ne      equ       kl1+18
kinfalt5lt      equ       kl1+19
kinfalt5gt      equ       kl1+20
kinfalt6ne      equ       kl1+21
kinfalt6lt      equ       kl1+22
kinfalt6gt      equ       kl1+23
;
;
kmata           equ       ke2bt+20
kmatb           equ       ke2bt+21
kmatc           equ       ke2bt+22
kmatd           equ       ke2bt+23
kmate           equ       ke2bt+24

kxmin           equ       ke2bt+25
kxmax           equ       ke2bt+26
kxscl           equ       ke2bt+27
kymin           equ       ke2bt+28
kymax           equ       ke2bt+29
kyscl           equ       ke2bt+30
ktmin           equ       ke2bt+31
ktmax           equ       ke2bt+32
ktstep          equ       ke2bt+33
komin           equ       ke2bt+34
komax           equ       ke2bt+35
kostep          equ       ke2bt+36
ku0             equ       ke2bt+37
kv0             equ       ke2bt+38
knmin           equ       ke2bt+39
knmax           equ       ke2bt+40
kdeltay         equ       ke2bt+41
kdeltax         equ       ke2bt+42

kzxmin          equ       ke2bt+43
kzxmax          equ       ke2bt+44
kzxscl          equ       ke2bt+45
kzymin          equ       ke2bt+46
kzymax          equ       ke2bt+47
kzyscl          equ       ke2bt+48
kztmin          equ       ke2bt+49
kztmax          equ       ke2bt+50
kztstep         equ       ke2bt+51
kzomin          equ       ke2bt+52
kzomax          equ       ke2bt+53
kzostep         equ       ke2bt+54
kzu0            equ       ke2bt+55
kzv0            equ       ke2bt+56
kznmin          equ       ke2bt+57
kznmax          equ       ke2bt+58

kdellast        equ       ke2bt+59
ksinreg         equ       ke2bt+60
kconste         equ       ke2bt+61

kpic1           equ       ke2bt+62
kpic2           equ       ke2bt+63
kpic3           equ       ke2bt+64

kdelvar         equ       ke2bt+65
kgetcalc        equ       ke2bt+66
krealm          equ       ke2bt+67
kpolarm         equ       ke2bt+68
krectm          equ       ke2bt+69
kuv             equ       ke2bt+70    ; u vs v
kvw             equ       ke2bt+71    ; v vs w
kuw             equ       ke2bt+72    ; u vs w
kfinpmtend      equ       ke2bt+73
kfinpmtbeg      equ       ke2bt+74
;
kgraphstyle     equ       ke2bt+75
;
kexpron         equ       ke2bt+76
kexproff        equ       ke2bt+77
kstata          equ       ke2bt+78
kstatb          equ       ke2bt+79
kstatc          equ       ke2bt+80
kcorr           equ       ke2bt+81
kstatd          equ       ke2bt+82
kstate          equ       ke2bt+83
kregeq          equ       ke2bt+84
kminx           equ       ke2bt+85
kq1             equ       ke2bt+86
kmd             equ       ke2bt+87
kq3             equ       ke2bt+88
kmaxx           equ       ke2bt+89
kstatx1         equ       ke2bt+90
kstaty1         equ       ke2bt+91
kstatx2         equ       ke2bt+92
kstaty2         equ       ke2bt+93
kstatx3         equ       ke2bt+94
kstaty3         equ       ke2bt+95
ktblmin         equ       ke2bt+96
ktblstep        equ       ke2bt+97
ksetuplst       equ       ke2bt+98
kclralllst      equ       ke2bt+99
klogistic       equ       ke2bt+100
kztest          equ       ke2bt+101
kttest          equ       ke2bt+102
k2sampztest     equ       ke2bt+103
k2sampttest     equ       ke2bt+104
k1propztest     equ       ke2bt+105
k2propztest     equ       ke2bt+106
kchitest        equ       ke2bt+107
k2sampftest     equ       ke2bt+108
kzintval        equ       ke2bt+109
ktintval        equ       ke2bt+110
k2samptint      equ       ke2bt+111
k2sampzint      equ       ke2bt+112
k1propzint      equ       ke2bt+113
k2propzint      equ       ke2bt+114
kdnormal        equ       ke2bt+115
kinvnorm        equ       ke2bt+116
kdt             equ       ke2bt+117
kchi            equ       ke2bt+118
kdf             equ       ke2bt+119
kbinpdf         equ       ke2bt+120
kbincdf         equ       ke2bt+121
kpoipdf         equ       ke2bt+122
kpoicdf         equ       ke2bt+123
kun             equ       ke2bt+124
kvn             equ       ke2bt+125
kwn             equ       ke2bt+126
krecn           equ       ke2bt+127
kplotstart      equ       ke2bt+128
kzplotstart     equ       ke2bt+129   ; recursion n
kxfact          equ       ke2bt+130   ; plotstart
kyfact          equ       ke2bt+131   ; zplotstart
kanova          equ       ke2bt+132   ; xfact
kmaxy           equ       ke2bt+133   ; yfact
kwebon          equ       ke2bt+134   ; miny
kweboff         equ       ke2bt+135   ; maxy
ktblinput       equ       ke2bt+136   ; web on
kgeopdf         equ       ke2bt+137   ; web off
kgeocdf         equ       ke2bt+138   ; web off
kshadenorm      equ       ke2bt+139
kshadet         equ       ke2bt+140
kshadechi       equ       ke2bt+141
kshadef         equ       ke2bt+142
kplotstep       equ       ke2bt+143
kzplotstep      equ       ke2bt+144
klinregttest    equ       ke2bt+145
kmgt		equ	  ke2bt+146   ; vert split
kselecta        equ       ke2bt+147
kzfita          equ       ke2bt+148
ke2bt_end       equ       kzfita
;
;
;======================================================================
;           more 2 byte keys 
;======================================================================

ke2bt2		equ       0

kgdb1		equ       ke2bt2
kgdb2		equ       ke2bt2+1
kgdb3		equ       ke2bt2+2
ky1		equ       ke2bt2+3
ky2		equ       ke2bt2+4
ky3		equ       ke2bt2+5
ky4		equ       ke2bt2+6
ky5		equ       ke2bt2+7
ky6		equ       ke2bt2+8
ky7		equ       ke2bt2+9
ky8		equ       ke2bt2+10
ky9		equ       ke2bt2+11
ky0		equ       ke2bt2+12
kx1t		equ       ke2bt2+13
ky1t		equ       ke2bt2+14
kx2t		equ       ke2bt2+15
ky2t		equ       ke2bt2+16
kx3t		equ       ke2bt2+17
ky3t		equ       ke2bt2+18
kx4t		equ       ke2bt2+19
ky4t		equ       ke2bt2+20
kx5t		equ       ke2bt2+21
ky5t		equ       ke2bt2+22
kx6t		equ       ke2bt2+23
ky6t		equ       ke2bt2+24
kr1		equ       ke2bt2+25
kr2		equ       ke2bt2+26
kr3		equ       ke2bt2+27
kr4		equ       ke2bt2+28
kr5		equ       ke2bt2+29
kr6		equ       ke2bt2+30
kgdb4		equ       ke2bt2+31
kgdb5		equ       ke2bt2+32
kgdb6		equ       ke2bt2+33
kpic4		equ       ke2bt2+34
kpic5		equ       ke2bt2+35
kpic6		equ       ke2bt2+36
kgdb7		equ       ke2bt2+37
kgdb8		equ       ke2bt2+38
kgdb9		equ       ke2bt2+39
kgdb0		equ       ke2bt2+40
kpic7		equ       ke2bt2+41
kpic8		equ       ke2bt2+42
kpic9		equ       ke2bt2+43
kpic0		equ       ke2bt2+44
kstatn		equ       ke2bt2+45
kxmean		equ       ke2bt2+46
kconj		equ       ke2bt2+47
kreal		equ       ke2bt2+48
kfangle		equ       ke2bt2+49
klcm		equ       ke2bt2+50
kgcd		equ       ke2bt2+51
krandint	equ       ke2bt2+52
krandnorm	equ       ke2bt2+53
ktopolar	equ       ke2bt2+54
ktorect		equ       ke2bt2+55
kymean		equ       ke2bt2+56
kstdx		equ       ke2bt2+57
kstdx1		equ       ke2bt2+58
kw0		equ       ke2bt2+59
kmatf		equ       ke2bt2+60
kmatg		equ       ke2bt2+61
kmatrh		equ       ke2bt2+62
kmati		equ       ke2bt2+63
kmatj		equ       ke2bt2+64
kymean1		equ       ke2bt2+65
kstdy		equ       ke2bt2+66
kstdy1		equ       ke2bt2+67
kmattolst	equ       ke2bt2+68
klsttomat	equ       ke2bt2+69
kcumsum		equ       ke2bt2+70
kdeltalst	equ       ke2bt2+71
kstddev		equ       ke2bt2+72
kvariance	equ       ke2bt2+73
klength		equ       ke2bt2+74
kequtostrng	equ       ke2bt2+75
kstrngtoequ	equ       ke2bt2+76
kexpr		equ       ke2bt2+77
ksubstrng	equ       ke2bt2+78
kinstrng	equ       ke2bt2+79
kstr1		equ       ke2bt2+80
kstr2		equ       ke2bt2+81
kstr3		equ       ke2bt2+82
kstr4           equ       ke2bt2+83
kstr5           equ       ke2bt2+84
kstr6           equ       ke2bt2+85
kstr7           equ       ke2bt2+86
kstr8           equ       ke2bt2+87
kstr9           equ       ke2bt2+88
kstr0           equ       ke2bt2+89
kfinn           equ       ke2bt2+90
kfini           equ       ke2bt2+91
kfinpv          equ       ke2bt2+92
kfinpmt         equ       ke2bt2+93
kfinfv          equ       ke2bt2+94
kfinpy          equ       ke2bt2+95
kfincy          equ       ke2bt2+96
kfinfpmt        equ       ke2bt2+97
kfinfi          equ       ke2bt2+98
kfinfpv         equ       ke2bt2+99
kfinfn          equ       ke2bt2+100
kfinffv         equ       ke2bt2+101
kfinnpv         equ       ke2bt2+102
kfinirr         equ       ke2bt2+103
kfinbal         equ       ke2bt2+104
kfinprn         equ       ke2bt2+105
kfinint         equ       ke2bt2+106
ksumx           equ       ke2bt2+107
ksumx2          equ       ke2bt2+108
kfintonom       equ       ke2bt2+109
kfintoeff       equ       ke2bt2+110
kfindbd         equ       ke2bt2+111
kstatvp         equ       ke2bt2+112
kstatz          equ       ke2bt2+113
kstatt          equ       ke2bt2+114
kstatchi        equ       ke2bt2+115
kstatf          equ       ke2bt2+116
kstatdf         equ       ke2bt2+117
kstatphat       equ       ke2bt2+118
kstatphat1      equ       ke2bt2+119
kstatphat2      equ       ke2bt2+120
kstatmeanx1     equ       ke2bt2+121
kstatmeanx2     equ       ke2bt2+122
kstatstdx1      equ       ke2bt2+123
kstatstdx2      equ       ke2bt2+124
kstatstdxp      equ       ke2bt2+125
kstatn1         equ       ke2bt2+126
kstatn2         equ       ke2bt2+127
kstatlower      equ       ke2bt2+128
kstatupper      equ       ke2bt2+129
kuw0            equ       ke2bt2+130
kimag           equ       ke2bt2+131
ksumy           equ       ke2bt2+132
kxres           equ       ke2bt2+133
kstat_s         equ       ke2bt2+134
ksumy2          equ       ke2bt2+135
ksumxy          equ       ke2bt2+136
kuxres          equ       ke2bt2+137
kmodbox		equ       ke2bt2+138
knormprob	equ       ke2bt2+139
knormalpdf      equ       ke2bt2+140
ktpdf           equ       ke2bt2+141
kchipdf         equ       ke2bt2+142
kfpdf           equ       ke2bt2+143
kminy           equ       ke2bt2+144   ; miny
krandbin        equ       ke2bt2+145
kref            equ       ke2bt2+146
krref           equ       ke2bt2+147
klrsqr          equ       ke2bt2+148
kbrsqr          equ       ke2bt2+149
kdiagon         equ       ke2bt2+150
kdiagoff        equ       ke2bt2+151
kun1            equ       ke2bt2+152   ; for rcl use when gotten from 82
kvn1            equ       ke2bt2+153   ; for rcl use when gotten from 82
;
k83_00end       equ	kvn1            ;end of original keys...
karchive        equ     k83_00end + 1
kunarchive      equ     k83_00end + 2
kasm            equ     k83_00end + 3   ; asm(
kasmprgm        equ     k83_00end + 4   ; asmprgm
kasmcomp        equ     k83_00end + 5   ; asmcomp(
;
kcapaacute	equ       k83_00end + 6
kcapagrave	equ       k83_00end + 7
kcapacaret	equ       k83_00end + 8
kcapadier	equ       k83_00end + 9
kaacute		equ       k83_00end + 10
kagrave		equ       k83_00end + 11
kacaret		equ       k83_00end + 12
kadier		equ       k83_00end + 13
kcapeacute	equ       k83_00end + 14
kcapegrave	equ       k83_00end + 15
kcapecaret	equ       k83_00end + 16
kcapedier	equ       k83_00end + 17
keacute		equ       k83_00end + 18
kegrave		equ       k83_00end + 19
kecaret		equ       k83_00end + 20
kedier		equ       k83_00end + 21
kcapiacute	equ       k83_00end + 22
kcapigrave	equ       k83_00end + 23
kcapicaret	equ       k83_00end + 24
kcapidier	equ       k83_00end + 25
kiacute		equ       k83_00end + 26
kigrave		equ       k83_00end + 27
kicaret		equ       k83_00end + 28
kidier		equ       k83_00end + 29
kcapoacute	equ       k83_00end + 30
kcapograve	equ       k83_00end + 31
kcapocaret	equ       k83_00end + 32
kcapodier	equ       k83_00end + 33
koacute		equ       k83_00end + 34
kograve		equ       k83_00end + 35
kocaret		equ       k83_00end + 36
kodier		equ       k83_00end + 37
kcapuacute	equ       k83_00end + 38
kcapugrave	equ       k83_00end + 39
kcapucaret	equ       k83_00end + 40
kcapudier	equ       k83_00end + 41
kuacute		equ       k83_00end + 42
kugrave		equ       k83_00end + 43
kucaret		equ       k83_00end + 44
kudier		equ       k83_00end + 45
kcapcced	equ       k83_00end + 46
kcced		equ       k83_00end + 47
kcapntilde	equ       k83_00end + 48
kntilde		equ       k83_00end + 49
kaccent		equ       k83_00end + 50
kgrave		equ       k83_00end + 51
kdieresis	equ       k83_00end + 52
kquesdown	equ       k83_00end + 53
kexclamdown     equ       k83_00end + 54
kalpha          equ       k83_00end + 55
kbeta           equ      k83_00end +  56
kgamma          equ      k83_00end +  57
kcapdelta       equ      k83_00end +  58
kdelta          equ      k83_00end +  59
kepsilon        equ      k83_00end +  60
klambda         equ      k83_00end +  61
kmu             equ      k83_00end +  62
kpi2            equ      k83_00end +  63
krho            equ      k83_00end +  64
kcapsigma       equ      k83_00end +  65
ksigma          equ      k83_00end +  66
ktau            equ      k83_00end +  67
kphi            equ      k83_00end +  68
kcapomega       equ      k83_00end +  69
kphat           equ      k83_00end +  70
kchi2           equ      k83_00end +  71
kstatf2         equ      k83_00end +  72
kla		equ     k83_00end + 73
klb		equ     k83_00end + 74
klc		equ     k83_00end + 75
kld		equ     k83_00end + 76
kle		equ     k83_00end + 77
klf		equ     k83_00end + 78
klg		equ     k83_00end + 79
klh		equ     k83_00end + 80
kli		equ     k83_00end + 81
klj		equ     k83_00end + 82
klk		equ     k83_00end + 83
kll		equ     k83_00end + 84
klm		equ     k83_00end + 85
klsmalln	equ    k83_00end +  86
klo		equ    k83_00end +  87
klp		equ    k83_00end +  88
klq		equ    k83_00end +  89
klsmallr	equ    k83_00end +  90
kls		equ    k83_00end +  91
klt		equ    k83_00end +  92
klu		equ    k83_00end +  93
klv		equ    k83_00end +  94
klw		equ    k83_00end +  95
klx		equ    k83_00end +  96
kly		equ    k83_00end +  97
klz		equ    k83_00end +  98
kgarbagec	equ  k83_00end +  99 ; garbagecollect
;
ke2bt2_end	equ     kgarbagec
;
;======================================================================
;           ti-83 plus context equates
;======================================================================
;
cxcmd           equ     kquit                   ;home screen
cxmatedit       equ     kmatrixed               ;matrix editor
cxprgmedit      equ     kprgmed                 ;program editor
cxequedit       equ     kyequ                   ;equation editor
cxgrrange       equ     kwindow                 ;graph range editor
cxgrzfact       equ     kzfacted                ;graph zoom factors editor
cxgraph         equ     kgraph                  ;graph mode
cxstatedit      equ     kstated                 ;statistics list editor
cxprgminput     equ     kprgminput              ;programmed input
cxerror         equ     kerror                  ;error handler
cxlinkio        equ     klinkio                 ;link i/o interface
cxmem           equ     kresetmem               ;reset memory
cxdefmem        equ     kresetdef               ;reset default
cxramapp        equ     kchkram                 ;ram usage screen
cxmode          equ     kmode                   ;mode settings screen
cxerase         equ     kdelmem                 ;memory erase
cxprgmcreate    equ     kprgmcr                 ;program create
cxtableeditor   equ     ktable                  ;table editor
cxtableset      equ     ktblset                 ;table set up
cxstatplot      equ     kstatp                  ;stat plots
cxinfstat	equ	kinfstat		;inferential statistic
cxformat        equ     kformat                 ;format context
cxsolvetvm      equ     ksolvetvm               ;solve tvm
cxsolveroot     equ     ksolveroot              ;solve root
lastoldapp      equ     kextapps                ;external applications
cxextapps       equ     kextapps
cxnewapps       equ     knewapps                ;new cerberus applications
cxgroup         equ     cxnewapps+0             ;1st new app.
cxungroup       equ     cxnewapps+1             ;2nd new app.
lastnewapp      equ     cxungroup               ;last new app for this ver

;
;======================================================================
;           scan code equates
;======================================================================;
skdown          equ	01h
skleft          equ	02h
skright         equ	03h
skup            equ	04h
skenter         equ	09h
skadd           equ	0ah
sksub           equ	0bh
skmul           equ	0ch
skdiv           equ	0dh
skpower         equ	0eh
skclear         equ	0fh
skchs           equ	11h
sk3             equ	12h
sk6             equ	13h
sk9             equ	14h
skrparen        equ	15h
sktan           equ	16h
skvars          equ	17h
skdecpnt        equ	19h
sk2             equ	1ah
sk5             equ	1bh
sk8             equ	1ch
sklparen        equ	1dh
skcos           equ	1eh
skprgm          equ	1fh
skstat          equ	20h
sk0             equ	21h
sk1             equ	22h
sk4             equ	23h
sk7             equ	24h
skcomma         equ	25h
sksin           equ	26h
skmatrix        equ	27h
skgraphvar      equ	28h
skstore         equ	2ah
skln            equ	2bh
sklog           equ	2ch
sksquare        equ	2dh
skrecip         equ	2eh
skmath          equ	2fh
skalpha         equ	30h
skgraph         equ	31h
sktrace         equ	32h
skzoom          equ	33h
skwindow        equ	34h
skyequ          equ	35h
sk2nd           equ	36h
skmode          equ	37h
skdel           equ	38h
;
;======================================================================
;           tokens
;======================================================================
;
eosstart        equ       0
;
;
;   display conversions come immediately before 'tstore'
;
dconv   equ     01h
;
ttodms          equ       dconv       ; 01h
ttodec          equ       dconv+1     ; 02h
ttoabc          equ       dconv+2     ; 03h    > a b/c
;
tstore          equ       dconv+3     ; 04h lstore                01
;
tboxplot        equ       05h
;
bracks          equ       06h
;
tlbrack         equ       bracks      ; 06h '['
trbrack         equ       bracks+1    ; 07h ']'
tlbrace         equ       bracks+2    ; 08h '{'
trbrace         equ       bracks+3    ; 09h '}'
;
tpost1          equ       bracks+4
;
tfromrad        equ       tpost1       ; 0ah lradian
tfromdeg        equ       tpost1+1     ; 0bh ldegree
trecip          equ       tpost1+2     ; 0ch linverse
tsqr            equ       tpost1+3     ; 0dh lsquare
ttrnspos        equ       tpost1+4     ; 0eh ltranspose
tcube           equ       tpost1+5     ; 0fh '^3'
;
tlparen         equ       10h         ; 10h '('
trparen         equ       11h         ; 11h ')'
;
;
imun            equ       12h
;
tround          equ       imun        ; 12h 'round'
tpxtst          equ       imun+1      ; 13h 'pxl-test'
taug            equ       imun+2      ; 14h 'aug'
trowswap        equ       imun+3      ; 15h 'rswap'
trowplus        equ       imun+4      ; 16h 'radd'
tmrow           equ       imun+5      ; 17h 'multr'
tmrowplus       equ       imun+6      ; 18h 'mradd'
tmax            equ       imun+7      ; 19h 'max'
tmin            equ       imun+8      ; 1ah 'min'
trtopr          equ       imun+9      ; 1bh 'r>pr
trtopo          equ       imun+10     ; 1ch 'r>po
tptorx          equ       imun+11     ; 1dh 'p>rx
tptory          equ       imun+12     ; 1eh 'p>ry
tmedian         equ       imun+13     ; 1fh 'median
trandm          equ       imun+14     ; 20h 'randm'
tmean           equ       imun+15     ; 21h
troot           equ       imun+16     ; 22h 'root'
tseries         equ       imun+17     ; 23h 'seq'
tfnint          equ       imun+18     ; 24h 'fnint'
tnderiv         equ       imun+19     ; 25h 'fnir'
tevalf          equ       imun+20     ; 26h
tfmin           equ       imun+21     ; 27h
tfmax           equ       imun+22     ; 28h
;
teosel          equ       imun+23
;
tspace          equ       teosel       ; 29h   ' '
tstring         equ       teosel+1     ; 2ah  '"'
tcomma          equ       teosel+2     ; 2bh  ','
;
;
tii             equ       2ch         ; i
;
;======================================================================
;           postfix functions
;======================================================================
;
tpost           equ       2dh
;
tfact           equ       tpost       ; 2dh '!'
;
tcubicr         equ       2eh
tquartr         equ       2fh

;======================================================================
;           number tokens 
;======================================================================
;
nums            equ       30h
;
t0              equ       nums        ; 30h
t1              equ       nums+1      ; 31h
t2              equ       nums+2      ; 32h
t3              equ       nums+3      ; 33h
t4              equ       nums+4      ; 34h
t5              equ       nums+5      ; 35h
t6              equ       nums+6      ; 36h
t7              equ       nums+7      ; 37h
t8              equ       nums+8      ; 38h
t9              equ       nums+9      ; 39h
tdecpt          equ       nums+10     ; 3ah
tee             equ       nums+11     ; 3bh
;
;======================================================================
;            binary op
;======================================================================
;
tor             equ       3ch         ; 3ch  '_or_'
txor            equ       3dh         ; 3dh
;
tcolon          equ       3eh         ; 3eh ':'
tenter          equ       3fh         ; 3fh  lenter
;
tand            equ       40h         ; 40h  '_and_'       
;
;======================================================================
;            letter tokens   
;======================================================================

;
let             equ       41h
;
ta              equ       let         ; 41h
tb              equ       let+1       ; 42h
tc              equ       let+2       ; 43h
td              equ       let+3       ; 44h
te              equ       let+4       ; 45h
tf              equ       let+5       ; 46h
tg              equ       let+6       ; 47h
th              equ       let+7       ; 48h
ti              equ       let+8       ; 49h
tj              equ       let+9       ; 4ah
tk              equ       let+10      ; 4bh
tl              equ       let+11      ; 4ch
tm              equ       let+12      ; 4dh
tn              equ       let+13      ; 4eh
to              equ       let+14      ; 4fh
tp              equ       let+15      ; 50h
tq              equ       let+16      ; 51h
tr              equ       let+17      ; 52h
ts              equ       let+18      ; 53h
tt              equ       let+19      ; 54h
tu              equ       let+20      ; 55h
tv              equ       let+21      ; 56h
tw              equ       let+22      ; 57h
tx              equ       let+23      ; 58h
ty              equ       let+24      ; 59h
tz              equ       let+25      ; 5ah
ttheta          equ       let+26      ; 5bh
;
;======================================================================
;   these var tokens are 1st of a double token 
;======================================================================

;
vtoks   equ     let+27    ;
;
;       user matrix token, 2nd token needed for name
;
tvarmat         equ       vtoks       ; 5ch
;
;       user list token, 2nd token needed for name
;
tvarlst         equ       vtoks+1     ; 5dh
;
;       user equation token, 2nd token needed for name
;
tvarequ         equ       vtoks+2     ; 5eh
tprog           equ       vtoks+3     ; 5fh
;
;       user pict token, 2nd token needed for name
;
tvarpict        equ       vtoks+4     ; 60h
;
;       user gdb token, 2nd token needed for name
;
tvargdb         equ       vtoks+5     ; 61h
tvarout         equ       vtoks+6     ; 62h
tvarsys         equ       vtoks+7     ; 63h
;
;
;======================================================================
;            mode setting commands
;======================================================================

;
modesa          equ       vtoks+8     ; 64h
;
trad            equ       modesa      ; 64h 'radian'
tdeg            equ       modesa+1    ; 65h 'degree'
tnormf          equ       modesa+2    ; 66h 'normal'
tsci            equ       modesa+3    ; 67h 'sci'
teng            equ       modesa+4    ; 68h 'eng'
tfloat          equ       modesa+5    ; 69h 'float'
;
cmps            equ       6ah
;
teq             equ       cmps        ; 6ah '=='
tlt             equ       cmps+1      ; 6bh '<'
tgt             equ       cmps+2      ; 6ch '>'
tle             equ       cmps+3      ; 6dh lle
tge             equ       cmps+4      ; 6eh lge
tne             equ       cmps+5      ; 6fh lne
;
;======================================================================
;            binary op
;======================================================================
;
tadd            equ       70h         ; 70h '+'
tsub            equ       71h         ; 71h '-'
tans            equ       72h         ; 72h
;
;======================================================================
;            mode setting commands
;======================================================================
;
modes           equ       73h
;
tfix            equ       modes       ; 73h 'fix_'
tspliton        equ       modes+1     ; 74h
tfullscreen     equ       modes+2     ; 75h
tstndrd         equ       modes+3     ; 76h 'func'
tparam          equ       modes+4     ; 77h 'param'
tpolar          equ       modes+5     ; 78h 'pol'
tseqg           equ       modes+6     ; 79h
tafillon        equ       modes+7     ; 7ah 'auto fill on
tafilloff       equ       modes+8     ; 7bh
tacalcon        equ       modes+9     ; 7ch
tacalcoff       equ       modes+10    ; 7dh 'autofill off
;
;       graph format tokens are 2 byte tokens
;
tgformat        equ       modes+11    ; 7eh
;
tboxicon        equ       7fh
tcrossicon      equ       80h
tdoticon        equ       81h
;
;======================================================================
;            (more) binary op
;======================================================================
;
tmul            equ       82h         ; 82h  '*'
tdiv            equ       83h         ; 83h  '/'
;
;======================================================================
;            some graph commands
;======================================================================
;
grcmds          equ       84h
;
ttrace          equ       grcmds      ; 84h 'trace'
tcldrw          equ       grcmds+1    ; 85h 'cldrw'
tzoomstd        equ       grcmds+2    ; 86h 'zstd'
tzoomtrg        equ       grcmds+3    ; 87h 'ztrg'
tzoombox        equ       grcmds+4    ; 88h 'zbox'
tzoomin         equ       grcmds+5    ; 89h 'zin'
tzoomout        equ       grcmds+6    ; 8ah 'zout'
tzoomsqr        equ       grcmds+7    ; 8bh 'zsqr'
tzoomint        equ       grcmds+8    ; 8ch 'zint'
tzoomprev       equ       grcmds+9    ; 8dh 'zprev'
tzoomdec        equ       grcmds+10   ; 8eh 'zdecm'
tzoomstat       equ       grcmds+11   ; 8fh 'zstat
tusrzm          equ       grcmds+12   ; 90h 'zrcl'
tprtscrn        equ       grcmds+13   ; 91h 'prtscrn'
tzoomsto        equ       grcmds+14   ; 92h  zoom store
ttext           equ       grcmds+15   ; 93h
;
;======================================================================
;           binary op  (combination & permutation)
;======================================================================
;
tnpr            equ       grcmds+16   ; 94h '_npr_'
tncr            equ       grcmds+17   ; 95h '_ncr_'
;
;======================================================================
;           more graph commands
;======================================================================
;
tyon            equ       grcmds+18   ; 96h 'fnon_'
tyoff           equ       grcmds+19   ; 97h 'fnoff_'
tstpic          equ       grcmds+20   ; 98h 'stpic_'
trcpic          equ       grcmds+21   ; 99h 'rcpic_'
tstodb          equ       grcmds+22   ; 9ah 'stgdb_'
trcldb          equ       grcmds+23   ; 9bh 'rcgdb_'
tline           equ       grcmds+24   ; 9ch 'line'
tvert           equ       grcmds+25   ; 9dh 'vert_'
tpton           equ       grcmds+26   ; 9eh 'pton'
tptoff          equ       grcmds+27   ; 9fh 'ptoff'
;
;======================================================================
;           token a0 cannot be an eos function
;           since low mult=a0 already
;======================================================================
;
tptchg          equ       grcmds+28   ; a0h 'ptchg'
tpxon           equ       grcmds+29   ; a1h
tpxoff          equ       grcmds+30   ; a2h
tpxchg          equ       grcmds+31   ; a3h
tshade          equ       grcmds+32   ; a4h 'shade'
tcircl          equ       grcmds+33   ; a5h 'circl'
thorz           equ       grcmds+34   ; a6h 'horizontal'
ttanln          equ       grcmds+35   ; a7h 'tanln'
tdrinv          equ       grcmds+36   ; a8h 'drinv_'
tdrawf          equ       grcmds+37   ; a9h 'drawf_'
;
tvarstrng       equ       0aah
;
;======================================================================
;           functions with no arguments                                    
;======================================================================
;
noarg   equ     0abh
;
trand           equ       noarg       ; abh 'rand'
tpi             equ       noarg+1     ; ach  lpi
tgetkey         equ       noarg+2     ; adh 'getky'
;
;
tapost          equ       tgetkey+1   ; apostrophy
tquest          equ       tapost+1    ; question mark
;
;
unary   equ     tquest+1              ; b0h
;
tchs            equ       unary       ; b0h
tint            equ       unary+1     ; b1h
tabs            equ       unary+2     ; b2h
tdet            equ       unary+3     ; b3h
tident          equ       unary+4     ; b4h
tdim            equ       unary+5     ; b5h
tsum            equ       unary+6     ; b6h
tprod           equ       unary+7     ; b7h
tnot            equ       unary+8     ; b8h
tipart          equ       unary+9     ; b9h
tfpart          equ       unary+10    ; bah
;
;
;======================================================================
;           new 2 byte tokens
;======================================================================
;
t2bytetok       equ       0bbh         ;
;
;
unarylr equ     unary+12
;
tsqrt           equ       unarylr     ; bch
tcubrt          equ       unarylr+1   ; bdh
tln             equ       unarylr+2   ; beh
texp            equ       unarylr+3   ; bfh
tlog            equ       unarylr+4   ; c0h
talog           equ       unarylr+5   ; c1h
tsin            equ       unarylr+6   ; c2h
tasin           equ       unarylr+7   ; c3h
tcos            equ       unarylr+8   ; c4h
tacos           equ       unarylr+9   ; c5h
ttan            equ       unarylr+10  ; c6h
tatan           equ       unarylr+11  ; c7h
tsinh           equ       unarylr+12  ; c8h
tasinh          equ       unarylr+13  ; c9h
tcosh           equ       unarylr+14  ; cah
tacosh          equ       unarylr+15  ; cbh
ttanh           equ       unarylr+16  ; cch
tatanh          equ       unarylr+17  ; cdh
; 
;======================================================================
;           some programming commands                                      
;======================================================================
;
progtok equ     unarylr+18
;
tif             equ       progtok     ; ceh
tthen           equ       progtok+1   ; cfh
telse           equ       progtok+2   ; d0h
twhile          equ       progtok+3   ; d1h
trepeat         equ       progtok+4   ; d2h
tfor            equ       progtok+5   ; d3h
tend            equ       progtok+6   ; d4h
treturn         equ       progtok+7   ; d5h
tlbl            equ       progtok+8   ; d6h   'lbl_'
tgoto           equ       progtok+9   ; d7h   'goto_'
tpause          equ       progtok+10  ; d8h   'pause_'
tstop           equ       progtok+11  ; d9h   'stop'
tisg            equ       progtok+12  ; dah   'is>'
tdsl            equ       progtok+13  ; dbh   'ds<'
tinput          equ       progtok+14  ; dch   'input_'
tprompt         equ       progtok+15  ; ddh   'prompt_'
tdisp           equ       progtok+16  ; deh   'disp_'
tdispg          equ       progtok+17  ; dfh   'dispg'
;
toutput         equ       progtok+18  ; e0h    'outpt'
tcllcd          equ       progtok+19  ; e1h    'cllcd'
tconst          equ       progtok+20  ; e2h    'fill'
tsorta          equ       progtok+21  ; e3h    'sorta_'
tsortd          equ       progtok+22  ; e4h    'sortd_'
tdisptab        equ       progtok+23  ; e5h    'disp table
tmenu           equ       progtok+24  ; e6h    'menu'
tsendmbl        equ       progtok+25  ; e7h    'send'
tgetmbl         equ       progtok+26  ; e8h    'get'
;
;======================================================================
;           stat plot commands                                             
;======================================================================
;
statpcmd        equ       progtok+27
;
tploton         equ       statpcmd    ; e9h     ' plotson'
tplotoff        equ       statpcmd+1   ; eah     ' plotsoff
;
tlistname       equ       0ebh        ; list designator
;
tplot1          equ       0ech
tplot2          equ       0edh
tplot3          equ       0eeh
;
tunused01       equ       0efh        ;        available?
;
tpower          equ       0f0h        ;        '^'
txroot          equ       0f1h        ;        lsupx,lroot

statcmd         equ       0f2h
;
tonevar         equ       statcmd     ; f2h    'onevar_'
ttwovar         equ       statcmd+1   ; f3h
tlr             equ       statcmd+2   ; f4h    'linr(a+bx
tlrexp          equ       statcmd+3   ; f5h    'expr_'
tlrln           equ       statcmd+4   ; f6h    'lnr_'
tlrpwr          equ       statcmd+5   ; f7h    'pwrr_'
tmedmed         equ       statcmd+6   ; f8h
tquad           equ       statcmd+7   ; f9h
tclrlst         equ       statcmd+8   ; fah    'clear list
tclrtbl         equ       statcmd+9   ; fbh    clear table
thist           equ       statcmd+10  ; fch    'hist_'
txyline         equ       statcmd+11  ; fdh    'xyline_'
tscatter        equ       statcmd+12  ; feh    'scatter_'
tlr1            equ       statcmd+13  ; ffh    'linr(ax+b

;       
;======================================================================
;           2nd half of graph format tokens
;======================================================================
;           format settings commands
;
gfmt            equ       0
;
tseq            equ       gfmt        ; 'seqg'
tsimulg         equ       gfmt+1      ; 'simulg'
tpolarg         equ       gfmt+2      ; 'polargc'
trectg          equ       gfmt+3      ; 'rectgc'
tcoordon        equ       gfmt+4      ; 'coordon'
tcoordoff       equ       gfmt+5      ; 'coordoff'
tdrawline       equ       gfmt+6      ; 'drawline'
tdrawdot        equ       gfmt+7      ; 'drawdot'
taxison         equ       gfmt+8      ; 'axeson'
taxisoff        equ       gfmt+9      ; 'axesoff'
tgridon         equ       gfmt+10     ; 'gridon'
tgridoff        equ       gfmt+11     ; 'gridoff'
tlblon          equ       gfmt+12     ; 'labelon'
tlbloff         equ       gfmt+13     ; 'labeloff'
twebon          equ       gfmt+14     ; 'webon'
tweboff         equ       gfmt+15     ; 'weboff'
tuv             equ       gfmt+16     ; u vs v
tvw             equ       gfmt+17     ; v vs w
tuw             equ       gfmt+18     ; u vs w
;
;======================================================================
;           2nd half of user matrix tokens                                 
;======================================================================
;
tmata           equ       00h         ; mat a
tmatb           equ       01h         ; mat b
tmatc           equ       02h         ; mat c
tmatd           equ       03h         ; mat d
tmate           equ       04h         ; mat e
tmatf           equ       05h         ; mat f
tmatg           equ       06h         ; mat g
tmath           equ       07h         ; mat h
tmati           equ       08h         ; mat i
tmatj           equ       09h         ; mat j
;      
;======================================================================
;           2nd half of user list tokens
;======================================================================
;
tl1             equ       00h         ; list 1
tl2             equ       01h         ; list 2
tl3             equ       02h         ; list 3
tl4             equ       03h         ; list 4
tl5             equ       04h         ; list 5
tl6             equ       05h         ; list 6

;
;======================================================================
;           2nd half of user equation tokens
;======================================================================
;
;       "y" equations have bit 4 set
;
ty1             equ       10h         ; y1
ty2             equ       11h         ; y2
ty3             equ       12h         ; y3
ty4             equ       13h         ; y4
ty5             equ       14h         ; y5
ty6             equ       15h         ; y6
ty7             equ       16h         ; y7
ty8             equ       17h         ; y8
ty9             equ       18h         ; y9
ty0             equ       19h         ; y0
;
;       param equations have bit 5 set
;
tx1t            equ       20h         ; x1t
ty1t            equ       21h         ; y1t
tx2t            equ       22h         ; x2t
ty2t            equ       23h         ; y2t
tx3t            equ       24h         ; x3t
ty3t            equ       25h         ; y3t
tx4t            equ       26h         ; x4t
ty4t            equ       27h         ; y4t
tx5t            equ       28h         ; x5t
ty5t            equ       29h         ; y5t
tx6t            equ       2ah         ; x6t
ty6t            equ       2bh         ; y6t
;
;       polar equations have bit 6 set
;
tr1             equ       40h         ; r1
tr2             equ       41h         ; r2
tr3             equ       42h         ; r3
tr4             equ       43h         ; r4
tr5             equ       44h         ; r5
tr6             equ       45h         ; r6
;
;       recursion equations have bit 7 set
;
tun             equ       80h         ; un
tvn             equ       81h         ; vn
twn             equ       82h         ; wn
;
;======================================================================
;           2nd half of user picture tokens                                
;======================================================================
;
tpic1           equ       00h         ; pic1
tpic2           equ       01h         ; pic2
tpic3           equ       02h         ; pic3
tpic4           equ       03h         ; pic4
tpic5           equ       04h         ; pic5
tpic6           equ       05h         ; pic6
tpic7           equ       06h         ; pic7
tpic8           equ       07h         ; pic8
tpic9           equ       08h         ; pic9
tpic0           equ       09h         ; pic0
; 
;======================================================================
;           2nd half of user graph database tokens                         
;======================================================================
;
tgdb1           equ       00h         ; gdb1
tgdb2           equ       01h         ; gdb2
tgdb3           equ       02h         ; gdb3
tgdb4           equ       03h         ; gdb4
tgdb5           equ       04h         ; gdb5
tgdb6           equ       05h         ; gdb6
tgdb7           equ       06h         ; gdb7
tgdb8           equ       07h         ; gdb8
tgdb9           equ       08h         ; gdb9
tgdb0           equ       09h         ; gdb0
;  
;======================================================================
;           2nd half of string vars                                        
;======================================================================
;
tstr1           equ       00h
tstr2           equ       01h
tstr3           equ       02h
tstr4           equ       03h
tstr5           equ       04h
tstr6           equ       05h
tstr7           equ       06h
tstr8           equ       07h
tstr9           equ       08h
tstr0           equ       09h

; 
;======================================================================
;           2nd half of system output only variables                       
;======================================================================
;
;
;   open        equ       00h
;
tregeq          equ       01h         ; regression equation
;
tstatn          equ       02h         ; statistics n
;
txmean          equ       03h         ; x mean
tsumx           equ       04h         ; sum(x)
tsumxsqr        equ       05h         ; sum(x^2)
tstdx           equ       06h         ; standard dev x
tstdpx          equ       07h         ; standard dev pop x
tminx           equ       08h         ; min x value
tmaxx           equ       09h         ; max x value
;
tminy           equ       0ah         ; min y value
tmaxy           equ       0bh         ; max y value
tymean          equ       0ch         ; y mean
tsumy           equ       0dh         ; sum(y)
tsumysqr        equ       0eh         ; sum(y^2)
tstdy           equ       0fh         ; standard dev y
tstdpy          equ       10h         ; standard dev pop y
;
tsumxy          equ       11h         ; sum(xy)
tcorr           equ       12h         ; correlation
tmedx           equ       13h         ; med(x)
tq1             equ       14h         ; 1st quadrant of x
tq3             equ       15h         ; 3rd quadrant of x
tquada          equ       16h         ; 1st term of quad poly reg/ y-int
tquadb          equ       17h         ; 2nd term of quad poly reg/ slope
tquadc          equ       18h         ; 3rd term of quad poly reg
tcubed          equ       19h         ; 4th term of cubic poly reg
tquarte         equ       1ah         ; 5th term of quart poly reg
tmedx1          equ       1bh         ; x1 for med-med
tmedx2          equ       1ch         ; x2 for med-med
tmedx3          equ       1dh         ; x3 for med-med
tmedy1          equ       1eh         ; y1 for med-med
tmedy2          equ       1fh         ; y2 for med-med
tmedy3          equ       20h         ; y3 for med-med
;
trecurn		equ       21h		; recursion n
tstatp          equ       22h
tstatz          equ       23h
tstatt          equ       24h
tstatchi        equ       25h
tstatf          equ       26h
tstatdf         equ       27h
tstatphat       equ       28h
tstatphat1      equ       29h
tstatphat2      equ       2ah
tstatmeanx1     equ       2bh
tstatstdx1      equ       2ch
tstatn1         equ       2dh
tstatmeanx2     equ       2eh
tstatstdx2      equ       2fh
tstatn2         equ       30h
tstatstdxp      equ       31h
tstatlower      equ       32h
tstatupper      equ       33h
tstat_s         equ       34h
tlrsqr          equ       35h         ; r^2
tbrsqr          equ       36h         ; r^2
;
;======================================================================
;       these next tokens are only used to access the data
;       they are display only and the user cannot access them at all
;======================================================================
;
tf_df    equ     37h       ; anofav factor df
tf_ss    equ     38h       ; anofav factor ss
tf_ms    equ     39h       ; anofav factor ms
te_df    equ     3ah       ; anofav error df
te_ss    equ     3bh       ; anofav error ss
te_ms    equ     3ch       ; anofav error ms
;
;
;======================================================================
;           2nd half of system input/output variables
;======================================================================
;      system variable equates
;
tuxscl       equ  0
tuyscl       equ  1
txscl        equ  2
tyscl        equ  3
trecuru0     equ  4        ; u 1st initial cond
trecurv0     equ  5        ; v 1st initial cond
tun1         equ  6        ; u(n-1)    ; not used
tvn1         equ  7        ; v(n-1)    ; not used
turecuru0    equ  8        ;
turecurv0    equ  9        ;
;
txmin        equ  0ah
txmax        equ  0bh
tymin        equ  0ch
tymax        equ  0dh
ttmin        equ  0eh
ttmax        equ  0fh
tthetamin    equ  10h
tthetamax    equ  11h
tuxmin       equ  12h
tuxmax       equ  13h
tuymin       equ  14h
tuymax       equ  15h
tuthetmin    equ  16h
tuthetmax    equ  17h
tutmin       equ  18h
tutmax       equ  19h
ttblmin      equ  1ah
tplotstart   equ  1bh
tuplotstart  equ  1ch
tnmax        equ  1dh
tunmax       equ  1eh
tnmin        equ  1fh
tunmin       equ  20h
;
ttblstep	equ  21h
ttstep		equ  22h
tthetastep	equ  23h
tutstep		equ  24h
tuthetstep	equ  25h
;
tdeltax		equ  26h
tdeltay		equ  27h
;
txfact		equ  28h
tyfact		equ  29h
;
ttblinput       equ       2ah
;
tfinn           equ       2bh
tfini           equ       2ch
tfinpv          equ       2dh
tfinpmt         equ       2eh
tfinfv          equ       2fh
tfinpy          equ       30h
tfincy          equ       31h
;
trecurw0        equ       32h         ; w0(1)
turecurw0       equ       33h
;
tplotstep       equ       34h
tuplotstep      equ       35h
;
txres           equ       36h
tuxres          equ       37h
;
trecuru02       equ       38h         ; u0(2)
turecuru02      equ       39h
trecurv02       equ       3ch         ; v0(2)
turecurv02      equ       3dh
trecurw02       equ       3eh         ; w0(2)
turecurw02      equ       3fh
;
;======================================================================
;           2nd byte of t2bytetok tokens
;======================================================================
;
tfinnpv         equ       00h         ;
tfinirr         equ       01h         ;
tfinbal         equ       02h         ;
tfinprn         equ       03h         ;
tfinint         equ       04h         ;
tfintonom       equ       05h         ;
tfintoeff       equ       06h         ;
tfindbd         equ       07h         ;
tlcm            equ       08h         ;
tgcd            equ       09h         ;
trandint        equ       0ah         ;
trandbin        equ       0bh         ;
tsubstrng       equ       0ch         ;
tstddev         equ       0dh         ;
tvariance       equ       0eh         ;
tinstrng        equ       0fh         ;
tdnormal        equ       10h         ;
tinvnorm        equ       11h         ;
tdt             equ       12h         ;
tchi            equ       13h         ;
tdf             equ       14h         ;
tbinpdf         equ       15h         ;
tbincdf         equ       16h         ;
tpoipdf         equ       17h         ;
tpoicdf         equ       18h         ;
tgeopdf         equ       19h         ;
tgeocdf         equ       1ah         ;
tnormalpdf      equ       1bh         ;
ttpdf           equ       1ch         ;
tchipdf         equ       1dh         ;
tfpdf           equ       1eh         ;
trandnorm       equ       1fh         ;
tfinfpmt        equ       20h         ;
tfinfi          equ       21h         ;
tfinfpv         equ       22h         ;
tfinfn          equ       23h         ;
tfinffv         equ       24h         ;
tconj           equ       25h         ;
treal           equ       26h         ;
timag           equ       27h         ;
tangle          equ       28h         ;
tcumsum         equ       29h         ;
texpr           equ       2ah         ;
tlength         equ       2bh         ;
tdeltalst       equ       2ch         ;
tref            equ       2dh         ;
trref           equ       2eh         ;
ttorect         equ       2fh         ;
ttopolar        equ       30h         ;
tconste         equ       31h         ;
tsinreg         equ       32h         ;
tlogistic       equ       33h         ;
tlinregttest    equ       34h         ;
tshadenorm      equ       35h         ;
tshadet         equ       36h         ;
tshadechi       equ       37h         ;
tshadef         equ       38h         ;
tmattolst       equ       39h         ;
tlsttomat       equ       3ah         ;
tztest          equ       3bh         ;
tttest          equ       3ch         ;
t2sampztest     equ       3dh         ;
t1propztest     equ       3eh         ;
t2propztest     equ       3fh         ;
tchitest        equ       40h         ;
tzintval        equ       41h         ;
t2sampzint      equ       42h         ;
t1propzint      equ       43h         ;
t2propzint      equ       44h         ;
tgraphstyle     equ       45h         ;
t2sampttest     equ       46h         ;
t2sampftest     equ       47h         ;
ttintval        equ       48h         ;
t2samptint      equ       49h         ;
tsetuplst       equ       4ah         ;
tfinpmtend      equ       4bh         ;
tfinpmtbeg      equ       4ch         ;
trealm          equ       4dh         ;
tpolarm         equ       4eh         ;
trectm          equ       4fh         ;
texpron         equ       50h         ;
texproff        equ       51h         ;
tclralllst      equ       52h         ;
tgetcalc        equ       53h         ;
tdelvar         equ       54h         ;
tequtostrng     equ       55h         ;
tstrngtoequ     equ       56h         ;
tdellast        equ       57h         ;
tselect         equ       58h         ;
tanova          equ       59h         ;
tmodbox         equ       5ah         ;
tnormprob       equ       5bh         ;
;
;
tmgt            equ       64h         ; vertical split
tzfit           equ       65h         ; zoom fit
tdiag_on        equ       66h         ; dianostic display on
tdiag_off       equ       67h         ; dianostic display off
tokend2v0       equ       67h         ;end of 2byte tokens for version 0.
tarchive        equ       68h         ;archive
tunarchive      equ       69h         ;unarchive
tasm		equ	  6ah
tasmcomp	equ	  6bh	      ; asm compile
tasmprgm	equ	  6ch	      ; signifies a program is asm
tasmcmp 	equ	  6dh	      ; asm program is compiled
;
tlcapaacute     equ       6eh
tlcapagrave     equ       6fh
tlcapacaret     equ       70h
tlcapadier      equ       71h
tlaacute        equ       72h
tlagrave        equ       73h
tlacaret        equ       74h
tladier         equ       75h
tlcapeacute     equ       76h
tlcapegrave     equ       77h
tlcapecaret     equ       78h
tlcapedier      equ       79h
tleacute        equ       7ah
tlegrave        equ       7bh
tlecaret        equ       7ch
tledier         equ       7dh
;
tlcapigrave     equ       7fh
tlcapicaret     equ       80h
tlcapidier      equ       81h
tliacute        equ       82h
tligrave        equ       83h
tlicaret        equ       84h
tlidier         equ       85h
tlcapoacute     equ       86h
tlcapograve     equ       87h
tlcapocaret     equ       88h
tlcapodier      equ       89h
tloacute        equ       8ah
tlograve        equ       8bh
tlocaret        equ       8ch
tlodier         equ       8dh
tlcapuacute     equ       8eh
tlcapugrave     equ       8fh
tlcapucaret     equ       90h
tlcapudier      equ       91h
tluacute        equ       92h
tlugrave        equ       93h
tlucaret        equ       94h
tludier         equ       95h
tlcapcced       equ       96h
tlcced          equ       97h
tlcapntilde     equ       98h
tlntilde        equ       99h
tlaccent        equ       9ah
tlgrave         equ       9bh
tldieresis      equ       9ch
tlquesdown      equ       9dh
tlexclamdown    equ       9eh
tlalpha         equ       9fh
tlbeta          equ      0a0h
tlgamma         equ      0a1h
tlcapdelta      equ      0a2h
tldelta         equ      0a3h
tlepsilon       equ      0a4h
tllambda        equ      0a5h
tlmu            equ      0a6h
tlpi            equ      0a7h
tlrho           equ      0a8h
tlcapsigma      equ      0a9h
;
;
tlphi           equ      0abh
tlcapomega      equ      0ach
tlphat          equ      0adh
tlchi           equ      0aeh
tlstatf         equ      0afh
;

tla      equ     0b0h
tlb      equ     0b1h
tlc      equ     0b2h
tld      equ     0b3h
tlsmalle equ     0b4h
tlf      equ     0b5h
tlsmallg equ     0b6h
tlh      equ     0b7h
tli      equ     0b8h
tlj      equ     0b9h
tlk      equ     0bah
;

tll      equ     0bch
tlm      equ     0bdh
tln      equ     0beh
tlo      equ     0bfh
tlp      equ     0c0h
tlq      equ     0c1h
tlsmallr equ     0c2h
tls      equ     0c3h
tlsmallt equ     0c4h
tlu      equ     0c5h
tlv      equ     0c6h
tlw      equ     0c7h
tlx      equ     0c8h
tly      equ     0c9h
tlz      equ     0cah
tlsigma         equ      0cbh
tltau           equ      0cch
tlcapiacute     equ      0cdh
;
;
tgarbagec equ    0ceh
lasttoken       equ      0ceh         ;tlast token in this version...
;
;======================================================================
;           data type equates                                                    
;======================================================================
;
realobj         equ       0
listobj         equ       1
matobj          equ       2
equobj          equ       3
strngobj        equ       4
progobj         equ       5
protprogobj     equ       6
pictobj         equ       7
gdbobj          equ       8
unknownobj      equ       9
unknownequobj   equ       0ah
newequobj       equ       0bh
cplxobj         equ       0ch
clistobj        equ       0dh
undefobj        equ       0eh
windowobj       equ       0fh
zstoobj         equ       10h
tblrngobj       equ       11h
lcdobj          equ       12h
backupobj       equ       13h
appobj		equ	  14h		;application, only used in menus/link
appvarobj	equ	  15h		;application variable
tempprogobj     equ       16h           ;program, home deletes when finished
groupobj        equ       17h           ;group.
; 
;======================================================================
;	    parser equates
;======================================================================
oprand      equ              trand
opprod      equ              tprod
opsum       equ              tsum
opadd       equ              tadd
opsub       equ              tsub
opmult      equ              tmul
opdiv       equ              tdiv
oppower     equ              tpower
opxroot     equ              txroot
opeq        equ              teq
opneq       equ              tne
opround2    equ              tround
opaug       equ              taug
opmax       equ              tmax
opmin       equ              tmin
opand       equ              tand
opor        equ              tor
opxor       equ              txor
opncr       equ              tncr
opnpr       equ              tnpr
oplt        equ              tlt
ople        equ              tle
opgt        equ              tgt
opge        equ              tge
;
oplog       equ              tlog
optenx      equ              talog
opln        equ              tln
opetox      equ              texp
opnot       equ              tnot
opsin       equ              tsin
opasin      equ              tasin
opcos       equ              tcos
opacos      equ              tacos
optan       equ              ttan
opatan      equ              tatan
opsinh      equ              tsinh
opasinh     equ              tasinh
opcosh      equ              tcosh
opacosh     equ              tacosh
optanh      equ              ttanh
opatanh     equ              tatanh
opinverse   equ              trecip
opdet       equ              tdet
opsqroot    equ              tsqrt
opsquare    equ              tsqr
opnegate    equ              tchs
opipart     equ              tipart
opfpart     equ              tfpart
opintgr     equ              tint
opfact      equ              tfact
opabs       equ              tabs
opident     equ              tident
optranspose equ              ttrnspos
opsorta     equ              tsorta
opsortd     equ              tsortd
optodec     equ              ttodec
optofrac    equ              ttoabc
opradd      equ              trowplus
oprmlt      equ              tmrow
oprswap     equ              trowswap
opmltradd   equ              tmrowplus
oprand1     equ              trandm
opsolve     equ              troot
opderiv81   equ              tnderiv
opquad      equ              tfnint
opseq       equ              tseries
opdim       equ              tdim
oprad       equ              tfromrad
opdeg       equ              tfromdeg
oprtopr     equ              trtopr
oprtopo     equ              trtopo
opptorx     equ              tptorx
opptory     equ              tptory
opcube      equ              tcube
opcbrt      equ              tcubrt
opfmin      equ              tfmin
opfmax      equ              tfmax
opnpv           equ       tif         ; immediate unary
opirr           equ       tif+1       ; immediate unary
opbal           equ       tif+2       ; immediate unary
opprn           equ       tif+3       ; immediate unary
opintr          equ       tif+4       ; immediate unary
optonom         equ       tif+5       ; immediate unary
optoeff         equ       tif+6       ; immediate unary
opfindbd           equ       tif+7       ; immediate unary
oplcm           equ       tif+8       ; immediate unary
opgcd           equ       tif+9       ; immediate unary
oprandint       equ       tif+10      ; immediate unary
oprandbin        equ      tif+11      ; immediate unary
opsubstr        equ       tif+12      ; immediate unary
opstdev         equ       tif+13      ; immediate unary
opvariance      equ       tif+14      ; immediate unary
opinstr         equ       tif+15      ; immediate unary
opnormal        equ       tif+16      ; immediate unary
opinormal       equ       tif+17      ; immediate unary
opdt            equ       tif+18      ; immediate unary
opchi           equ       tif+19      ; immediate unary
opdf            equ       tif+20      ; immediate unary
opbinpdf        equ       tif+21      ; immediate unary
opbincdf        equ       tif+22      ; immediate unary
oppoipdf        equ       tif+23      ; immediate unary
oppoicdf        equ       tif+24      ; immediate unary
opgeopdf        equ       tif+25      ; immediate unary
opgeocdf        equ       tif+26      ; immediate unary
opnormalpdf     equ       tif+27      ; immediate unary
optpdf          equ       tif+28      ; immediate unary
opchipdf        equ       tif+29      ; immediate unary
opfpdf          equ       tif+30      ; immediate unary
oprandnrm       equ       tif+31      ; immediate unary
opconj           equ       tzoomin
opreal           equ       tzoomin	   +1
opimag           equ       tzoomin	   +2
opangle          equ       tzoomin	   +3
opcumsum         equ       tzoomin	   +4
opexpr           equ       tzoomin	   +5
oplength         equ       tzoomin	   +6
opdeltalst       equ       tzoomin	   +7
opref            equ       tzoomin	   +8
oprref           equ       tzoomin	   +9
opconst          equ       tzoomin	   +10

; 
;======================================================================
;           i/o equates                                                          
;======================================================================
;
d0d1_bits	equ  03h
d0ld1l		equ  03h
d0ld1h		equ  01h
d0hd1l		equ  02h
d0hd1h		equ  00h
bport		equ  0			; 4-bit link port (i/o) 
;
;======================================================================
;	interrupt equates
;======================================================================
;
iall	equ 1011b
intrptenport	equ 3 ; interrupt enable (in/out) 
;
;======================================================================
;	Memory paging equates
;======================================================================
;
mempageaport	equ 6 ; memory page A (in/out)
;
;======================================================================
;	lcd driver equates
;======================================================================
lcdinstport	equ 10h
lcddataport equ 11h
;
;======================================================================
;           device codes                                                       
;======================================================================
;
;
ti82dev		equ  82h
pc82dev		equ  02h
mac82dev	equ  12h
;
ti83fdev	equ  73h
link83fdev	equ  23h
;
ti83dev		equ  83h
pc83dev		equ  03h
mac83dev	equ  13h
;
ti85dev		equ  95h      ; diff then real 85 so me talk
pc85dev		equ  05h
mac85dev	equ  15h
;
ti73dev		equ  74h      ; device x3 is always an 83
pc73dev		equ  07h
mac73dev	equ  17h
;
ti83fdev	equ  73h
link73fdev	equ  23h
pc83fdev	equ  23h
;
;======================================================================
;           system error codes                                                  
;======================================================================
;
e_editf		equ	7		;allow re-entering application
e_edit		equ	1<<e_editf
e_mask		equ	7fh

e_overflow	equ	1+e_edit
e_divby0	equ	2+e_edit
e_singularmat	equ	3+e_edit
e_domain	equ	4+e_edit
e_increment	equ	5+e_edit
e_break		equ	6+e_edit
e_nonreal       equ     7+e_edit
e_syntax        equ     8+e_edit
e_datatype      equ     9+e_edit
e_argument      equ     10+e_edit
e_dimmismatch   equ     11+e_edit
e_dimension     equ     12+e_edit
e_undefined     equ     13+e_edit
e_memory        equ     14+e_edit
e_invalid       equ     15+e_edit
e_illegalnest   equ     16+e_edit
e_bound         equ     17+e_edit
e_graphrange    equ     18+e_edit
e_zoom          equ     19+e_edit
e_label         equ     20
e_stat          equ     21
e_solver        equ     22+e_edit
e_singularity   equ     23+e_edit
e_signchange    equ     24+e_edit
e_iterations    equ     25+e_edit
e_badguess      equ     26+e_edit
e_statplot      equ     27
e_toltoosmall   equ     28+e_edit
e_reserved      equ     29+e_edit
e_mode          equ     30+e_edit
e_lnkerr        equ     31+e_edit
e_lnkmemerr     equ     32+e_edit
e_lnktranserr   equ     33+e_edit
e_lnkduperr     equ     34+e_edit
e_lnkmemfull    equ     35+e_edit
e_unknown       equ     36+e_edit

e_scale         equ     37+e_edit
e_idnotfound    equ     38
e_nomode        equ     39+e_edit
e_validation    equ     40
e_length        equ     41+e_edit
e_application   equ     42+e_edit
e_apperr1       equ     43+e_edit
e_apperr2       equ     44+e_edit
e_expiredapp	equ	45
e_badaddr	equ	46
e_archived      equ     47+e_edit
e_version       equ     48
e_archfull      equ     49
e_variable      equ     50+e_edit
e_duplicate     equ     51+e_edit
higerrnum       equ     51


; obsolete error numbers34              ;first link error
e_linkiochksum	equ	34
e_linkiotimeout	equ	35
e_linkiobusy	equ	36
e_linkiover	equ	37

;
;======================================================================
;           equates to ram locations for stat vars 
;======================================================================
;
;
fplen	equ	9		;length of a floating-point number.
statn   equ     statvars
xmean   equ     statn      + fplen
sumx    equ     xmean      + fplen
sumxsqr equ     sumx       + fplen
stdx    equ     sumxsqr    + fplen
stdpx   equ     stdx       + fplen
minx    equ     stdpx      + fplen
maxx    equ     minx       + fplen
miny    equ     maxx       + fplen
maxy    equ     miny       + fplen
ymean   equ     maxy       + fplen
sumy    equ     ymean      + fplen
sumysqr equ     sumy       + fplen
stdy    equ     sumysqr    + fplen
stdpy   equ     stdy       + fplen
sumxy   equ     stdpy      + fplen
corr    equ     sumxy      + fplen
medx    equ     corr       + fplen
q1      equ     medx       + fplen
q3      equ     q1         + fplen
quada   equ     q3         + fplen
quadb   equ     quada      + fplen
quadc   equ     quadb      + fplen
cubed   equ     quadc      + fplen
quarte  equ     cubed      + fplen
medx1   equ     quarte     + fplen
medx2   equ     medx1      + fplen
medx3   equ     medx2      + fplen
medy1   equ     medx3      + fplen
medy2   equ     medy1      + fplen
medy3   equ     medy2      + fplen
pstat   equ     medy3      + 2*fplen
zstat   equ     pstat      + fplen
tstat   equ     zstat      + fplen
chistat equ     tstat      + fplen
fstat   equ     chistat    + fplen
df      equ     fstat      + fplen
phat    equ     df         + fplen
phat1   equ     phat       + fplen
phat2   equ     phat1      + fplen
meanx1  equ     phat2      + fplen
stdx1   equ     meanx1     + fplen
statn1  equ     stdx1      + fplen
meanx2  equ     statn1     + fplen
stdx2   equ     meanx2     + fplen
statn2  equ     stdx2      + fplen
stdxp2  equ     statn2     + fplen
slower  equ     stdxp2     + fplen
supper  equ     slower     + fplen
sstat   equ     supper     + fplen

f_df	equ     anovaf_vars
f_ss	equ     f_df + fplen
f_ms	equ     f_ss + fplen
e_df	equ     f_ms + fplen
e_ss	equ     e_df + fplen
e_ms	equ     e_ss + fplen

