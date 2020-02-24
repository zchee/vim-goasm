" Copyright 2020 The vim-goasm Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.

" vim-goasm: Vim syntax highlighting for Go Plan9 assembly.

" Language:     Go Plan9 assembly
" Maintainer:   Koichi Shiraishi <zchee.io@gmail.com>
" License:      BSD (3 clause), see LICENSE

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

setlocal iskeyword +=%,.,-,_
setlocal isident   +=%,.,-,_

syntax case ignore

syntax keyword goasmPseudoRegister FP PC SB SP
hi def link goasmPseudoRegister    Statement

"" i386
" syntax keyword goasmSymbol AX BX CX DX BP DI SI

" General purpose registers
"  https://github.com/golang/arch/blob/master/x86/x86asm/inst.go
"  https://github.com/mmcloughlin/avo/blob/master/reg/x86.go

" low byte
syntax keyword goasmRegisterLowByte           AL CL DL BL

" hi byte
syntax keyword goasmRegisterHiByte            AH CH DH BH

" 8-bit
syntax keyword goasmRegister8bit              SPB BPB SIB DIB R8B R9B R10B R11B R12B R13B R14B R15B
" 16-bit
syntax keyword goasmRegister16bit             AX CX DX BX SP BP SI DI R8W R9W R10W R11W R12W R13W R14W R15W
" 32-bit
syntax keyword goasmRegister32bit             EAX ECX EDX EBX ESP EBP ESI EDI R8L R9L R10L R11L R12L R13L R14L R15L
" 64-bit
syntax keyword goasmRegister64bit             RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15

" Vector registers
" 128-bit
syntax keyword goasmVectorRegister128bit      X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19 X20 X21 X22 X23 X24 X25 X26 X27 X28 X29 X30 X31
" 256-bit
syntax keyword goasmVectorRegister256bit      Y0 Y1 Y2 Y3 Y4 Y5 Y6 Y7 Y8 Y9 Y10 Y11 Y12 Y13 Y14 Y15 Y16 Y17 Y18 Y19 Y20 Y21 Y22 Y23 Y24 Y25 Y26 Y27 Y28 Y29 Y30 Y31
" 512-bit
syntax keyword goasmVectorRegister512bit      Z0 Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8 Z9 Z10 Z11 Z12 Z13 Z14 Z15 Z16 Z17 Z18 Z19 Z20 Z21 Z22 Z23 Z24 Z25 Z26 Z27 Z28 Z29 Z30 Z31

syntax keyword goasmRegisterData              R0 R1 R2 R3 R4 R5 R6 R7
syntax keyword goasmAegisterAddress           A0 A1 A2 A3 A4 A5 A6 A7
syntax keyword goasmFegisterFloating          F0 F1 F2 F3 F4 F5 F6 F7

" Instruction pointer
" 16-bit
syntax keyword goasmRegisterInstructionPointer16bit  IP
" 32-bit
syntax keyword goasmRegisterInstructionPointer32bit  EIP
" 64-bit
syntax keyword goasmRegisterInstructionPointer64bit  RIP

" MMX registers
syntax keyword goasmRegisterMMX                M0 M1 M2 M3 M4 M5 M6 M7
" Segment registers
syntax keyword goasmRegisterSegment            ES CS SS DS FS GS
" System registers
syntax keyword goasmRegisterSystem             GDTR IDTR LDTR MSW TASK
" Control registers
syntax keyword goasmRegisterControl            CR0 CR1 CR2 CR3 CR4 CR5 CR6 CR7 CR8 CR9 CR10 CR11 CR12 CR13 CR14 CR15
" Debug registers
syntax keyword goasmRegisterDebug              DR0 DR1 DR2 DR3 DR4 DR5 DR6 DR7 DR8 DR9 DR10 DR11 DR12 DR13 DR14 DR15
" Task registers
syntax keyword goasmRegisterTask               TR0 TR1 TR2 TR3 TR4 TR5 TR6 TR7


" directives
syntax keyword goasmDirective                  TEXT DATA GLOBL FUNCDATA PCDATA
syntax keyword goasmDirective                  NOPROF DUPOK NOSPLIT RODATA NOPTR WRAPPER NEEDCTXT TLSBSS NOFRAME REFLECTMETHOD TOPFRAME
syntax keyword goasmDirectiveStore             BYTE


" links
highlight default link goasmRegisterLowByte                  goasmRegister
highlight default link goasmRegisterHiByte                   goasmRegister
highlight default link goasmRegister8bit                     goasmRegister
highlight default link goasmRegister16bit                    goasmRegister
highlight default link goasmRegister32bit                    goasmRegister
highlight default link goasmRegister64bit                    goasmRegister
highlight default link goasmVectorRegister128bit             goasmRegister
highlight default link goasmRegisterData                     goasmRegister
highlight default link goasmAegisterAddress                  goasmRegister
highlight default link goasmFegisterFloating                 goasmRegister
highlight default link goasmRegisterInstructionPointer16bit  goasmRegister
highlight default link goasmRegisterInstructionPointer32bit  goasmRegister
highlight default link goasmRegisterInstructionPointer64bit  goasmRegister
highlight default link goasmRegisterMMX	                     goasmRegister
highlight default link goasmRegisterSegment                  goasmRegister
highlight default link goasmRegisterSystem                   goasmRegister
highlight default link goasmRegisterDebug                    goasmRegister
highlight default link goasmRegisterTask                     goasmRegister

hi def link goasmDirectiveStore       goasmDirective
" hi def link goasmRegisterARM	        goasmRegister
" hi def link goasmDirectiveMacroARM    goasmDirectiveMacro
" hi def link goasmDirectiveStoreARM    goasmDirectiveStore


syntax keyword goasmDirectiveMacro .altmacro .macro .noaltmacro .endm .endmacro .func .endfunc

" i*86 directives
syntax keyword goasmDirectiveX86	.att_syntax .intel_syntax .att_mnemonic .intel_mnemonic .code16 .code32 .code64 .lcomm

" i*86 register set
syntax keyword goasmRegisterX86	%rax %rbx %rcx %rdx %rdi %rsi %rsp %rbp
syntax keyword goasmRegisterX86	%eax %ebx %ecx %edx %ax %bx %cx %dx %ah %al %bh %bl %ch %cl %dh %dl
syntax keyword goasmRegisterX86	%edi %esi %esp %ebp %di %si %sp %bp %sph %spl %bph %bpl
syntax keyword goasmRegisterX86	%cs %ds %es %fs %gs %ss %ip %eip %rip %eflags
syntax match   goasmRegisterX86	/\<%r\([8-9]\|1[0-5]\)[lwd]\?\>/

" i*86 special registers
syntax match goasmRegisterX86Cr	/\<%cr[0-8]\>/
syntax match goasmRegisterX86Dr	/\<%dr[0-8]\>/
syntax match goasmRegisterX86Tr	/\<%tr[0-8]\>/
syntax match goasmRegisterX86Fp	/\<%sp\(([0-7])\)\?\>/
" syn match goasmRegisterX86MMX	/\<%x\?mm[0-7]\>/

" symbols and labels

syntax match   goasmLabel		  /[-_$.A-Za-z0-9]\+\s*:/
syntax match   goasmSymbols		/\<[^; \t()]\+\>/
syntax match   goasmSymbolRef	/\$[-_$.A-Za-z][-_$·.A-Za-z0-9]*\>/
syntax match   goasmSpecial		/\<[$.]\>/

" constants
syntax region  goasmString		start=/"/  end=/"/ skip=/\\"/
syntax match   goasmCharacter	/'\(?\|\\?\)/
syntax match   goasmDecimalNumber	/\$\?-\?\d\+/
syntax match   goasmBinaryNumber	/\$\?-\?0b[01]\+/
syntax match   goasmOctalNumber	/\$\?-\?0\d\+/
syntax match   goasmHexNumber	/\$\?-\?0x\x\+/
" -- TODO: goasmFloatNumber

" local label needs to be matched *after* numerics
syntax match   goasmLocalLabel	/\d\{1,2\}[:fb]/

" comments etc.
syntax match   goasmOperator		/[+-/*=|&~<>]\|<=\|>=\|<>/
syntax region  goasmComment		start=/\/\*/ end=/\*\//
syntax region  goasmCommentSingle    start=/#/ end=/$/
syntax region  goasmCommentSingle    start=/@/ end=/$/
" if exists('g:goasmCppComments')
syntax region  goasmCommentSingle start=/\/\// end=/$/
" endif

" ARM specific directives
syntax keyword goasmDirectiveStoreARM	.2byte .4byte .8byte

syntax keyword goasmDirectiveARM	.arch .arch_expression .arm .asciiz .cantunwind .code .cpu .dn .qn .eabi_attribute .even .extend .ldouble .fnend .fbstart .force_thumb .fpu .handlerdata .inst .inst.n .inst.w .ltorg .lmovsp .movsp .object_arch .packed .pad .personality .personalityindex .pool .req .save .setfp .secrel32 .syntax .thumb .thumb_func .thumb_set .tlsdescseq .unreq .unwind_raw .vsave

" ARM register set
" Must be defined after goasmSymbol to have higher precedence
" syntax keyword goasmRegisterARM	        sp lr pc
syntax match   goasmRegisterARM	        /\<%\?r\([0-9]\|1[0-5]\)\>/

syntax keyword goasmDirectiveMacroARM	.dn .dq .req .unreq .tlsdescseq

" Opcodes

syntax keyword goasmOpcode        JLS REP
 
"-- Section: Willamette MMX instructions (SSE2 SIMD Integer Instructions)
syntax keyword goasmOpcode_SSE2  MOVD MOVDB MOVDW MOVDL MOVDQ MOVOU
syntax keyword goasmOpcode_SSE2  MOVDQA
syntax keyword goasmOpcode_SSE2  MOVDQU
syntax keyword goasmOpcode_SSE2  MOVDQ2Q
syntax keyword goasmOpcode_X64_SSE2  MOVQ
syntax keyword goasmOpcode_SSE2  MOVQ2DQ
syntax keyword goasmOpcode_SSE2  PACKSSWB PACKSSWBB PACKSSWBW PACKSSWBL PACKSSWBQ
syntax keyword goasmOpcode_SSE2  PACKSSDW PACKSSDWB PACKSSDWW PACKSSDWL PACKSSDWQ
syntax keyword goasmOpcode_SSE2  PACKUSWB PACKUSWBB PACKUSWBW PACKUSWBL PACKUSWBQ
syntax keyword goasmOpcode_SSE2  PADDB PADDBB PADDBW PADDBL PADDBQ
syntax keyword goasmOpcode_SSE2  PADDW PADDWB PADDWW PADDWL PADDWQ
syntax keyword goasmOpcode_SSE2  PADDD PADDDB PADDDW PADDDL PADDDQ
syntax keyword goasmOpcode_SSE2  PADDQ PADDQB PADDQW PADDQL PADDQQ
syntax keyword goasmOpcode_SSE2  PADDSB PADDSBB PADDSBW PADDSBL PADDSBQ
syntax keyword goasmOpcode_SSE2  PADDSW PADDSWB PADDSWW PADDSWL PADDSWQ
syntax keyword goasmOpcode_SSE2  PADDUSB PADDUSBB PADDUSBW PADDUSBL PADDUSBQ
syntax keyword goasmOpcode_SSE2  PADDUSW PADDUSWB PADDUSWW PADDUSWL PADDUSWQ
syntax keyword goasmOpcode_SSE2  PAND PANDB PANDW PANDL PANDQ
syntax keyword goasmOpcode_SSE2  PANDN PANDNB PANDNW PANDNL PANDNQ
syntax keyword goasmOpcode_SSE2  PAVGB PAVGBB PAVGBW PAVGBL PAVGBQ
syntax keyword goasmOpcode_SSE2  PAVGW PAVGWB PAVGWW PAVGWL PAVGWQ
syntax keyword goasmOpcode_SSE2  PCMPEQB PCMPEQBB PCMPEQBW PCMPEQBL PCMPEQBQ
syntax keyword goasmOpcode_SSE2  PCMPEQW PCMPEQWB PCMPEQWW PCMPEQWL PCMPEQWQ
syntax keyword goasmOpcode_SSE2  PCMPEQD PCMPEQDB PCMPEQDW PCMPEQDL PCMPEQDQ
syntax keyword goasmOpcode_SSE2  PCMPGTB PCMPGTBB PCMPGTBW PCMPGTBL PCMPGTBQ
syntax keyword goasmOpcode_SSE2  PCMPGTW PCMPGTWB PCMPGTWW PCMPGTWL PCMPGTWQ
syntax keyword goasmOpcode_SSE2  PCMPGTD PCMPGTDB PCMPGTDW PCMPGTDL PCMPGTDQ
syntax keyword goasmOpcode_SSE2  PEXTRW PEXTRWB PEXTRWW PEXTRWL PEXTRWQ
syntax keyword goasmOpcode_SSE2  PINSRW PINSRWB PINSRWW PINSRWL PINSRWQ
syntax keyword goasmOpcode_SSE2  PMADDWD PMADDWDB PMADDWDW PMADDWDL PMADDWDQ
syntax keyword goasmOpcode_SSE2  PMAXSW PMAXSWB PMAXSWW PMAXSWL PMAXSWQ
syntax keyword goasmOpcode_SSE2  PMAXUB PMAXUBB PMAXUBW PMAXUBL PMAXUBQ
syntax keyword goasmOpcode_SSE2  PMINSW PMINSWB PMINSWW PMINSWL PMINSWQ
syntax keyword goasmOpcode_SSE2  PMINUB PMINUBB PMINUBW PMINUBL PMINUBQ
syntax keyword goasmOpcode_SSE2  PMOVMSKB
syntax keyword goasmOpcode_SSE2  PMULHUW PMULHUWB PMULHUWW PMULHUWL PMULHUWQ
syntax keyword goasmOpcode_SSE2  PMULHW PMULHWB PMULHWW PMULHWL PMULHWQ
syntax keyword goasmOpcode_SSE2  PMULLW PMULLWB PMULLWW PMULLWL PMULLWQ
syntax keyword goasmOpcode_SSE2  PMULUDQ PMULUDQB PMULUDQW PMULUDQL PMULUDQQ
syntax keyword goasmOpcode_SSE2  POR PORB PORW PORL PORQ
syntax keyword goasmOpcode_SSE2  PSADBW PSADBWB PSADBWW PSADBWL PSADBWQ
syntax keyword goasmOpcode_Base  PSHUFD PSHUFDB PSHUFDW PSHUFDL PSHUFDQ
syntax keyword goasmOpcode_Base  PSHUFHW PSHUFHWB PSHUFHWW PSHUFHWL PSHUFHWQ
syntax keyword goasmOpcode_Base  PSHUFLW PSHUFLWB PSHUFLWW PSHUFLWL PSHUFLWQ
syntax keyword goasmOpcode_SSE2  PSLLDQ PSLLDQB PSLLDQW PSLLDQL PSLLDQQ
syntax keyword goasmOpcode_SSE2  PSLLW PSLLWB PSLLWW PSLLWL PSLLWQ
syntax keyword goasmOpcode_SSE2  PSLLD PSLLDB PSLLDW PSLLDL PSLLDQ
syntax keyword goasmOpcode_SSE2  PSLLQ PSLLQB PSLLQW PSLLQL PSLLQQ
syntax keyword goasmOpcode_SSE2  PSRAW PSRAWB PSRAWW PSRAWL PSRAWQ
syntax keyword goasmOpcode_SSE2  PSRAD PSRADB PSRADW PSRADL PSRADQ
syntax keyword goasmOpcode_SSE2  PSRLDQ PSRLDQB PSRLDQW PSRLDQL PSRLDQQ
syntax keyword goasmOpcode_SSE2  PSRLW PSRLWB PSRLWW PSRLWL PSRLWQ
syntax keyword goasmOpcode_SSE2  PSRLD PSRLDB PSRLDW PSRLDL PSRLDQ
syntax keyword goasmOpcode_SSE2  PSRLQ PSRLQB PSRLQW PSRLQL PSRLQQ
syntax keyword goasmOpcode_SSE2  PSUBB PSUBBB PSUBBW PSUBBL PSUBBQ
syntax keyword goasmOpcode_SSE2  PSUBW PSUBWB PSUBWW PSUBWL PSUBWQ
syntax keyword goasmOpcode_SSE2  PSUBD PSUBDB PSUBDW PSUBDL PSUBDQ
syntax keyword goasmOpcode_SSE2  PSUBQ PSUBQB PSUBQW PSUBQL PSUBQQ
syntax keyword goasmOpcode_SSE2  PSUBSB PSUBSBB PSUBSBW PSUBSBL PSUBSBQ
syntax keyword goasmOpcode_SSE2  PSUBSW PSUBSWB PSUBSWW PSUBSWL PSUBSWQ
syntax keyword goasmOpcode_SSE2  PSUBUSB PSUBUSBB PSUBUSBW PSUBUSBL PSUBUSBQ
syntax keyword goasmOpcode_SSE2  PSUBUSW PSUBUSWB PSUBUSWW PSUBUSWL PSUBUSWQ
syntax keyword goasmOpcode_SSE2  PUNPCKHBW PUNPCKHBWB PUNPCKHBWW PUNPCKHBWL PUNPCKHBWQ
syntax keyword goasmOpcode_SSE2  PUNPCKHWD PUNPCKHWDB PUNPCKHWDW PUNPCKHWDL PUNPCKHWDQ
syntax keyword goasmOpcode_SSE2  PUNPCKHDQ PUNPCKHDQB PUNPCKHDQW PUNPCKHDQL PUNPCKHDQQ
syntax keyword goasmOpcode_SSE2  PUNPCKHQDQ PUNPCKHQDQB PUNPCKHQDQW PUNPCKHQDQL PUNPCKHQDQQ
syntax keyword goasmOpcode_SSE2  PUNPCKLBW PUNPCKLBWB PUNPCKLBWW PUNPCKLBWL PUNPCKLBWQ
syntax keyword goasmOpcode_SSE2  PUNPCKLWD PUNPCKLWDB PUNPCKLWDW PUNPCKLWDL PUNPCKLWDQ
syntax keyword goasmOpcode_SSE2  PUNPCKLDQ PUNPCKLDQB PUNPCKLDQW PUNPCKLDQL PUNPCKLDQQ
syntax keyword goasmOpcode_SSE2  PUNPCKLQDQ PUNPCKLQDQB PUNPCKLQDQW PUNPCKLQDQL PUNPCKLQDQQ
syntax keyword goasmOpcode_SSE2  PXOR PXORB PXORW PXORL PXORQ

"-- Section: Nehalem New Instructions (SSE4.2)
syntax keyword goasmOpcode_X64_SSE42  CRC32
syntax keyword goasmOpcode_SSE42  PCMPESTRI PCMPESTRIB PCMPESTRIW PCMPESTRIL PCMPESTRIQ
syntax keyword goasmOpcode_SSE42  PCMPESTRM PCMPESTRMB PCMPESTRMW PCMPESTRML PCMPESTRMQ
syntax keyword goasmOpcode_SSE42  PCMPISTRI PCMPISTRIB PCMPISTRIW PCMPISTRIL PCMPISTRIQ
syntax keyword goasmOpcode_SSE42  PCMPISTRM PCMPISTRMB PCMPISTRMW PCMPISTRML PCMPISTRMQ
syntax keyword goasmOpcode_SSE42  PCMPGTQ PCMPGTQB PCMPGTQW PCMPGTQL PCMPGTQQ
syntax keyword goasmOpcode_NEHALEM_Base POPCNT

"-- Section: Intel new instructions in ???
syntax keyword goasmOpcode_NEHALEM_Base MOVBE MOVBEB MOVBEW MOVBEL MOVBEQ

"-- Section: AMD XOP, FMA4 and CVT16 instructions (SSE5)
syntax keyword goasmOpcode_AMD_SSE5  VCVTPH2PS VCVTPH2PSB VCVTPH2PSW VCVTPH2PSL VCVTPH2PSQ
syntax keyword goasmOpcode_AMD_SSE5  VCVTPS2PH VCVTPS2PHB VCVTPS2PHW VCVTPS2PHL VCVTPS2PHQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDPD VFMADDPDB VFMADDPDW VFMADDPDL VFMADDPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDPS VFMADDPSB VFMADDPSW VFMADDPSL VFMADDPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDSD VFMADDSDB VFMADDSDW VFMADDSDL VFMADDSDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDSS VFMADDSSB VFMADDSSW VFMADDSSL VFMADDSSQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDSUBPD VFMADDSUBPDB VFMADDSUBPDW VFMADDSUBPDL VFMADDSUBPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMADDSUBPS VFMADDSUBPSB VFMADDSUBPSW VFMADDSUBPSL VFMADDSUBPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBADDPD VFMSUBADDPDB VFMSUBADDPDW VFMSUBADDPDL VFMSUBADDPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBADDPS VFMSUBADDPSB VFMSUBADDPSW VFMSUBADDPSL VFMSUBADDPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBPD VFMSUBPDB VFMSUBPDW VFMSUBPDL VFMSUBPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBPS VFMSUBPSB VFMSUBPSW VFMSUBPSL VFMSUBPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBSD VFMSUBSDB VFMSUBSDW VFMSUBSDL VFMSUBSDQ
syntax keyword goasmOpcode_AMD_SSE5  VFMSUBSS VFMSUBSSB VFMSUBSSW VFMSUBSSL VFMSUBSSQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMADDPD VFNMADDPDB VFNMADDPDW VFNMADDPDL VFNMADDPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMADDPS VFNMADDPSB VFNMADDPSW VFNMADDPSL VFNMADDPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMADDSD VFNMADDSDB VFNMADDSDW VFNMADDSDL VFNMADDSDQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMADDSS VFNMADDSSB VFNMADDSSW VFNMADDSSL VFNMADDSSQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMSUBPD VFNMSUBPDB VFNMSUBPDW VFNMSUBPDL VFNMSUBPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMSUBPS VFNMSUBPSB VFNMSUBPSW VFNMSUBPSL VFNMSUBPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMSUBSD VFNMSUBSDB VFNMSUBSDW VFNMSUBSDL VFNMSUBSDQ
syntax keyword goasmOpcode_AMD_SSE5  VFNMSUBSS VFNMSUBSSB VFNMSUBSSW VFNMSUBSSL VFNMSUBSSQ
syntax keyword goasmOpcode_AMD_SSE5  VFRCZPD VFRCZPDB VFRCZPDW VFRCZPDL VFRCZPDQ
syntax keyword goasmOpcode_AMD_SSE5  VFRCZPS VFRCZPSB VFRCZPSW VFRCZPSL VFRCZPSQ
syntax keyword goasmOpcode_AMD_SSE5  VFRCZSD VFRCZSDB VFRCZSDW VFRCZSDL VFRCZSDQ
syntax keyword goasmOpcode_AMD_SSE5  VFRCZSS VFRCZSSB VFRCZSSW VFRCZSSL VFRCZSSQ
syntax keyword goasmOpcode_AMD_SSE5  VPCMOV VPCMOVB VPCMOVW VPCMOVL VPCMOVQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMB VPCOMBB VPCOMBW VPCOMBL VPCOMBQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMD VPCOMDB VPCOMDW VPCOMDL VPCOMDQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMQ VPCOMQB VPCOMQW VPCOMQL VPCOMQQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMUB VPCOMUBB VPCOMUBW VPCOMUBL VPCOMUBQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMUD VPCOMUDB VPCOMUDW VPCOMUDL VPCOMUDQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMUQ VPCOMUQB VPCOMUQW VPCOMUQL VPCOMUQQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMUW VPCOMUWB VPCOMUWW VPCOMUWL VPCOMUWQ
syntax keyword goasmOpcode_AMD_SSE5  VPCOMW VPCOMWB VPCOMWW VPCOMWL VPCOMWQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDBD VPHADDBDB VPHADDBDW VPHADDBDL VPHADDBDQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDBQ VPHADDBQB VPHADDBQW VPHADDBQL VPHADDBQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDBW VPHADDBWB VPHADDBWW VPHADDBWL VPHADDBWQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDDQ VPHADDDQB VPHADDDQW VPHADDDQL VPHADDDQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUBD VPHADDUBDB VPHADDUBDW VPHADDUBDL VPHADDUBDQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUBQ VPHADDUBQB VPHADDUBQW VPHADDUBQL VPHADDUBQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUBWD VPHADDUBWDB VPHADDUBWDW VPHADDUBWDL VPHADDUBWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUDQ VPHADDUDQB VPHADDUDQW VPHADDUDQL VPHADDUDQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUWD VPHADDUWDB VPHADDUWDW VPHADDUWDL VPHADDUWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDUWQ VPHADDUWQB VPHADDUWQW VPHADDUWQL VPHADDUWQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDWD VPHADDWDB VPHADDWDW VPHADDWDL VPHADDWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPHADDWQ VPHADDWQB VPHADDWQW VPHADDWQL VPHADDWQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHSUBBW VPHSUBBWB VPHSUBBWW VPHSUBBWL VPHSUBBWQ
syntax keyword goasmOpcode_AMD_SSE5  VPHSUBDQ VPHSUBDQB VPHSUBDQW VPHSUBDQL VPHSUBDQQ
syntax keyword goasmOpcode_AMD_SSE5  VPHSUBWD VPHSUBWDB VPHSUBWDW VPHSUBWDL VPHSUBWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSDD VPMACSDDB VPMACSDDW VPMACSDDL VPMACSDDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSDQH VPMACSDQHB VPMACSDQHW VPMACSDQHL VPMACSDQHQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSDQL VPMACSDQLB VPMACSDQLW VPMACSDQLL VPMACSDQLQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSSDD VPMACSSDDB VPMACSSDDW VPMACSSDDL VPMACSSDDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSSDQH VPMACSSDQHB VPMACSSDQHW VPMACSSDQHL VPMACSSDQHQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSSDQL VPMACSSDQLB VPMACSSDQLW VPMACSSDQLL VPMACSSDQLQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSSWD VPMACSSWDB VPMACSSWDW VPMACSSWDL VPMACSSWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSSWW VPMACSSWWB VPMACSSWWW VPMACSSWWL VPMACSSWWQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSWD VPMACSWDB VPMACSWDW VPMACSWDL VPMACSWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMACSWW VPMACSWWB VPMACSWWW VPMACSWWL VPMACSWWQ
syntax keyword goasmOpcode_AMD_SSE5  VPMADCSSWD VPMADCSSWDB VPMADCSSWDW VPMADCSSWDL VPMADCSSWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPMADCSWD VPMADCSWDB VPMADCSWDW VPMADCSWDL VPMADCSWDQ
syntax keyword goasmOpcode_AMD_SSE5  VPPERM VPPERMB VPPERMW VPPERML VPPERMQ
syntax keyword goasmOpcode_AMD_SSE5  VPROTB VPROTBB VPROTBW VPROTBL VPROTBQ
syntax keyword goasmOpcode_AMD_SSE5  VPROTD VPROTDB VPROTDW VPROTDL VPROTDQ
syntax keyword goasmOpcode_AMD_SSE5  VPROTQ VPROTQB VPROTQW VPROTQL VPROTQQ
syntax keyword goasmOpcode_AMD_SSE5  VPROTW VPROTWB VPROTWW VPROTWL VPROTWQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHAB VPSHABB VPSHABW VPSHABL VPSHABQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHAD VPSHADB VPSHADW VPSHADL VPSHADQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHAQ VPSHAQB VPSHAQW VPSHAQL VPSHAQQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHAW VPSHAWB VPSHAWW VPSHAWL VPSHAWQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHLB VPSHLBB VPSHLBW VPSHLBL VPSHLBQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHLD VPSHLDB VPSHLDW VPSHLDL VPSHLDQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHLQ VPSHLQB VPSHLQW VPSHLQL VPSHLQQ
syntax keyword goasmOpcode_AMD_SSE5  VPSHLW VPSHLWB VPSHLWW VPSHLWL VPSHLWQ

"-- Section: Generic memory operations
syntax keyword goasmOpcode_KATMAI_Base PREFETCHNTA PREFETCHNTAB PREFETCHNTAW PREFETCHNTAL PREFETCHNTAQ
syntax keyword goasmOpcode_KATMAI_Base PREFETCHT0 PREFETCHT0B PREFETCHT0W PREFETCHT0L PREFETCHT0Q
syntax keyword goasmOpcode_KATMAI_Base PREFETCHT1 PREFETCHT1B PREFETCHT1W PREFETCHT1L PREFETCHT1Q
syntax keyword goasmOpcode_KATMAI_Base PREFETCHT2 PREFETCHT2B PREFETCHT2W PREFETCHT2L PREFETCHT2Q
syntax keyword goasmOpcode_KATMAI_Base SFENCE

"-- Section: Tejas New Instructions (SSSE3)
syntax keyword goasmOpcode_Base  PABSB PABSBB PABSBW PABSBL PABSBQ
syntax keyword goasmOpcode_Base  PABSW PABSWB PABSWW PABSWL PABSWQ
syntax keyword goasmOpcode_Base  PABSD PABSDB PABSDW PABSDL PABSDQ
syntax keyword goasmOpcode_Base  PALIGNR PALIGNRB PALIGNRW PALIGNRL PALIGNRQ
syntax keyword goasmOpcode_Base  PHADDW PHADDWB PHADDWW PHADDWL PHADDWQ
syntax keyword goasmOpcode_Base  PHADDD PHADDDB PHADDDW PHADDDL PHADDDQ
syntax keyword goasmOpcode_Base  PHADDSW PHADDSWB PHADDSWW PHADDSWL PHADDSWQ
syntax keyword goasmOpcode_Base  PHSUBW PHSUBWB PHSUBWW PHSUBWL PHSUBWQ
syntax keyword goasmOpcode_Base  PHSUBD PHSUBDB PHSUBDW PHSUBDL PHSUBDQ
syntax keyword goasmOpcode_Base  PHSUBSW PHSUBSWB PHSUBSWW PHSUBSWL PHSUBSWQ
syntax keyword goasmOpcode_Base  PMADDUBSW PMADDUBSWB PMADDUBSWW PMADDUBSWL PMADDUBSWQ
syntax keyword goasmOpcode_Base  PMULHRSW PMULHRSWB PMULHRSWW PMULHRSWL PMULHRSWQ
syntax keyword goasmOpcode_Base  PSHUFB PSHUFBB PSHUFBW PSHUFBL PSHUFBQ
syntax keyword goasmOpcode_Base  PSIGNB PSIGNBB PSIGNBW PSIGNBL PSIGNBQ
syntax keyword goasmOpcode_Base  PSIGNW PSIGNWB PSIGNWW PSIGNWL PSIGNWQ
syntax keyword goasmOpcode_Base  PSIGND PSIGNDB PSIGNDW PSIGNDL PSIGNDQ

"-- Section: Intel Fused Multiply-Add instructions (FMA)
syntax keyword goasmOpcode_FUTURE_FMA VFMADD132PS VFMADD132PSB VFMADD132PSW VFMADD132PSL VFMADD132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD132PD VFMADD132PDB VFMADD132PDW VFMADD132PDL VFMADD132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD312PS VFMADD312PSB VFMADD312PSW VFMADD312PSL VFMADD312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD312PD VFMADD312PDB VFMADD312PDW VFMADD312PDL VFMADD312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD213PS VFMADD213PSB VFMADD213PSW VFMADD213PSL VFMADD213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD213PD VFMADD213PDB VFMADD213PDW VFMADD213PDL VFMADD213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD123PS VFMADD123PSB VFMADD123PSW VFMADD123PSL VFMADD123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD123PD VFMADD123PDB VFMADD123PDW VFMADD123PDL VFMADD123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD231PS VFMADD231PSB VFMADD231PSW VFMADD231PSL VFMADD231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD231PD VFMADD231PDB VFMADD231PDW VFMADD231PDL VFMADD231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD321PS VFMADD321PSB VFMADD321PSW VFMADD321PSL VFMADD321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD321PD VFMADD321PDB VFMADD321PDW VFMADD321PDL VFMADD321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB132PS VFMADDSUB132PSB VFMADDSUB132PSW VFMADDSUB132PSL VFMADDSUB132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB132PD VFMADDSUB132PDB VFMADDSUB132PDW VFMADDSUB132PDL VFMADDSUB132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB312PS VFMADDSUB312PSB VFMADDSUB312PSW VFMADDSUB312PSL VFMADDSUB312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB312PD VFMADDSUB312PDB VFMADDSUB312PDW VFMADDSUB312PDL VFMADDSUB312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB213PS VFMADDSUB213PSB VFMADDSUB213PSW VFMADDSUB213PSL VFMADDSUB213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB213PD VFMADDSUB213PDB VFMADDSUB213PDW VFMADDSUB213PDL VFMADDSUB213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB123PS VFMADDSUB123PSB VFMADDSUB123PSW VFMADDSUB123PSL VFMADDSUB123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB123PD VFMADDSUB123PDB VFMADDSUB123PDW VFMADDSUB123PDL VFMADDSUB123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB231PS VFMADDSUB231PSB VFMADDSUB231PSW VFMADDSUB231PSL VFMADDSUB231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB231PD VFMADDSUB231PDB VFMADDSUB231PDW VFMADDSUB231PDL VFMADDSUB231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB321PS VFMADDSUB321PSB VFMADDSUB321PSW VFMADDSUB321PSL VFMADDSUB321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADDSUB321PD VFMADDSUB321PDB VFMADDSUB321PDW VFMADDSUB321PDL VFMADDSUB321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB132PS VFMSUB132PSB VFMSUB132PSW VFMSUB132PSL VFMSUB132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB132PD VFMSUB132PDB VFMSUB132PDW VFMSUB132PDL VFMSUB132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB312PS VFMSUB312PSB VFMSUB312PSW VFMSUB312PSL VFMSUB312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB312PD VFMSUB312PDB VFMSUB312PDW VFMSUB312PDL VFMSUB312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB213PS VFMSUB213PSB VFMSUB213PSW VFMSUB213PSL VFMSUB213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB213PD VFMSUB213PDB VFMSUB213PDW VFMSUB213PDL VFMSUB213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB123PS VFMSUB123PSB VFMSUB123PSW VFMSUB123PSL VFMSUB123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB123PD VFMSUB123PDB VFMSUB123PDW VFMSUB123PDL VFMSUB123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB231PS VFMSUB231PSB VFMSUB231PSW VFMSUB231PSL VFMSUB231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB231PD VFMSUB231PDB VFMSUB231PDW VFMSUB231PDL VFMSUB231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB321PS VFMSUB321PSB VFMSUB321PSW VFMSUB321PSL VFMSUB321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB321PD VFMSUB321PDB VFMSUB321PDW VFMSUB321PDL VFMSUB321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD132PS VFMSUBADD132PSB VFMSUBADD132PSW VFMSUBADD132PSL VFMSUBADD132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD132PD VFMSUBADD132PDB VFMSUBADD132PDW VFMSUBADD132PDL VFMSUBADD132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD312PS VFMSUBADD312PSB VFMSUBADD312PSW VFMSUBADD312PSL VFMSUBADD312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD312PD VFMSUBADD312PDB VFMSUBADD312PDW VFMSUBADD312PDL VFMSUBADD312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD213PS VFMSUBADD213PSB VFMSUBADD213PSW VFMSUBADD213PSL VFMSUBADD213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD213PD VFMSUBADD213PDB VFMSUBADD213PDW VFMSUBADD213PDL VFMSUBADD213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD123PS VFMSUBADD123PSB VFMSUBADD123PSW VFMSUBADD123PSL VFMSUBADD123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD123PD VFMSUBADD123PDB VFMSUBADD123PDW VFMSUBADD123PDL VFMSUBADD123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD231PS VFMSUBADD231PSB VFMSUBADD231PSW VFMSUBADD231PSL VFMSUBADD231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD231PD VFMSUBADD231PDB VFMSUBADD231PDW VFMSUBADD231PDL VFMSUBADD231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD321PS VFMSUBADD321PSB VFMSUBADD321PSW VFMSUBADD321PSL VFMSUBADD321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUBADD321PD VFMSUBADD321PDB VFMSUBADD321PDW VFMSUBADD321PDL VFMSUBADD321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD132PS VFNMADD132PSB VFNMADD132PSW VFNMADD132PSL VFNMADD132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD132PD VFNMADD132PDB VFNMADD132PDW VFNMADD132PDL VFNMADD132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD312PS VFNMADD312PSB VFNMADD312PSW VFNMADD312PSL VFNMADD312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD312PD VFNMADD312PDB VFNMADD312PDW VFNMADD312PDL VFNMADD312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD213PS VFNMADD213PSB VFNMADD213PSW VFNMADD213PSL VFNMADD213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD213PD VFNMADD213PDB VFNMADD213PDW VFNMADD213PDL VFNMADD213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD123PS VFNMADD123PSB VFNMADD123PSW VFNMADD123PSL VFNMADD123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD123PD VFNMADD123PDB VFNMADD123PDW VFNMADD123PDL VFNMADD123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD231PS VFNMADD231PSB VFNMADD231PSW VFNMADD231PSL VFNMADD231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD231PD VFNMADD231PDB VFNMADD231PDW VFNMADD231PDL VFNMADD231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD321PS VFNMADD321PSB VFNMADD321PSW VFNMADD321PSL VFNMADD321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD321PD VFNMADD321PDB VFNMADD321PDW VFNMADD321PDL VFNMADD321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB132PS VFNMSUB132PSB VFNMSUB132PSW VFNMSUB132PSL VFNMSUB132PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB132PD VFNMSUB132PDB VFNMSUB132PDW VFNMSUB132PDL VFNMSUB132PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB312PS VFNMSUB312PSB VFNMSUB312PSW VFNMSUB312PSL VFNMSUB312PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB312PD VFNMSUB312PDB VFNMSUB312PDW VFNMSUB312PDL VFNMSUB312PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB213PS VFNMSUB213PSB VFNMSUB213PSW VFNMSUB213PSL VFNMSUB213PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB213PD VFNMSUB213PDB VFNMSUB213PDW VFNMSUB213PDL VFNMSUB213PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB123PS VFNMSUB123PSB VFNMSUB123PSW VFNMSUB123PSL VFNMSUB123PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB123PD VFNMSUB123PDB VFNMSUB123PDW VFNMSUB123PDL VFNMSUB123PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB231PS VFNMSUB231PSB VFNMSUB231PSW VFNMSUB231PSL VFNMSUB231PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB231PD VFNMSUB231PDB VFNMSUB231PDW VFNMSUB231PDL VFNMSUB231PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB321PS VFNMSUB321PSB VFNMSUB321PSW VFNMSUB321PSL VFNMSUB321PSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB321PD VFNMSUB321PDB VFNMSUB321PDW VFNMSUB321PDL VFNMSUB321PDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD132SS VFMADD132SSB VFMADD132SSW VFMADD132SSL VFMADD132SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD132SD VFMADD132SDB VFMADD132SDW VFMADD132SDL VFMADD132SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD312SS VFMADD312SSB VFMADD312SSW VFMADD312SSL VFMADD312SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD312SD VFMADD312SDB VFMADD312SDW VFMADD312SDL VFMADD312SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD213SS VFMADD213SSB VFMADD213SSW VFMADD213SSL VFMADD213SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD213SD VFMADD213SDB VFMADD213SDW VFMADD213SDL VFMADD213SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD123SS VFMADD123SSB VFMADD123SSW VFMADD123SSL VFMADD123SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD123SD VFMADD123SDB VFMADD123SDW VFMADD123SDL VFMADD123SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD231SS VFMADD231SSB VFMADD231SSW VFMADD231SSL VFMADD231SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD231SD VFMADD231SDB VFMADD231SDW VFMADD231SDL VFMADD231SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD321SS VFMADD321SSB VFMADD321SSW VFMADD321SSL VFMADD321SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMADD321SD VFMADD321SDB VFMADD321SDW VFMADD321SDL VFMADD321SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB132SS VFMSUB132SSB VFMSUB132SSW VFMSUB132SSL VFMSUB132SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB132SD VFMSUB132SDB VFMSUB132SDW VFMSUB132SDL VFMSUB132SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB312SS VFMSUB312SSB VFMSUB312SSW VFMSUB312SSL VFMSUB312SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB312SD VFMSUB312SDB VFMSUB312SDW VFMSUB312SDL VFMSUB312SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB213SS VFMSUB213SSB VFMSUB213SSW VFMSUB213SSL VFMSUB213SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB213SD VFMSUB213SDB VFMSUB213SDW VFMSUB213SDL VFMSUB213SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB123SS VFMSUB123SSB VFMSUB123SSW VFMSUB123SSL VFMSUB123SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB123SD VFMSUB123SDB VFMSUB123SDW VFMSUB123SDL VFMSUB123SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB231SS VFMSUB231SSB VFMSUB231SSW VFMSUB231SSL VFMSUB231SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB231SD VFMSUB231SDB VFMSUB231SDW VFMSUB231SDL VFMSUB231SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB321SS VFMSUB321SSB VFMSUB321SSW VFMSUB321SSL VFMSUB321SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFMSUB321SD VFMSUB321SDB VFMSUB321SDW VFMSUB321SDL VFMSUB321SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD132SS VFNMADD132SSB VFNMADD132SSW VFNMADD132SSL VFNMADD132SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD132SD VFNMADD132SDB VFNMADD132SDW VFNMADD132SDL VFNMADD132SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD312SS VFNMADD312SSB VFNMADD312SSW VFNMADD312SSL VFNMADD312SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD312SD VFNMADD312SDB VFNMADD312SDW VFNMADD312SDL VFNMADD312SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD213SS VFNMADD213SSB VFNMADD213SSW VFNMADD213SSL VFNMADD213SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD213SD VFNMADD213SDB VFNMADD213SDW VFNMADD213SDL VFNMADD213SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD123SS VFNMADD123SSB VFNMADD123SSW VFNMADD123SSL VFNMADD123SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD123SD VFNMADD123SDB VFNMADD123SDW VFNMADD123SDL VFNMADD123SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD231SS VFNMADD231SSB VFNMADD231SSW VFNMADD231SSL VFNMADD231SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD231SD VFNMADD231SDB VFNMADD231SDW VFNMADD231SDL VFNMADD231SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD321SS VFNMADD321SSB VFNMADD321SSW VFNMADD321SSL VFNMADD321SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMADD321SD VFNMADD321SDB VFNMADD321SDW VFNMADD321SDL VFNMADD321SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB132SS VFNMSUB132SSB VFNMSUB132SSW VFNMSUB132SSL VFNMSUB132SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB132SD VFNMSUB132SDB VFNMSUB132SDW VFNMSUB132SDL VFNMSUB132SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB312SS VFNMSUB312SSB VFNMSUB312SSW VFNMSUB312SSL VFNMSUB312SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB312SD VFNMSUB312SDB VFNMSUB312SDW VFNMSUB312SDL VFNMSUB312SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB213SS VFNMSUB213SSB VFNMSUB213SSW VFNMSUB213SSL VFNMSUB213SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB213SD VFNMSUB213SDB VFNMSUB213SDW VFNMSUB213SDL VFNMSUB213SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB123SS VFNMSUB123SSB VFNMSUB123SSW VFNMSUB123SSL VFNMSUB123SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB123SD VFNMSUB123SDB VFNMSUB123SDW VFNMSUB123SDL VFNMSUB123SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB231SS VFNMSUB231SSB VFNMSUB231SSW VFNMSUB231SSL VFNMSUB231SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB231SD VFNMSUB231SDB VFNMSUB231SDW VFNMSUB231SDL VFNMSUB231SDQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB321SS VFNMSUB321SSB VFNMSUB321SSW VFNMSUB321SSL VFNMSUB321SSQ
syntax keyword goasmOpcode_FUTURE_FMA VFNMSUB321SD VFNMSUB321SDB VFNMSUB321SDW VFNMSUB321SDL VFNMSUB321SDQ

"-- Section: Willamette SSE2 Cacheability Instructions
syntax keyword goasmOpcode_SSE2  MASKMOVDQU
syntax keyword goasmOpcode_SSE2  CLFLUSH CLFLUSHB CLFLUSHW CLFLUSHL CLFLUSHQ
syntax keyword goasmOpcode_SSE2  MOVNTDQ MOVNTDQB MOVNTDQW MOVNTDQL MOVNTDQQ
syntax keyword goasmOpcode_X64_Base  MOVNTI MOVNTIB MOVNTIW MOVNTIL MOVNTIQ
syntax keyword goasmOpcode_SSE2  MOVNTPD MOVNTPDB MOVNTPDW MOVNTPDL MOVNTPDQ
syntax keyword goasmOpcode_SSE2  LFENCE
syntax keyword goasmOpcode_SSE2  MFENCE

"-- Section: Systematic names for the hinting nop instructions
syntax keyword goasmOpcode_X64_Base  HINT_NOP0
syntax keyword goasmOpcode_X64_Base  HINT_NOP1
syntax keyword goasmOpcode_X64_Base  HINT_NOP2
syntax keyword goasmOpcode_X64_Base  HINT_NOP3
syntax keyword goasmOpcode_X64_Base  HINT_NOP4
syntax keyword goasmOpcode_X64_Base  HINT_NOP5
syntax keyword goasmOpcode_X64_Base  HINT_NOP6
syntax keyword goasmOpcode_X64_Base  HINT_NOP7
syntax keyword goasmOpcode_X64_Base  HINT_NOP8
syntax keyword goasmOpcode_X64_Base  HINT_NOP9
syntax keyword goasmOpcode_X64_Base  HINT_NOP10
syntax keyword goasmOpcode_X64_Base  HINT_NOP11
syntax keyword goasmOpcode_X64_Base  HINT_NOP12
syntax keyword goasmOpcode_X64_Base  HINT_NOP13
syntax keyword goasmOpcode_X64_Base  HINT_NOP14
syntax keyword goasmOpcode_X64_Base  HINT_NOP15
syntax keyword goasmOpcode_X64_Base  HINT_NOP16
syntax keyword goasmOpcode_X64_Base  HINT_NOP17
syntax keyword goasmOpcode_X64_Base  HINT_NOP18
syntax keyword goasmOpcode_X64_Base  HINT_NOP19
syntax keyword goasmOpcode_X64_Base  HINT_NOP20
syntax keyword goasmOpcode_X64_Base  HINT_NOP21
syntax keyword goasmOpcode_X64_Base  HINT_NOP22
syntax keyword goasmOpcode_X64_Base  HINT_NOP23
syntax keyword goasmOpcode_X64_Base  HINT_NOP24
syntax keyword goasmOpcode_X64_Base  HINT_NOP25
syntax keyword goasmOpcode_X64_Base  HINT_NOP26
syntax keyword goasmOpcode_X64_Base  HINT_NOP27
syntax keyword goasmOpcode_X64_Base  HINT_NOP28
syntax keyword goasmOpcode_X64_Base  HINT_NOP29
syntax keyword goasmOpcode_X64_Base  HINT_NOP30
syntax keyword goasmOpcode_X64_Base  HINT_NOP31
syntax keyword goasmOpcode_X64_Base  HINT_NOP32
syntax keyword goasmOpcode_X64_Base  HINT_NOP33
syntax keyword goasmOpcode_X64_Base  HINT_NOP34
syntax keyword goasmOpcode_X64_Base  HINT_NOP35
syntax keyword goasmOpcode_X64_Base  HINT_NOP36
syntax keyword goasmOpcode_X64_Base  HINT_NOP37
syntax keyword goasmOpcode_X64_Base  HINT_NOP38
syntax keyword goasmOpcode_X64_Base  HINT_NOP39
syntax keyword goasmOpcode_X64_Base  HINT_NOP40
syntax keyword goasmOpcode_X64_Base  HINT_NOP41
syntax keyword goasmOpcode_X64_Base  HINT_NOP42
syntax keyword goasmOpcode_X64_Base  HINT_NOP43
syntax keyword goasmOpcode_X64_Base  HINT_NOP44
syntax keyword goasmOpcode_X64_Base  HINT_NOP45
syntax keyword goasmOpcode_X64_Base  HINT_NOP46
syntax keyword goasmOpcode_X64_Base  HINT_NOP47
syntax keyword goasmOpcode_X64_Base  HINT_NOP48
syntax keyword goasmOpcode_X64_Base  HINT_NOP49
syntax keyword goasmOpcode_X64_Base  HINT_NOP50
syntax keyword goasmOpcode_X64_Base  HINT_NOP51
syntax keyword goasmOpcode_X64_Base  HINT_NOP52
syntax keyword goasmOpcode_X64_Base  HINT_NOP53
syntax keyword goasmOpcode_X64_Base  HINT_NOP54
syntax keyword goasmOpcode_X64_Base  HINT_NOP55
syntax keyword goasmOpcode_X64_Base  HINT_NOP56
syntax keyword goasmOpcode_X64_Base  HINT_NOP57
syntax keyword goasmOpcode_X64_Base  HINT_NOP58
syntax keyword goasmOpcode_X64_Base  HINT_NOP59
syntax keyword goasmOpcode_X64_Base  HINT_NOP60
syntax keyword goasmOpcode_X64_Base  HINT_NOP61
syntax keyword goasmOpcode_X64_Base  HINT_NOP62

"-- Section: Geode (Cyrix) 3DNow! additions
syntax keyword goasmOpcode_PENT_3DNOW PFRCPV PFRCPVB PFRCPVW PFRCPVL PFRCPVQ
syntax keyword goasmOpcode_PENT_3DNOW PFRSQRTV PFRSQRTVB PFRSQRTVW PFRSQRTVL PFRSQRTVQ

"-- Section: XSAVE group (AVX and extended state)
syntax keyword goasmOpcode_NEHALEM_Base XGETBV
syntax keyword goasmOpcode_NEHALEM_Base XSETBV
syntax keyword goasmOpcode_NEHALEM_Base XSAVE XSAVEB XSAVEW XSAVEL XSAVEQ
syntax keyword goasmOpcode_NEHALEM_Base XRSTOR XRSTORB XRSTORW XRSTORL XRSTORQ

"-- Section: Conventional instructions
syntax keyword goasmOpcode_8086_Base  FNSTCW FNSTCWB FNSTCWW FNSTCWL FNSTCWQ
syntax keyword goasmOpcode_8086_Base  FNSTENV FNSTENVB FNSTENVW FNSTENVL FNSTENVQ
syntax keyword goasmOpcode_286_Base  FNSTSW
syntax keyword goasmOpcode_8086_Base  FPATAN
syntax keyword goasmOpcode_8086_Base  FPREM
syntax keyword goasmOpcode_386_Base  FPREM1
syntax keyword goasmOpcode_8086_Base  FPTAN
syntax keyword goasmOpcode_8086_Base  FRNDINT
syntax keyword goasmOpcode_8086_Base  FRSTOR FRSTORB FRSTORW FRSTORL FRSTORQ
syntax keyword goasmOpcode_8086_Base  FSAVE FSAVEB FSAVEW FSAVEL FSAVEQ
syntax keyword goasmOpcode_8086_Base  FSCALE
syntax keyword goasmOpcode_286_Base  FSETPM
syntax keyword goasmOpcode_386_Base  FSIN
syntax keyword goasmOpcode_386_Base  FSINCOS
syntax keyword goasmOpcode_8086_Base  FSQRT
syntax keyword goasmOpcode_8086_Base  FST
syntax keyword goasmOpcode_8086_Base  FSTCW FSTCWB FSTCWW FSTCWL FSTCWQ
syntax keyword goasmOpcode_8086_Base  FSTENV FSTENVB FSTENVW FSTENVL FSTENVQ
syntax keyword goasmOpcode_8086_Base  FSTP
syntax keyword goasmOpcode_286_Base  FSTSW
syntax keyword goasmOpcode_8086_Base  FSUB
syntax keyword goasmOpcode_8086_Base  FSUBP
syntax keyword goasmOpcode_8086_Base  FSUBR
syntax keyword goasmOpcode_8086_Base  FSUBRP
syntax keyword goasmOpcode_8086_Base  FTST
syntax keyword goasmOpcode_386_Base  FUCOM
syntax keyword goasmOpcode_P6_Base  FUCOMI
syntax keyword goasmOpcode_P6_Base  FUCOMIP
syntax keyword goasmOpcode_386_Base  FUCOMP
syntax keyword goasmOpcode_386_Base  FUCOMPP
syntax keyword goasmOpcode_8086_Base  FXAM
syntax keyword goasmOpcode_8086_Base  FXCH
syntax keyword goasmOpcode_8086_Base  FXTRACT
syntax keyword goasmOpcode_8086_Base  FYL2X
syntax keyword goasmOpcode_8086_Base  FYL2XP1
syntax keyword goasmOpcode_8086_Base  HLT
syntax keyword goasmOpcode_386_Base  IBTS
syntax keyword goasmOpcode_386_Base  ICEBP
syntax keyword goasmOpcode_X64_Base  IDIV
syntax keyword goasmOpcode_X64_Base  IMUL IMULB IMULW IMULL IMULQ
syntax keyword goasmOpcode_386_Base  IN
syntax keyword goasmOpcode_X64_Base  INC INCB INCW INCL INCQ
syntax keyword goasmOpcode_Base  INCBIN
syntax keyword goasmOpcode_186_Base  INSB
syntax keyword goasmOpcode_386_Base  INSD
syntax keyword goasmOpcode_186_Base  INSW
syntax keyword goasmOpcode_8086_Base  INT INTB INTW INTL INTQ
syntax keyword goasmOpcode_386_Base  INT01
syntax keyword goasmOpcode_386_Base  INT1
syntax keyword goasmOpcode_8086_Base  INT03
syntax keyword goasmOpcode_8086_Base  INT3
syntax keyword goasmOpcode_8086_Base  INTO
syntax keyword goasmOpcode_486_Base  INVD
syntax keyword goasmOpcode_486_Base  INVLPG INVLPGB INVLPGW INVLPGL INVLPGQ
syntax keyword goasmOpcode_X86_64_Base INVLPGA
syntax keyword goasmOpcode_8086_Base  IRET
syntax keyword goasmOpcode_386_Base  IRETD
syntax keyword goasmOpcode_X64_Base  IRETQ
syntax keyword goasmOpcode_8086_Base  IRETW
syntax keyword goasmOpcode_8086_Base  JCXZ JCXZB JCXZW JCXZL JCXZQ
syntax keyword goasmOpcode_386_Base  JECXZ JECXZB JECXZW JECXZL JECXZQ
syntax keyword goasmOpcode_X64_Base  JRCXZ JRCXZB JRCXZW JRCXZL JRCXZQ
syntax keyword goasmOpcode_X64_Base  JMP JMPB JMPW JMPL JMPQ
syntax keyword goasmOpcode_IA64_Base  JMPE
syntax keyword goasmOpcode_8086_Base  LAHF
syntax keyword goasmOpcode_X64_Base  LAR
syntax keyword goasmOpcode_386_Base  LDS LDSB LDSW LDSL LDSQ
syntax keyword goasmOpcode_X64_Base  LEA LEAB LEAW LEAL LEAQ
syntax keyword goasmOpcode_186_Base  LEAVE
syntax keyword goasmOpcode_386_Base  LES LESB LESW LESL LESQ
syntax keyword goasmOpcode_X64_Base  LFENCE
syntax keyword goasmOpcode_386_Base  LFS LFSB LFSW LFSL LFSQ
syntax keyword goasmOpcode_286_Base  LGDT LGDTB LGDTW LGDTL LGDTQ
syntax keyword goasmOpcode_386_Base  LGS LGSB LGSW LGSL LGSQ
syntax keyword goasmOpcode_286_Base  LIDT LIDTB LIDTW LIDTL LIDTQ
syntax keyword goasmOpcode_286_Base  LLDT
syntax keyword goasmOpcode_286_Base  LMSW
syntax keyword goasmOpcode_386_Base  LOADALL
syntax keyword goasmOpcode_286_Base  LOADALL286
syntax keyword goasmOpcode_8086_Base  LODSB
syntax keyword goasmOpcode_386_Base  LODSD
syntax keyword goasmOpcode_X64_Base  LODSQ
syntax keyword goasmOpcode_8086_Base  LODSW
syntax keyword goasmOpcode_X64_Base  LOOP LOOPB LOOPW LOOPL LOOPQ
syntax keyword goasmOpcode_X64_Base  LOOPE LOOPEB LOOPEW LOOPEL LOOPEQ
syntax keyword goasmOpcode_X64_Base  LOOPNE LOOPNEB LOOPNEW LOOPNEL LOOPNEQ
syntax keyword goasmOpcode_X64_Base  LOOPNZ LOOPNZB LOOPNZW LOOPNZL LOOPNZQ
syntax keyword goasmOpcode_X64_Base  LOOPZ LOOPZB LOOPZW LOOPZL LOOPZQ
syntax keyword goasmOpcode_X64_Base  LSL
syntax keyword goasmOpcode_386_Base  LSS LSSB LSSW LSSL LSSQ
syntax keyword goasmOpcode_286_Base  LTR
syntax keyword goasmOpcode_X64_Base  MFENCE
syntax keyword goasmOpcode_PRESCOTT_Base MONITOR
syntax keyword goasmOpcode_386_Base  MOV MOVB MOVW MOVL MOVQ
syntax keyword goasmOpcode_X64_SSE  MOVD
syntax keyword goasmOpcode_X64_MMX  MOVQ
syntax keyword goasmOpcode_8086_Base  MOVSB
syntax keyword goasmOpcode_386_Base  MOVSD
syntax keyword goasmOpcode_X64_Base  MOVSQ
syntax keyword goasmOpcode_8086_Base  MOVSW
syntax keyword goasmOpcode_X64_Base  MOVSX
syntax keyword goasmOpcode_X64_Base  MOVSXD
syntax keyword goasmOpcode_X64_Base  MOVSX
syntax keyword goasmOpcode_X64_Base  MOVZX
syntax keyword goasmOpcode_X64_Base  MUL
syntax keyword goasmOpcode_PRESCOTT_Base MWAIT
syntax keyword goasmOpcode_X64_Base  NEG
syntax keyword goasmOpcode_X64_Base  NOP
syntax keyword goasmOpcode_X64_Base  NOT
syntax keyword goasmOpcode_386_Base  OR ORB ORW ORL ORQ
syntax keyword goasmOpcode_386_Base  OUT
syntax keyword goasmOpcode_186_Base  OUTSB
syntax keyword goasmOpcode_386_Base  OUTSD
syntax keyword goasmOpcode_186_Base  OUTSW
syntax keyword goasmOpcode_PENT_MMX  PACKSSDW PACKSSDWB PACKSSDWW PACKSSDWL PACKSSDWQ
syntax keyword goasmOpcode_PENT_MMX  PACKSSWB PACKSSWBB PACKSSWBW PACKSSWBL PACKSSWBQ
syntax keyword goasmOpcode_PENT_MMX  PACKUSWB PACKUSWBB PACKUSWBW PACKUSWBL PACKUSWBQ
syntax keyword goasmOpcode_PENT_MMX  PADDB PADDBB PADDBW PADDBL PADDBQ
syntax keyword goasmOpcode_PENT_MMX  PADDD PADDDB PADDDW PADDDL PADDDQ
syntax keyword goasmOpcode_PENT_MMX  PADDSB PADDSBB PADDSBW PADDSBL PADDSBQ
syntax keyword goasmOpcode_PENT_MMX  PADDSIW PADDSIWB PADDSIWW PADDSIWL PADDSIWQ
syntax keyword goasmOpcode_PENT_MMX  PADDSW PADDSWB PADDSWW PADDSWL PADDSWQ
syntax keyword goasmOpcode_PENT_MMX  PADDUSB PADDUSBB PADDUSBW PADDUSBL PADDUSBQ
syntax keyword goasmOpcode_PENT_MMX  PADDUSW PADDUSWB PADDUSWW PADDUSWL PADDUSWQ
syntax keyword goasmOpcode_PENT_MMX  PADDW PADDWB PADDWW PADDWL PADDWQ
syntax keyword goasmOpcode_PENT_MMX  PAND PANDB PANDW PANDL PANDQ
syntax keyword goasmOpcode_PENT_MMX  PANDN PANDNB PANDNW PANDNL PANDNQ
syntax keyword goasmOpcode_8086_Base  PAUSE
syntax keyword goasmOpcode_PENT_MMX  PAVEB PAVEBB PAVEBW PAVEBL PAVEBQ
syntax keyword goasmOpcode_PENT_3DNOW PAVGUSB PAVGUSBB PAVGUSBW PAVGUSBL PAVGUSBQ
syntax keyword goasmOpcode_PENT_MMX  PCMPEQB PCMPEQBB PCMPEQBW PCMPEQBL PCMPEQBQ
syntax keyword goasmOpcode_PENT_MMX  PCMPEQD PCMPEQDB PCMPEQDW PCMPEQDL PCMPEQDQ
syntax keyword goasmOpcode_PENT_MMX  PCMPEQW PCMPEQWB PCMPEQWW PCMPEQWL PCMPEQWQ
syntax keyword goasmOpcode_PENT_MMX  PCMPGTB PCMPGTBB PCMPGTBW PCMPGTBL PCMPGTBQ
syntax keyword goasmOpcode_PENT_MMX  PCMPGTD PCMPGTDB PCMPGTDW PCMPGTDL PCMPGTDQ
syntax keyword goasmOpcode_PENT_MMX  PCMPGTW PCMPGTWB PCMPGTWW PCMPGTWL PCMPGTWQ
syntax keyword goasmOpcode_PENT_MMX  PDISTIB PDISTIBB PDISTIBW PDISTIBL PDISTIBQ
syntax keyword goasmOpcode_PENT_3DNOW PF2ID PF2IDB PF2IDW PF2IDL PF2IDQ
syntax keyword goasmOpcode_PENT_3DNOW PFACC PFACCB PFACCW PFACCL PFACCQ
syntax keyword goasmOpcode_PENT_3DNOW PFADD PFADDB PFADDW PFADDL PFADDQ
syntax keyword goasmOpcode_PENT_3DNOW PFCMPEQ PFCMPEQB PFCMPEQW PFCMPEQL PFCMPEQQ
syntax keyword goasmOpcode_PENT_3DNOW PFCMPGE PFCMPGEB PFCMPGEW PFCMPGEL PFCMPGEQ
syntax keyword goasmOpcode_PENT_3DNOW PFCMPGT PFCMPGTB PFCMPGTW PFCMPGTL PFCMPGTQ
syntax keyword goasmOpcode_PENT_3DNOW PFMAX PFMAXB PFMAXW PFMAXL PFMAXQ
syntax keyword goasmOpcode_PENT_3DNOW PFMIN PFMINB PFMINW PFMINL PFMINQ
syntax keyword goasmOpcode_PENT_3DNOW PFMUL PFMULB PFMULW PFMULL PFMULQ
syntax keyword goasmOpcode_PENT_3DNOW PFRCP PFRCPB PFRCPW PFRCPL PFRCPQ
syntax keyword goasmOpcode_PENT_3DNOW PFRCPIT1 PFRCPIT1B PFRCPIT1W PFRCPIT1L PFRCPIT1Q
syntax keyword goasmOpcode_PENT_3DNOW PFRCPIT2 PFRCPIT2B PFRCPIT2W PFRCPIT2L PFRCPIT2Q
syntax keyword goasmOpcode_PENT_3DNOW PFRSQIT1 PFRSQIT1B PFRSQIT1W PFRSQIT1L PFRSQIT1Q
syntax keyword goasmOpcode_PENT_3DNOW PFRSQRT PFRSQRTB PFRSQRTW PFRSQRTL PFRSQRTQ
syntax keyword goasmOpcode_PENT_3DNOW PFSUB PFSUBB PFSUBW PFSUBL PFSUBQ
syntax keyword goasmOpcode_PENT_3DNOW PFSUBR PFSUBRB PFSUBRW PFSUBRL PFSUBRQ
syntax keyword goasmOpcode_PENT_3DNOW PI2FD PI2FDB PI2FDW PI2FDL PI2FDQ
syntax keyword goasmOpcode_PENT_MMX  PMACHRIW PMACHRIWB PMACHRIWW PMACHRIWL PMACHRIWQ
syntax keyword goasmOpcode_PENT_MMX  PMADDWD PMADDWDB PMADDWDW PMADDWDL PMADDWDQ
syntax keyword goasmOpcode_PENT_MMX  PMAGW PMAGWB PMAGWW PMAGWL PMAGWQ
syntax keyword goasmOpcode_PENT_MMX  PMULHRIW PMULHRIWB PMULHRIWW PMULHRIWL PMULHRIWQ
syntax keyword goasmOpcode_PENT_3DNOW PMULHRWA PMULHRWAB PMULHRWAW PMULHRWAL PMULHRWAQ
syntax keyword goasmOpcode_PENT_MMX  PMULHRWC PMULHRWCB PMULHRWCW PMULHRWCL PMULHRWCQ
syntax keyword goasmOpcode_PENT_MMX  PMULHW PMULHWB PMULHWW PMULHWL PMULHWQ
syntax keyword goasmOpcode_PENT_MMX  PMULLW PMULLWB PMULLWW PMULLWL PMULLWQ
syntax keyword goasmOpcode_PENT_MMX  PMVGEZB PMVGEZBB PMVGEZBW PMVGEZBL PMVGEZBQ
syntax keyword goasmOpcode_PENT_MMX  PMVLZB PMVLZBB PMVLZBW PMVLZBL PMVLZBQ
syntax keyword goasmOpcode_PENT_MMX  PMVNZB PMVNZBB PMVNZBW PMVNZBL PMVNZBQ
syntax keyword goasmOpcode_PENT_MMX  PMVZB PMVZBB PMVZBW PMVZBL PMVZBQ
syntax keyword goasmOpcode_386_Base  POP POPB POPW POPL POPQ
syntax keyword goasmOpcode_186_Base  POPA
syntax keyword goasmOpcode_386_Base  POPAL
syntax keyword goasmOpcode_186_Base  POPAW
syntax keyword goasmOpcode_8086_Base  POPF
syntax keyword goasmOpcode_386_Base  POPFD POPFL
syntax keyword goasmOpcode_X64_Base  POPFQ
syntax keyword goasmOpcode_8086_Base  POPFW
syntax keyword goasmOpcode_PENT_MMX  POR PORB PORW PORL PORQ
syntax keyword goasmOpcode_PENT_3DNOW PREFETCH PREFETCHB PREFETCHW PREFETCHL PREFETCHQ
syntax keyword goasmOpcode_PENT_3DNOW PREFETCHW PREFETCHWB PREFETCHWW PREFETCHWL PREFETCHWQ
syntax keyword goasmOpcode_PENT_MMX  PSLLD PSLLDB PSLLDW PSLLDL PSLLDQ
syntax keyword goasmOpcode_PENT_MMX  PSLLQ PSLLQB PSLLQW PSLLQL PSLLQQ
syntax keyword goasmOpcode_PENT_MMX  PSLLW PSLLWB PSLLWW PSLLWL PSLLWQ
syntax keyword goasmOpcode_PENT_MMX  PSRAD PSRADB PSRADW PSRADL PSRADQ
syntax keyword goasmOpcode_PENT_MMX  PSRAW PSRAWB PSRAWW PSRAWL PSRAWQ
syntax keyword goasmOpcode_PENT_MMX  PSRLD PSRLDB PSRLDW PSRLDL PSRLDQ
syntax keyword goasmOpcode_PENT_MMX  PSRLQ PSRLQB PSRLQW PSRLQL PSRLQQ
syntax keyword goasmOpcode_PENT_MMX  PSRLW PSRLWB PSRLWW PSRLWL PSRLWQ
syntax keyword goasmOpcode_PENT_MMX  PSUBB PSUBBB PSUBBW PSUBBL PSUBBQ
syntax keyword goasmOpcode_PENT_MMX  PSUBD PSUBDB PSUBDW PSUBDL PSUBDQ
syntax keyword goasmOpcode_PENT_MMX  PSUBSB PSUBSBB PSUBSBW PSUBSBL PSUBSBQ
syntax keyword goasmOpcode_PENT_MMX  PSUBSIW PSUBSIWB PSUBSIWW PSUBSIWL PSUBSIWQ
syntax keyword goasmOpcode_PENT_MMX  PSUBSW PSUBSWB PSUBSWW PSUBSWL PSUBSWQ
syntax keyword goasmOpcode_PENT_MMX  PSUBUSB PSUBUSBB PSUBUSBW PSUBUSBL PSUBUSBQ
syntax keyword goasmOpcode_PENT_MMX  PSUBUSW PSUBUSWB PSUBUSWW PSUBUSWL PSUBUSWQ
syntax keyword goasmOpcode_PENT_MMX  PSUBW PSUBWB PSUBWW PSUBWL PSUBWQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKHBW PUNPCKHBWB PUNPCKHBWW PUNPCKHBWL PUNPCKHBWQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKHDQ PUNPCKHDQB PUNPCKHDQW PUNPCKHDQL PUNPCKHDQQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKHWD PUNPCKHWDB PUNPCKHWDW PUNPCKHWDL PUNPCKHWDQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKLBW PUNPCKLBWB PUNPCKLBWW PUNPCKLBWL PUNPCKLBWQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKLDQ PUNPCKLDQB PUNPCKLDQW PUNPCKLDQL PUNPCKLDQQ
syntax keyword goasmOpcode_PENT_MMX  PUNPCKLWD PUNPCKLWDB PUNPCKLWDW PUNPCKLWDL PUNPCKLWDQ
syntax keyword goasmOpcode_X64_Base  PUSH PUSHB PUSHW PUSHL PUSHQ
syntax keyword goasmOpcode_186_Base  PUSHA
syntax keyword goasmOpcode_386_Base  PUSHAL
syntax keyword goasmOpcode_186_Base  PUSHAW
syntax keyword goasmOpcode_8086_Base  PUSHF
syntax keyword goasmOpcode_386_Base  PUSHFD
syntax keyword goasmOpcode_X64_Base  PUSHFQ
syntax keyword goasmOpcode_8086_Base  PUSHFW
syntax keyword goasmOpcode_PENT_MMX  PXOR PXORB PXORW PXORL PXORQ
syntax keyword goasmOpcode_X64_Base  RCL RCLB RCLW RCLL RCLQ
syntax keyword goasmOpcode_X64_Base  RCR RCRB RCRW RCRL RCRQ
syntax keyword goasmOpcode_P6_Base  RDSHR
syntax keyword goasmOpcode_PENT_Base  RDMSR
syntax keyword goasmOpcode_P6_Base  RDPMC
syntax keyword goasmOpcode_PENT_Base  RDTSC
syntax keyword goasmOpcode_X86_64_Base  RDTSCP
syntax keyword goasmOpcode_8086_Base  RET RETB RETW RETL RETQ
syntax keyword goasmOpcode_8086_Base  RETF RETFB RETFW RETFL RETFQ
syntax keyword goasmOpcode_8086_Base  RETN RETNB RETNW RETNL RETNQ
syntax keyword goasmOpcode_X64_Base  ROL ROLB ROLW ROLL ROLQ
syntax keyword goasmOpcode_X64_Base  ROR RORB RORW RORL RORQ
syntax keyword goasmOpcode_P6_Base  RDM
syntax keyword goasmOpcode_486_Base  RSDC RSDCB RSDCW RSDCL RSDCQ
syntax keyword goasmOpcode_486_Base  RSLDT RSLDTB RSLDTW RSLDTL RSLDTQ
syntax keyword goasmOpcode_PENTM_Base  RSM
syntax keyword goasmOpcode_486_Base  RSTS RSTSB RSTSW RSTSL RSTSQ
syntax keyword goasmOpcode_8086_Base  SAHF
syntax keyword goasmOpcode_X64_Base  SAL SALB SALW SALL SALQ
syntax keyword goasmOpcode_8086_Base  SALC
syntax keyword goasmOpcode_X64_Base  SAR SARB SARW SARL SARQ
syntax keyword goasmOpcode_386_Base  SBB SBBB SBBW SBBL SBBQ
syntax keyword goasmOpcode_8086_Base  SCASB
syntax keyword goasmOpcode_386_Base  SCASD
syntax keyword goasmOpcode_X64_Base  SCASQ
syntax keyword goasmOpcode_8086_Base  SCASW
syntax keyword goasmOpcode_X64_Base  SFENCE
syntax keyword goasmOpcode_286_Base  SGDT SGDTB SGDTW SGDTL SGDTQ
syntax keyword goasmOpcode_X64_Base  SHL SHLB SHLW SHLL SHLQ
syntax keyword goasmOpcode_X64_Base  SHLD
syntax keyword goasmOpcode_X64_Base  SHR SHRB SHRW SHRL SHRQ
syntax keyword goasmOpcode_X64_Base  SHRD
syntax keyword goasmOpcode_286_Base  SIDT SIDTB SIDTW SIDTL SIDTQ
syntax keyword goasmOpcode_X64_Base  SLDT
syntax keyword goasmOpcode_X64_Base  SKINIT
syntax keyword goasmOpcode_386_Base  SMI
syntax keyword goasmOpcode_P6_Base  SMINT
syntax keyword goasmOpcode_486_Base  SMINTOLD
syntax keyword goasmOpcode_386_Base  SMSW
syntax keyword goasmOpcode_8086_Base  STC
syntax keyword goasmOpcode_8086_Base  STD
syntax keyword goasmOpcode_X64_Base  STGI
syntax keyword goasmOpcode_8086_Base  STI
syntax keyword goasmOpcode_8086_Base  STOSB
syntax keyword goasmOpcode_386_Base  STOSD STOSL
syntax keyword goasmOpcode_X64_Base  STOSQ
syntax keyword goasmOpcode_8086_Base  STOSW
syntax keyword goasmOpcode_X64_Base  STR
syntax keyword goasmOpcode_386_Base  SUB SUBB SUBW SUBL SUBQ
syntax keyword goasmOpcode_486_Base  SVDC SVDCB SVDCW SVDCL SVDCQ
syntax keyword goasmOpcode_486_Base  SVLDT SVLDTB SVLDTW SVLDTL SVLDTQ
syntax keyword goasmOpcode_486_Base  SVTS SVTSB SVTSW SVTSL SVTSQ
syntax keyword goasmOpcode_X64_Base  SWAPGS
syntax keyword goasmOpcode_P6_Base  SYSCALL
syntax keyword goasmOpcode_P6_Base  SYSENTER
syntax keyword goasmOpcode_P6_Base  SYSEXIT
syntax keyword goasmOpcode_P6_Base  SYSRET
syntax keyword goasmOpcode_386_Base  TEST TESTB TESTW TESTL TESTQ
syntax keyword goasmOpcode_186_Base  UD0
syntax keyword goasmOpcode_186_Base  UD1
syntax keyword goasmOpcode_186_Base  UD2B
syntax keyword goasmOpcode_186_Base  UD2
syntax keyword goasmOpcode_186_Base  UD2A
syntax keyword goasmOpcode_386_Base  UMOV
syntax keyword goasmOpcode_286_Base  VERR
syntax keyword goasmOpcode_286_Base  VERW
syntax keyword goasmOpcode_8086_Base  FWAIT
syntax keyword goasmOpcode_486_Base  WBINVD
syntax keyword goasmOpcode_P6_Base  WRSHR
syntax keyword goasmOpcode_PENT_Base  WRMSR
syntax keyword goasmOpcode_X64_Base  XADD
syntax keyword goasmOpcode_386_Base  XBTS
syntax keyword goasmOpcode_X64_Base  XCHG
syntax keyword goasmOpcode_8086_Base  XLATB
syntax keyword goasmOpcode_8086_Base  XLAT
syntax keyword goasmOpcode_386_Base  XOR XORB XORW XORL XORQ
syntax keyword goasmOpcode_X64_Base  CMOVCC
syntax match   goasmOpcode_8086_Base  /\<j\(e\|ne\|a\|ae\|b\|be\|nbe\|g\|ge\|ng\|nge\|l\|le\|\|z\|nz\|c\|nc\|d\|nd\|o\|no\|p\|np\|s\|ns\)[bwlq]\?\>/
syntax match   goasmOpcode_386_Base  /\<set\(e\|ne\|a\|ae\|b\|be\|nbe\|g\|ge\|ng\|nge\|l\|le\|\|z\|nz\|c\|nc\|d\|nd\|o\|no\|p\|np\|s\|ns\)[bwlq]\?\>/

"-- Section: VIA (Centaur) security instructions
syntax keyword goasmOpcode_PENT_Base  XSTORE
syntax keyword goasmOpcode_PENT_Base  XCRYPTECB
syntax keyword goasmOpcode_PENT_Base  XCRYPTCBC
syntax keyword goasmOpcode_PENT_Base  XCRYPTCTR
syntax keyword goasmOpcode_PENT_Base  XCRYPTCFB
syntax keyword goasmOpcode_PENT_Base  XCRYPTOFB
syntax keyword goasmOpcode_PENT_Base  MONTMUL
syntax keyword goasmOpcode_PENT_Base  XSHA1
syntax keyword goasmOpcode_PENT_Base  XSHA256

"-- Section: Intel AVX Carry-Less Multiplication instructions (CLMUL)
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCLMULLQLQDQ VPCLMULLQLQDQB VPCLMULLQLQDQW VPCLMULLQLQDQL VPCLMULLQLQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCLMULHQLQDQ VPCLMULHQLQDQB VPCLMULHQLQDQW VPCLMULHQLQDQL VPCLMULHQLQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCLMULLQHQDQ VPCLMULLQHQDQB VPCLMULLQHQDQW VPCLMULLQHQDQL VPCLMULLQHQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCLMULHQHQDQ VPCLMULHQHQDQB VPCLMULHQHQDQW VPCLMULHQHQDQL VPCLMULHQHQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCLMULQDQ VPCLMULQDQB VPCLMULQDQW VPCLMULQDQL VPCLMULQDQQ

"-- Section: AMD SSE5 instructions
syntax keyword goasmOpcode_AMD_SSE5  FMADDPS FMADDPSB FMADDPSW FMADDPSL FMADDPSQ
syntax keyword goasmOpcode_AMD_SSE5  FMADDPD FMADDPDB FMADDPDW FMADDPDL FMADDPDQ
syntax keyword goasmOpcode_AMD_SSE5  FMADDSS FMADDSSB FMADDSSW FMADDSSL FMADDSSQ
syntax keyword goasmOpcode_AMD_SSE5  FMADDSD FMADDSDB FMADDSDW FMADDSDL FMADDSDQ
syntax keyword goasmOpcode_AMD_SSE5  FMSUBPS FMSUBPSB FMSUBPSW FMSUBPSL FMSUBPSQ
syntax keyword goasmOpcode_AMD_SSE5  FMSUBPD FMSUBPDB FMSUBPDW FMSUBPDL FMSUBPDQ
syntax keyword goasmOpcode_AMD_SSE5  FMSUBSS FMSUBSSB FMSUBSSW FMSUBSSL FMSUBSSQ
syntax keyword goasmOpcode_AMD_SSE5  FMSUBSD FMSUBSDB FMSUBSDW FMSUBSDL FMSUBSDQ
syntax keyword goasmOpcode_AMD_SSE5  FNMADDPS FNMADDPSB FNMADDPSW FNMADDPSL FNMADDPSQ
syntax keyword goasmOpcode_AMD_SSE5  FNMADDPD FNMADDPDB FNMADDPDW FNMADDPDL FNMADDPDQ
syntax keyword goasmOpcode_AMD_SSE5  FNMADDSS FNMADDSSB FNMADDSSW FNMADDSSL FNMADDSSQ
syntax keyword goasmOpcode_AMD_SSE5  FNMADDSD FNMADDSDB FNMADDSDW FNMADDSDL FNMADDSDQ
syntax keyword goasmOpcode_AMD_SSE5  FNMSUBPS FNMSUBPSB FNMSUBPSW FNMSUBPSL FNMSUBPSQ
syntax keyword goasmOpcode_AMD_SSE5  FNMSUBPD FNMSUBPDB FNMSUBPDW FNMSUBPDL FNMSUBPDQ
syntax keyword goasmOpcode_AMD_SSE5  FNMSUBSS FNMSUBSSB FNMSUBSSW FNMSUBSSL FNMSUBSSQ
syntax keyword goasmOpcode_AMD_SSE5  FNMSUBSD FNMSUBSDB FNMSUBSDW FNMSUBSDL FNMSUBSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMEQPS COMEQPSB COMEQPSW COMEQPSL COMEQPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMLTPS COMLTPSB COMLTPSW COMLTPSL COMLTPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMLEPS COMLEPSB COMLEPSW COMLEPSL COMLEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNORDPS COMUNORDPSB COMUNORDPSW COMUNORDPSL COMUNORDPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNEQPS COMUNEQPSB COMUNEQPSW COMUNEQPSL COMUNEQPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLTPS COMUNLTPSB COMUNLTPSW COMUNLTPSL COMUNLTPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLEPS COMUNLEPSB COMUNLEPSW COMUNLEPSL COMUNLEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMORDPS COMORDPSB COMORDPSW COMORDPSL COMORDPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUEQPS COMUEQPSB COMUEQPSW COMUEQPSL COMUEQPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMULTPS COMULTPSB COMULTPSW COMULTPSL COMULTPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMULEPS COMULEPSB COMULEPSW COMULEPSL COMULEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMFALSEPS COMFALSEPSB COMFALSEPSW COMFALSEPSL COMFALSEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNEQPS COMNEQPSB COMNEQPSW COMNEQPSL COMNEQPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLTPS COMNLTPSB COMNLTPSW COMNLTPSL COMNLTPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLEPS COMNLEPSB COMNLEPSW COMNLEPSL COMNLEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMTRUEPS COMTRUEPSB COMTRUEPSW COMTRUEPSL COMTRUEPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMPS COMPSB COMPSW COMPSL COMPSQ
syntax keyword goasmOpcode_AMD_SSE5  COMEQPD COMEQPDB COMEQPDW COMEQPDL COMEQPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMLTPD COMLTPDB COMLTPDW COMLTPDL COMLTPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMLEPD COMLEPDB COMLEPDW COMLEPDL COMLEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNORDPD COMUNORDPDB COMUNORDPDW COMUNORDPDL COMUNORDPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNEQPD COMUNEQPDB COMUNEQPDW COMUNEQPDL COMUNEQPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLTPD COMUNLTPDB COMUNLTPDW COMUNLTPDL COMUNLTPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLEPD COMUNLEPDB COMUNLEPDW COMUNLEPDL COMUNLEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMORDPD COMORDPDB COMORDPDW COMORDPDL COMORDPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUEQPD COMUEQPDB COMUEQPDW COMUEQPDL COMUEQPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMULTPD COMULTPDB COMULTPDW COMULTPDL COMULTPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMULEPD COMULEPDB COMULEPDW COMULEPDL COMULEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMFALSEPD COMFALSEPDB COMFALSEPDW COMFALSEPDL COMFALSEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNEQPD COMNEQPDB COMNEQPDW COMNEQPDL COMNEQPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLTPD COMNLTPDB COMNLTPDW COMNLTPDL COMNLTPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLEPD COMNLEPDB COMNLEPDW COMNLEPDL COMNLEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMTRUEPD COMTRUEPDB COMTRUEPDW COMTRUEPDL COMTRUEPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMPD COMPDB COMPDW COMPDL COMPDQ
syntax keyword goasmOpcode_AMD_SSE5  COMEQSS COMEQSSB COMEQSSW COMEQSSL COMEQSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMLTSS COMLTSSB COMLTSSW COMLTSSL COMLTSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMLESS COMLESSB COMLESSW COMLESSL COMLESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNORDSS COMUNORDSSB COMUNORDSSW COMUNORDSSL COMUNORDSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNEQSS COMUNEQSSB COMUNEQSSW COMUNEQSSL COMUNEQSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLTSS COMUNLTSSB COMUNLTSSW COMUNLTSSL COMUNLTSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLESS COMUNLESSB COMUNLESSW COMUNLESSL COMUNLESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMORDSS COMORDSSB COMORDSSW COMORDSSL COMORDSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMUEQSS COMUEQSSB COMUEQSSW COMUEQSSL COMUEQSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMULTSS COMULTSSB COMULTSSW COMULTSSL COMULTSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMULESS COMULESSB COMULESSW COMULESSL COMULESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMFALSESS COMFALSESSB COMFALSESSW COMFALSESSL COMFALSESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNEQSS COMNEQSSB COMNEQSSW COMNEQSSL COMNEQSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLTSS COMNLTSSB COMNLTSSW COMNLTSSL COMNLTSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLESS COMNLESSB COMNLESSW COMNLESSL COMNLESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMTRUESS COMTRUESSB COMTRUESSW COMTRUESSL COMTRUESSQ
syntax keyword goasmOpcode_AMD_SSE5  COMSS COMSSB COMSSW COMSSL COMSSQ
syntax keyword goasmOpcode_AMD_SSE5  COMEQSD COMEQSDB COMEQSDW COMEQSDL COMEQSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMLTSD COMLTSDB COMLTSDW COMLTSDL COMLTSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMLESD COMLESDB COMLESDW COMLESDL COMLESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNORDSD COMUNORDSDB COMUNORDSDW COMUNORDSDL COMUNORDSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNEQSD COMUNEQSDB COMUNEQSDW COMUNEQSDL COMUNEQSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLTSD COMUNLTSDB COMUNLTSDW COMUNLTSDL COMUNLTSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUNLESD COMUNLESDB COMUNLESDW COMUNLESDL COMUNLESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMORDSD COMORDSDB COMORDSDW COMORDSDL COMORDSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMUEQSD COMUEQSDB COMUEQSDW COMUEQSDL COMUEQSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMULTSD COMULTSDB COMULTSDW COMULTSDL COMULTSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMULESD COMULESDB COMULESDW COMULESDL COMULESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMFALSESD COMFALSESDB COMFALSESDW COMFALSESDL COMFALSESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNEQSD COMNEQSDB COMNEQSDW COMNEQSDL COMNEQSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLTSD COMNLTSDB COMNLTSDW COMNLTSDL COMNLTSDQ
syntax keyword goasmOpcode_AMD_SSE5  COMNLESD COMNLESDB COMNLESDW COMNLESDL COMNLESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMTRUESD COMTRUESDB COMTRUESDW COMTRUESDL COMTRUESDQ
syntax keyword goasmOpcode_AMD_SSE5  COMSD COMSDB COMSDW COMSDL COMSDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTB PCOMLTBB PCOMLTBW PCOMLTBL PCOMLTBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEB PCOMLEBB PCOMLEBW PCOMLEBL PCOMLEBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTB PCOMGTBB PCOMGTBW PCOMGTBL PCOMGTBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEB PCOMGEBB PCOMGEBW PCOMGEBL PCOMGEBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQB PCOMEQBB PCOMEQBW PCOMEQBL PCOMEQBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQB PCOMNEQBB PCOMNEQBW PCOMNEQBL PCOMNEQBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEB PCOMFALSEBB PCOMFALSEBW PCOMFALSEBL PCOMFALSEBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEB PCOMTRUEBB PCOMTRUEBW PCOMTRUEBL PCOMTRUEBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMB PCOMBB PCOMBW PCOMBL PCOMBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTW PCOMLTWB PCOMLTWW PCOMLTWL PCOMLTWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEW PCOMLEWB PCOMLEWW PCOMLEWL PCOMLEWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTW PCOMGTWB PCOMGTWW PCOMGTWL PCOMGTWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEW PCOMGEWB PCOMGEWW PCOMGEWL PCOMGEWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQW PCOMEQWB PCOMEQWW PCOMEQWL PCOMEQWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQW PCOMNEQWB PCOMNEQWW PCOMNEQWL PCOMNEQWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEW PCOMFALSEWB PCOMFALSEWW PCOMFALSEWL PCOMFALSEWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEW PCOMTRUEWB PCOMTRUEWW PCOMTRUEWL PCOMTRUEWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMW PCOMWB PCOMWW PCOMWL PCOMWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTD PCOMLTDB PCOMLTDW PCOMLTDL PCOMLTDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLED PCOMLEDB PCOMLEDW PCOMLEDL PCOMLEDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTD PCOMGTDB PCOMGTDW PCOMGTDL PCOMGTDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGED PCOMGEDB PCOMGEDW PCOMGEDL PCOMGEDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQD PCOMEQDB PCOMEQDW PCOMEQDL PCOMEQDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQD PCOMNEQDB PCOMNEQDW PCOMNEQDL PCOMNEQDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSED PCOMFALSEDB PCOMFALSEDW PCOMFALSEDL PCOMFALSEDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUED PCOMTRUEDB PCOMTRUEDW PCOMTRUEDL PCOMTRUEDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMD PCOMDB PCOMDW PCOMDL PCOMDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTQ PCOMLTQB PCOMLTQW PCOMLTQL PCOMLTQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEQ PCOMLEQB PCOMLEQW PCOMLEQL PCOMLEQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTQ PCOMGTQB PCOMGTQW PCOMGTQL PCOMGTQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEQ PCOMGEQB PCOMGEQW PCOMGEQL PCOMGEQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQQ PCOMEQQB PCOMEQQW PCOMEQQL PCOMEQQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQQ PCOMNEQQB PCOMNEQQW PCOMNEQQL PCOMNEQQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEQ PCOMFALSEQB PCOMFALSEQW PCOMFALSEQL PCOMFALSEQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEQ PCOMTRUEQB PCOMTRUEQW PCOMTRUEQL PCOMTRUEQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMQ PCOMQB PCOMQW PCOMQL PCOMQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTUB PCOMLTUBB PCOMLTUBW PCOMLTUBL PCOMLTUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEUB PCOMLEUBB PCOMLEUBW PCOMLEUBL PCOMLEUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTUB PCOMGTUBB PCOMGTUBW PCOMGTUBL PCOMGTUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEUB PCOMGEUBB PCOMGEUBW PCOMGEUBL PCOMGEUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQUB PCOMEQUBB PCOMEQUBW PCOMEQUBL PCOMEQUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQUB PCOMNEQUBB PCOMNEQUBW PCOMNEQUBL PCOMNEQUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEUB PCOMFALSEUBB PCOMFALSEUBW PCOMFALSEUBL PCOMFALSEUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEUB PCOMTRUEUBB PCOMTRUEUBW PCOMTRUEUBL PCOMTRUEUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMUB PCOMUBB PCOMUBW PCOMUBL PCOMUBQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTUW PCOMLTUWB PCOMLTUWW PCOMLTUWL PCOMLTUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEUW PCOMLEUWB PCOMLEUWW PCOMLEUWL PCOMLEUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTUW PCOMGTUWB PCOMGTUWW PCOMGTUWL PCOMGTUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEUW PCOMGEUWB PCOMGEUWW PCOMGEUWL PCOMGEUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQUW PCOMEQUWB PCOMEQUWW PCOMEQUWL PCOMEQUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQUW PCOMNEQUWB PCOMNEQUWW PCOMNEQUWL PCOMNEQUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEUW PCOMFALSEUWB PCOMFALSEUWW PCOMFALSEUWL PCOMFALSEUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEUW PCOMTRUEUWB PCOMTRUEUWW PCOMTRUEUWL PCOMTRUEUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMUW PCOMUWB PCOMUWW PCOMUWL PCOMUWQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTUD PCOMLTUDB PCOMLTUDW PCOMLTUDL PCOMLTUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEUD PCOMLEUDB PCOMLEUDW PCOMLEUDL PCOMLEUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTUD PCOMGTUDB PCOMGTUDW PCOMGTUDL PCOMGTUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEUD PCOMGEUDB PCOMGEUDW PCOMGEUDL PCOMGEUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQUD PCOMEQUDB PCOMEQUDW PCOMEQUDL PCOMEQUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQUD PCOMNEQUDB PCOMNEQUDW PCOMNEQUDL PCOMNEQUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEUD PCOMFALSEUDB PCOMFALSEUDW PCOMFALSEUDL PCOMFALSEUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEUD PCOMTRUEUDB PCOMTRUEUDW PCOMTRUEUDL PCOMTRUEUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMUD PCOMUDB PCOMUDW PCOMUDL PCOMUDQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLTUQ PCOMLTUQB PCOMLTUQW PCOMLTUQL PCOMLTUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMLEUQ PCOMLEUQB PCOMLEUQW PCOMLEUQL PCOMLEUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGTUQ PCOMGTUQB PCOMGTUQW PCOMGTUQL PCOMGTUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMGEUQ PCOMGEUQB PCOMGEUQW PCOMGEUQL PCOMGEUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMEQUQ PCOMEQUQB PCOMEQUQW PCOMEQUQL PCOMEQUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMNEQUQ PCOMNEQUQB PCOMNEQUQW PCOMNEQUQL PCOMNEQUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMFALSEUQ PCOMFALSEUQB PCOMFALSEUQW PCOMFALSEUQL PCOMFALSEUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMTRUEUQ PCOMTRUEUQB PCOMTRUEUQW PCOMTRUEUQL PCOMTRUEUQQ
syntax keyword goasmOpcode_AMD_SSE5  PCOMUQ PCOMUQB PCOMUQW PCOMUQL PCOMUQQ
syntax keyword goasmOpcode_AMD_SSE5  PERMPS PERMPSB PERMPSW PERMPSL PERMPSQ
syntax keyword goasmOpcode_AMD_SSE5  PERMPD PERMPDB PERMPDW PERMPDL PERMPDQ
syntax keyword goasmOpcode_AMD_SSE5  PCMOV PCMOVB PCMOVW PCMOVL PCMOVQ
syntax keyword goasmOpcode_AMD_SSE5  PPERM PPERMB PPERMW PPERML PPERMQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSSWW PMACSSWWB PMACSSWWW PMACSSWWL PMACSSWWQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSWW PMACSWWB PMACSWWW PMACSWWL PMACSWWQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSSWD PMACSSWDB PMACSSWDW PMACSSWDL PMACSSWDQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSWD PMACSWDB PMACSWDW PMACSWDL PMACSWDQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSSDD PMACSSDDB PMACSSDDW PMACSSDDL PMACSSDDQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSDD PMACSDDB PMACSDDW PMACSDDL PMACSDDQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSSDQL PMACSSDQLB PMACSSDQLW PMACSSDQLL PMACSSDQLQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSDQL PMACSDQLB PMACSDQLW PMACSDQLL PMACSDQLQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSSDQH PMACSSDQHB PMACSSDQHW PMACSSDQHL PMACSSDQHQ
syntax keyword goasmOpcode_AMD_SSE5  PMACSDQH PMACSDQHB PMACSDQHW PMACSDQHL PMACSDQHQ
syntax keyword goasmOpcode_AMD_SSE5  PMADCSSWD PMADCSSWDB PMADCSSWDW PMADCSSWDL PMADCSSWDQ
syntax keyword goasmOpcode_AMD_SSE5  PMADCSWD PMADCSWDB PMADCSWDW PMADCSWDL PMADCSWDQ
syntax keyword goasmOpcode_AMD_SSE5  PROTB PROTBB PROTBW PROTBL PROTBQ
syntax keyword goasmOpcode_AMD_SSE5  PROTW PROTWB PROTWW PROTWL PROTWQ
syntax keyword goasmOpcode_AMD_SSE5  PROTD PROTDB PROTDW PROTDL PROTDQ
syntax keyword goasmOpcode_AMD_SSE5  PROTQ PROTQB PROTQW PROTQL PROTQQ
syntax keyword goasmOpcode_AMD_SSE5  PSHLB PSHLBB PSHLBW PSHLBL PSHLBQ
syntax keyword goasmOpcode_AMD_SSE5  PSHLW PSHLWB PSHLWW PSHLWL PSHLWQ
syntax keyword goasmOpcode_AMD_SSE5  PSHLD PSHLDB PSHLDW PSHLDL PSHLDQ
syntax keyword goasmOpcode_AMD_SSE5  PSHLQ PSHLQB PSHLQW PSHLQL PSHLQQ
syntax keyword goasmOpcode_AMD_SSE5  PSHAB PSHABB PSHABW PSHABL PSHABQ
syntax keyword goasmOpcode_AMD_SSE5  PSHAW PSHAWB PSHAWW PSHAWL PSHAWQ
syntax keyword goasmOpcode_AMD_SSE5  PSHAD PSHADB PSHADW PSHADL PSHADQ
syntax keyword goasmOpcode_AMD_SSE5  PSHAQ PSHAQB PSHAQW PSHAQL PSHAQQ
syntax keyword goasmOpcode_AMD_SSE5  FRCZPS FRCZPSB FRCZPSW FRCZPSL FRCZPSQ
syntax keyword goasmOpcode_AMD_SSE5  FRCZPD FRCZPDB FRCZPDW FRCZPDL FRCZPDQ
syntax keyword goasmOpcode_AMD_SSE5  FRCZSS FRCZSSB FRCZSSW FRCZSSL FRCZSSQ
syntax keyword goasmOpcode_AMD_SSE5  FRCZSD FRCZSDB FRCZSDW FRCZSDL FRCZSDQ
syntax keyword goasmOpcode_AMD_SSE5  CVTPH2PS CVTPH2PSB CVTPH2PSW CVTPH2PSL CVTPH2PSQ
syntax keyword goasmOpcode_AMD_SSE5  CVTPS2PH CVTPS2PHB CVTPS2PHW CVTPS2PHL CVTPS2PHQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDBW PHADDBWB PHADDBWW PHADDBWL PHADDBWQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDBD PHADDBDB PHADDBDW PHADDBDL PHADDBDQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDBQ PHADDBQB PHADDBQW PHADDBQL PHADDBQQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDWD PHADDWDB PHADDWDW PHADDWDL PHADDWDQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDWQ PHADDWQB PHADDWQW PHADDWQL PHADDWQQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDDQ PHADDDQB PHADDDQW PHADDDQL PHADDDQQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUBW PHADDUBWB PHADDUBWW PHADDUBWL PHADDUBWQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUBD PHADDUBDB PHADDUBDW PHADDUBDL PHADDUBDQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUBQ PHADDUBQB PHADDUBQW PHADDUBQL PHADDUBQQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUWD PHADDUWDB PHADDUWDW PHADDUWDL PHADDUWDQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUWQ PHADDUWQB PHADDUWQW PHADDUWQL PHADDUWQQ
syntax keyword goasmOpcode_AMD_SSE5  PHADDUDQ PHADDUDQB PHADDUDQW PHADDUDQL PHADDUDQQ
syntax keyword goasmOpcode_AMD_SSE5  PHSUBBW PHSUBBWB PHSUBBWW PHSUBBWL PHSUBBWQ
syntax keyword goasmOpcode_AMD_SSE5  PHSUBWD PHSUBWDB PHSUBWDW PHSUBWDL PHSUBWDQ
syntax keyword goasmOpcode_AMD_SSE5  PHSUBDQ PHSUBDQB PHSUBDQW PHSUBDQL PHSUBDQQ
syntax keyword goasmOpcode_AMD_SSE5  PROTB PROTBB PROTBW PROTBL PROTBQ
syntax keyword goasmOpcode_AMD_SSE5  PROTW PROTWB PROTWW PROTWL PROTWQ
syntax keyword goasmOpcode_AMD_SSE5  PROTD PROTDB PROTDW PROTDL PROTDQ
syntax keyword goasmOpcode_AMD_SSE5  PROTQ PROTQB PROTQW PROTQL PROTQQ
syntax keyword goasmOpcode_AMD_SSE5  ROUNDPS ROUNDPSB ROUNDPSW ROUNDPSL ROUNDPSQ
syntax keyword goasmOpcode_AMD_SSE5  ROUNDPD ROUNDPDB ROUNDPDW ROUNDPDL ROUNDPDQ
syntax keyword goasmOpcode_AMD_SSE5  ROUNDSS ROUNDSSB ROUNDSSW ROUNDSSL ROUNDSSQ
syntax keyword goasmOpcode_AMD_SSE5  ROUNDSD ROUNDSDB ROUNDSDW ROUNDSDL ROUNDSDQ

"-- Section: Introduced in Deschutes but necessary for SSE support
syntax keyword goasmOpcode_P6_SSE  FXRSTOR FXRSTORB FXRSTORW FXRSTORL FXRSTORQ
syntax keyword goasmOpcode_P6_SSE  FXSAVE FXSAVEB FXSAVEW FXSAVEL FXSAVEQ

"-- Section: Prescott New Instructions (SSE3)
syntax keyword goasmOpcode_PRESCOTT_SSE3 ADDSUBPD ADDSUBPDB ADDSUBPDW ADDSUBPDL ADDSUBPDQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 ADDSUBPS ADDSUBPSB ADDSUBPSW ADDSUBPSL ADDSUBPSQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 HADDPD HADDPDB HADDPDW HADDPDL HADDPDQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 HADDPS HADDPSB HADDPSW HADDPSL HADDPSQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 HSUBPD HSUBPDB HSUBPDW HSUBPDL HSUBPDQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 HSUBPS HSUBPSB HSUBPSW HSUBPSL HSUBPSQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 LDDQU LDDQUB LDDQUW LDDQUL LDDQUQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 MOVDDUP MOVDDUPB MOVDDUPW MOVDDUPL MOVDDUPQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 MOVSHDUP MOVSHDUPB MOVSHDUPW MOVSHDUPL MOVSHDUPQ
syntax keyword goasmOpcode_PRESCOTT_SSE3 MOVSLDUP MOVSLDUPB MOVSLDUPW MOVSLDUPL MOVSLDUPQ

"-- Section: Intel AES instructions
syntax keyword goasmOpcode_SSE  AESENC AESENCB AESENCW AESENCL AESENCQ
syntax keyword goasmOpcode_SSE  AESENCLAST AESENCLASTB AESENCLASTW AESENCLASTL AESENCLASTQ
syntax keyword goasmOpcode_SSE  AESDEC AESDECB AESDECW AESDECL AESDECQ
syntax keyword goasmOpcode_SSE  AESDECLAST AESDECLASTB AESDECLASTW AESDECLASTL AESDECLASTQ
syntax keyword goasmOpcode_SSE  AESIMC AESIMCB AESIMCW AESIMCL AESIMCQ
syntax keyword goasmOpcode_SSE  AESKEYGENASSIST AESKEYGENASSISTB AESKEYGENASSISTW AESKEYGENASSISTL AESKEYGENASSISTQ

"-- Section: Willamette Streaming SIMD instructions (SSE2)
syntax keyword goasmOpcode_SSE2  ADDPD ADDPDB ADDPDW ADDPDL ADDPDQ
syntax keyword goasmOpcode_SSE2  ADDSD ADDSDB ADDSDW ADDSDL ADDSDQ
syntax keyword goasmOpcode_SSE2  ANDNPD ANDNPDB ANDNPDW ANDNPDL ANDNPDQ
syntax keyword goasmOpcode_SSE2  ANDPD ANDPDB ANDPDW ANDPDL ANDPDQ
syntax keyword goasmOpcode_SSE2  CMPEQPD CMPEQPDB CMPEQPDW CMPEQPDL CMPEQPDQ
syntax keyword goasmOpcode_SSE2  CMPEQSD CMPEQSDB CMPEQSDW CMPEQSDL CMPEQSDQ
syntax keyword goasmOpcode_SSE2  CMPLEPD CMPLEPDB CMPLEPDW CMPLEPDL CMPLEPDQ
syntax keyword goasmOpcode_SSE2  CMPLESD CMPLESDB CMPLESDW CMPLESDL CMPLESDQ
syntax keyword goasmOpcode_SSE2  CMPLTPD CMPLTPDB CMPLTPDW CMPLTPDL CMPLTPDQ
syntax keyword goasmOpcode_SSE2  CMPLTSD CMPLTSDB CMPLTSDW CMPLTSDL CMPLTSDQ
syntax keyword goasmOpcode_SSE2  CMPNEQPD CMPNEQPDB CMPNEQPDW CMPNEQPDL CMPNEQPDQ
syntax keyword goasmOpcode_SSE2  CMPNEQSD CMPNEQSDB CMPNEQSDW CMPNEQSDL CMPNEQSDQ
syntax keyword goasmOpcode_SSE2  CMPNLEPD CMPNLEPDB CMPNLEPDW CMPNLEPDL CMPNLEPDQ
syntax keyword goasmOpcode_SSE2  CMPNLESD CMPNLESDB CMPNLESDW CMPNLESDL CMPNLESDQ
syntax keyword goasmOpcode_SSE2  CMPNLTPD CMPNLTPDB CMPNLTPDW CMPNLTPDL CMPNLTPDQ
syntax keyword goasmOpcode_SSE2  CMPNLTSD CMPNLTSDB CMPNLTSDW CMPNLTSDL CMPNLTSDQ
syntax keyword goasmOpcode_SSE2  CMPORDPD CMPORDPDB CMPORDPDW CMPORDPDL CMPORDPDQ
syntax keyword goasmOpcode_SSE2  CMPORDSD CMPORDSDB CMPORDSDW CMPORDSDL CMPORDSDQ
syntax keyword goasmOpcode_SSE2  CMPUNORDPD CMPUNORDPDB CMPUNORDPDW CMPUNORDPDL CMPUNORDPDQ
syntax keyword goasmOpcode_SSE2  CMPUNORDSD CMPUNORDSDB CMPUNORDSDW CMPUNORDSDL CMPUNORDSDQ
syntax keyword goasmOpcode_Base  CMPPD CMPPDB CMPPDW CMPPDL CMPPDQ
syntax keyword goasmOpcode_SSE2  CMPSD CMPSDB CMPSDW CMPSDL CMPSDQ
syntax keyword goasmOpcode_SSE2  COMISD COMISDB COMISDW COMISDL COMISDQ
syntax keyword goasmOpcode_SSE2  CVTDQ2PD CVTDQ2PDB CVTDQ2PDW CVTDQ2PDL CVTDQ2PDQ
syntax keyword goasmOpcode_SSE2  CVTDQ2PS CVTDQ2PSB CVTDQ2PSW CVTDQ2PSL CVTDQ2PSQ
syntax keyword goasmOpcode_SSE2  CVTPD2DQ CVTPD2DQB CVTPD2DQW CVTPD2DQL CVTPD2DQQ
syntax keyword goasmOpcode_SSE2  CVTPD2PI CVTPD2PIB CVTPD2PIW CVTPD2PIL CVTPD2PIQ
syntax keyword goasmOpcode_SSE2  CVTPD2PS CVTPD2PSB CVTPD2PSW CVTPD2PSL CVTPD2PSQ
syntax keyword goasmOpcode_SSE2  CVTPI2PD CVTPI2PDB CVTPI2PDW CVTPI2PDL CVTPI2PDQ
syntax keyword goasmOpcode_SSE2  CVTPS2DQ CVTPS2DQB CVTPS2DQW CVTPS2DQL CVTPS2DQQ
syntax keyword goasmOpcode_SSE2  CVTPS2PD CVTPS2PDB CVTPS2PDW CVTPS2PDL CVTPS2PDQ
syntax keyword goasmOpcode_X64_SSE2  CVTSD2SI CVTSD2SIB CVTSD2SIW CVTSD2SIL CVTSD2SIQ
syntax keyword goasmOpcode_SSE2  CVTSD2SS CVTSD2SSB CVTSD2SSW CVTSD2SSL CVTSD2SSQ
syntax keyword goasmOpcode_X64_SSE2  CVTSI2SD
syntax keyword goasmOpcode_SSE2  CVTSS2SD CVTSS2SDB CVTSS2SDW CVTSS2SDL CVTSS2SDQ
syntax keyword goasmOpcode_SSE2  CVTTPD2PI CVTTPD2PIB CVTTPD2PIW CVTTPD2PIL CVTTPD2PIQ
syntax keyword goasmOpcode_SSE2  CVTTPD2DQ CVTTPD2DQB CVTTPD2DQW CVTTPD2DQL CVTTPD2DQQ
syntax keyword goasmOpcode_SSE2  CVTTPS2DQ CVTTPS2DQB CVTTPS2DQW CVTTPS2DQL CVTTPS2DQQ
syntax keyword goasmOpcode_X64_SSE2  CVTTSD2SI CVTTSD2SIB CVTTSD2SIW CVTTSD2SIL CVTTSD2SIQ
syntax keyword goasmOpcode_SSE2  DIVPD DIVPDB DIVPDW DIVPDL DIVPDQ
syntax keyword goasmOpcode_SSE2  DIVSD DIVSDB DIVSDW DIVSDL DIVSDQ
syntax keyword goasmOpcode_SSE2  MAXPD MAXPDB MAXPDW MAXPDL MAXPDQ
syntax keyword goasmOpcode_SSE2  MAXSD MAXSDB MAXSDW MAXSDL MAXSDQ
syntax keyword goasmOpcode_SSE2  MINPD MINPDB MINPDW MINPDL MINPDQ
syntax keyword goasmOpcode_SSE2  MINSD MINSDB MINSDW MINSDL MINSDQ
syntax keyword goasmOpcode_SSE2  MOVAPD MOVAPDB MOVAPDW MOVAPDL MOVAPDQ
syntax keyword goasmOpcode_SSE2  MOVHPD MOVHPDB MOVHPDW MOVHPDL MOVHPDQ
syntax keyword goasmOpcode_SSE2  MOVLPD MOVLPDB MOVLPDW MOVLPDL MOVLPDQ
syntax keyword goasmOpcode_X64_SSE2  MOVMSKPD
syntax keyword goasmOpcode_SSE2  MOVSD MOVSDB MOVSDW MOVSDL MOVSDQ
syntax keyword goasmOpcode_SSE2  MOVUPD MOVUPDB MOVUPDW MOVUPDL MOVUPDQ
syntax keyword goasmOpcode_SSE2  MULPD MULPDB MULPDW MULPDL MULPDQ
syntax keyword goasmOpcode_SSE2  MULSD MULSDB MULSDW MULSDL MULSDQ
syntax keyword goasmOpcode_SSE2  ORPD ORPDB ORPDW ORPDL ORPDQ
syntax keyword goasmOpcode_SSE2  SHUFPD SHUFPDB SHUFPDW SHUFPDL SHUFPDQ
syntax keyword goasmOpcode_SSE2  SQRTPD SQRTPDB SQRTPDW SQRTPDL SQRTPDQ
syntax keyword goasmOpcode_SSE2  SQRTSD SQRTSDB SQRTSDW SQRTSDL SQRTSDQ
syntax keyword goasmOpcode_SSE2  SUBPD SUBPDB SUBPDW SUBPDL SUBPDQ
syntax keyword goasmOpcode_SSE2  SUBSD SUBSDB SUBSDW SUBSDL SUBSDQ
syntax keyword goasmOpcode_SSE2  UCOMISD UCOMISDB UCOMISDW UCOMISDL UCOMISDQ
syntax keyword goasmOpcode_SSE2  UNPCKHPD UNPCKHPDB UNPCKHPDW UNPCKHPDL UNPCKHPDQ
syntax keyword goasmOpcode_SSE2  UNPCKLPD UNPCKLPDB UNPCKLPDW UNPCKLPDL UNPCKLPDQ
syntax keyword goasmOpcode_SSE2  XORPD XORPDB XORPDW XORPDL XORPDQ

"-- Section: Intel Carry-Less Multiplication instructions (CLMUL)
syntax keyword goasmOpcode_SSE  PCLMULLQLQDQ PCLMULLQLQDQB PCLMULLQLQDQW PCLMULLQLQDQL PCLMULLQLQDQQ
syntax keyword goasmOpcode_SSE  PCLMULHQLQDQ PCLMULHQLQDQB PCLMULHQLQDQW PCLMULHQLQDQL PCLMULHQLQDQQ
syntax keyword goasmOpcode_SSE  PCLMULLQHQDQ PCLMULLQHQDQB PCLMULLQHQDQW PCLMULLQHQDQL PCLMULLQHQDQQ
syntax keyword goasmOpcode_SSE  PCLMULHQHQDQ PCLMULHQHQDQB PCLMULHQHQDQW PCLMULHQHQDQL PCLMULHQHQDQQ
syntax keyword goasmOpcode_SSE  PCLMULQDQ PCLMULQDQB PCLMULQDQW PCLMULQDQL PCLMULQDQQ

"-- Section: New MMX instructions introduced in Katmai
syntax keyword goasmOpcode_KATMAI_MMX MASKMOVQ
syntax keyword goasmOpcode_KATMAI_MMX MOVNTQ MOVNTQB MOVNTQW MOVNTQL MOVNTQQ
syntax keyword goasmOpcode_KATMAI_MMX PAVGB PAVGBB PAVGBW PAVGBL PAVGBQ
syntax keyword goasmOpcode_KATMAI_MMX PAVGW PAVGWB PAVGWW PAVGWL PAVGWQ
syntax keyword goasmOpcode_KATMAI_MMX PEXTRW PEXTRWB PEXTRWW PEXTRWL PEXTRWQ
syntax keyword goasmOpcode_KATMAI_MMX PINSRW PINSRWB PINSRWW PINSRWL PINSRWQ
syntax keyword goasmOpcode_KATMAI_MMX PMAXSW PMAXSWB PMAXSWW PMAXSWL PMAXSWQ
syntax keyword goasmOpcode_KATMAI_MMX PMAXUB PMAXUBB PMAXUBW PMAXUBL PMAXUBQ
syntax keyword goasmOpcode_KATMAI_MMX PMINSW PMINSWB PMINSWW PMINSWL PMINSWQ
syntax keyword goasmOpcode_KATMAI_MMX PMINUB PMINUBB PMINUBW PMINUBL PMINUBQ
syntax keyword goasmOpcode_KATMAI_MMX PMOVMSKB
syntax keyword goasmOpcode_KATMAI_MMX PMULHUW PMULHUWB PMULHUWW PMULHUWL PMULHUWQ
syntax keyword goasmOpcode_KATMAI_MMX PSADBW PSADBWB PSADBWW PSADBWL PSADBWQ
syntax keyword goasmOpcode_KATMAI_MMX2 PSHUFW PSHUFWB PSHUFWW PSHUFWL PSHUFWQ

"-- Section: Intel SMX
syntax keyword goasmOpcode_KATMAI_Base GETSEC

"-- Section: Katmai Streaming SIMD instructions (SSE -- a.k.a. KNI, XMM, MMX2)
syntax keyword goasmOpcode_KATMAI_SSE ADDPS ADDPSB ADDPSW ADDPSL ADDPSQ
syntax keyword goasmOpcode_KATMAI_SSE ADDSS ADDSSB ADDSSW ADDSSL ADDSSQ
syntax keyword goasmOpcode_KATMAI_SSE ANDNPS ANDNPSB ANDNPSW ANDNPSL ANDNPSQ
syntax keyword goasmOpcode_KATMAI_SSE ANDPS ANDPSB ANDPSW ANDPSL ANDPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPEQPS CMPEQPSB CMPEQPSW CMPEQPSL CMPEQPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPEQSS CMPEQSSB CMPEQSSW CMPEQSSL CMPEQSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPLEPS CMPLEPSB CMPLEPSW CMPLEPSL CMPLEPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPLESS CMPLESSB CMPLESSW CMPLESSL CMPLESSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPLTPS CMPLTPSB CMPLTPSW CMPLTPSL CMPLTPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPLTSS CMPLTSSB CMPLTSSW CMPLTSSL CMPLTSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNEQPS CMPNEQPSB CMPNEQPSW CMPNEQPSL CMPNEQPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNEQSS CMPNEQSSB CMPNEQSSW CMPNEQSSL CMPNEQSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNLEPS CMPNLEPSB CMPNLEPSW CMPNLEPSL CMPNLEPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNLESS CMPNLESSB CMPNLESSW CMPNLESSL CMPNLESSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNLTPS CMPNLTPSB CMPNLTPSW CMPNLTPSL CMPNLTPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPNLTSS CMPNLTSSB CMPNLTSSW CMPNLTSSL CMPNLTSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPORDPS CMPORDPSB CMPORDPSW CMPORDPSL CMPORDPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPORDSS CMPORDSSB CMPORDSSW CMPORDSSL CMPORDSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPUNORDPS CMPUNORDPSB CMPUNORDPSW CMPUNORDPSL CMPUNORDPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPUNORDSS CMPUNORDSSB CMPUNORDSSW CMPUNORDSSL CMPUNORDSSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPPS CMPPSB CMPPSW CMPPSL CMPPSQ
syntax keyword goasmOpcode_KATMAI_SSE CMPSS CMPSSB CMPSSW CMPSSL CMPSSQ
syntax keyword goasmOpcode_KATMAI_SSE COMISS COMISSB COMISSW COMISSL COMISSQ
syntax keyword goasmOpcode_KATMAI_SSE CVTPI2PS CVTPI2PSB CVTPI2PSW CVTPI2PSL CVTPI2PSQ
syntax keyword goasmOpcode_KATMAI_SSE CVTPS2PI CVTPS2PIB CVTPS2PIW CVTPS2PIL CVTPS2PIQ
syntax keyword goasmOpcode_X64_SSE  CVTSI2SS
syntax keyword goasmOpcode_X64_SSE  CVTSS2SI CVTSS2SIB CVTSS2SIW CVTSS2SIL CVTSS2SIQ
syntax keyword goasmOpcode_KATMAI_SSE CVTTPS2PI CVTTPS2PIB CVTTPS2PIW CVTTPS2PIL CVTTPS2PIQ
syntax keyword goasmOpcode_X64_SSE  CVTTSS2SI CVTTSS2SIB CVTTSS2SIW CVTTSS2SIL CVTTSS2SIQ
syntax keyword goasmOpcode_KATMAI_SSE DIVPS DIVPSB DIVPSW DIVPSL DIVPSQ
syntax keyword goasmOpcode_KATMAI_SSE DIVSS DIVSSB DIVSSW DIVSSL DIVSSQ
syntax keyword goasmOpcode_KATMAI_SSE LDMXCSR LDMXCSRB LDMXCSRW LDMXCSRL LDMXCSRQ
syntax keyword goasmOpcode_KATMAI_SSE MAXPS MAXPSB MAXPSW MAXPSL MAXPSQ
syntax keyword goasmOpcode_KATMAI_SSE MAXSS MAXSSB MAXSSW MAXSSL MAXSSQ
syntax keyword goasmOpcode_KATMAI_SSE MINPS MINPSB MINPSW MINPSL MINPSQ
syntax keyword goasmOpcode_KATMAI_SSE MINSS MINSSB MINSSW MINSSL MINSSQ
syntax keyword goasmOpcode_KATMAI_SSE MOVAPS
syntax keyword goasmOpcode_KATMAI_SSE MOVHPS MOVHPSB MOVHPSW MOVHPSL MOVHPSQ
syntax keyword goasmOpcode_KATMAI_SSE MOVLHPS
syntax keyword goasmOpcode_KATMAI_SSE MOVLPS MOVLPSB MOVLPSW MOVLPSL MOVLPSQ
syntax keyword goasmOpcode_KATMAI_SSE MOVHLPS
syntax keyword goasmOpcode_X64_SSE  MOVMSKPS
syntax keyword goasmOpcode_KATMAI_SSE MOVNTPS MOVNTPSB MOVNTPSW MOVNTPSL MOVNTPSQ
syntax keyword goasmOpcode_KATMAI_SSE MOVSS
syntax keyword goasmOpcode_KATMAI_SSE MOVUPS
syntax keyword goasmOpcode_KATMAI_SSE MULPS MULPSB MULPSW MULPSL MULPSQ
syntax keyword goasmOpcode_KATMAI_SSE MULSS MULSSB MULSSW MULSSL MULSSQ
syntax keyword goasmOpcode_KATMAI_SSE ORPS ORPSB ORPSW ORPSL ORPSQ
syntax keyword goasmOpcode_KATMAI_SSE RCPPS RCPPSB RCPPSW RCPPSL RCPPSQ
syntax keyword goasmOpcode_KATMAI_SSE RCPSS RCPSSB RCPSSW RCPSSL RCPSSQ
syntax keyword goasmOpcode_KATMAI_SSE RSQRTPS RSQRTPSB RSQRTPSW RSQRTPSL RSQRTPSQ
syntax keyword goasmOpcode_KATMAI_SSE RSQRTSS RSQRTSSB RSQRTSSW RSQRTSSL RSQRTSSQ
syntax keyword goasmOpcode_KATMAI_SSE SHUFPS SHUFPSB SHUFPSW SHUFPSL SHUFPSQ
syntax keyword goasmOpcode_KATMAI_SSE SQRTPS SQRTPSB SQRTPSW SQRTPSL SQRTPSQ
syntax keyword goasmOpcode_KATMAI_SSE SQRTSS SQRTSSB SQRTSSW SQRTSSL SQRTSSQ
syntax keyword goasmOpcode_KATMAI_SSE STMXCSR STMXCSRB STMXCSRW STMXCSRL STMXCSRQ
syntax keyword goasmOpcode_KATMAI_SSE SUBPS SUBPSB SUBPSW SUBPSL SUBPSQ
syntax keyword goasmOpcode_KATMAI_SSE SUBSS SUBSSB SUBSSW SUBSSL SUBSSQ
syntax keyword goasmOpcode_KATMAI_SSE UCOMISS UCOMISSB UCOMISSW UCOMISSL UCOMISSQ
syntax keyword goasmOpcode_KATMAI_SSE UNPCKHPS UNPCKHPSB UNPCKHPSW UNPCKHPSL UNPCKHPSQ
syntax keyword goasmOpcode_KATMAI_SSE UNPCKLPS UNPCKLPSB UNPCKLPSW UNPCKLPSL UNPCKLPSQ
syntax keyword goasmOpcode_KATMAI_SSE XORPS XORPSB XORPSW XORPSL XORPSQ

"-- Section: Extended Page Tables VMX instructions
syntax keyword goasmOpcode_VMX  INVEPT INVEPTB INVEPTW INVEPTL INVEPTQ
syntax keyword goasmOpcode_VMX  INVVPID INVVPIDB INVVPIDW INVVPIDL INVVPIDQ

"-- Section: VMX Instructions
syntax keyword goasmOpcode_VMX  VMCALL
syntax keyword goasmOpcode_VMX  VMCLEAR VMCLEARB VMCLEARW VMCLEARL VMCLEARQ
syntax keyword goasmOpcode_VMX  VMLAUNCH
syntax keyword goasmOpcode_X64_VMX  VMLOAD
syntax keyword goasmOpcode_X64_VMX  VMMCALL
syntax keyword goasmOpcode_VMX  VMPTRLD VMPTRLDB VMPTRLDW VMPTRLDL VMPTRLDQ
syntax keyword goasmOpcode_VMX  VMPTRST VMPTRSTB VMPTRSTW VMPTRSTL VMPTRSTQ
syntax keyword goasmOpcode_X64_VMX  VMREAD
syntax keyword goasmOpcode_VMX  VMRESUME
syntax keyword goasmOpcode_X64_VMX  VMRUN
syntax keyword goasmOpcode_X64_VMX  VMSAVE
syntax keyword goasmOpcode_X64_VMX  VMWRITE
syntax keyword goasmOpcode_VMX  VMXOFF
syntax keyword goasmOpcode_VMX  VMXON VMXONB VMXONW VMXONL VMXONQ

"-- Section: Intel AVX AES instructions
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESENC VAESENCB VAESENCW VAESENCL VAESENCQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESENCLAST VAESENCLASTB VAESENCLASTW VAESENCLASTL VAESENCLASTQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESDEC VAESDECB VAESDECW VAESDECL VAESDECQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESDECLAST VAESDECLASTB VAESDECLASTW VAESDECLASTL VAESDECLASTQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESIMC VAESIMCB VAESIMCW VAESIMCL VAESIMCQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VAESKEYGENASSIST VAESKEYGENASSISTB VAESKEYGENASSISTW VAESKEYGENASSISTL VAESKEYGENASSISTQ

"-- Section: New instructions in Barcelona
syntax keyword goasmOpcode_X64_Base  lzcnt

"-- Section: Intel AVX instructions
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDPD VADDPDB VADDPDW VADDPDL VADDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDPS VADDPSB VADDPSW VADDPSL VADDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDSD VADDSDB VADDSDW VADDSDL VADDSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDSS VADDSSB VADDSSW VADDSSL VADDSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDSUBPD VADDSUBPDB VADDSUBPDW VADDSUBPDL VADDSUBPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VADDSUBPS VADDSUBPSB VADDSUBPSW VADDSUBPSL VADDSUBPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VANDPD VANDPDB VANDPDW VANDPDL VANDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VANDPS VANDPSB VANDPSW VANDPSL VANDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VANDNPD VANDNPDB VANDNPDW VANDNPDL VANDNPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VANDNPS VANDNPSB VANDNPSW VANDNPSL VANDNPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBLENDPD VBLENDPDB VBLENDPDW VBLENDPDL VBLENDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBLENDPS VBLENDPSB VBLENDPSW VBLENDPSL VBLENDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBLENDVPD VBLENDVPDB VBLENDVPDW VBLENDVPDL VBLENDVPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBLENDVPS VBLENDVPSB VBLENDVPSW VBLENDVPSL VBLENDVPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBLENDVPD VBLENDVPDB VBLENDVPDW VBLENDVPDL VBLENDVPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBROADCASTSS VBROADCASTSSB VBROADCASTSSW VBROADCASTSSL VBROADCASTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBROADCASTSD VBROADCASTSDB VBROADCASTSDW VBROADCASTSDL VBROADCASTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VBROADCASTF128 VBROADCASTF128B VBROADCASTF128W VBROADCASTF128L VBROADCASTF128Q
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQPD VCMPEQPDB VCMPEQPDW VCMPEQPDL VCMPEQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLTPD VCMPLTPDB VCMPLTPDW VCMPLTPDL VCMPLTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLEPD VCMPLEPDB VCMPLEPDW VCMPLEPDL VCMPLEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORDPD VCMPUNORDPDB VCMPUNORDPDW VCMPUNORDPDL VCMPUNORDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQPD VCMPNEQPDB VCMPNEQPDW VCMPNEQPDL VCMPNEQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLTPD VCMPNLTPDB VCMPNLTPDW VCMPNLTPDL VCMPNLTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLEPD VCMPNLEPDB VCMPNLEPDW VCMPNLEPDL VCMPNLEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORDPD VCMPORDPDB VCMPORDPDW VCMPORDPDL VCMPORDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_UQPD VCMPEQ_UQPDB VCMPEQ_UQPDW VCMPEQ_UQPDL VCMPEQ_UQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGEPD VCMPNGEPDB VCMPNGEPDW VCMPNGEPDL VCMPNGEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGTPD VCMPNGTPDB VCMPNGTPDW VCMPNGTPDL VCMPNGTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSEPD VCMPFALSEPDB VCMPFALSEPDW VCMPFALSEPDL VCMPFALSEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OQPD VCMPNEQ_OQPDB VCMPNEQ_OQPDW VCMPNEQ_OQPDL VCMPNEQ_OQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGEPD VCMPGEPDB VCMPGEPDW VCMPGEPDL VCMPGEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGTPD VCMPGTPDB VCMPGTPDW VCMPGTPDL VCMPGTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUEPD VCMPTRUEPDB VCMPTRUEPDW VCMPTRUEPDL VCMPTRUEPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_OSPD VCMPEQ_OSPDB VCMPEQ_OSPDW VCMPEQ_OSPDL VCMPEQ_OSPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLT_OQPD VCMPLT_OQPDB VCMPLT_OQPDW VCMPLT_OQPDL VCMPLT_OQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLE_OQPD VCMPLE_OQPDB VCMPLE_OQPDW VCMPLE_OQPDL VCMPLE_OQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORD_SPD VCMPUNORD_SPDB VCMPUNORD_SPDW VCMPUNORD_SPDL VCMPUNORD_SPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_USPD VCMPNEQ_USPDB VCMPNEQ_USPDW VCMPNEQ_USPDL VCMPNEQ_USPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLT_UQPD VCMPNLT_UQPDB VCMPNLT_UQPDW VCMPNLT_UQPDL VCMPNLT_UQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLE_UQPD VCMPNLE_UQPDB VCMPNLE_UQPDW VCMPNLE_UQPDL VCMPNLE_UQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORD_SPD VCMPORD_SPDB VCMPORD_SPDW VCMPORD_SPDL VCMPORD_SPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_USPD VCMPEQ_USPDB VCMPEQ_USPDW VCMPEQ_USPDL VCMPEQ_USPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGE_UQPD VCMPNGE_UQPDB VCMPNGE_UQPDW VCMPNGE_UQPDL VCMPNGE_UQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGT_UQPD VCMPNGT_UQPDB VCMPNGT_UQPDW VCMPNGT_UQPDL VCMPNGT_UQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSE_OSPD VCMPFALSE_OSPDB VCMPFALSE_OSPDW VCMPFALSE_OSPDL VCMPFALSE_OSPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OSPD VCMPNEQ_OSPDB VCMPNEQ_OSPDW VCMPNEQ_OSPDL VCMPNEQ_OSPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGE_OQPD VCMPGE_OQPDB VCMPGE_OQPDW VCMPGE_OQPDL VCMPGE_OQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGT_OQPD VCMPGT_OQPDB VCMPGT_OQPDW VCMPGT_OQPDL VCMPGT_OQPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUE_USPD VCMPTRUE_USPDB VCMPTRUE_USPDW VCMPTRUE_USPDL VCMPTRUE_USPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPPD VCMPPDB VCMPPDW VCMPPDL VCMPPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQPS VCMPEQPSB VCMPEQPSW VCMPEQPSL VCMPEQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLTPS VCMPLTPSB VCMPLTPSW VCMPLTPSL VCMPLTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLEPS VCMPLEPSB VCMPLEPSW VCMPLEPSL VCMPLEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORDPS VCMPUNORDPSB VCMPUNORDPSW VCMPUNORDPSL VCMPUNORDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQPS VCMPNEQPSB VCMPNEQPSW VCMPNEQPSL VCMPNEQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLTPS VCMPNLTPSB VCMPNLTPSW VCMPNLTPSL VCMPNLTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLEPS VCMPNLEPSB VCMPNLEPSW VCMPNLEPSL VCMPNLEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORDPS VCMPORDPSB VCMPORDPSW VCMPORDPSL VCMPORDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_UQPS VCMPEQ_UQPSB VCMPEQ_UQPSW VCMPEQ_UQPSL VCMPEQ_UQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGEPS VCMPNGEPSB VCMPNGEPSW VCMPNGEPSL VCMPNGEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGTPS VCMPNGTPSB VCMPNGTPSW VCMPNGTPSL VCMPNGTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSEPS VCMPFALSEPSB VCMPFALSEPSW VCMPFALSEPSL VCMPFALSEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OQPS VCMPNEQ_OQPSB VCMPNEQ_OQPSW VCMPNEQ_OQPSL VCMPNEQ_OQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGEPS VCMPGEPSB VCMPGEPSW VCMPGEPSL VCMPGEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGTPS VCMPGTPSB VCMPGTPSW VCMPGTPSL VCMPGTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUEPS VCMPTRUEPSB VCMPTRUEPSW VCMPTRUEPSL VCMPTRUEPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_OSPS VCMPEQ_OSPSB VCMPEQ_OSPSW VCMPEQ_OSPSL VCMPEQ_OSPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLT_OQPS VCMPLT_OQPSB VCMPLT_OQPSW VCMPLT_OQPSL VCMPLT_OQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLE_OQPS VCMPLE_OQPSB VCMPLE_OQPSW VCMPLE_OQPSL VCMPLE_OQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORD_SPS VCMPUNORD_SPSB VCMPUNORD_SPSW VCMPUNORD_SPSL VCMPUNORD_SPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_USPS VCMPNEQ_USPSB VCMPNEQ_USPSW VCMPNEQ_USPSL VCMPNEQ_USPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLT_UQPS VCMPNLT_UQPSB VCMPNLT_UQPSW VCMPNLT_UQPSL VCMPNLT_UQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLE_UQPS VCMPNLE_UQPSB VCMPNLE_UQPSW VCMPNLE_UQPSL VCMPNLE_UQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORD_SPS VCMPORD_SPSB VCMPORD_SPSW VCMPORD_SPSL VCMPORD_SPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_USPS VCMPEQ_USPSB VCMPEQ_USPSW VCMPEQ_USPSL VCMPEQ_USPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGE_UQPS VCMPNGE_UQPSB VCMPNGE_UQPSW VCMPNGE_UQPSL VCMPNGE_UQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGT_UQPS VCMPNGT_UQPSB VCMPNGT_UQPSW VCMPNGT_UQPSL VCMPNGT_UQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSE_OSPS VCMPFALSE_OSPSB VCMPFALSE_OSPSW VCMPFALSE_OSPSL VCMPFALSE_OSPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OSPS VCMPNEQ_OSPSB VCMPNEQ_OSPSW VCMPNEQ_OSPSL VCMPNEQ_OSPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGE_OQPS VCMPGE_OQPSB VCMPGE_OQPSW VCMPGE_OQPSL VCMPGE_OQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGT_OQPS VCMPGT_OQPSB VCMPGT_OQPSW VCMPGT_OQPSL VCMPGT_OQPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUE_USPS VCMPTRUE_USPSB VCMPTRUE_USPSW VCMPTRUE_USPSL VCMPTRUE_USPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPPS VCMPPSB VCMPPSW VCMPPSL VCMPPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQSD VCMPEQSDB VCMPEQSDW VCMPEQSDL VCMPEQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLTSD VCMPLTSDB VCMPLTSDW VCMPLTSDL VCMPLTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLESD VCMPLESDB VCMPLESDW VCMPLESDL VCMPLESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORDSD VCMPUNORDSDB VCMPUNORDSDW VCMPUNORDSDL VCMPUNORDSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQSD VCMPNEQSDB VCMPNEQSDW VCMPNEQSDL VCMPNEQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLTSD VCMPNLTSDB VCMPNLTSDW VCMPNLTSDL VCMPNLTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLESD VCMPNLESDB VCMPNLESDW VCMPNLESDL VCMPNLESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORDSD VCMPORDSDB VCMPORDSDW VCMPORDSDL VCMPORDSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_UQSD VCMPEQ_UQSDB VCMPEQ_UQSDW VCMPEQ_UQSDL VCMPEQ_UQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGESD VCMPNGESDB VCMPNGESDW VCMPNGESDL VCMPNGESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGTSD VCMPNGTSDB VCMPNGTSDW VCMPNGTSDL VCMPNGTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSESD VCMPFALSESDB VCMPFALSESDW VCMPFALSESDL VCMPFALSESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OQSD VCMPNEQ_OQSDB VCMPNEQ_OQSDW VCMPNEQ_OQSDL VCMPNEQ_OQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGESD VCMPGESDB VCMPGESDW VCMPGESDL VCMPGESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGTSD VCMPGTSDB VCMPGTSDW VCMPGTSDL VCMPGTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUESD VCMPTRUESDB VCMPTRUESDW VCMPTRUESDL VCMPTRUESDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_OSSD VCMPEQ_OSSDB VCMPEQ_OSSDW VCMPEQ_OSSDL VCMPEQ_OSSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLT_OQSD VCMPLT_OQSDB VCMPLT_OQSDW VCMPLT_OQSDL VCMPLT_OQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLE_OQSD VCMPLE_OQSDB VCMPLE_OQSDW VCMPLE_OQSDL VCMPLE_OQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORD_SSD VCMPUNORD_SSDB VCMPUNORD_SSDW VCMPUNORD_SSDL VCMPUNORD_SSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_USSD VCMPNEQ_USSDB VCMPNEQ_USSDW VCMPNEQ_USSDL VCMPNEQ_USSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLT_UQSD VCMPNLT_UQSDB VCMPNLT_UQSDW VCMPNLT_UQSDL VCMPNLT_UQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLE_UQSD VCMPNLE_UQSDB VCMPNLE_UQSDW VCMPNLE_UQSDL VCMPNLE_UQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORD_SSD VCMPORD_SSDB VCMPORD_SSDW VCMPORD_SSDL VCMPORD_SSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_USSD VCMPEQ_USSDB VCMPEQ_USSDW VCMPEQ_USSDL VCMPEQ_USSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGE_UQSD VCMPNGE_UQSDB VCMPNGE_UQSDW VCMPNGE_UQSDL VCMPNGE_UQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGT_UQSD VCMPNGT_UQSDB VCMPNGT_UQSDW VCMPNGT_UQSDL VCMPNGT_UQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSE_OSSD VCMPFALSE_OSSDB VCMPFALSE_OSSDW VCMPFALSE_OSSDL VCMPFALSE_OSSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OSSD VCMPNEQ_OSSDB VCMPNEQ_OSSDW VCMPNEQ_OSSDL VCMPNEQ_OSSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGE_OQSD VCMPGE_OQSDB VCMPGE_OQSDW VCMPGE_OQSDL VCMPGE_OQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGT_OQSD VCMPGT_OQSDB VCMPGT_OQSDW VCMPGT_OQSDL VCMPGT_OQSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUE_USSD VCMPTRUE_USSDB VCMPTRUE_USSDW VCMPTRUE_USSDL VCMPTRUE_USSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPSD VCMPSDB VCMPSDW VCMPSDL VCMPSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQSS VCMPEQSSB VCMPEQSSW VCMPEQSSL VCMPEQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLTSS VCMPLTSSB VCMPLTSSW VCMPLTSSL VCMPLTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLESS VCMPLESSB VCMPLESSW VCMPLESSL VCMPLESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORDSS VCMPUNORDSSB VCMPUNORDSSW VCMPUNORDSSL VCMPUNORDSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQSS VCMPNEQSSB VCMPNEQSSW VCMPNEQSSL VCMPNEQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLTSS VCMPNLTSSB VCMPNLTSSW VCMPNLTSSL VCMPNLTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLESS VCMPNLESSB VCMPNLESSW VCMPNLESSL VCMPNLESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORDSS VCMPORDSSB VCMPORDSSW VCMPORDSSL VCMPORDSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_UQSS VCMPEQ_UQSSB VCMPEQ_UQSSW VCMPEQ_UQSSL VCMPEQ_UQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGESS VCMPNGESSB VCMPNGESSW VCMPNGESSL VCMPNGESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGTSS VCMPNGTSSB VCMPNGTSSW VCMPNGTSSL VCMPNGTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSESS VCMPFALSESSB VCMPFALSESSW VCMPFALSESSL VCMPFALSESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OQSS VCMPNEQ_OQSSB VCMPNEQ_OQSSW VCMPNEQ_OQSSL VCMPNEQ_OQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGESS VCMPGESSB VCMPGESSW VCMPGESSL VCMPGESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGTSS VCMPGTSSB VCMPGTSSW VCMPGTSSL VCMPGTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUESS VCMPTRUESSB VCMPTRUESSW VCMPTRUESSL VCMPTRUESSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_OSSS VCMPEQ_OSSSB VCMPEQ_OSSSW VCMPEQ_OSSSL VCMPEQ_OSSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLT_OQSS VCMPLT_OQSSB VCMPLT_OQSSW VCMPLT_OQSSL VCMPLT_OQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPLE_OQSS VCMPLE_OQSSB VCMPLE_OQSSW VCMPLE_OQSSL VCMPLE_OQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPUNORD_SSS VCMPUNORD_SSSB VCMPUNORD_SSSW VCMPUNORD_SSSL VCMPUNORD_SSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_USSS VCMPNEQ_USSSB VCMPNEQ_USSSW VCMPNEQ_USSSL VCMPNEQ_USSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLT_UQSS VCMPNLT_UQSSB VCMPNLT_UQSSW VCMPNLT_UQSSL VCMPNLT_UQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNLE_UQSS VCMPNLE_UQSSB VCMPNLE_UQSSW VCMPNLE_UQSSL VCMPNLE_UQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPORD_SSS VCMPORD_SSSB VCMPORD_SSSW VCMPORD_SSSL VCMPORD_SSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPEQ_USSS VCMPEQ_USSSB VCMPEQ_USSSW VCMPEQ_USSSL VCMPEQ_USSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGE_UQSS VCMPNGE_UQSSB VCMPNGE_UQSSW VCMPNGE_UQSSL VCMPNGE_UQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNGT_UQSS VCMPNGT_UQSSB VCMPNGT_UQSSW VCMPNGT_UQSSL VCMPNGT_UQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPFALSE_OSSS VCMPFALSE_OSSSB VCMPFALSE_OSSSW VCMPFALSE_OSSSL VCMPFALSE_OSSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPNEQ_OSSS VCMPNEQ_OSSSB VCMPNEQ_OSSSW VCMPNEQ_OSSSL VCMPNEQ_OSSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGE_OQSS VCMPGE_OQSSB VCMPGE_OQSSW VCMPGE_OQSSL VCMPGE_OQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPGT_OQSS VCMPGT_OQSSB VCMPGT_OQSSW VCMPGT_OQSSL VCMPGT_OQSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPTRUE_USSS VCMPTRUE_USSSB VCMPTRUE_USSSW VCMPTRUE_USSSL VCMPTRUE_USSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCMPSS VCMPSSB VCMPSSW VCMPSSL VCMPSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCOMISD VCOMISDB VCOMISDW VCOMISDL VCOMISDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCOMISS VCOMISSB VCOMISSW VCOMISSL VCOMISSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTDQ2PD VCVTDQ2PDB VCVTDQ2PDW VCVTDQ2PDL VCVTDQ2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTDQ2PS VCVTDQ2PSB VCVTDQ2PSW VCVTDQ2PSL VCVTDQ2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTPD2DQ VCVTPD2DQB VCVTPD2DQW VCVTPD2DQL VCVTPD2DQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTPD2PS VCVTPD2PSB VCVTPD2PSW VCVTPD2PSL VCVTPD2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTPS2DQ VCVTPS2DQB VCVTPS2DQW VCVTPS2DQL VCVTPS2DQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTPS2PD VCVTPS2PDB VCVTPS2PDW VCVTPS2PDL VCVTPS2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSD2SI VCVTSD2SIB VCVTSD2SIW VCVTSD2SIL VCVTSD2SIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSD2SS VCVTSD2SSB VCVTSD2SSW VCVTSD2SSL VCVTSD2SSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSI2SD
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSI2SS
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSS2SD VCVTSS2SDB VCVTSS2SDW VCVTSS2SDL VCVTSS2SDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTSS2SI VCVTSS2SIB VCVTSS2SIW VCVTSS2SIL VCVTSS2SIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTTPD2DQ VCVTTPD2DQB VCVTTPD2DQW VCVTTPD2DQL VCVTTPD2DQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTTPS2DQ VCVTTPS2DQB VCVTTPS2DQW VCVTTPS2DQL VCVTTPS2DQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTTSD2SI VCVTTSD2SIB VCVTTSD2SIW VCVTTSD2SIL VCVTTSD2SIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VCVTTSS2SI VCVTTSS2SIB VCVTTSS2SIW VCVTTSS2SIL VCVTTSS2SIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDIVPD VDIVPDB VDIVPDW VDIVPDL VDIVPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDIVPS VDIVPSB VDIVPSW VDIVPSL VDIVPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDIVSD VDIVSDB VDIVSDW VDIVSDL VDIVSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDIVSS VDIVSSB VDIVSSW VDIVSSL VDIVSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDPPD VDPPDB VDPPDW VDPPDL VDPPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VDPPS VDPPSB VDPPSW VDPPSL VDPPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VEXTRACTF128 VEXTRACTF128B VEXTRACTF128W VEXTRACTF128L VEXTRACTF128Q
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VEXTRACTPS VEXTRACTPSB VEXTRACTPSW VEXTRACTPSL VEXTRACTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VHADDPD VHADDPDB VHADDPDW VHADDPDL VHADDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VHADDPS VHADDPSB VHADDPSW VHADDPSL VHADDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VHSUBPD VHSUBPDB VHSUBPDW VHSUBPDL VHSUBPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VHSUBPS VHSUBPSB VHSUBPSW VHSUBPSL VHSUBPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VINSERTF128 VINSERTF128B VINSERTF128W VINSERTF128L VINSERTF128Q
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VINSERTPS VINSERTPSB VINSERTPSW VINSERTPSL VINSERTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VLDDQU VLDDQUB VLDDQUW VLDDQUL VLDDQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VLDQQU VLDQQUB VLDQQUW VLDQQUL VLDQQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VLDDQU VLDDQUB VLDDQUW VLDDQUL VLDDQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VLDMXCSR VLDMXCSRB VLDMXCSRW VLDMXCSRL VLDMXCSRQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMASKMOVDQU
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMASKMOVPS VMASKMOVPSB VMASKMOVPSW VMASKMOVPSL VMASKMOVPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMASKMOVPD VMASKMOVPDB VMASKMOVPDW VMASKMOVPDL VMASKMOVPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMAXPD VMAXPDB VMAXPDW VMAXPDL VMAXPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMAXPS VMAXPSB VMAXPSW VMAXPSL VMAXPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMAXSD VMAXSDB VMAXSDW VMAXSDL VMAXSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMAXSS VMAXSSB VMAXSSW VMAXSSL VMAXSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMINPD VMINPDB VMINPDW VMINPDL VMINPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMINPS VMINPSB VMINPSW VMINPSL VMINPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMINSD VMINSDB VMINSDW VMINSDL VMINSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMINSS VMINSSB VMINSSW VMINSSL VMINSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVAPD VMOVAPDB VMOVAPDW VMOVAPDL VMOVAPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVAPS VMOVAPSB VMOVAPSW VMOVAPSL VMOVAPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVQ VMOVQB VMOVQW VMOVQL VMOVQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVD
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVD
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVDDUP VMOVDDUPB VMOVDDUPW VMOVDDUPL VMOVDDUPQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVDQA VMOVDQAB VMOVDQAW VMOVDQAL VMOVDQAQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVQQA VMOVQQAB VMOVQQAW VMOVQQAL VMOVQQAQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVDQA VMOVDQAB VMOVDQAW VMOVDQAL VMOVDQAQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVDQU VMOVDQUB VMOVDQUW VMOVDQUL VMOVDQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVQQU VMOVQQUB VMOVQQUW VMOVQQUL VMOVQQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVDQU VMOVDQUB VMOVDQUW VMOVDQUL VMOVDQUQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVHLPS
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVHPD VMOVHPDB VMOVHPDW VMOVHPDL VMOVHPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVHPS VMOVHPSB VMOVHPSW VMOVHPSL VMOVHPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVLHPS
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVLPD VMOVLPDB VMOVLPDW VMOVLPDL VMOVLPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVLPS VMOVLPSB VMOVLPSW VMOVLPSL VMOVLPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVMSKPD VMOVMSKPDB VMOVMSKPDW VMOVMSKPDL VMOVMSKPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVMSKPS VMOVMSKPSB VMOVMSKPSW VMOVMSKPSL VMOVMSKPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTDQ VMOVNTDQB VMOVNTDQW VMOVNTDQL VMOVNTDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTQQ VMOVNTQQB VMOVNTQQW VMOVNTQQL VMOVNTQQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTDQ VMOVNTDQB VMOVNTDQW VMOVNTDQL VMOVNTDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTDQA VMOVNTDQAB VMOVNTDQAW VMOVNTDQAL VMOVNTDQAQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTPD VMOVNTPDB VMOVNTPDW VMOVNTPDL VMOVNTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVNTPS VMOVNTPSB VMOVNTPSW VMOVNTPSL VMOVNTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVSD VMOVSDB VMOVSDW VMOVSDL VMOVSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVSHDUP VMOVSHDUPB VMOVSHDUPW VMOVSHDUPL VMOVSHDUPQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVSLDUP VMOVSLDUPB VMOVSLDUPW VMOVSLDUPL VMOVSLDUPQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVSS VMOVSSB VMOVSSW VMOVSSL VMOVSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVUPD VMOVUPDB VMOVUPDW VMOVUPDL VMOVUPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMOVUPS VMOVUPSB VMOVUPSW VMOVUPSL VMOVUPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMPSADBW VMPSADBWB VMPSADBWW VMPSADBWL VMPSADBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMULPD VMULPDB VMULPDW VMULPDL VMULPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMULPS VMULPSB VMULPSW VMULPSL VMULPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMULSD VMULSDB VMULSDW VMULSDL VMULSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VMULSS VMULSSB VMULSSW VMULSSL VMULSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VORPD VORPDB VORPDW VORPDL VORPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VORPS VORPSB VORPSW VORPSL VORPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPABSB VPABSBB VPABSBW VPABSBL VPABSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPABSW VPABSWB VPABSWW VPABSWL VPABSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPABSD VPABSDB VPABSDW VPABSDL VPABSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPACKSSWB VPACKSSWBB VPACKSSWBW VPACKSSWBL VPACKSSWBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPACKSSDW VPACKSSDWB VPACKSSDWW VPACKSSDWL VPACKSSDWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPACKUSWB VPACKUSWBB VPACKUSWBW VPACKUSWBL VPACKUSWBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPACKUSDW VPACKUSDWB VPACKUSDWW VPACKUSDWL VPACKUSDWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDB VPADDBB VPADDBW VPADDBL VPADDBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDW VPADDWB VPADDWW VPADDWL VPADDWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDD VPADDDB VPADDDW VPADDDL VPADDDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDQ VPADDQB VPADDQW VPADDQL VPADDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDSB VPADDSBB VPADDSBW VPADDSBL VPADDSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDSW VPADDSWB VPADDSWW VPADDSWL VPADDSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDUSB VPADDUSBB VPADDUSBW VPADDUSBL VPADDUSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPADDUSW VPADDUSWB VPADDUSWW VPADDUSWL VPADDUSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPALIGNR VPALIGNRB VPALIGNRW VPALIGNRL VPALIGNRQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPAND VPANDB VPANDW VPANDL VPANDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPANDN VPANDNB VPANDNW VPANDNL VPANDNQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPAVGB VPAVGBB VPAVGBW VPAVGBL VPAVGBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPAVGW VPAVGWB VPAVGWW VPAVGWL VPAVGWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPBLENDVB VPBLENDVBB VPBLENDVBW VPBLENDVBL VPBLENDVBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPBLENDW VPBLENDWB VPBLENDWW VPBLENDWL VPBLENDWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPESTRI VPCMPESTRIB VPCMPESTRIW VPCMPESTRIL VPCMPESTRIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPESTRM VPCMPESTRMB VPCMPESTRMW VPCMPESTRML VPCMPESTRMQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPISTRI VPCMPISTRIB VPCMPISTRIW VPCMPISTRIL VPCMPISTRIQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPISTRM VPCMPISTRMB VPCMPISTRMW VPCMPISTRML VPCMPISTRMQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPEQB VPCMPEQBB VPCMPEQBW VPCMPEQBL VPCMPEQBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPEQW VPCMPEQWB VPCMPEQWW VPCMPEQWL VPCMPEQWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPEQD VPCMPEQDB VPCMPEQDW VPCMPEQDL VPCMPEQDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPEQQ VPCMPEQQB VPCMPEQQW VPCMPEQQL VPCMPEQQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPGTB VPCMPGTBB VPCMPGTBW VPCMPGTBL VPCMPGTBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPGTW VPCMPGTWB VPCMPGTWW VPCMPGTWL VPCMPGTWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPGTD VPCMPGTDB VPCMPGTDW VPCMPGTDL VPCMPGTDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPCMPGTQ VPCMPGTQB VPCMPGTQW VPCMPGTQL VPCMPGTQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILPD VPERMILPDB VPERMILPDW VPERMILPDL VPERMILPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILTD2PD VPERMILTD2PDB VPERMILTD2PDW VPERMILTD2PDL VPERMILTD2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILMO2PD VPERMILMO2PDB VPERMILMO2PDW VPERMILMO2PDL VPERMILMO2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILMZ2PD VPERMILMZ2PDB VPERMILMZ2PDW VPERMILMZ2PDL VPERMILMZ2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMIL2PD VPERMIL2PDB VPERMIL2PDW VPERMIL2PDL VPERMIL2PDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILPS VPERMILPSB VPERMILPSW VPERMILPSL VPERMILPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILTD2PS VPERMILTD2PSB VPERMILTD2PSW VPERMILTD2PSL VPERMILTD2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILMO2PS VPERMILMO2PSB VPERMILMO2PSW VPERMILMO2PSL VPERMILMO2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMILMZ2PS VPERMILMZ2PSB VPERMILMZ2PSW VPERMILMZ2PSL VPERMILMZ2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERMIL2PS VPERMIL2PSB VPERMIL2PSW VPERMIL2PSL VPERMIL2PSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPERM2F128 VPERM2F128B VPERM2F128W VPERM2F128L VPERM2F128Q
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPEXTRB VPEXTRBB VPEXTRBW VPEXTRBL VPEXTRBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPEXTRW VPEXTRWB VPEXTRWW VPEXTRWL VPEXTRWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPEXTRD VPEXTRDB VPEXTRDW VPEXTRDL VPEXTRDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPEXTRQ VPEXTRQB VPEXTRQW VPEXTRQL VPEXTRQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHADDW VPHADDWB VPHADDWW VPHADDWL VPHADDWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHADDD VPHADDDB VPHADDDW VPHADDDL VPHADDDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHADDSW VPHADDSWB VPHADDSWW VPHADDSWL VPHADDSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHMINPOSUW VPHMINPOSUWB VPHMINPOSUWW VPHMINPOSUWL VPHMINPOSUWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHSUBW VPHSUBWB VPHSUBWW VPHSUBWL VPHSUBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHSUBD VPHSUBDB VPHSUBDW VPHSUBDL VPHSUBDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPHSUBSW VPHSUBSWB VPHSUBSWW VPHSUBSWL VPHSUBSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPINSRB VPINSRBB VPINSRBW VPINSRBL VPINSRBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPINSRW VPINSRWB VPINSRWW VPINSRWL VPINSRWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPINSRD VPINSRDB VPINSRDW VPINSRDL VPINSRDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPINSRQ VPINSRQB VPINSRQW VPINSRQL VPINSRQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMADDWD VPMADDWDB VPMADDWDW VPMADDWDL VPMADDWDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMADDUBSW VPMADDUBSWB VPMADDUBSWW VPMADDUBSWL VPMADDUBSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXSB VPMAXSBB VPMAXSBW VPMAXSBL VPMAXSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXSW VPMAXSWB VPMAXSWW VPMAXSWL VPMAXSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXSD VPMAXSDB VPMAXSDW VPMAXSDL VPMAXSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXUB VPMAXUBB VPMAXUBW VPMAXUBL VPMAXUBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXUW VPMAXUWB VPMAXUWW VPMAXUWL VPMAXUWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMAXUD VPMAXUDB VPMAXUDW VPMAXUDL VPMAXUDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINSB VPMINSBB VPMINSBW VPMINSBL VPMINSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINSW VPMINSWB VPMINSWW VPMINSWL VPMINSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINSD VPMINSDB VPMINSDW VPMINSDL VPMINSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINUB VPMINUBB VPMINUBW VPMINUBL VPMINUBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINUW VPMINUWB VPMINUWW VPMINUWL VPMINUWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMINUD VPMINUDB VPMINUDW VPMINUDL VPMINUDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVMSKB
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXBW VPMOVSXBWB VPMOVSXBWW VPMOVSXBWL VPMOVSXBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXBD VPMOVSXBDB VPMOVSXBDW VPMOVSXBDL VPMOVSXBDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXBQ VPMOVSXBQB VPMOVSXBQW VPMOVSXBQL VPMOVSXBQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXWD VPMOVSXWDB VPMOVSXWDW VPMOVSXWDL VPMOVSXWDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXWQ VPMOVSXWQB VPMOVSXWQW VPMOVSXWQL VPMOVSXWQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVSXDQ VPMOVSXDQB VPMOVSXDQW VPMOVSXDQL VPMOVSXDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXBW VPMOVZXBWB VPMOVZXBWW VPMOVZXBWL VPMOVZXBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXBD VPMOVZXBDB VPMOVZXBDW VPMOVZXBDL VPMOVZXBDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXBQ VPMOVZXBQB VPMOVZXBQW VPMOVZXBQL VPMOVZXBQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXWD VPMOVZXWDB VPMOVZXWDW VPMOVZXWDL VPMOVZXWDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXWQ VPMOVZXWQB VPMOVZXWQW VPMOVZXWQL VPMOVZXWQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMOVZXDQ VPMOVZXDQB VPMOVZXDQW VPMOVZXDQL VPMOVZXDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULHUW VPMULHUWB VPMULHUWW VPMULHUWL VPMULHUWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULHRSW VPMULHRSWB VPMULHRSWW VPMULHRSWL VPMULHRSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULHW VPMULHWB VPMULHWW VPMULHWL VPMULHWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULLW VPMULLWB VPMULLWW VPMULLWL VPMULLWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULLD VPMULLDB VPMULLDW VPMULLDL VPMULLDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULUDQ VPMULUDQB VPMULUDQW VPMULUDQL VPMULUDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPMULDQ VPMULDQB VPMULDQW VPMULDQL VPMULDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPOR VPORB VPORW VPORL VPORQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSADBW VPSADBWB VPSADBWW VPSADBWL VPSADBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSHUFB VPSHUFBB VPSHUFBW VPSHUFBL VPSHUFBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSHUFD VPSHUFDB VPSHUFDW VPSHUFDL VPSHUFDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSHUFHW VPSHUFHWB VPSHUFHWW VPSHUFHWL VPSHUFHWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSHUFLW VPSHUFLWB VPSHUFLWW VPSHUFLWL VPSHUFLWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSIGNB VPSIGNBB VPSIGNBW VPSIGNBL VPSIGNBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSIGNW VPSIGNWB VPSIGNWW VPSIGNWL VPSIGNWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSIGND VPSIGNDB VPSIGNDW VPSIGNDL VPSIGNDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSLLDQ VPSLLDQB VPSLLDQW VPSLLDQL VPSLLDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRLDQ VPSRLDQB VPSRLDQW VPSRLDQL VPSRLDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSLLW VPSLLWB VPSLLWW VPSLLWL VPSLLWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSLLD VPSLLDB VPSLLDW VPSLLDL VPSLLDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSLLQ VPSLLQB VPSLLQW VPSLLQL VPSLLQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRAW VPSRAWB VPSRAWW VPSRAWL VPSRAWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRAD VPSRADB VPSRADW VPSRADL VPSRADQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRLW VPSRLWB VPSRLWW VPSRLWL VPSRLWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRLD VPSRLDB VPSRLDW VPSRLDL VPSRLDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSRLQ VPSRLQB VPSRLQW VPSRLQL VPSRLQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPTEST VPTESTB VPTESTW VPTESTL VPTESTQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBB VPSUBBB VPSUBBW VPSUBBL VPSUBBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBW VPSUBWB VPSUBWW VPSUBWL VPSUBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBD VPSUBDB VPSUBDW VPSUBDL VPSUBDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBQ VPSUBQB VPSUBQW VPSUBQL VPSUBQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBSB VPSUBSBB VPSUBSBW VPSUBSBL VPSUBSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBSW VPSUBSWB VPSUBSWW VPSUBSWL VPSUBSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBUSB VPSUBUSBB VPSUBUSBW VPSUBUSBL VPSUBUSBQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPSUBUSW VPSUBUSWB VPSUBUSWW VPSUBUSWL VPSUBUSWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKHBW VPUNPCKHBWB VPUNPCKHBWW VPUNPCKHBWL VPUNPCKHBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKHWD VPUNPCKHWDB VPUNPCKHWDW VPUNPCKHWDL VPUNPCKHWDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKHDQ VPUNPCKHDQB VPUNPCKHDQW VPUNPCKHDQL VPUNPCKHDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKHQDQ VPUNPCKHQDQB VPUNPCKHQDQW VPUNPCKHQDQL VPUNPCKHQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKLBW VPUNPCKLBWB VPUNPCKLBWW VPUNPCKLBWL VPUNPCKLBWQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKLWD VPUNPCKLWDB VPUNPCKLWDW VPUNPCKLWDL VPUNPCKLWDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKLDQ VPUNPCKLDQB VPUNPCKLDQW VPUNPCKLDQL VPUNPCKLDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPUNPCKLQDQ VPUNPCKLQDQB VPUNPCKLQDQW VPUNPCKLQDQL VPUNPCKLQDQQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VPXOR VPXORB VPXORW VPXORL VPXORQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VRCPPS VRCPPSB VRCPPSW VRCPPSL VRCPPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VRCPSS VRCPSSB VRCPSSW VRCPSSL VRCPSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VRSQRTPS VRSQRTPSB VRSQRTPSW VRSQRTPSL VRSQRTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VRSQRTSS VRSQRTSSB VRSQRTSSW VRSQRTSSL VRSQRTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VROUNDPD VROUNDPDB VROUNDPDW VROUNDPDL VROUNDPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VROUNDPS VROUNDPSB VROUNDPSW VROUNDPSL VROUNDPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VROUNDSD VROUNDSDB VROUNDSDW VROUNDSDL VROUNDSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VROUNDSS VROUNDSSB VROUNDSSW VROUNDSSL VROUNDSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSHUFPD VSHUFPDB VSHUFPDW VSHUFPDL VSHUFPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSHUFPS VSHUFPSB VSHUFPSW VSHUFPSL VSHUFPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSQRTPD VSQRTPDB VSQRTPDW VSQRTPDL VSQRTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSQRTPS VSQRTPSB VSQRTPSW VSQRTPSL VSQRTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSQRTSD VSQRTSDB VSQRTSDW VSQRTSDL VSQRTSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSQRTSS VSQRTSSB VSQRTSSW VSQRTSSL VSQRTSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSTMXCSR VSTMXCSRB VSTMXCSRW VSTMXCSRL VSTMXCSRQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSUBPD VSUBPDB VSUBPDW VSUBPDL VSUBPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSUBPS VSUBPSB VSUBPSW VSUBPSL VSUBPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSUBSD VSUBSDB VSUBSDW VSUBSDL VSUBSDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VSUBSS VSUBSSB VSUBSSW VSUBSSL VSUBSSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VTESTPS VTESTPSB VTESTPSW VTESTPSL VTESTPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VTESTPD VTESTPDB VTESTPDW VTESTPDL VTESTPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUCOMISD VUCOMISDB VUCOMISDW VUCOMISDL VUCOMISDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUCOMISS VUCOMISSB VUCOMISSW VUCOMISSL VUCOMISSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUNPCKHPD VUNPCKHPDB VUNPCKHPDW VUNPCKHPDL VUNPCKHPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUNPCKHPS VUNPCKHPSB VUNPCKHPSW VUNPCKHPSL VUNPCKHPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUNPCKLPD VUNPCKLPDB VUNPCKLPDW VUNPCKLPDL VUNPCKLPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VUNPCKLPS VUNPCKLPSB VUNPCKLPSW VUNPCKLPSL VUNPCKLPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VXORPD VXORPDB VXORPDW VXORPDL VXORPDQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VXORPS VXORPSB VXORPSW VXORPSL VXORPSQ
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VZEROALL
syntax keyword goasmOpcode_SANDYBRIDGE_AVX VZEROUPPER

"-- Section: AMD Enhanced 3DNow! (Athlon) instructions
syntax keyword goasmOpcode_PENT_3DNOW PF2IW PF2IWB PF2IWW PF2IWL PF2IWQ
syntax keyword goasmOpcode_PENT_3DNOW PFNACC PFNACCB PFNACCW PFNACCL PFNACCQ
syntax keyword goasmOpcode_PENT_3DNOW PFPNACC PFPNACCB PFPNACCW PFPNACCL PFPNACCQ
syntax keyword goasmOpcode_PENT_3DNOW PI2FW PI2FWB PI2FWW PI2FWL PI2FWQ
syntax keyword goasmOpcode_PENT_3DNOW PSWAPD PSWAPDB PSWAPDW PSWAPDL PSWAPDQ

"-- Section: Penryn New Instructions (SSE4.1)
syntax keyword goasmOpcode_SSE41  BLENDPD BLENDPDB BLENDPDW BLENDPDL BLENDPDQ
syntax keyword goasmOpcode_SSE41  BLENDPS BLENDPSB BLENDPSW BLENDPSL BLENDPSQ
syntax keyword goasmOpcode_SSE41  BLENDVPD BLENDVPDB BLENDVPDW BLENDVPDL BLENDVPDQ
syntax keyword goasmOpcode_SSE41  BLENDVPS BLENDVPSB BLENDVPSW BLENDVPSL BLENDVPSQ
syntax keyword goasmOpcode_SSE41  DPPD DPPDB DPPDW DPPDL DPPDQ
syntax keyword goasmOpcode_SSE41  DPPS DPPSB DPPSW DPPSL DPPSQ
syntax keyword goasmOpcode_X64_SSE41  EXTRACTPS EXTRACTPSB EXTRACTPSW EXTRACTPSL EXTRACTPSQ
syntax keyword goasmOpcode_SSE41  INSERTPS INSERTPSB INSERTPSW INSERTPSL INSERTPSQ
syntax keyword goasmOpcode_SSE41  MOVNTDQA MOVNTDQAB MOVNTDQAW MOVNTDQAL MOVNTDQAQ
syntax keyword goasmOpcode_SSE41  MPSADBW MPSADBWB MPSADBWW MPSADBWL MPSADBWQ
syntax keyword goasmOpcode_SSE41  PACKUSDW PACKUSDWB PACKUSDWW PACKUSDWL PACKUSDWQ
syntax keyword goasmOpcode_SSE41  PBLENDVB PBLENDVBB PBLENDVBW PBLENDVBL PBLENDVBQ
syntax keyword goasmOpcode_SSE41  PBLENDW PBLENDWB PBLENDWW PBLENDWL PBLENDWQ
syntax keyword goasmOpcode_SSE41  PCMPEQQ PCMPEQQB PCMPEQQW PCMPEQQL PCMPEQQQ
syntax keyword goasmOpcode_X64_SSE41  PEXTRB PEXTRBB PEXTRBW PEXTRBL PEXTRBQ
syntax keyword goasmOpcode_SSE41  PEXTRD PEXTRDB PEXTRDW PEXTRDL PEXTRDQ
syntax keyword goasmOpcode_X64_SSE41  PEXTRQ PEXTRQB PEXTRQW PEXTRQL PEXTRQQ
syntax keyword goasmOpcode_X64_SSE41  PEXTRW PEXTRWB PEXTRWW PEXTRWL PEXTRWQ
syntax keyword goasmOpcode_SSE41  PHMINPOSUW PHMINPOSUWB PHMINPOSUWW PHMINPOSUWL PHMINPOSUWQ
syntax keyword goasmOpcode_SSE41  PINSRB PINSRBB PINSRBW PINSRBL PINSRBQ
syntax keyword goasmOpcode_SSE41  PINSRD PINSRDB PINSRDW PINSRDL PINSRDQ
syntax keyword goasmOpcode_X64_SSE41  PINSRQ PINSRQB PINSRQW PINSRQL PINSRQQ
syntax keyword goasmOpcode_SSE41  PMAXSB PMAXSBB PMAXSBW PMAXSBL PMAXSBQ
syntax keyword goasmOpcode_SSE41  PMAXSD PMAXSDB PMAXSDW PMAXSDL PMAXSDQ
syntax keyword goasmOpcode_SSE41  PMAXUD PMAXUDB PMAXUDW PMAXUDL PMAXUDQ
syntax keyword goasmOpcode_SSE41  PMAXUW PMAXUWB PMAXUWW PMAXUWL PMAXUWQ
syntax keyword goasmOpcode_SSE41  PMINSB PMINSBB PMINSBW PMINSBL PMINSBQ
syntax keyword goasmOpcode_SSE41  PMINSD PMINSDB PMINSDW PMINSDL PMINSDQ
syntax keyword goasmOpcode_SSE41  PMINUD PMINUDB PMINUDW PMINUDL PMINUDQ
syntax keyword goasmOpcode_SSE41  PMINUW PMINUWB PMINUWW PMINUWL PMINUWQ
syntax keyword goasmOpcode_SSE41  PMOVSXBW PMOVSXBWB PMOVSXBWW PMOVSXBWL PMOVSXBWQ
syntax keyword goasmOpcode_SSE41  PMOVSXBD PMOVSXBDB PMOVSXBDW PMOVSXBDL PMOVSXBDQ
syntax keyword goasmOpcode_SSE41  PMOVSXBQ PMOVSXBQB PMOVSXBQW PMOVSXBQL PMOVSXBQQ
syntax keyword goasmOpcode_SSE41  PMOVSXWD PMOVSXWDB PMOVSXWDW PMOVSXWDL PMOVSXWDQ
syntax keyword goasmOpcode_SSE41  PMOVSXWQ PMOVSXWQB PMOVSXWQW PMOVSXWQL PMOVSXWQQ
syntax keyword goasmOpcode_SSE41  PMOVSXDQ PMOVSXDQB PMOVSXDQW PMOVSXDQL PMOVSXDQQ
syntax keyword goasmOpcode_SSE41  PMOVZXBW PMOVZXBWB PMOVZXBWW PMOVZXBWL PMOVZXBWQ
syntax keyword goasmOpcode_SSE41  PMOVZXBD PMOVZXBDB PMOVZXBDW PMOVZXBDL PMOVZXBDQ
syntax keyword goasmOpcode_SSE41  PMOVZXBQ PMOVZXBQB PMOVZXBQW PMOVZXBQL PMOVZXBQQ
syntax keyword goasmOpcode_SSE41  PMOVZXWD PMOVZXWDB PMOVZXWDW PMOVZXWDL PMOVZXWDQ
syntax keyword goasmOpcode_SSE41  PMOVZXWQ PMOVZXWQB PMOVZXWQW PMOVZXWQL PMOVZXWQQ
syntax keyword goasmOpcode_SSE41  PMOVZXDQ PMOVZXDQB PMOVZXDQW PMOVZXDQL PMOVZXDQQ
syntax keyword goasmOpcode_SSE41  PMULDQ PMULDQB PMULDQW PMULDQL PMULDQQ
syntax keyword goasmOpcode_SSE41  PMULLD PMULLDB PMULLDW PMULLDL PMULLDQ
syntax keyword goasmOpcode_SSE41  PTEST PTESTB PTESTW PTESTL PTESTQ
syntax keyword goasmOpcode_SSE41  ROUNDPD ROUNDPDB ROUNDPDW ROUNDPDL ROUNDPDQ
syntax keyword goasmOpcode_SSE41  ROUNDPS ROUNDPSB ROUNDPSW ROUNDPSL ROUNDPSQ
syntax keyword goasmOpcode_SSE41  ROUNDSD ROUNDSDB ROUNDSDW ROUNDSDL ROUNDSDQ
syntax keyword goasmOpcode_SSE41  ROUNDSS ROUNDSSB ROUNDSSW ROUNDSSL ROUNDSSQ

"-- Section: AMD SSE4A
syntax keyword goasmOpcode_AMD_SSE4A  EXTRQ
syntax keyword goasmOpcode_AMD_SSE4A  INSERTQ
syntax keyword goasmOpcode_AMD_SSE4A  MOVNTSD MOVNTSDB MOVNTSDW MOVNTSDL MOVNTSDQ
syntax keyword goasmOpcode_AMD_SSE4A  MOVNTSS MOVNTSSB MOVNTSSW MOVNTSSL MOVNTSSQ

"-- Section: ARM Thumb
syntax keyword goasmOpcode_ARM_THUMB         ADC ADCEQ ADCNE ADCCS ADCHS ADCCC ADCLO
syntax keyword goasmOpcode_ARM_THUMB         ADCMI ADCPL ADCVS ADCVC ADCHI ADCLS
syntax keyword goasmOpcode_ARM_THUMB         ADCGE ADCLT ADCGT ADCLE ADCAL
syntax keyword goasmOpcode_ARM_THUMB         ADD ADDEQ ADDNE ADDCS ADDHS ADDCC ADDLO
syntax keyword goasmOpcode_ARM_THUMB         ADDMI ADDPL ADDVS ADDVC ADDHI ADDLS
syntax keyword goasmOpcode_ARM_THUMB         ADDGE ADDLT ADDGT ADDLE ADDAL
syntax keyword goasmOpcode_ARM_THUMB         AND ANDEQ ANDNE ANDCS ANDHS ANDCC ANDLO
syntax keyword goasmOpcode_ARM_THUMB         ANDMI ANDPL ANDVS ANDVC ANDHI ANDLS
syntax keyword goasmOpcode_ARM_THUMB         ANDGE ANDLT ANDGT ANDLE ANDAL
syntax keyword goasmOpcode_ARM_THUMB         ASR ASREQ ASRNE ASRCS ASRHS ASRCC ASRLO
syntax keyword goasmOpcode_ARM_THUMB         ASRMI ASRPL ASRVS ASRVC ASRHI ASRLS
syntax keyword goasmOpcode_ARM_THUMB         ASRGE ASRLT ASRGT ASRLE ASRAL
syntax keyword goasmOpcode_ARM_THUMB         B BEQ BNE BCS BHS BCC BLO BMI BPL BVS
syntax keyword goasmOpcode_ARM_THUMB         BVC BHI BLS BGE BLT BGT BLE BAL
syntax keyword goasmOpcode_ARM_THUMB         BL BLEQ BLNE BLCS BLHS BLCC BLLO BLMI
syntax keyword goasmOpcode_ARM_THUMB         BLPL BLVS BLVC BLHI BLLS BLGE BLLT BLGT
syntax keyword goasmOpcode_ARM_THUMB         BLLE BLAL
syntax keyword goasmOpcode_ARM_THUMB         BX BXPL BXVS BXVC BXHI BXLS BXGE BXLT BXGT
syntax keyword goasmOpcode_ARM_THUMB         BXLE
syntax keyword goasmOpcode_ARM_THUMB         BLX BLXEQ BLXNE  BLXCS  BLXHS  BLXCC
syntax keyword goasmOpcode_ARM_THUMB         BLXLO  BLXMI  BLXPL  BLXVS  BLXVC  BLXHI
syntax keyword goasmOpcode_ARM_THUMB         BLXLS  BLXGE  BLXLT  BLXGT  BLXLE  BLXAL
syntax keyword goasmOpcode_ARM_THUMB         BI BICEQ BICNE BICCS BICHS BICCC BICLO
syntax keyword goasmOpcode_ARM_THUMB         BICMI BICPL BICVS BICVC BICHI BICLS
syntax keyword goasmOpcode_ARM_THUMB         BICGE BICLT BICGT BICLE BICAL
syntax keyword goasmOpcode_ARM_THUMB         CMN CMNEQ CMNNE CMNCS CMNHS CMNCC CMNLO
syntax keyword goasmOpcode_ARM_THUMB         CMNMI CMNPL CMNVS CMNVC CMNHI CMNLS
syntax keyword goasmOpcode_ARM_THUMB         CMNGE CMNLT CMNGT CMNLE CMNAL
syntax keyword goasmOpcode_ARM_THUMB         CMP CMPEQ CMPNE CMPCS CMPHS CMPCC CMPLO
syntax keyword goasmOpcode_ARM_THUMB         CMPMI CMPPL CMPVS CMPVC CMPHI CMPLS
syntax keyword goasmOpcode_ARM_THUMB         CMPGE CMPLT CMPGT CMPLE CMPAL
syntax keyword goasmOpcode_ARM_THUMB         EOR EOREQ EORNE EORCS EORHS EORCC EORLO
syntax keyword goasmOpcode_ARM_THUMB         EORMI EORPL EORVS EORVC EORHI EORLS
syntax keyword goasmOpcode_ARM_THUMB         EORGE EORLT EORGT EORLE EORAL
syntax keyword goasmOpcode_ARM_THUMB         LDMIA LDMIAEQ LDMIANE LDMIACS LDMIAHS
syntax keyword goasmOpcode_ARM_THUMB         LDMIACC LDMIALO LDMIAMI LDMIAPL LDMIAVS
syntax keyword goasmOpcode_ARM_THUMB         LDMIAVC LDMIAHI LDMIALS LDMIAGE LDMIALT
syntax keyword goasmOpcode_ARM_THUMB         LDMIAGT LDMIALE LDMIAAL
syntax keyword goasmOpcode_ARM_THUMB         LDR LDREQ LDRNE LDRCS LDRHS LDRCC LDRLO
syntax keyword goasmOpcode_ARM_THUMB         LDRMI LDRPL LDRVS LDRVC LDRHI LDRLS
syntax keyword goasmOpcode_ARM_THUMB         LDRGE LDRLT LDRGT LDRLE LDRAL
syntax keyword goasmOpcode_ARM_THUMB         LDRB LDRBEQ LDRBNE LDRBCS LDRBHS LDRBCC
syntax keyword goasmOpcode_ARM_THUMB         LDRBLO LDRBMI LDRBPL LDRBVS LDRBVC
syntax keyword goasmOpcode_ARM_THUMB         LDRBHI LDRBLS LDRBGE LDRBLT LDRBGT
syntax keyword goasmOpcode_ARM_THUMB         LDRBLE LDRBAL
syntax keyword goasmOpcode_ARM_THUMB         LDRH LDRHEQ LDRHNE LDRHCS LDRHHS LDRHCC
syntax keyword goasmOpcode_ARM_THUMB         LDRHLO LDRHMI LDRHPL LDRHVS LDRHVC
syntax keyword goasmOpcode_ARM_THUMB         LDRHHI LDRHLS LDRHGE LDRHLT LDRHGT
syntax keyword goasmOpcode_ARM_THUMB         LDRHLE LDRHAL
syntax keyword goasmOpcode_ARM_THUMB         LSL LSLEQ LSLNE LSLCS LSLHS LSLCC LSLLO
syntax keyword goasmOpcode_ARM_THUMB         LSLMI LSLPL LSLVS LSLVC LSLHI LSLLS
syntax keyword goasmOpcode_ARM_THUMB         LSLGE LSLLT LSLGT LSLLE LSLAL
syntax keyword goasmOpcode_ARM_THUMB         LDSB LDSBEQ LDSBNE LDSBCS LDSBHS LDSBCC
syntax keyword goasmOpcode_ARM_THUMB         LDSBLO LDSBMI LDSBPL LDSBVS LDSBVC
syntax keyword goasmOpcode_ARM_THUMB         LDSBHI LDSBLS LDSBGE LDSBLT LDSBGT
syntax keyword goasmOpcode_ARM_THUMB         LDSBLE LDSBAL
syntax keyword goasmOpcode_ARM_THUMB         LDSD LDSHEQ LDSHNE LDSHCS LDSHHS LDSHCC
syntax keyword goasmOpcode_ARM_THUMB         LDSHLO LDSHMI LDSHPL LDSHVS LDSHVC
syntax keyword goasmOpcode_ARM_THUMB         LDSHHI LDSHLS LDSHGE LDSHLT LDSHGT
syntax keyword goasmOpcode_ARM_THUMB         LDSHLE LDSHAL
syntax keyword goasmOpcode_ARM_THUMB         MOV MOVEQ MOVNE MOVCS MOVHS MOVCC MOVLO
syntax keyword goasmOpcode_ARM_THUMB         MOVMI MOVPL MOVVS MOVVC MOVHI MOVLS
syntax keyword goasmOpcode_ARM_THUMB         MOVGE MOVLT MOVGT MOVLE MOVAL
syntax keyword goasmOpcode_ARM_THUMB         MUL MULEQ MULNE MULCS MULHS MULCC MULLO
syntax keyword goasmOpcode_ARM_THUMB         MULMI MULPL MULVS MULVC MULHI MULLS
syntax keyword goasmOpcode_ARM_THUMB         MULGE MULLT MULGT MULLE MULAL
syntax keyword goasmOpcode_ARM_THUMB         MVN MVNEQ MVNNE MVNCS MVNHS MVNCC MVNLO
syntax keyword goasmOpcode_ARM_THUMB         MVNMI MVNPL MVNVS MVNVC MVNHI MVNLS
syntax keyword goasmOpcode_ARM_THUMB         MVNGE MVNLT MVNGT MVNLE MVNAL
syntax keyword goasmOpcode_ARM_THUMB         NEG NEGEQ NEGNE NEGCS NEGHS NEGCC NEGLO
syntax keyword goasmOpcode_ARM_THUMB         NEGMI NEGPL NEGVS NEGVC NEGHI NEGLS
syntax keyword goasmOpcode_ARM_THUMB         NEGGE NEGLT NEGGT NEGLE NEGAL
syntax keyword goasmOpcode_ARM_THUMB         OR ORREQ ORRNE ORRCS ORRHS ORRCC ORRLO
syntax keyword goasmOpcode_ARM_THUMB         ORRMI ORRPL ORRVS ORRVC ORRHI ORRLS
syntax keyword goasmOpcode_ARM_THUMB         ORRGE ORRLT ORRGT ORRLE ORRAL
syntax keyword goasmOpcode_ARM_THUMB         POP POPEQ POPNE POPCS POPHS POPCC POPLO
syntax keyword goasmOpcode_ARM_THUMB         POPMI POPPL POPVS POPVC POPHI POPLS
syntax keyword goasmOpcode_ARM_THUMB         POPGE POPLT POPGT POPLE POPAL
syntax keyword goasmOpcode_ARM_THUMB         PUSH PUSHEQ PUSHNE PUSHCS PUSHHS PUSHCC
syntax keyword goasmOpcode_ARM_THUMB         PUSHLO PUSHMI PUSHPL PUSHVS PUSHVC
syntax keyword goasmOpcode_ARM_THUMB         PUSHHI PUSHLS PUSHGE PUSHLT PUSHGT
syntax keyword goasmOpcode_ARM_THUMB         PUSHLE PUSHAL
syntax keyword goasmOpcode_ARM_THUMB         ROR ROREQ RORNE RORCS RORHS RORCC RORLO
syntax keyword goasmOpcode_ARM_THUMB         RORMI RORPL RORVS RORVC RORHI RORLS
syntax keyword goasmOpcode_ARM_THUMB         RORGE RORLT RORGT RORLE RORAL
syntax keyword goasmOpcode_ARM_THUMB         SB SBCEQ SBCNE SBCCS SBCHS SBCCC SBCLO
syntax keyword goasmOpcode_ARM_THUMB         SBCMI SBCPL SBCVS SBCVC SBCHI SBCLS
syntax keyword goasmOpcode_ARM_THUMB         SBCGE SBCLT SBCGT SBCLE SBCAL
syntax keyword goasmOpcode_ARM_THUMB         STMIA STMIAEQ STMIANE STMIACS STMIAHS
syntax keyword goasmOpcode_ARM_THUMB         STMIACC STMIALO STMIAMI STMIAPL STMIAVS
syntax keyword goasmOpcode_ARM_THUMB         STMIAVC STMIAHI STMIALS STMIAGE STMIALT
syntax keyword goasmOpcode_ARM_THUMB         STMIAGT STMIALE STMIAAL
syntax keyword goasmOpcode_ARM_THUMB         STR STREQ STRNE STRCS STRHS STRCC STRLO
syntax keyword goasmOpcode_ARM_THUMB         STRMI STRPL STRVS STRVC STRHI STRLS
syntax keyword goasmOpcode_ARM_THUMB         STRGE STRLT STRGT STRLE STRAL
syntax keyword goasmOpcode_ARM_THUMB         STRB STRBEQ STRBNE STRBCS STRBHS STRBCC
syntax keyword goasmOpcode_ARM_THUMB         STRBLO STRBMI STRBPL STRBVS STRBVC
syntax keyword goasmOpcode_ARM_THUMB         STRBHI STRBLS STRBGE STRBLT STRBGT
syntax keyword goasmOpcode_ARM_THUMB         STRBLE STRBAL
syntax keyword goasmOpcode_ARM_THUMB         STRH STRHEQ STRHNE STRHCS STRHHS STRHCC
syntax keyword goasmOpcode_ARM_THUMB         STRHLO STRHMI STRHPL STRHVS STRHVC
syntax keyword goasmOpcode_ARM_THUMB         STRHHI STRHLS STRHGE STRHLT STRHGT
syntax keyword goasmOpcode_ARM_THUMB         STRHLE STRHAL
syntax keyword goasmOpcode_ARM_THUMB         SWI SWIEQ SWINE SWICS SWIHS SWICC SWILO
syntax keyword goasmOpcode_ARM_THUMB         SWIMI SWIPL SWIVS SWIVC SWIHI SWILS
syntax keyword goasmOpcode_ARM_THUMB         SWIGE SWILT SWIGT SWILE SWIAL
syntax keyword goasmOpcode_ARM_THUMB         SUB SUBEQ SUBNE SUBCS SUBHS SUBCC SUBLO
syntax keyword goasmOpcode_ARM_THUMB         SUBMI SUBPL SUBVS SUBVC SUBHI SUBLS
syntax keyword goasmOpcode_ARM_THUMB         SUBGE SUBLT SUBGT SUBLE SUBAL
syntax keyword goasmOpcode_ARM_THUMB         TST TSTEQ TSTNE TSTCS TSTHS TSTCC TSTLO
syntax keyword goasmOpcode_ARM_THUMB         TSTMI TSTPL TSTVS TSTVC TSTHI TSTLS
syntax keyword goasmOpcode_ARM_THUMB         TSTGE TSTLT TSTGT TSTLE TSTAL

"-- Section: AVR
syntax keyword goasmOpcode_AVR       ADC ADD ADIW AND ANDI ASR
syntax keyword goasmOpcode_AVR       BCLR BLD BRBC BRBS BRCC BRCS BREAK BREQ BRGE
syntax keyword goasmOpcode_AVR       BRHC BRHS BRID BRIE BRLO BRLT BRMI BRNE BRPL
syntax keyword goasmOpcode_AVR       BRSH BRTC BRTS BRVC BRVS BSET BST
syntax keyword goasmOpcode_AVR       CALL CBI CBR CLC CLH CLI CLN CLR CLS CLT CLV
syntax keyword goasmOpcode_AVR       CLZ COM CP CPC CPI CPSE DEC DES EICALL EIJMP
syntax keyword goasmOpcode_AVR       ELPM EOR FMUL FMULS FMULSU ICALL IJMP IN INC
syntax keyword goasmOpcode_AVR       JMP LAC LAS LAT LD LDD LDI LDS LPM LSL LSR
syntax keyword goasmOpcode_AVR       MOV MOVW MUL MULS MULSU NEG NOP OR ORI OUT
syntax keyword goasmOpcode_AVR       POP PUSH RCALL RET RETI RJMP ROL ROR SBC SBCI
syntax keyword goasmOpcode_AVR       SBI SBIC SBIS SBIW SBR SBRC SBRS SEC SEH SEI
syntax keyword goasmOpcode_AVR       SEN SER SES SET SEV SEZ SLEEP SPM ST STD STS
syntax keyword goasmOpcode_AVR       SUB SUBI SWAP TST WDR XCH

" link to defaults
highlight default link goasmDirective            Preproc
highlight default link goasmDirectiveStore       Type
highlight default link goasmDirectiveMacro       Macro
highlight default link goasmRegister             Identifier
highlight default link goasmString               String
highlight default link goasmCharacter            Character
highlight default link goasmBinaryNumber         Constant
highlight default link goasmOctalNumber          Constant
highlight default link goasmHexNumber            Constant
highlight default link goasmDecimalNumber        Constant
highlight default link goasmSymbols              Function
highlight default link goasmSymbolRef            Special
highlight default link goasmSpecial              Special
highlight default link goasmLabel                Function
highlight default link goasmLocalLabel           Label
highlight default link goasmOperator             Operator
highlight default link goasmOpcode               Keyword
highlight default link goasmComment              Comment
highlight default link goasmCommentSingle        Comment

highlight default link goasmOpcode               Statement
highlight default link goasmOpcode_SSE           Statement
highlight default link goasmOpcode_SSE2          Statement
highlight default link goasmOpcode_X64_Base      Statement
highlight default link goasmOpcode_X64_SSE2      Statement
highlight default link goasmOpcode_X64_SSE42     Statement
highlight default link goasmOpcode_SSE42         Statement
highlight default link goasmOpcode_NEHALEM_Base  Statement
highlight default link goasmOpcode_AMD_SSE5      Statement
highlight default link goasmOpcode_P6_SSE        Statement
highlight default link goasmOpcode_PRESCOTT_SSE3 Statement
highlight default link goasmOpcode_KATMAI_MMX    Statement
highlight default link goasmOpcode_KATMAI_Base   Statement
highlight default link goasmOpcode_KATMAI_SSE    Statement
highlight default link goasmOpcode_VMX           Statement
highlight default link goasmOpcode_X64_VMX       Statement
highlight default link goasmOpcode_SANDYBRIDGE_AVX Statement
highlight default link goasmOpcode_PENT_3DNOW    Statement
highlight default link goasmOpcode_SSE41         Statement
highlight default link goasmOpcode_AMD_SSE4A     Statement
highlight default link goasmOpcode_ARM_THUMB     Statement
highlight default link goasmOpcode_AVR           Statement

"-- initial mapping => Keyword
func! s:MapOpcode (group, cpu, ext)
  let himap = 'Keyword'

  if exists('g:goasmDisableOpcodes')
    if index(split(g:goasmDisableOpcodes), a:cpu) != -1
      let himap = 'Error'
    endif
    if index(split(g:goasmDisableOpcodes), a:ext) != -1
      let himap = 'Error'
    endif
  endif

  if exists('b:goasmDisableOpcodes')
    if index(split(b:goasmDisableOpcodes), a:cpu) != -1
      let himap = 'Error'
    endif
    if index(split(b:goasmDisableOpcodes), a:ext) != -1
      let himap = 'Error'
    endif
  endif

  exe 'hi link '.a:group.' '.himap
endf

call <SID>MapOpcode('goasmOpcode_186_Base'       , '186'        , 'base')
call <SID>MapOpcode('goasmOpcode_286_Base'       , '286'        , 'base')
call <SID>MapOpcode('goasmOpcode_3862_Base'      , '3862'       , 'base')
call <SID>MapOpcode('goasmOpcode_386_Base'       , '386'        , 'base')
call <SID>MapOpcode('goasmOpcode_486_Base'       , '486'        , 'base')
call <SID>MapOpcode('goasmOpcode_8086_Base'      , '8086'       , 'base')
call <SID>MapOpcode('goasmOpcode_AMD_SSE4A'      , 'amd'        , 'sse4a')
call <SID>MapOpcode('goasmOpcode_AMD_SSE5'       , 'amd'        , 'sse5')
call <SID>MapOpcode('goasmOpcode_ARM_THUMB'      , 'arm'        , 'thumb')
call <SID>MapOpcode('goasmOpcode_AVR'            , 'avr'        , 'base')
call <SID>MapOpcode('goasmOpcode_FUTURE_FMA'     , 'future'     , 'fma')
call <SID>MapOpcode('goasmOpcode_IA64_Base'      , 'ia64'       , 'base')
call <SID>MapOpcode('goasmOpcode_KATMAI_Base'    , 'katmai'     , 'base')
call <SID>MapOpcode('goasmOpcode_KATMAI_MMX'     , 'katmai'     , 'mmx')
call <SID>MapOpcode('goasmOpcode_KATMAI_MMX2'    , 'katmai'     , 'mmx2')
call <SID>MapOpcode('goasmOpcode_KATMAI_SSE'     , 'katmai'     , 'sse')
call <SID>MapOpcode('goasmOpcode_NEHALEM_Base'   , 'nehalem'    , 'base')
call <SID>MapOpcode('goasmOpcode_P6_Base'        , 'p6'         , 'base')
call <SID>MapOpcode('goasmOpcode_P6_SSE'         , 'p6'         , 'sse')
call <SID>MapOpcode('goasmOpcode_PENTM_Base'     , 'pentium_m'  , 'base')
call <SID>MapOpcode('goasmOpcode_PENT_3DNOW'     , 'pentium'    , '3dnow')
call <SID>MapOpcode('goasmOpcode_PENT_Base'      , 'pentium'    , 'base')
call <SID>MapOpcode('goasmOpcode_PENT_MMX'       , 'pentium'    , 'mmx')
call <SID>MapOpcode('goasmOpcode_PRESCOTT_Base'  , 'prescott'   , 'base')
call <SID>MapOpcode('goasmOpcode_PRESCOTT_SSE3'  , 'prescott'   , 'sse3')
call <SID>MapOpcode('goasmOpcode_SANDYBRIDGE_AVX', 'sandybridge', 'avx')
call <SID>MapOpcode('goasmOpcode_X642_Base'      , 'x642'       , 'base')
call <SID>MapOpcode('goasmOpcode_X64_Base'       , 'x64'        , 'base')
call <SID>MapOpcode('goasmOpcode_X64_MMX'        , 'x64'        , 'mmx')
call <SID>MapOpcode('goasmOpcode_X64_SSE'        , 'x64'        , 'sse')
call <SID>MapOpcode('goasmOpcode_X64_SSE2'       , 'x64'        , 'sse2')
call <SID>MapOpcode('goasmOpcode_X64_SSE41'      , 'x64'        , 'sse4.1')
call <SID>MapOpcode('goasmOpcode_X64_SSE42'      , 'x64'        , 'sse4.2')
call <SID>MapOpcode('goasmOpcode_X64_VMX'        , 'x64'        , 'vmx')
call <SID>MapOpcode('goasmOpcode_X86_64_Base'    , 'x64'        , 'base')

" support CPP preprocessor tags
if !exists('g:goasmDisablePreproc') && !exists('b:goasmDisablePreproc')
  syntax case match

  syntax include @cPP syntax/c.vim
  syntax match   cPPLineCont "\\$" contained

  syntax region  cPPPreProc start=/^\s*#\s*\(if\|else\|endif\|define\|include\)/ end=/$/ contains=@cPP,cPPLineCont
endif

syn match   goasmBuildKeyword      display contained "+build"
" Highlight the known values of GOOS, GOARCH, and other +build options.
syn keyword goasmBuildDirectives   contained
      \ appengine android darwin dragonfly freebsd linux nacl netbsd openbsd plan9
      \ solaris windows 386 amd64 amd64p32 arm armbe arm64 arm64be ppc64
      \ ppc64le mips mipsle mips64 mips64le mips64p32 mips64p32le ppc
      \ s390 s390x sparc sparc64 cgo ignore race
syn region  goasmBuildComment      matchgroup=goasmBuildCommentStart
      \ start="//\s*+build\s"rs=s+2 end="$"
      \ contains=goasmBuildKeyword,goasmBuildDirectives
hi def link goasmBuildCommentStart Comment
hi def link goasmBuildDirectives   Type
hi def link goasmBuildKeyword      PreProc

let b:current_syntax = "goasm"

syntax sync ccomment
syntax sync linebreaks=1

" vim: sts=2:sw=2:ts=2:et
