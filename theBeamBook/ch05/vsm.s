	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0	sdk_version 15, 0
	.globl	_run                            ; -- Begin function run
	.p2align	2
_run:                                   ; @run
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	sub	sp, sp, #4048
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w27, -24
	.cfi_offset w28, -32
	adrp	x8, ___stack_chk_guard@GOTPAGE
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x8, [x8]
	stur	x8, [x29, #-24]
	str	x0, [sp, #32]
	str	wzr, [sp, #28]
	str	wzr, [sp, #24]
	str	wzr, [sp, #20]
	ldr	x8, [sp, #32]
	str	x8, [sp, #8]
	b	LBB0_1
LBB0_1:                                 ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB0_8 Depth 2
	ldr	x8, [sp, #8]
	ldrsb	w8, [x8]
	subs	w8, w8, #0
	cset	w8, eq
	tbnz	w8, #0, LBB0_12
	b	LBB0_2
LBB0_2:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	x8, [sp, #8]
	add	x9, x8, #1
	str	x9, [sp, #8]
	ldrsb	w8, [x8]
	str	w8, [sp, #4]                    ; 4-byte Folded Spill
	subs	w8, w8, #1
	cset	w8, eq
	tbnz	w8, #0, LBB0_5
	b	LBB0_3
LBB0_3:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	w8, [sp, #4]                    ; 4-byte Folded Reload
	subs	w8, w8, #2
	cset	w8, eq
	tbnz	w8, #0, LBB0_6
	b	LBB0_4
LBB0_4:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	w8, [sp, #4]                    ; 4-byte Folded Reload
	subs	w8, w8, #3
	cset	w8, eq
	tbnz	w8, #0, LBB0_7
	b	LBB0_11
LBB0_5:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	w8, [sp, #28]
	subs	w8, w8, #1
	str	w8, [sp, #28]
	add	x9, sp, #40
	ldr	w8, [x9, w8, sxtw #2]
	ldr	w10, [sp, #28]
	subs	w10, w10, #1
	str	w10, [sp, #28]
	ldr	w10, [x9, w10, sxtw #2]
	add	w8, w8, w10
	ldrsw	x10, [sp, #28]
	mov	x11, x10
	add	w11, w11, #1
	str	w11, [sp, #28]
	str	w8, [x9, x10, lsl #2]
	b	LBB0_11
LBB0_6:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	w8, [sp, #28]
	subs	w8, w8, #1
	str	w8, [sp, #28]
	add	x9, sp, #40
	ldr	w8, [x9, w8, sxtw #2]
	ldr	w10, [sp, #28]
	subs	w10, w10, #1
	str	w10, [sp, #28]
	ldr	w10, [x9, w10, sxtw #2]
	mul	w8, w8, w10
	ldrsw	x10, [sp, #28]
	mov	x11, x10
	add	w11, w11, #1
	str	w11, [sp, #28]
	str	w8, [x9, x10, lsl #2]
	b	LBB0_11
LBB0_7:                                 ;   in Loop: Header=BB0_1 Depth=1
	ldr	x8, [sp, #8]
	add	x9, x8, #1
	str	x9, [sp, #8]
	ldrsb	w8, [x8]
	str	w8, [sp, #24]
	str	wzr, [sp, #20]
	b	LBB0_8
LBB0_8:                                 ;   Parent Loop BB0_1 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	w8, [sp, #24]
	subs	w9, w8, #1
	str	w9, [sp, #24]
	subs	w8, w8, #0
	cset	w8, eq
	tbnz	w8, #0, LBB0_10
	b	LBB0_9
LBB0_9:                                 ;   in Loop: Header=BB0_8 Depth=2
	ldr	w9, [sp, #20]
	ldr	x8, [sp, #8]
	add	x10, x8, #1
	str	x10, [sp, #8]
	ldrsb	w8, [x8]
	add	w8, w8, w9, lsl #8
	str	w8, [sp, #20]
	b	LBB0_8
LBB0_10:                                ;   in Loop: Header=BB0_1 Depth=1
	ldr	w8, [sp, #20]
	ldrsw	x10, [sp, #28]
	mov	x9, x10
	add	w9, w9, #1
	str	w9, [sp, #28]
	add	x9, sp, #40
	str	w8, [x9, x10, lsl #2]
	b	LBB0_11
LBB0_11:                                ;   in Loop: Header=BB0_1 Depth=1
	b	LBB0_1
LBB0_12:
	ldr	w8, [sp, #28]
	subs	w9, w8, #1
	str	w9, [sp, #28]
	add	x8, sp, #40
	ldr	w8, [x8, w9, sxtw #2]
	str	w8, [sp]                        ; 4-byte Folded Spill
	ldur	x9, [x29, #-24]
	adrp	x8, ___stack_chk_guard@GOTPAGE
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
	ldr	x8, [x8]
	subs	x8, x8, x9
	cset	w8, eq
	tbnz	w8, #0, LBB0_14
	b	LBB0_13
LBB0_13:
	bl	___stack_chk_fail
LBB0_14:
	ldr	w0, [sp]                        ; 4-byte Folded Reload
	add	sp, sp, #4048
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
