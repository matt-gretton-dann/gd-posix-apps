	.build_version macos, 27, 0	sdk_version 27, 0
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_mac8                           ; -- Begin function mac8
	.p2align	2
_mac8:                                  ; @mac8
	.cfi_startproc
; %bb.0:
	b	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE3macEhh
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #144
	stp	x28, x27, [sp, #48]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #64]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #80]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #96]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #112]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #128]            ; 16-byte Folded Spill
	add	x29, sp, #128
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x20, x2
	mov	x21, x1
	mov	x19, x0
	bl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv
	ldr	x9, [x19, #8]
	cbz	x9, LBB1_2
; %bb.1:
	ldp	x8, x9, [x9]
	sub	x9, x9, x8
	cbnz	x9, LBB1_5
	b	LBB1_7
LBB1_2:
	ldrb	w8, [x19, #3]
	cmp	w8, #255
	b.ne	LBB1_4
; %bb.3:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #3]
LBB1_4:
	mov	w9, w8
	mov	x8, x19
	cbz	x9, LBB1_7
LBB1_5:
	mov	w10, #5243                      ; =0x147b
	mov	w11, #100                       ; =0x64
LBB1_6:                                 ; =>This Inner Loop Header: Depth=1
	ldrb	w12, [x8]
	mul	w12, w12, w21
	add	w12, w12, w20
	ubfx	w13, w12, #2, #14
	mul	w13, w13, w10
	lsr	w20, w13, #17
	msub	w12, w20, w11, w12
	strb	w12, [x8], #1
	subs	x9, x9, #1
	b.ne	LBB1_6
LBB1_7:
	cbz	w20, LBB1_17
; %bb.8:
	mov	w21, #5243                      ; =0x147b
	mov	w22, #100                       ; =0x64
	add	x23, sp, #40
	add	x24, sp, #15
	b	LBB1_11
LBB1_9:                                 ;   in Loop: Header=BB1_11 Depth=1
	ldrb	w8, [x19, #3]
LBB1_10:                                ;   in Loop: Header=BB1_11 Depth=1
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	add	w9, w8, #1
	strb	w9, [x19, #3]
	strb	w26, [x19, w8, uxtw]
	and	w8, w20, #0xffff
	mov	x20, x25
	cmp	w8, #99
	b.ls	LBB1_17
LBB1_11:                                ; =>This Inner Loop Header: Depth=1
	ubfx	w8, w20, #2, #14
	mul	w8, w8, w21
	lsr	w25, w8, #17
	msub	w26, w25, w22, w20
	strb	w26, [sp, #15]
	ldr	x27, [x19, #8]
	cbz	x27, LBB1_14
LBB1_12:                                ;   in Loop: Header=BB1_11 Depth=1
	ldp	x8, x9, [x27, #8]
	stp	x27, x8, [sp, #32]
	stp	x23, x24, [sp, #16]
	cmp	x8, x9
	b.hs	LBB1_16
; %bb.13:                               ;   in Loop: Header=BB1_11 Depth=1
	strb	w26, [x8], #1
	str	x8, [x27, #8]
	and	w8, w20, #0xffff
	mov	x20, x25
	cmp	w8, #99
	b.hi	LBB1_11
	b	LBB1_17
LBB1_14:                                ;   in Loop: Header=BB1_11 Depth=1
	ldrb	w8, [x19, #3]
	cmp	w8, #3
	b.ne	LBB1_10
; %bb.15:                               ;   in Loop: Header=BB1_11 Depth=1
	mov	x0, x19
	mov	w1, #6                          ; =0x6
	bl	__ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm
	ldr	x27, [x19, #8]
	cbnz	x27, LBB1_12
	b	LBB1_9
LBB1_16:                                ;   in Loop: Header=BB1_11 Depth=1
	add	x0, sp, #16
	bl	__ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv
	ldr	x8, [sp, #40]
	str	x8, [x27, #8]
	and	w8, w20, #0xffff
	mov	x20, x25
	cmp	w8, #99
	b.hi	LBB1_11
LBB1_17:
	ldp	x29, x30, [sp, #128]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #112]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #96]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #80]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #64]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #144
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_mac16                          ; -- Begin function mac16
	.p2align	2
_mac16:                                 ; @mac16
	.cfi_startproc
; %bb.0:
	b	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE3macEtt
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #144
	stp	x28, x27, [sp, #48]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #64]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #80]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #96]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #112]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #128]            ; 16-byte Folded Spill
	add	x29, sp, #128
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x20, x2
	mov	x21, x1
	mov	x19, x0
	bl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv
	ldr	x9, [x19, #8]
	cbz	x9, LBB3_2
; %bb.1:
	ldp	x8, x9, [x9]
	sub	x9, x9, x8
	cbnz	x9, LBB3_5
	b	LBB3_7
LBB3_2:
	ldrb	w8, [x19, #6]
	cmp	w8, #255
	b.ne	LBB3_4
; %bb.3:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #6]
LBB3_4:
	ubfiz	x9, x8, #1, #32
	mov	x8, x19
	cbz	x9, LBB3_7
LBB3_5:
	mov	w10, #5977                      ; =0x1759
	movk	w10, #53687, lsl #16
	mov	w11, #10000                     ; =0x2710
LBB3_6:                                 ; =>This Inner Loop Header: Depth=1
	ldrh	w12, [x8]
	mul	w12, w12, w21
	add	w12, w12, w20
	umull	x13, w12, w10
	lsr	x20, x13, #45
	msub	w12, w20, w11, w12
	strh	w12, [x8], #2
	subs	x9, x9, #2
	b.ne	LBB3_6
LBB3_7:
	cbz	w20, LBB3_17
; %bb.8:
	mov	w21, #5977                      ; =0x1759
	movk	w21, #53687, lsl #16
	mov	w22, #10000                     ; =0x2710
	add	x23, sp, #40
	add	x24, sp, #14
                                        ; kill: def $w20 killed $w20 killed $x20 def $x20
	b	LBB3_11
LBB3_9:                                 ;   in Loop: Header=BB3_11 Depth=1
	ldrb	w8, [x19, #6]
LBB3_10:                                ;   in Loop: Header=BB3_11 Depth=1
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	add	w9, w8, #1
	strb	w9, [x19, #6]
	strh	w26, [x19, w8, uxtw #1]
	lsr	w8, w25, #4
	cmp	w8, #624
	b.ls	LBB3_17
LBB3_11:                                ; =>This Inner Loop Header: Depth=1
	mov	x25, x20
	umull	x8, w20, w21
	lsr	x20, x8, #45
	msub	w26, w20, w22, w25
	strh	w26, [sp, #14]
	ldr	x27, [x19, #8]
	cbz	x27, LBB3_14
LBB3_12:                                ;   in Loop: Header=BB3_11 Depth=1
	ldp	x8, x9, [x27, #8]
	stp	x27, x8, [sp, #32]
	stp	x23, x24, [sp, #16]
	cmp	x8, x9
	b.hs	LBB3_16
; %bb.13:                               ;   in Loop: Header=BB3_11 Depth=1
	strh	w26, [x8], #2
	str	x8, [x27, #8]
	lsr	w8, w25, #4
	cmp	w8, #624
	b.hi	LBB3_11
	b	LBB3_17
LBB3_14:                                ;   in Loop: Header=BB3_11 Depth=1
	ldrb	w8, [x19, #6]
	cmp	w8, #3
	b.ne	LBB3_10
; %bb.15:                               ;   in Loop: Header=BB3_11 Depth=1
	mov	x0, x19
	mov	w1, #6                          ; =0x6
	bl	__ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm
	ldr	x27, [x19, #8]
	cbnz	x27, LBB3_12
	b	LBB3_9
LBB3_16:                                ;   in Loop: Header=BB3_11 Depth=1
	add	x0, sp, #16
	bl	__ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv
	ldr	x8, [sp, #40]
	str	x8, [x27, #8]
	lsr	w8, w25, #4
	cmp	w8, #624
	b.hi	LBB3_11
LBB3_17:
	ldp	x29, x30, [sp, #128]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #112]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #96]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #80]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #64]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #144
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_mac32                          ; -- Begin function mac32
	.p2align	2
_mac32:                                 ; @mac32
	.cfi_startproc
; %bb.0:
	b	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE3macEjj
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #144
	stp	x28, x27, [sp, #48]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #64]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #80]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #96]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #112]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #128]            ; 16-byte Folded Spill
	add	x29, sp, #128
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x21, x2
	mov	x20, x1
	mov	x19, x0
	bl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv
	mov	w21, w21
	ldr	x9, [x19, #16]
	cbz	x9, LBB5_2
; %bb.1:
	ldp	x8, x9, [x9]
	sub	x9, x9, x8
	mov	w22, #51712                     ; =0xca00
	movk	w22, #15258, lsl #16
	cbnz	x9, LBB5_5
	b	LBB5_7
LBB5_2:
	ldrb	w8, [x19, #12]
	cmp	w8, #255
	b.ne	LBB5_4
; %bb.3:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #12]
LBB5_4:
	ubfiz	x9, x8, #2, #32
	mov	x8, x19
	mov	w22, #51712                     ; =0xca00
	movk	w22, #15258, lsl #16
	cbz	x9, LBB5_7
LBB5_5:
	mov	w10, w20
	mov	x11, #23123                     ; =0x5a53
	movk	x11, #41115, lsl #16
	movk	x11, #47151, lsl #32
	movk	x11, #68, lsl #48
LBB5_6:                                 ; =>This Inner Loop Header: Depth=1
	ldr	w12, [x8]
	umull	x12, w12, w10
	add	x12, x12, x21
	lsr	x13, x12, #9
	umulh	x13, x13, x11
	lsr	x21, x13, #11
	msub	w12, w21, w22, w12
	str	w12, [x8], #4
	subs	x9, x9, #4
	b.ne	LBB5_6
LBB5_7:
	cbz	x21, LBB5_17
; %bb.8:
	mov	x20, #23123                     ; =0x5a53
	movk	x20, #41115, lsl #16
	movk	x20, #47151, lsl #32
	movk	x20, #68, lsl #48
	add	x23, sp, #40
	add	x24, sp, #12
	b	LBB5_11
LBB5_9:                                 ;   in Loop: Header=BB5_11 Depth=1
	ldrb	w8, [x19, #12]
LBB5_10:                                ;   in Loop: Header=BB5_11 Depth=1
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	add	w9, w8, #1
	strb	w9, [x19, #12]
	str	w26, [x19, w8, uxtw #2]
	cmp	x21, x22
	mov	x21, x25
	b.lo	LBB5_17
LBB5_11:                                ; =>This Inner Loop Header: Depth=1
	lsr	x8, x21, #9
	umulh	x8, x8, x20
	lsr	x25, x8, #11
	msub	w26, w25, w22, w21
	str	w26, [sp, #12]
	ldr	x27, [x19, #16]
	cbz	x27, LBB5_14
LBB5_12:                                ;   in Loop: Header=BB5_11 Depth=1
	ldp	x8, x9, [x27, #8]
	stp	x27, x8, [sp, #32]
	stp	x23, x24, [sp, #16]
	cmp	x8, x9
	b.hs	LBB5_16
; %bb.13:                               ;   in Loop: Header=BB5_11 Depth=1
	str	w26, [x8], #4
	str	x8, [x27, #8]
	cmp	x21, x22
	mov	x21, x25
	b.hs	LBB5_11
	b	LBB5_17
LBB5_14:                                ;   in Loop: Header=BB5_11 Depth=1
	ldrb	w8, [x19, #12]
	cmp	w8, #3
	b.ne	LBB5_10
; %bb.15:                               ;   in Loop: Header=BB5_11 Depth=1
	mov	x0, x19
	mov	w1, #6                          ; =0x6
	bl	__ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm
	ldr	x27, [x19, #16]
	cbnz	x27, LBB5_12
	b	LBB5_9
LBB5_16:                                ;   in Loop: Header=BB5_11 Depth=1
	add	x0, sp, #16
	bl	__ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv
	ldr	x8, [sp, #40]
	str	x8, [x27, #8]
	cmp	x21, x22
	mov	x21, x25
	b.hs	LBB5_11
LBB5_17:
	ldp	x29, x30, [sp, #128]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #112]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #96]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #80]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #64]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #144
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIhtLj2EEEE13copy_on_writeEv
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #80
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x20, x0
	ldr	x8, [x20, #8]!
	cbz	x8, LBB6_6
; %bb.1:
	ldr	x9, [x0, #16]
	cbz	x9, LBB6_8
; %bb.2:
	ldr	x9, [x9, #8]
	add	x9, x9, #1
	cbz	x9, LBB6_7
LBB6_3:
	cmp	x9, #2
	b.lt	LBB6_19
; %bb.4:
	cbz	x8, LBB6_20
; %bb.5:
	mov	x19, x0
	ldp	x1, x8, [x8]
	sub	x8, x8, x1
	add	x21, sp, #8
	add	x0, sp, #8
	add	x2, x1, x8
	bl	__ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_
	cmp	x19, x21
	b.ne	LBB6_21
	b	LBB6_33
LBB6_6:
	ldrb	w9, [x0, #3]
	cmp	w9, #255
	cset	w9, ne
	cbnz	x9, LBB6_3
LBB6_7:
	ldr	x19, [x0, #16]
	strb	wzr, [sp, #11]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbnz	x19, LBB6_9
	b	LBB6_13
LBB6_8:
	mov	x19, #0                         ; =0x0
	strb	wzr, [sp, #11]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbz	x19, LBB6_13
LBB6_9:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB6_12
; %bb.10:
	ldr	x8, [x20]
	cbz	x8, LBB6_13
LBB6_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #3]
	b	LBB6_15
LBB6_12:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
	ldr	x8, [x20]
	cbnz	x8, LBB6_11
LBB6_13:
	ldrb	w2, [sp, #11]
	strb	w2, [x0, #3]
	cmp	x2, #255
	b.eq	LBB6_15
; %bb.14:
	add	x1, sp, #8
	bl	_memcpy
LBB6_15:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB6_19
; %bb.16:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB6_18
; %bb.17:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB6_18:
	ldr	x19, [sp, #24]
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #11]
	cbnz	x19, LBB6_34
LBB6_19:
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB6_20:
	ldrb	w8, [x0, #3]
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	mov	x19, x0
	mov	x1, x0
	add	x21, sp, #8
	add	x0, sp, #8
	add	x2, x1, x8
	bl	__ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_
	cmp	x19, x21
	b.eq	LBB6_33
LBB6_21:
	mov	x0, x19
	ldur	q0, [sp, #16]
	stp	xzr, xzr, [sp, #16]
	ldr	x19, [x19, #16]
	stur	q0, [x0, #8]
	cbz	x19, LBB6_26
; %bb.22:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB6_24
; %bb.23:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
LBB6_24:
	ldr	x8, [x20]
	cbz	x8, LBB6_27
LBB6_25:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #3]
	b	LBB6_29
LBB6_26:
	fmov	x8, d0
	cbnz	x8, LBB6_25
LBB6_27:
	ldrb	w2, [sp, #11]
	strb	w2, [x0, #3]
	cmp	x2, #255
	b.eq	LBB6_29
; %bb.28:
	add	x1, sp, #8
	bl	_memcpy
LBB6_29:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB6_32
; %bb.30:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB6_32
; %bb.31:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB6_32:
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #11]
LBB6_33:
	ldr	x19, [sp, #24]
	cbz	x19, LBB6_19
LBB6_34:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB6_19
; %bb.35:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev: ; @_ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #16]
	cbz	x19, LBB7_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB7_3
LBB7_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB7_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev ; -- Begin function _ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	.globl	__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	.p2align	2
__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev: ; @_ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #8]
	cbz	x19, LBB8_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB8_3
LBB8_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB8_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_ ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_: ; @_ZN2GD2Bc7Details12DigitStorageIhLm3EEC1EPKhS5_
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception0
; %bb.0:
	stp	x24, x23, [sp, #-64]!           ; 16-byte Folded Spill
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov	x21, x1
	mov	x19, x0
	strb	wzr, [x0, #3]
	stp	xzr, xzr, [x0, #8]
	subs	x22, x2, x1
	csel	x20, xzr, x22, eq
	cmp	x20, #3
	b.hi	LBB9_4
; %bb.1:
	cbz	x20, LBB9_10
; %bb.2:
	mov	x0, x19
	mov	x1, x21
	mov	x2, x22
	bl	_memmove
	ldrb	w8, [x19, #3]
	cmp	w8, #255
	b.ne	LBB9_10
; %bb.3:
	strb	wzr, [x19, #3]
	b	LBB9_10
LBB9_4:
Ltmp0:
	mov	x23, x2
	mov	w0, #48                         ; =0x30
	bl	__Znwm
Ltmp1:
; %bb.5:
	mov	x20, x0
	str	xzr, [x0, #16]
Lloh0:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGE
Lloh1:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	mov	x24, x0
	str	xzr, [x24, #24]!
	stp	xzr, xzr, [x0, #32]
	cmp	x21, x23
	b.eq	LBB9_9
; %bb.6:
	tbnz	x22, #63, LBB9_11
; %bb.7:
Ltmp3:
	mov	x0, x22
	bl	__Znwm
Ltmp4:
; %bb.8:
	str	x0, [x20, #24]
	add	x23, x0, x22
	str	x23, [x20, #40]
	mov	x1, x21
	mov	x2, x22
	bl	_memcpy
	str	x23, [x20, #32]
LBB9_9:
	stp	x24, x20, [x19, #8]
	mov	w20, #255                       ; =0xff
LBB9_10:
	strb	w20, [x19, #3]
	mov	x0, x19
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp], #64             ; 16-byte Folded Reload
	ret
LBB9_11:
Ltmp5:
	bl	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
Ltmp6:
; %bb.12:
	brk	#0x1
LBB9_13:
Ltmp2:
	mov	x21, x0
	mov	x0, x19
	bl	__ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev
	mov	x0, x21
	bl	__Unwind_Resume
LBB9_14:
Ltmp7:
	mov	x21, x0
	ldr	x0, [x24]
	cbz	x0, LBB9_16
; %bb.15:
	str	x0, [x20, #32]
	bl	__ZdlPv
LBB9_16:
	mov	x0, x20
	bl	__ZNSt3__119__shared_weak_countD2Ev
	bl	__ZdlPv
	mov	x0, x19
	bl	__ZN2GD2Bc7Details12DigitStorageIhLm3EED1Ev
	mov	x0, x21
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh0, Lloh1
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table9:
Lexception0:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end0-Lcst_begin0
Lcst_begin0:
	.uleb128 Lfunc_begin0-Lfunc_begin0      ; >> Call Site 1 <<
	.uleb128 Ltmp0-Lfunc_begin0             ;   Call between Lfunc_begin0 and Ltmp0
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp0-Lfunc_begin0             ; >> Call Site 2 <<
	.uleb128 Ltmp1-Ltmp0                    ;   Call between Ltmp0 and Ltmp1
	.uleb128 Ltmp2-Lfunc_begin0             ;     jumps to Ltmp2
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp3-Lfunc_begin0             ; >> Call Site 3 <<
	.uleb128 Ltmp4-Ltmp3                    ;   Call between Ltmp3 and Ltmp4
	.uleb128 Ltmp7-Lfunc_begin0             ;     jumps to Ltmp7
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp4-Lfunc_begin0             ; >> Call Site 4 <<
	.uleb128 Ltmp5-Ltmp4                    ;   Call between Ltmp4 and Ltmp5
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp5-Lfunc_begin0             ; >> Call Site 5 <<
	.uleb128 Ltmp6-Ltmp5                    ;   Call between Ltmp5 and Ltmp6
	.uleb128 Ltmp7-Lfunc_begin0             ;     jumps to Ltmp7
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp6-Lfunc_begin0             ; >> Call Site 6 <<
	.uleb128 Lfunc_end0-Ltmp6               ;   Call between Ltmp6 and Lfunc_end0
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end0:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.private_extern	__ZSt28__throw_bad_array_new_lengthB9nqe220106v ; -- Begin function _ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.globl	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.weak_def_can_be_hidden	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.p2align	2
__ZSt28__throw_bad_array_new_lengthB9nqe220106v: ; @_ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w0, #8                          ; =0x8
	bl	___cxa_allocate_exception
	bl	__ZNSt20bad_array_new_lengthC1Ev
Lloh2:
	adrp	x1, __ZTISt20bad_array_new_length@GOTPAGE
Lloh3:
	ldr	x1, [x1, __ZTISt20bad_array_new_length@GOTPAGEOFF]
Lloh4:
	adrp	x2, __ZNSt20bad_array_new_lengthD1Ev@GOTPAGE
Lloh5:
	ldr	x2, [x2, __ZNSt20bad_array_new_lengthD1Ev@GOTPAGEOFF]
	bl	___cxa_throw
	.loh AdrpLdrGot	Lloh4, Lloh5
	.loh AdrpLdrGot	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev
	.cfi_startproc
; %bb.0:
Lloh6:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGE
Lloh7:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	b	__ZNSt3__119__shared_weak_countD2Ev
	.loh AdrpLdrGot	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh8:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGE
Lloh9:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	bl	__ZNSt3__119__shared_weak_countD2Ev
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	b	__ZdlPv
	.loh AdrpLdrGot	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv
	.cfi_startproc
; %bb.0:
	mov	x8, x0
	ldr	x0, [x0, #24]
	cbz	x0, LBB13_2
; %bb.1:
	str	x0, [x8, #32]
	b	__ZdlPv
LBB13_2:
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.cfi_startproc
; %bb.0:
	b	__ZdlPv
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev ; -- Begin function _ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.globl	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.p2align	2
__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev: ; @_ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh10:
	adrp	x0, l_.str@PAGE
Lloh11:
	add	x0, x0, l_.str@PAGEOFF
	bl	__ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.loh AdrpAdd	Lloh10, Lloh11
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__throw_length_errorB9nqe220106EPKc ; -- Begin function _ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.globl	__ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.weak_def_can_be_hidden	__ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.p2align	2
__ZNSt3__120__throw_length_errorB9nqe220106EPKc: ; @_ZNSt3__120__throw_length_errorB9nqe220106EPKc
Lfunc_begin1:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception1
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x20, x0
	mov	w0, #16                         ; =0x10
	bl	___cxa_allocate_exception
	mov	x19, x0
Ltmp8:
	mov	x1, x20
	bl	__ZNSt12length_errorC1B9nqe220106EPKc
Ltmp9:
; %bb.1:
Lloh12:
	adrp	x1, __ZTISt12length_error@GOTPAGE
Lloh13:
	ldr	x1, [x1, __ZTISt12length_error@GOTPAGEOFF]
Lloh14:
	adrp	x2, __ZNSt12length_errorD1Ev@GOTPAGE
Lloh15:
	ldr	x2, [x2, __ZNSt12length_errorD1Ev@GOTPAGEOFF]
	mov	x0, x19
	bl	___cxa_throw
LBB16_2:
Ltmp10:
	mov	x20, x0
	mov	x0, x19
	bl	___cxa_free_exception
	mov	x0, x20
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh14, Lloh15
	.loh AdrpLdrGot	Lloh12, Lloh13
Lfunc_end1:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table16:
Lexception1:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end1-Lcst_begin1
Lcst_begin1:
	.uleb128 Lfunc_begin1-Lfunc_begin1      ; >> Call Site 1 <<
	.uleb128 Ltmp8-Lfunc_begin1             ;   Call between Lfunc_begin1 and Ltmp8
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp8-Lfunc_begin1             ; >> Call Site 2 <<
	.uleb128 Ltmp9-Ltmp8                    ;   Call between Ltmp8 and Ltmp9
	.uleb128 Ltmp10-Lfunc_begin1            ;     jumps to Ltmp10
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp9-Lfunc_begin1             ; >> Call Site 3 <<
	.uleb128 Lfunc_end1-Ltmp9               ;   Call between Ltmp9 and Lfunc_end1
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end1:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.private_extern	__ZNSt12length_errorC1B9nqe220106EPKc ; -- Begin function _ZNSt12length_errorC1B9nqe220106EPKc
	.globl	__ZNSt12length_errorC1B9nqe220106EPKc
	.weak_def_can_be_hidden	__ZNSt12length_errorC1B9nqe220106EPKc
	.p2align	2
__ZNSt12length_errorC1B9nqe220106EPKc:  ; @_ZNSt12length_errorC1B9nqe220106EPKc
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	bl	__ZNSt11logic_errorC2EPKc
Lloh16:
	adrp	x8, __ZTVSt12length_error@GOTPAGE
Lloh17:
	ldr	x8, [x8, __ZTVSt12length_error@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpLdrGot	Lloh16, Lloh17
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm: ; @_ZN2GD2Bc7Details12DigitStorageIhLm3EE7promoteEm
Lfunc_begin2:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception2
; %bb.0:
	sub	sp, sp, #64
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	ldr	x8, [x0, #8]
	cbz	x8, LBB18_2
LBB18_1:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB18_2:
	mov	x22, x1
	mov	x19, x0
	mov	w0, #48                         ; =0x30
	bl	__Znwm
	mov	x20, x0
	str	xzr, [x0, #16]
	mov	x21, x0
	str	xzr, [x21, #24]!
Lloh18:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGE
Lloh19:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	stp	xzr, xzr, [x0, #32]
	stp	x21, x0, [sp]
	cbz	x22, LBB18_14
; %bb.3:
	tbnz	x22, #63, LBB18_15
; %bb.4:
Ltmp11:
	mov	x0, x22
	bl	__Znwm
	mov	x1, x0
Ltmp12:
; %bb.5:
	add	x8, x1, x22
	stp	x1, x1, [x20, #24]
	str	x8, [x20, #40]
	ldrb	w8, [x19, #3]
	cmp	w8, #255
	b.ne	LBB18_7
LBB18_6:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #3]
LBB18_7:
	mov	w4, w8
Ltmp16:
	add	x3, x19, x4
	mov	x0, x21
	mov	x2, x19
	bl	__ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l
Ltmp17:
; %bb.8:
	stp	xzr, xzr, [sp]
	ldr	x22, [x19, #16]
	stp	x21, x20, [x19, #8]
	cbz	x22, LBB18_11
; %bb.9:
	add	x8, x22, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB18_11
; %bb.10:
	ldr	x8, [x22]
	ldr	x8, [x8, #16]
	mov	x0, x22
	blr	x8
	mov	x0, x22
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB18_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x19, #3]
	ldr	x19, [sp, #8]
	cbz	x19, LBB18_1
; %bb.12:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB18_1
; %bb.13:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB18_14:
	mov	x1, #0                          ; =0x0
	ldrb	w8, [x19, #3]
	cmp	w8, #255
	b.ne	LBB18_7
	b	LBB18_6
LBB18_15:
Ltmp13:
	bl	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
Ltmp14:
; %bb.16:
	brk	#0x1
LBB18_17:
Ltmp18:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
LBB18_18:
Ltmp15:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorIhNS_9allocatorIhEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh18, Lloh19
Lfunc_end2:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table18:
Lexception2:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end2-Lcst_begin2
Lcst_begin2:
	.uleb128 Lfunc_begin2-Lfunc_begin2      ; >> Call Site 1 <<
	.uleb128 Ltmp11-Lfunc_begin2            ;   Call between Lfunc_begin2 and Ltmp11
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp11-Lfunc_begin2            ; >> Call Site 2 <<
	.uleb128 Ltmp12-Ltmp11                  ;   Call between Ltmp11 and Ltmp12
	.uleb128 Ltmp15-Lfunc_begin2            ;     jumps to Ltmp15
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp16-Lfunc_begin2            ; >> Call Site 3 <<
	.uleb128 Ltmp17-Ltmp16                  ;   Call between Ltmp16 and Ltmp17
	.uleb128 Ltmp18-Lfunc_begin2            ;     jumps to Ltmp18
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp17-Lfunc_begin2            ; >> Call Site 4 <<
	.uleb128 Ltmp13-Ltmp17                  ;   Call between Ltmp17 and Ltmp13
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp13-Lfunc_begin2            ; >> Call Site 5 <<
	.uleb128 Ltmp14-Ltmp13                  ;   Call between Ltmp13 and Ltmp14
	.uleb128 Ltmp15-Lfunc_begin2            ;     jumps to Ltmp15
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp14-Lfunc_begin2            ; >> Call Site 6 <<
	.uleb128 Lfunc_end2-Ltmp14              ;   Call between Ltmp14 and Lfunc_end2
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end2:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l ; -- Begin function _ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l
	.weak_def_can_be_hidden	__ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l
	.p2align	2
__ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l: ; @_ZNSt3__16vectorIhNS_9allocatorIhEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKhS7_EENS_11__wrap_iterIPhEENS8_IS7_EET0_T1_l
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-96]!           ; 16-byte Folded Spill
	stp	x26, x25, [sp, #16]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #32]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #48]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #64]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #80]             ; 16-byte Folded Spill
	add	x29, sp, #80
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x19, x1
	cmp	x4, #1
	b.lt	LBB19_37
; %bb.1:
	mov	x21, x4
	mov	x20, x2
	ldp	x27, x8, [x0, #8]
	sub	x9, x8, x27
	cmp	x4, x9
	b.le	LBB19_5
; %bb.2:
	ldr	x22, [x0]
	sub	x9, x27, x22
	adds	x9, x9, x21
	b.mi	LBB19_38
; %bb.3:
	mov	x28, x0
	sub	x23, x19, x22
	mov	x10, #4611686018427387903       ; =0x3fffffffffffffff
	sub	x8, x8, x22
	lsl	x11, x8, #1
	cmp	x11, x9
	csel	x9, x11, x9, hi
	cmp	x8, x10
	mov	x8, #9223372036854775807        ; =0x7fffffffffffffff
	csel	x26, x9, x8, lo
	cbz	x26, LBB19_19
; %bb.4:
	mov	x0, x26
	bl	__Znwm
	mov	x25, x0
	b	LBB19_20
LBB19_5:
	sub	x22, x27, x19
	cmp	x21, x22
	b.le	LBB19_12
; %bb.6:
	add	x9, x20, x22
	cmp	x9, x3
	b.eq	LBB19_23
; %bb.7:
	add	x8, x3, x19
	sub	x8, x8, x20
	sub	x10, x8, x27
	cmp	x10, #64
	b.lo	LBB19_24
; %bb.8:
	sub	x11, x19, x20
	mov	x8, x27
	cmp	x11, #64
	b.lo	LBB19_25
; %bb.9:
	and	x11, x10, #0xffffffffffffffc0
	add	x9, x9, x11
	add	x8, x27, x11
	add	x12, x27, #32
	sub	x13, x12, x19
	add	x13, x20, x13
	mov	x14, x11
LBB19_10:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x13, #-32]
	ldp	q2, q3, [x13], #64
	stp	q0, q1, [x12, #-32]
	stp	q2, q3, [x12], #64
	subs	x14, x14, #64
	b.ne	LBB19_10
; %bb.11:
	cmp	x10, x11
	b.ne	LBB19_25
	b	LBB19_26
LBB19_12:
	add	x8, x19, x21
	sub	x2, x27, x8
	sub	x8, x27, x21
	mov	x9, x27
	cmp	x8, x27
	b.hs	LBB19_18
; %bb.13:
	cmp	x21, #64
	b.lo	LBB19_17
; %bb.14:
	and	x10, x21, #0x7fffffffffffffc0
	add	x8, x8, x10
	add	x9, x27, x10
	add	x11, x27, #32
	sub	x12, x11, x21
	mov	x13, x10
LBB19_15:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x12, #-32]
	ldp	q2, q3, [x12], #64
	stp	q0, q1, [x11, #-32]
	stp	q2, q3, [x11], #64
	subs	x13, x13, #64
	b.ne	LBB19_15
; %bb.16:
	cmp	x21, x10
	b.eq	LBB19_18
LBB19_17:                               ; =>This Inner Loop Header: Depth=1
	ldrb	w10, [x8], #1
	strb	w10, [x9], #1
	cmp	x8, x27
	b.ne	LBB19_17
LBB19_18:
	str	x9, [x0, #8]
	sub	x0, x27, x2
	mov	x1, x19
	bl	_memmove
	mov	x0, x19
	mov	x1, x20
	mov	x2, x21
	b	LBB19_36
LBB19_19:
	mov	x25, #0                         ; =0x0
LBB19_20:
	add	x24, x25, x23
	mov	x0, x24
	mov	x1, x20
	mov	x2, x21
	bl	_memcpy
	add	x26, x25, x26
	add	x20, x24, x21
	sub	x21, x27, x19
	mov	x0, x20
	mov	x1, x19
	mov	x2, x21
	bl	_memcpy
	add	x20, x20, x21
	str	x19, [x28, #8]
	mov	x0, x25
	mov	x1, x22
	mov	x2, x23
	bl	_memcpy
	stp	x25, x20, [x28]
	str	x26, [x28, #16]
	cbz	x22, LBB19_22
; %bb.21:
	mov	x0, x22
	bl	__ZdlPv
LBB19_22:
	mov	x19, x24
	b	LBB19_37
LBB19_23:
	mov	x8, x27
	b	LBB19_26
LBB19_24:
	mov	x8, x27
LBB19_25:                               ; =>This Inner Loop Header: Depth=1
	ldrb	w10, [x9], #1
	strb	w10, [x8], #1
	cmp	x9, x3
	b.ne	LBB19_25
LBB19_26:
	str	x8, [x0, #8]
	cmp	x22, #1
	b.lt	LBB19_37
; %bb.27:
	add	x9, x19, x21
	sub	x2, x8, x9
	sub	x9, x8, x21
	cmp	x9, x27
	b.hs	LBB19_34
; %bb.28:
	add	x10, x21, x27
	sub	x11, x10, x8
	mov	x10, x8
	cmp	x11, #64
	b.lo	LBB19_33
; %bb.29:
	cmp	x21, #64
	b.lo	LBB19_33
; %bb.30:
	and	x12, x11, #0xffffffffffffffc0
	add	x9, x9, x12
	add	x10, x8, x12
	add	x13, x8, #32
	sub	x14, x13, x21
	mov	x15, x12
LBB19_31:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x14, #-32]
	ldp	q2, q3, [x14], #64
	stp	q0, q1, [x13, #-32]
	stp	q2, q3, [x13], #64
	subs	x15, x15, #64
	b.ne	LBB19_31
; %bb.32:
	cmp	x11, x12
	b.eq	LBB19_35
LBB19_33:                               ; =>This Inner Loop Header: Depth=1
	ldrb	w11, [x9], #1
	strb	w11, [x10], #1
	cmp	x9, x27
	b.ne	LBB19_33
	b	LBB19_35
LBB19_34:
	mov	x10, x8
LBB19_35:
	str	x10, [x0, #8]
	sub	x0, x8, x2
	mov	x1, x19
	bl	_memmove
	mov	x0, x19
	mov	x1, x20
	mov	x2, x22
LBB19_36:
	bl	_memmove
LBB19_37:
	mov	x0, x19
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #64]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #48]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #32]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #96             ; 16-byte Folded Reload
	ret
LBB19_38:
	bl	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.cfi_endproc
                                        ; -- End function
	.globl	__ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv ; -- Begin function _ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv
	.weak_def_can_be_hidden	__ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv
	.p2align	2
__ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv: ; @_ZZNSt3__16vectorIhNS_9allocatorIhEEE12emplace_backIJRKhEEERhDpOT_ENKUlvE0_clEv
	.cfi_startproc
; %bb.0:
	stp	x26, x25, [sp, #-80]!           ; 16-byte Folded Spill
	stp	x24, x23, [sp, #16]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	ldr	x24, [x0, #16]
	ldp	x20, x8, [x24]
	sub	x21, x8, x20
	adds	x8, x21, #1
	b.mi	LBB20_7
; %bb.1:
	mov	x19, x0
	ldr	x25, [x0, #8]
	ldr	x9, [x24, #16]
	mov	x10, #4611686018427387903       ; =0x3fffffffffffffff
	sub	x9, x9, x20
	lsl	x11, x9, #1
	cmp	x11, x8
	csel	x8, x11, x8, hi
	cmp	x9, x10
	mov	x9, #9223372036854775807        ; =0x7fffffffffffffff
	csel	x22, x8, x9, lo
	cbz	x22, LBB20_3
; %bb.2:
	mov	x0, x22
	bl	__Znwm
	mov	x23, x0
	b	LBB20_4
LBB20_3:
	mov	x23, #0                         ; =0x0
LBB20_4:
	add	x26, x23, x21
	add	x22, x23, x22
	ldrb	w8, [x25]
	strb	w8, [x26], #1
	mov	x0, x23
	mov	x1, x20
	mov	x2, x21
	bl	_memcpy
	stp	x23, x26, [x24]
	str	x22, [x24, #16]
	cbz	x20, LBB20_6
; %bb.5:
	mov	x0, x20
	bl	__ZdlPv
LBB20_6:
	ldr	x8, [x19]
	str	x26, [x8]
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #16]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp], #80             ; 16-byte Folded Reload
	ret
LBB20_7:
	bl	__ZNSt3__16vectorIhNS_9allocatorIhEEE20__throw_length_errorB9nqe220106Ev
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseItjLj4EEEE13copy_on_writeEv
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #80
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x20, x0
	ldr	x8, [x20, #8]!
	cbz	x8, LBB21_6
; %bb.1:
	ldr	x9, [x0, #16]
	cbz	x9, LBB21_8
; %bb.2:
	ldr	x9, [x9, #8]
	add	x9, x9, #1
	cbz	x9, LBB21_7
LBB21_3:
	cmp	x9, #2
	b.lt	LBB21_19
; %bb.4:
	cbz	x8, LBB21_20
; %bb.5:
	mov	x19, x0
	ldp	x1, x8, [x8]
	sub	x8, x8, x1
	asr	x8, x8, #1
	add	x2, x1, x8, lsl #1
	add	x21, sp, #8
	add	x0, sp, #8
	bl	__ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_
	cmp	x19, x21
	b.ne	LBB21_21
	b	LBB21_33
LBB21_6:
	ldrb	w9, [x0, #6]
	cmp	w9, #255
	cset	w9, ne
	cbnz	x9, LBB21_3
LBB21_7:
	ldr	x19, [x0, #16]
	strb	wzr, [sp, #14]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbnz	x19, LBB21_9
	b	LBB21_13
LBB21_8:
	mov	x19, #0                         ; =0x0
	strb	wzr, [sp, #14]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbz	x19, LBB21_13
LBB21_9:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB21_12
; %bb.10:
	ldr	x8, [x20]
	cbz	x8, LBB21_13
LBB21_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #6]
	b	LBB21_15
LBB21_12:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
	ldr	x8, [x20]
	cbnz	x8, LBB21_11
LBB21_13:
	ldrb	w8, [sp, #14]
	strb	w8, [x0, #6]
	cmp	x8, #255
	b.eq	LBB21_15
; %bb.14:
	lsl	x2, x8, #1
	add	x1, sp, #8
	bl	_memcpy
LBB21_15:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB21_19
; %bb.16:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB21_18
; %bb.17:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB21_18:
	ldr	x19, [sp, #24]
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #14]
	cbnz	x19, LBB21_34
LBB21_19:
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB21_20:
	ldrb	w8, [x0, #6]
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	mov	x19, x0
	mov	x1, x0
	add	x2, x0, x8, lsl #1
	add	x21, sp, #8
	add	x0, sp, #8
	bl	__ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_
	cmp	x19, x21
	b.eq	LBB21_33
LBB21_21:
	mov	x0, x19
	ldur	q0, [sp, #16]
	stp	xzr, xzr, [sp, #16]
	ldr	x19, [x19, #16]
	stur	q0, [x0, #8]
	cbz	x19, LBB21_26
; %bb.22:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB21_24
; %bb.23:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
LBB21_24:
	ldr	x8, [x20]
	cbz	x8, LBB21_27
LBB21_25:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #6]
	b	LBB21_29
LBB21_26:
	fmov	x8, d0
	cbnz	x8, LBB21_25
LBB21_27:
	ldrb	w8, [sp, #14]
	strb	w8, [x0, #6]
	cmp	x8, #255
	b.eq	LBB21_29
; %bb.28:
	lsl	x2, x8, #1
	add	x1, sp, #8
	bl	_memcpy
LBB21_29:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB21_32
; %bb.30:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB21_32
; %bb.31:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB21_32:
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #14]
LBB21_33:
	ldr	x19, [sp, #24]
	cbz	x19, LBB21_19
LBB21_34:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB21_19
; %bb.35:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev ; -- Begin function _ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev: ; @_ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #16]
	cbz	x19, LBB22_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB22_3
LBB22_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB22_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev ; -- Begin function _ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	.globl	__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	.p2align	2
__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev: ; @_ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #8]
	cbz	x19, LBB23_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB23_3
LBB23_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB23_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_ ; -- Begin function _ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_: ; @_ZN2GD2Bc7Details12DigitStorageItLm3EEC1EPKtS5_
Lfunc_begin3:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception3
; %bb.0:
	sub	sp, sp, #80
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x0
	stp	x2, x1, [sp, #24]
	strb	wzr, [x0, #6]
	stp	xzr, xzr, [x0, #8]
	subs	x2, x2, x1
	asr	x8, x2, #1
	csel	x20, xzr, x8, eq
	cmp	x20, #3
	b.hi	LBB24_4
; %bb.1:
	cbz	x20, LBB24_3
; %bb.2:
	mov	x0, x19
	bl	_memmove
	ldrb	w8, [x19, #6]
	cmp	w8, #255
	b.eq	LBB24_12
LBB24_3:
	strb	w20, [x19, #6]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB24_4:
Ltmp19:
	mov	x8, sp
	sub	x0, x29, #17
	add	x1, sp, #32
	add	x2, sp, #24
	bl	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
Ltmp20:
; %bb.5:
	ldr	q0, [sp]
	stp	xzr, xzr, [sp]
	ldr	x20, [x19, #16]
	stur	q0, [x19, #8]
	cbz	x20, LBB24_8
; %bb.6:
	add	x8, x20, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB24_8
; %bb.7:
	ldr	x8, [x20]
	ldr	x8, [x8, #16]
	mov	x0, x20
	blr	x8
	mov	x0, x20
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB24_8:
	ldr	x20, [sp, #8]
	cbz	x20, LBB24_11
; %bb.9:
	add	x8, x20, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB24_11
; %bb.10:
	ldr	x8, [x20]
	ldr	x8, [x8, #16]
	mov	x0, x20
	blr	x8
	mov	x0, x20
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB24_11:
	mov	w20, #255                       ; =0xff
	strb	w20, [x19, #6]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB24_12:
	strb	wzr, [x19, #6]
	strb	w20, [x19, #6]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB24_13:
Ltmp21:
	mov	x20, x0
	mov	x0, x19
	bl	__ZN2GD2Bc7Details12DigitStorageItLm3EED1Ev
	mov	x0, x20
	bl	__Unwind_Resume
Lfunc_end3:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table24:
Lexception3:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end3-Lcst_begin3
Lcst_begin3:
	.uleb128 Lfunc_begin3-Lfunc_begin3      ; >> Call Site 1 <<
	.uleb128 Ltmp19-Lfunc_begin3            ;   Call between Lfunc_begin3 and Ltmp19
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp19-Lfunc_begin3            ; >> Call Site 2 <<
	.uleb128 Ltmp20-Ltmp19                  ;   Call between Ltmp19 and Ltmp20
	.uleb128 Ltmp21-Lfunc_begin3            ;     jumps to Ltmp21
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp20-Lfunc_begin3            ; >> Call Site 3 <<
	.uleb128 Lfunc_end3-Ltmp20              ;   Call between Ltmp20 and Lfunc_end3
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end3:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.private_extern	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_ ; -- Begin function _ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.globl	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.weak_def_can_be_hidden	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.p2align	2
__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_: ; @_ZNSt3__115allocate_sharedB9nqe220106INS_6vectorItNS_9allocatorItEEEENS2_IS4_EEJRPKtS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
Lfunc_begin4:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception4
; %bb.0:
	stp	x24, x23, [sp, #-64]!           ; 16-byte Folded Spill
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov	x22, x2
	mov	x21, x1
	mov	x20, x8
	mov	w0, #48                         ; =0x30
	bl	__Znwm
	mov	x19, x0
	str	xzr, [x0, #16]
	mov	x24, x0
	str	xzr, [x24, #24]!
Lloh20:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGE
Lloh21:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	ldr	x21, [x21]
	ldr	x8, [x22]
	stp	xzr, xzr, [x0, #32]
	subs	x22, x8, x21
	b.eq	LBB25_4
; %bb.1:
	tbnz	x22, #63, LBB25_5
; %bb.2:
Ltmp22:
	mov	x0, x22
	bl	__Znwm
Ltmp23:
; %bb.3:
	mov	x23, x0
	str	x0, [x19, #24]
	add	x8, x0, x22
	str	x8, [x19, #40]
	and	x22, x22, #0x7ffffffffffffffe
	mov	x1, x21
	mov	x2, x22
	bl	_memcpy
	add	x8, x23, x22
	str	x8, [x19, #32]
LBB25_4:
	stp	x24, x19, [x20]
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp], #64             ; 16-byte Folded Reload
	ret
LBB25_5:
Ltmp24:
	bl	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
Ltmp25:
; %bb.6:
	brk	#0x1
LBB25_7:
Ltmp26:
	mov	x20, x0
	ldr	x0, [x24]
	cbz	x0, LBB25_9
; %bb.8:
	str	x0, [x19, #32]
	bl	__ZdlPv
LBB25_9:
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_countD2Ev
	bl	__ZdlPv
	mov	x0, x20
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh20, Lloh21
Lfunc_end4:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table25:
Lexception4:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end4-Lcst_begin4
Lcst_begin4:
	.uleb128 Lfunc_begin4-Lfunc_begin4      ; >> Call Site 1 <<
	.uleb128 Ltmp22-Lfunc_begin4            ;   Call between Lfunc_begin4 and Ltmp22
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp22-Lfunc_begin4            ; >> Call Site 2 <<
	.uleb128 Ltmp23-Ltmp22                  ;   Call between Ltmp22 and Ltmp23
	.uleb128 Ltmp26-Lfunc_begin4            ;     jumps to Ltmp26
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp23-Lfunc_begin4            ; >> Call Site 3 <<
	.uleb128 Ltmp24-Ltmp23                  ;   Call between Ltmp23 and Ltmp24
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp24-Lfunc_begin4            ; >> Call Site 4 <<
	.uleb128 Ltmp25-Ltmp24                  ;   Call between Ltmp24 and Ltmp25
	.uleb128 Ltmp26-Lfunc_begin4            ;     jumps to Ltmp26
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp25-Lfunc_begin4            ; >> Call Site 5 <<
	.uleb128 Lfunc_end4-Ltmp25              ;   Call between Ltmp25 and Lfunc_end4
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end4:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev
	.cfi_startproc
; %bb.0:
Lloh22:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGE
Lloh23:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	b	__ZNSt3__119__shared_weak_countD2Ev
	.loh AdrpLdrGot	Lloh22, Lloh23
	.cfi_endproc
                                        ; -- End function
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh24:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGE
Lloh25:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	bl	__ZNSt3__119__shared_weak_countD2Ev
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	b	__ZdlPv
	.loh AdrpLdrGot	Lloh24, Lloh25
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv
	.cfi_startproc
; %bb.0:
	mov	x8, x0
	ldr	x0, [x0, #24]
	cbz	x0, LBB28_2
; %bb.1:
	str	x0, [x8, #32]
	b	__ZdlPv
LBB28_2:
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.cfi_startproc
; %bb.0:
	b	__ZdlPv
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev ; -- Begin function _ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
	.globl	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
	.p2align	2
__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev: ; @_ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh26:
	adrp	x0, l_.str@PAGE
Lloh27:
	add	x0, x0, l_.str@PAGEOFF
	bl	__ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.loh AdrpAdd	Lloh26, Lloh27
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm ; -- Begin function _ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm: ; @_ZN2GD2Bc7Details12DigitStorageItLm3EE7promoteEm
Lfunc_begin5:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception5
; %bb.0:
	sub	sp, sp, #64
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	ldr	x8, [x0, #8]
	cbz	x8, LBB31_2
LBB31_1:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB31_2:
	mov	x22, x1
	mov	x19, x0
	mov	w0, #48                         ; =0x30
	bl	__Znwm
	mov	x20, x0
	str	xzr, [x0, #16]
	mov	x21, x0
	str	xzr, [x21, #24]!
Lloh28:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGE
Lloh29:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	stp	xzr, xzr, [x0, #32]
	stp	x21, x0, [sp]
	cbz	x22, LBB31_14
; %bb.3:
	tbnz	x22, #63, LBB31_15
; %bb.4:
	lsl	x0, x22, #1
Ltmp27:
	bl	__Znwm
	mov	x1, x0
Ltmp28:
; %bb.5:
	add	x8, x1, x22, lsl #1
	stp	x1, x1, [x20, #24]
	str	x8, [x20, #40]
	ldrb	w8, [x19, #6]
	cmp	w8, #255
	b.ne	LBB31_7
LBB31_6:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #6]
LBB31_7:
	mov	w4, w8
	add	x3, x19, w8, uxtw #1
Ltmp32:
	mov	x0, x21
	mov	x2, x19
	bl	__ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l
Ltmp33:
; %bb.8:
	stp	xzr, xzr, [sp]
	ldr	x22, [x19, #16]
	stp	x21, x20, [x19, #8]
	cbz	x22, LBB31_11
; %bb.9:
	add	x8, x22, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB31_11
; %bb.10:
	ldr	x8, [x22]
	ldr	x8, [x8, #16]
	mov	x0, x22
	blr	x8
	mov	x0, x22
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB31_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x19, #6]
	ldr	x19, [sp, #8]
	cbz	x19, LBB31_1
; %bb.12:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB31_1
; %bb.13:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB31_14:
	mov	x1, #0                          ; =0x0
	ldrb	w8, [x19, #6]
	cmp	w8, #255
	b.ne	LBB31_7
	b	LBB31_6
LBB31_15:
Ltmp29:
	bl	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
Ltmp30:
; %bb.16:
	brk	#0x1
LBB31_17:
Ltmp34:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
LBB31_18:
Ltmp31:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorItNS_9allocatorItEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh28, Lloh29
Lfunc_end5:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table31:
Lexception5:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end5-Lcst_begin5
Lcst_begin5:
	.uleb128 Lfunc_begin5-Lfunc_begin5      ; >> Call Site 1 <<
	.uleb128 Ltmp27-Lfunc_begin5            ;   Call between Lfunc_begin5 and Ltmp27
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp27-Lfunc_begin5            ; >> Call Site 2 <<
	.uleb128 Ltmp28-Ltmp27                  ;   Call between Ltmp27 and Ltmp28
	.uleb128 Ltmp31-Lfunc_begin5            ;     jumps to Ltmp31
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp32-Lfunc_begin5            ; >> Call Site 3 <<
	.uleb128 Ltmp33-Ltmp32                  ;   Call between Ltmp32 and Ltmp33
	.uleb128 Ltmp34-Lfunc_begin5            ;     jumps to Ltmp34
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp33-Lfunc_begin5            ; >> Call Site 4 <<
	.uleb128 Ltmp29-Ltmp33                  ;   Call between Ltmp33 and Ltmp29
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp29-Lfunc_begin5            ; >> Call Site 5 <<
	.uleb128 Ltmp30-Ltmp29                  ;   Call between Ltmp29 and Ltmp30
	.uleb128 Ltmp31-Lfunc_begin5            ;     jumps to Ltmp31
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp30-Lfunc_begin5            ; >> Call Site 6 <<
	.uleb128 Lfunc_end5-Ltmp30              ;   Call between Ltmp30 and Lfunc_end5
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end5:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l ; -- Begin function _ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l
	.weak_def_can_be_hidden	__ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l
	.p2align	2
__ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l: ; @_ZNSt3__16vectorItNS_9allocatorItEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKtS7_EENS_11__wrap_iterIPtEENS8_IS7_EET0_T1_l
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #112
	stp	x28, x27, [sp, #16]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #32]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #48]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #64]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #80]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #96]             ; 16-byte Folded Spill
	add	x29, sp, #96
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x19, x1
	cmp	x4, #1
	b.lt	LBB32_40
; %bb.1:
	mov	x20, x2
	ldp	x26, x8, [x0, #8]
	sub	x9, x8, x26
	cmp	x4, x9, asr #1
	b.le	LBB32_6
; %bb.2:
	ldr	x21, [x0]
	sub	x9, x26, x21
	adds	x9, x4, x9, asr #1
	b.mi	LBB32_41
; %bb.3:
	sub	x8, x8, x21
	cmp	x8, x9
	csel	x9, x8, x9, hi
	mov	x10, #9223372036854775806       ; =0x7ffffffffffffffe
	mov	x11, #9223372036854775807       ; =0x7fffffffffffffff
	cmp	x8, x10
	csel	x28, x9, x11, lo
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	cbz	x28, LBB32_20
; %bb.4:
	tbnz	x28, #63, LBB32_42
; %bb.5:
	mov	x22, x4
	lsl	x0, x28, #1
	bl	__Znwm
	mov	x23, x0
	mov	x4, x22
	b	LBB32_21
LBB32_6:
	sub	x21, x26, x19
	asr	x9, x21, #1
	cmp	x4, x9
	b.le	LBB32_13
; %bb.7:
	add	x10, x20, x21
	cmp	x10, x3
	b.eq	LBB32_26
; %bb.8:
	add	x8, x3, x19
	sub	x8, x8, x20
	sub	x8, x8, x26
	sub	x11, x8, #2
	cmp	x11, #62
	b.lo	LBB32_27
; %bb.9:
	sub	x12, x19, x20
	mov	x8, x26
	cmp	x12, #64
	b.lo	LBB32_28
; %bb.10:
	lsr	x8, x11, #1
	add	x11, x8, #1
	and	x12, x11, #0xffffffffffffffe0
	lsl	x8, x12, #1
	add	x10, x10, x8
	add	x8, x26, x8
	add	x13, x26, #32
	sub	x14, x13, x19
	add	x14, x20, x14
	mov	x15, x12
LBB32_11:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x14, #-32]
	ldp	q2, q3, [x14], #64
	stp	q0, q1, [x13, #-32]
	stp	q2, q3, [x13], #64
	subs	x15, x15, #32
	b.ne	LBB32_11
; %bb.12:
	cmp	x11, x12
	b.ne	LBB32_28
	b	LBB32_29
LBB32_13:
	lsl	x21, x4, #1
	add	x8, x19, x21
	sub	x2, x26, x8
	sub	x8, x26, x21
	cmp	x8, x26
	b.hs	LBB32_24
; %bb.14:
	add	x9, x8, #2
	cmp	x26, x9
	csel	x9, x26, x9, hi
	mvn	x10, x26
	add	x10, x21, x10
	add	x10, x9, x10
	mov	x9, x26
	cmp	x10, #62
	b.lo	LBB32_19
; %bb.15:
	cmp	x4, #32
	b.lo	LBB32_19
; %bb.16:
	lsr	x9, x10, #1
	add	x10, x9, #1
	and	x11, x10, #0xffffffffffffffe0
	lsl	x9, x11, #1
	add	x8, x8, x9
	add	x9, x26, x9
	add	x12, x26, #32
	sub	x13, x12, x21
	mov	x14, x11
LBB32_17:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x13, #-32]
	ldp	q2, q3, [x13], #64
	stp	q0, q1, [x12, #-32]
	stp	q2, q3, [x12], #64
	subs	x14, x14, #32
	b.ne	LBB32_17
; %bb.18:
	cmp	x10, x11
	b.eq	LBB32_25
LBB32_19:                               ; =>This Inner Loop Header: Depth=1
	ldrh	w10, [x8], #2
	strh	w10, [x9], #2
	cmp	x8, x26
	b.lo	LBB32_19
	b	LBB32_25
LBB32_20:
	mov	x23, #0                         ; =0x0
LBB32_21:
	sub	x24, x19, x21
	add	x22, x23, x24
	asr	x27, x24, #1
	lsl	x25, x4, #1
	mov	x0, x22
	mov	x1, x20
	mov	x2, x25
	bl	_memcpy
	add	x28, x23, x28, lsl #1
	add	x20, x22, x25
	sub	x23, x26, x19
	mov	x0, x20
	mov	x1, x19
	mov	x2, x23
	bl	_memcpy
	add	x20, x20, x23
	ldr	x23, [sp, #8]                   ; 8-byte Folded Reload
	str	x19, [x23, #8]
	sub	x19, x22, x27, lsl #1
	mov	x0, x19
	mov	x1, x21
	mov	x2, x24
	bl	_memcpy
	stp	x19, x20, [x23]
	str	x28, [x23, #16]
	cbz	x21, LBB32_23
; %bb.22:
	mov	x0, x21
	bl	__ZdlPv
LBB32_23:
	mov	x19, x22
	b	LBB32_40
LBB32_24:
	mov	x9, x26
LBB32_25:
	str	x9, [x0, #8]
	sub	x0, x26, x2
	b	LBB32_39
LBB32_26:
	mov	x8, x26
	b	LBB32_29
LBB32_27:
	mov	x8, x26
LBB32_28:                               ; =>This Inner Loop Header: Depth=1
	ldrh	w11, [x10], #2
	strh	w11, [x8], #2
	cmp	x10, x3
	b.ne	LBB32_28
LBB32_29:
	str	x8, [x0, #8]
	cmp	x9, #1
	b.lt	LBB32_40
; %bb.30:
	lsl	x13, x4, #1
	add	x9, x19, x13
	sub	x2, x8, x9
	sub	x9, x8, x13
	cmp	x9, x26
	b.hs	LBB32_37
; %bb.31:
	add	x10, x9, #2
	cmp	x26, x10
	csel	x10, x26, x10, hi
	mvn	x11, x8
	add	x11, x13, x11
	add	x11, x10, x11
	mov	x10, x8
	cmp	x11, #62
	b.lo	LBB32_36
; %bb.32:
	cmp	x4, #32
	b.lo	LBB32_36
; %bb.33:
	lsr	x10, x11, #1
	add	x11, x10, #1
	and	x12, x11, #0xffffffffffffffe0
	lsl	x10, x12, #1
	add	x9, x9, x10
	add	x10, x8, x10
	add	x14, x8, #32
	sub	x13, x14, x13
	mov	x15, x12
LBB32_34:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x13, #-32]
	ldp	q2, q3, [x13], #64
	stp	q0, q1, [x14, #-32]
	stp	q2, q3, [x14], #64
	subs	x15, x15, #32
	b.ne	LBB32_34
; %bb.35:
	cmp	x11, x12
	b.eq	LBB32_38
LBB32_36:                               ; =>This Inner Loop Header: Depth=1
	ldrh	w11, [x9], #2
	strh	w11, [x10], #2
	cmp	x9, x26
	b.lo	LBB32_36
	b	LBB32_38
LBB32_37:
	mov	x10, x8
LBB32_38:
	str	x10, [x0, #8]
	sub	x0, x8, x2
LBB32_39:
	mov	x1, x19
	bl	_memmove
	mov	x0, x19
	mov	x1, x20
	mov	x2, x21
	bl	_memmove
LBB32_40:
	mov	x0, x19
	ldp	x29, x30, [sp, #96]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #80]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #64]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #48]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #32]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #112
	ret
LBB32_41:
	bl	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
LBB32_42:
	bl	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.cfi_endproc
                                        ; -- End function
	.globl	__ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv ; -- Begin function _ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv
	.weak_def_can_be_hidden	__ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv
	.p2align	2
__ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv: ; @_ZZNSt3__16vectorItNS_9allocatorItEEE12emplace_backIJRKtEEERtDpOT_ENKUlvE0_clEv
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-96]!           ; 16-byte Folded Spill
	stp	x26, x25, [sp, #16]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #32]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #48]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #64]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #80]             ; 16-byte Folded Spill
	add	x29, sp, #80
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	ldr	x23, [x0, #16]
	ldp	x20, x8, [x23]
	sub	x21, x8, x20
	asr	x24, x21, #1
	cmn	x24, #2
	b.le	LBB33_5
; %bb.1:
	add	x8, x24, #1
	ldr	x9, [x23, #16]
	sub	x9, x9, x20
	cmp	x9, x8
	csinc	x8, x9, x24, hi
	mov	x10, #9223372036854775806       ; =0x7ffffffffffffffe
	mov	x11, #9223372036854775807       ; =0x7fffffffffffffff
	cmp	x9, x10
	csel	x8, x8, x11, lo
	tbnz	x8, #63, LBB33_6
; %bb.2:
	mov	x19, x0
	ldr	x26, [x0, #8]
	lsl	x22, x8, #1
	mov	x0, x22
	bl	__Znwm
	add	x25, x0, x21
	add	x27, x0, x22
	ldrh	w8, [x26]
	sub	x22, x25, x24, lsl #1
	strh	w8, [x25], #2
	mov	x0, x22
	mov	x1, x20
	mov	x2, x21
	bl	_memcpy
	stp	x22, x25, [x23]
	str	x27, [x23, #16]
	cbz	x20, LBB33_4
; %bb.3:
	mov	x0, x20
	bl	__ZdlPv
LBB33_4:
	ldr	x8, [x19]
	str	x25, [x8]
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #64]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #48]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #32]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #96             ; 16-byte Folded Reload
	ret
LBB33_5:
	bl	__ZNSt3__16vectorItNS_9allocatorItEEE20__throw_length_errorB9nqe220106Ev
LBB33_6:
	bl	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv ; -- Begin function _ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv
	.p2align	2
__ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv: ; @_ZN2GD2Bc7Details11BasicDigitsINS0_16NumberTraitsBaseIjyLj9EEEE13copy_on_writeEv
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #80
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x20, x0
	ldr	x8, [x20, #16]!
	cbz	x8, LBB34_6
; %bb.1:
	ldr	x9, [x0, #24]
	cbz	x9, LBB34_8
; %bb.2:
	ldr	x9, [x9, #8]
	add	x9, x9, #1
	cbz	x9, LBB34_7
LBB34_3:
	cmp	x9, #2
	b.lt	LBB34_19
; %bb.4:
	cbz	x8, LBB34_20
; %bb.5:
	mov	x19, x0
	ldp	x1, x8, [x8]
	sub	x8, x8, x1
	asr	x8, x8, #2
	add	x2, x1, x8, lsl #2
	mov	x21, sp
	mov	x0, sp
	bl	__ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_
	cmp	x19, x21
	b.ne	LBB34_21
	b	LBB34_33
LBB34_6:
	ldrb	w9, [x0, #12]
	cmp	w9, #255
	cset	w9, ne
	cbnz	x9, LBB34_3
LBB34_7:
	ldr	x19, [x0, #24]
	strb	wzr, [sp, #12]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbnz	x19, LBB34_9
	b	LBB34_13
LBB34_8:
	mov	x19, #0                         ; =0x0
	strb	wzr, [sp, #12]
	stp	xzr, xzr, [sp, #16]
	stp	xzr, xzr, [x20]
	cbz	x19, LBB34_13
LBB34_9:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB34_12
; %bb.10:
	ldr	x8, [x20]
	cbz	x8, LBB34_13
LBB34_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #12]
	b	LBB34_15
LBB34_12:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
	ldr	x8, [x20]
	cbnz	x8, LBB34_11
LBB34_13:
	ldrb	w8, [sp, #12]
	strb	w8, [x0, #12]
	cmp	x8, #255
	b.eq	LBB34_15
; %bb.14:
	lsl	x2, x8, #2
	mov	x1, sp
	bl	_memcpy
LBB34_15:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB34_19
; %bb.16:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB34_18
; %bb.17:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB34_18:
	ldr	x19, [sp, #24]
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #12]
	cbnz	x19, LBB34_34
LBB34_19:
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB34_20:
	ldrb	w8, [x0, #12]
	cmp	w8, #255
	csel	w8, wzr, w8, eq
	mov	x19, x0
	mov	x1, x0
	add	x2, x0, x8, lsl #2
	mov	x21, sp
	mov	x0, sp
	bl	__ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_
	cmp	x19, x21
	b.eq	LBB34_33
LBB34_21:
	mov	x0, x19
	ldr	q0, [sp, #16]
	stp	xzr, xzr, [sp, #16]
	ldr	x19, [x19, #24]
	str	q0, [x0, #16]
	cbz	x19, LBB34_26
; %bb.22:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB34_24
; %bb.23:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x21, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x21
LBB34_24:
	ldr	x8, [x20]
	cbz	x8, LBB34_27
LBB34_25:
	mov	w8, #255                        ; =0xff
	strb	w8, [x0, #12]
	b	LBB34_29
LBB34_26:
	fmov	x8, d0
	cbnz	x8, LBB34_25
LBB34_27:
	ldrb	w8, [sp, #12]
	strb	w8, [x0, #12]
	cmp	x8, #255
	b.eq	LBB34_29
; %bb.28:
	lsl	x2, x8, #2
	mov	x1, sp
	bl	_memcpy
LBB34_29:
	ldr	x19, [sp, #24]
	stp	xzr, xzr, [sp, #16]
	cbz	x19, LBB34_32
; %bb.30:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB34_32
; %bb.31:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB34_32:
	mov	w8, #255                        ; =0xff
	strb	w8, [sp, #12]
LBB34_33:
	ldr	x19, [sp, #24]
	cbz	x19, LBB34_19
LBB34_34:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB34_19
; %bb.35:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev: ; @_ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #24]
	cbz	x19, LBB35_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB35_3
LBB35_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB35_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev ; -- Begin function _ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	.globl	__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	.p2align	2
__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev: ; @_ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	x19, [x0, #8]
	cbz	x19, LBB36_2
; %bb.1:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbz	x8, LBB36_3
LBB36_2:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB36_3:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x20, x0
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_ ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_: ; @_ZN2GD2Bc7Details12DigitStorageIjLm3EEC1EPKjS5_
Lfunc_begin6:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception6
; %bb.0:
	sub	sp, sp, #80
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x0
	stp	x2, x1, [sp, #24]
	strb	wzr, [x0, #12]
	stp	xzr, xzr, [x0, #16]
	subs	x2, x2, x1
	asr	x8, x2, #2
	csel	x20, xzr, x8, eq
	cmp	x20, #3
	b.hi	LBB37_4
; %bb.1:
	cbz	x20, LBB37_3
; %bb.2:
	mov	x0, x19
	bl	_memmove
	ldrb	w8, [x19, #12]
	cmp	w8, #255
	b.eq	LBB37_12
LBB37_3:
	strb	w20, [x19, #12]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB37_4:
Ltmp35:
	mov	x8, sp
	sub	x0, x29, #17
	add	x1, sp, #32
	add	x2, sp, #24
	bl	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
Ltmp36:
; %bb.5:
	ldr	q0, [sp]
	stp	xzr, xzr, [sp]
	ldr	x20, [x19, #24]
	str	q0, [x19, #16]
	cbz	x20, LBB37_8
; %bb.6:
	add	x8, x20, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB37_8
; %bb.7:
	ldr	x8, [x20]
	ldr	x8, [x8, #16]
	mov	x0, x20
	blr	x8
	mov	x0, x20
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB37_8:
	ldr	x20, [sp, #8]
	cbz	x20, LBB37_11
; %bb.9:
	add	x8, x20, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB37_11
; %bb.10:
	ldr	x8, [x20]
	ldr	x8, [x8, #16]
	mov	x0, x20
	blr	x8
	mov	x0, x20
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB37_11:
	mov	w20, #255                       ; =0xff
	strb	w20, [x19, #12]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB37_12:
	strb	wzr, [x19, #12]
	strb	w20, [x19, #12]
	mov	x0, x19
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
LBB37_13:
Ltmp37:
	mov	x20, x0
	mov	x0, x19
	bl	__ZN2GD2Bc7Details12DigitStorageIjLm3EED1Ev
	mov	x0, x20
	bl	__Unwind_Resume
Lfunc_end6:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table37:
Lexception6:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end6-Lcst_begin6
Lcst_begin6:
	.uleb128 Lfunc_begin6-Lfunc_begin6      ; >> Call Site 1 <<
	.uleb128 Ltmp35-Lfunc_begin6            ;   Call between Lfunc_begin6 and Ltmp35
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp35-Lfunc_begin6            ; >> Call Site 2 <<
	.uleb128 Ltmp36-Ltmp35                  ;   Call between Ltmp35 and Ltmp36
	.uleb128 Ltmp37-Lfunc_begin6            ;     jumps to Ltmp37
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp36-Lfunc_begin6            ; >> Call Site 3 <<
	.uleb128 Lfunc_end6-Ltmp36              ;   Call between Ltmp36 and Lfunc_end6
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end6:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.private_extern	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_ ; -- Begin function _ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.globl	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.weak_def_can_be_hidden	__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
	.p2align	2
__ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_: ; @_ZNSt3__115allocate_sharedB9nqe220106INS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEJRPKjS8_ELi0EEENS_10shared_ptrIT_EERKT0_DpOT1_
Lfunc_begin7:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception7
; %bb.0:
	stp	x24, x23, [sp, #-64]!           ; 16-byte Folded Spill
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov	x22, x2
	mov	x21, x1
	mov	x20, x8
	mov	w0, #48                         ; =0x30
	bl	__Znwm
	mov	x19, x0
	str	xzr, [x0, #16]
	mov	x24, x0
	str	xzr, [x24, #24]!
Lloh30:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGE
Lloh31:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	ldr	x21, [x21]
	ldr	x8, [x22]
	stp	xzr, xzr, [x0, #32]
	subs	x22, x8, x21
	b.eq	LBB38_4
; %bb.1:
	tbnz	x22, #63, LBB38_5
; %bb.2:
Ltmp38:
	mov	x0, x22
	bl	__Znwm
Ltmp39:
; %bb.3:
	mov	x23, x0
	str	x0, [x19, #24]
	add	x8, x0, x22
	str	x8, [x19, #40]
	and	x22, x22, #0x7ffffffffffffffc
	mov	x1, x21
	mov	x2, x22
	bl	_memcpy
	add	x8, x23, x22
	str	x8, [x19, #32]
LBB38_4:
	stp	x24, x19, [x20]
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp], #64             ; 16-byte Folded Reload
	ret
LBB38_5:
Ltmp40:
	bl	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
Ltmp41:
; %bb.6:
	brk	#0x1
LBB38_7:
Ltmp42:
	mov	x20, x0
	ldr	x0, [x24]
	cbz	x0, LBB38_9
; %bb.8:
	str	x0, [x19, #32]
	bl	__ZdlPv
LBB38_9:
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_countD2Ev
	bl	__ZdlPv
	mov	x0, x20
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh30, Lloh31
Lfunc_end7:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table38:
Lexception7:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end7-Lcst_begin7
Lcst_begin7:
	.uleb128 Lfunc_begin7-Lfunc_begin7      ; >> Call Site 1 <<
	.uleb128 Ltmp38-Lfunc_begin7            ;   Call between Lfunc_begin7 and Ltmp38
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp38-Lfunc_begin7            ; >> Call Site 2 <<
	.uleb128 Ltmp39-Ltmp38                  ;   Call between Ltmp38 and Ltmp39
	.uleb128 Ltmp42-Lfunc_begin7            ;     jumps to Ltmp42
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp39-Lfunc_begin7            ; >> Call Site 3 <<
	.uleb128 Ltmp40-Ltmp39                  ;   Call between Ltmp39 and Ltmp40
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp40-Lfunc_begin7            ; >> Call Site 4 <<
	.uleb128 Ltmp41-Ltmp40                  ;   Call between Ltmp40 and Ltmp41
	.uleb128 Ltmp42-Lfunc_begin7            ;     jumps to Ltmp42
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp41-Lfunc_begin7            ; >> Call Site 5 <<
	.uleb128 Lfunc_end7-Ltmp41              ;   Call between Ltmp41 and Lfunc_end7
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end7:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev
	.cfi_startproc
; %bb.0:
Lloh32:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGE
Lloh33:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	b	__ZNSt3__119__shared_weak_countD2Ev
	.loh AdrpLdrGot	Lloh32, Lloh33
	.cfi_endproc
                                        ; -- End function
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh34:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGE
Lloh35:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	str	x8, [x0]
	bl	__ZNSt3__119__shared_weak_countD2Ev
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	b	__ZdlPv
	.loh AdrpLdrGot	Lloh34, Lloh35
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv
	.cfi_startproc
; %bb.0:
	mov	x8, x0
	ldr	x0, [x0, #24]
	cbz	x0, LBB41_2
; %bb.1:
	str	x0, [x8, #32]
	b	__ZdlPv
LBB41_2:
	ret
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv ; -- Begin function _ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.globl	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.weak_def_can_be_hidden	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.p2align	2
__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv: ; @_ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv
	.cfi_startproc
; %bb.0:
	b	__ZdlPv
	.cfi_endproc
                                        ; -- End function
	.private_extern	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev ; -- Begin function _ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
	.globl	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
	.weak_def_can_be_hidden	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
	.p2align	2
__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev: ; @_ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh36:
	adrp	x0, l_.str@PAGE
Lloh37:
	add	x0, x0, l_.str@PAGEOFF
	bl	__ZNSt3__120__throw_length_errorB9nqe220106EPKc
	.loh AdrpAdd	Lloh36, Lloh37
	.cfi_endproc
                                        ; -- End function
	.globl	__ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm ; -- Begin function _ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm
	.weak_def_can_be_hidden	__ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm
	.p2align	2
__ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm: ; @_ZN2GD2Bc7Details12DigitStorageIjLm3EE7promoteEm
Lfunc_begin8:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception8
; %bb.0:
	sub	sp, sp, #64
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	ldr	x8, [x0, #16]
	cbz	x8, LBB44_2
LBB44_1:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB44_2:
	mov	x22, x1
	mov	x19, x0
	mov	w0, #48                         ; =0x30
	bl	__Znwm
	mov	x20, x0
	str	xzr, [x0, #16]
	mov	x21, x0
	str	xzr, [x21, #24]!
Lloh38:
	adrp	x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGE
Lloh39:
	ldr	x8, [x8, __ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE@GOTPAGEOFF]
	add	x8, x8, #16
	stp	x8, xzr, [x0]
	stp	xzr, xzr, [x0, #32]
	stp	x21, x0, [sp]
	cbz	x22, LBB44_14
; %bb.3:
	lsr	x8, x22, #62
	cbnz	x8, LBB44_15
; %bb.4:
	lsl	x0, x22, #2
Ltmp43:
	bl	__Znwm
	mov	x1, x0
Ltmp44:
; %bb.5:
	add	x8, x1, x22, lsl #2
	stp	x1, x1, [x20, #24]
	str	x8, [x20, #40]
	ldrb	w8, [x19, #12]
	cmp	w8, #255
	b.ne	LBB44_7
LBB44_6:
	mov	w8, #0                          ; =0x0
	strb	wzr, [x19, #12]
LBB44_7:
	mov	w4, w8
	add	x3, x19, w8, uxtw #2
Ltmp48:
	mov	x0, x21
	mov	x2, x19
	bl	__ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l
Ltmp49:
; %bb.8:
	stp	xzr, xzr, [sp]
	ldr	x22, [x19, #24]
	stp	x21, x20, [x19, #16]
	cbz	x22, LBB44_11
; %bb.9:
	add	x8, x22, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB44_11
; %bb.10:
	ldr	x8, [x22]
	ldr	x8, [x8, #16]
	mov	x0, x22
	blr	x8
	mov	x0, x22
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
LBB44_11:
	mov	w8, #255                        ; =0xff
	strb	w8, [x19, #12]
	ldr	x19, [sp, #8]
	cbz	x19, LBB44_1
; %bb.12:
	add	x8, x19, #8
	mov	x9, #-1                         ; =0xffffffffffffffff
	ldaddal	x9, x8, [x8]
	cbnz	x8, LBB44_1
; %bb.13:
	ldr	x8, [x19]
	ldr	x8, [x8, #16]
	mov	x0, x19
	blr	x8
	mov	x0, x19
	bl	__ZNSt3__119__shared_weak_count14__release_weakEv
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB44_14:
	mov	x1, #0                          ; =0x0
	ldrb	w8, [x19, #12]
	cmp	w8, #255
	b.ne	LBB44_7
	b	LBB44_6
LBB44_15:
Ltmp45:
	bl	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
Ltmp46:
; %bb.16:
	brk	#0x1
LBB44_17:
Ltmp50:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
LBB44_18:
Ltmp47:
	mov	x19, x0
	mov	x0, sp
	bl	__ZNSt3__110shared_ptrINS_6vectorIjNS_9allocatorIjEEEEED1B9nqe220106Ev
	mov	x0, x19
	bl	__Unwind_Resume
	.loh AdrpLdrGot	Lloh38, Lloh39
Lfunc_end8:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table44:
Lexception8:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end8-Lcst_begin8
Lcst_begin8:
	.uleb128 Lfunc_begin8-Lfunc_begin8      ; >> Call Site 1 <<
	.uleb128 Ltmp43-Lfunc_begin8            ;   Call between Lfunc_begin8 and Ltmp43
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp43-Lfunc_begin8            ; >> Call Site 2 <<
	.uleb128 Ltmp44-Ltmp43                  ;   Call between Ltmp43 and Ltmp44
	.uleb128 Ltmp47-Lfunc_begin8            ;     jumps to Ltmp47
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp48-Lfunc_begin8            ; >> Call Site 3 <<
	.uleb128 Ltmp49-Ltmp48                  ;   Call between Ltmp48 and Ltmp49
	.uleb128 Ltmp50-Lfunc_begin8            ;     jumps to Ltmp50
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp49-Lfunc_begin8            ; >> Call Site 4 <<
	.uleb128 Ltmp45-Ltmp49                  ;   Call between Ltmp49 and Ltmp45
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp45-Lfunc_begin8            ; >> Call Site 5 <<
	.uleb128 Ltmp46-Ltmp45                  ;   Call between Ltmp45 and Ltmp46
	.uleb128 Ltmp47-Lfunc_begin8            ;     jumps to Ltmp47
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp46-Lfunc_begin8            ; >> Call Site 6 <<
	.uleb128 Lfunc_end8-Ltmp46              ;   Call between Ltmp46 and Lfunc_end8
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end8:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l ; -- Begin function _ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l
	.weak_def_can_be_hidden	__ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l
	.p2align	2
__ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l: ; @_ZNSt3__16vectorIjNS_9allocatorIjEEE18__insert_with_sizeB9nqe220106INS_17_ClassicAlgPolicyEPKjS7_EENS_11__wrap_iterIPjEENS8_IS7_EET0_T1_l
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #112
	stp	x28, x27, [sp, #16]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #32]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #48]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #64]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #80]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #96]             ; 16-byte Folded Spill
	add	x29, sp, #96
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x19, x1
	cmp	x4, #1
	b.lt	LBB45_38
; %bb.1:
	mov	x20, x2
	ldp	x26, x8, [x0, #8]
	sub	x9, x8, x26
	cmp	x4, x9, asr #2
	b.le	LBB45_6
; %bb.2:
	ldr	x21, [x0]
	sub	x9, x26, x21
	add	x9, x4, x9, asr #2
	lsr	x10, x9, #62
	cbnz	x10, LBB45_39
; %bb.3:
	mov	x10, #9223372036854775804       ; =0x7ffffffffffffffc
	sub	x8, x8, x21
	asr	x11, x8, #1
	cmp	x11, x9
	csel	x9, x11, x9, hi
	cmp	x8, x10
	mov	x8, #4611686018427387903        ; =0x3fffffffffffffff
	csel	x28, x9, x8, lo
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	cbz	x28, LBB45_19
; %bb.4:
	lsr	x8, x28, #62
	cbnz	x8, LBB45_40
; %bb.5:
	mov	x22, x4
	lsl	x0, x28, #2
	bl	__Znwm
	mov	x23, x0
	mov	x4, x22
	b	LBB45_20
LBB45_6:
	sub	x21, x26, x19
	asr	x9, x21, #2
	cmp	x4, x9
	b.le	LBB45_13
; %bb.7:
	add	x10, x20, x21
	cmp	x10, x3
	b.eq	LBB45_25
; %bb.8:
	add	x8, x3, x19
	sub	x8, x8, x20
	sub	x8, x8, x26
	sub	x11, x8, #4
	cmp	x11, #60
	b.lo	LBB45_26
; %bb.9:
	sub	x12, x19, x20
	mov	x8, x26
	cmp	x12, #64
	b.lo	LBB45_27
; %bb.10:
	lsr	x8, x11, #2
	add	x11, x8, #1
	and	x12, x11, #0x7ffffffffffffff0
	lsl	x8, x12, #2
	add	x10, x10, x8
	add	x8, x26, x8
	add	x13, x26, #32
	sub	x14, x13, x19
	add	x14, x20, x14
	mov	x15, x12
LBB45_11:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x14, #-32]
	ldp	q2, q3, [x14], #64
	stp	q0, q1, [x13, #-32]
	stp	q2, q3, [x13], #64
	subs	x15, x15, #16
	b.ne	LBB45_11
; %bb.12:
	cmp	x11, x12
	b.ne	LBB45_27
	b	LBB45_28
LBB45_13:
	lsl	x21, x4, #2
	add	x8, x19, x21
	sub	x2, x26, x8
	sub	x8, x26, x21
	cmp	x8, x26
	b.hs	LBB45_23
; %bb.14:
	add	x9, x8, #4
	cmp	x26, x9
	csel	x9, x26, x9, hi
	mvn	x10, x26
	add	x10, x21, x10
	add	x10, x9, x10
	and	x9, x4, #0x3ffffffffffffff0
	cmp	x10, #60
	ccmp	x9, #0, #4, hs
	mov	x9, x26
	b.eq	LBB45_18
; %bb.15:
	lsr	x9, x10, #2
	add	x10, x9, #1
	and	x11, x10, #0x7ffffffffffffff0
	lsl	x9, x11, #2
	add	x8, x8, x9
	add	x9, x26, x9
	add	x12, x26, #32
	sub	x13, x12, x21
	mov	x14, x11
LBB45_16:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x13, #-32]
	ldp	q2, q3, [x13], #64
	stp	q0, q1, [x12, #-32]
	stp	q2, q3, [x12], #64
	subs	x14, x14, #16
	b.ne	LBB45_16
; %bb.17:
	cmp	x10, x11
	b.eq	LBB45_24
LBB45_18:                               ; =>This Inner Loop Header: Depth=1
	ldr	w10, [x8], #4
	str	w10, [x9], #4
	cmp	x8, x26
	b.lo	LBB45_18
	b	LBB45_24
LBB45_19:
	mov	x23, #0                         ; =0x0
LBB45_20:
	sub	x24, x19, x21
	add	x22, x23, x24
	asr	x27, x24, #2
	lsl	x25, x4, #2
	mov	x0, x22
	mov	x1, x20
	mov	x2, x25
	bl	_memcpy
	add	x28, x23, x28, lsl #2
	add	x20, x22, x25
	sub	x23, x26, x19
	mov	x0, x20
	mov	x1, x19
	mov	x2, x23
	bl	_memcpy
	add	x20, x20, x23
	ldr	x23, [sp, #8]                   ; 8-byte Folded Reload
	str	x19, [x23, #8]
	sub	x19, x22, x27, lsl #2
	mov	x0, x19
	mov	x1, x21
	mov	x2, x24
	bl	_memcpy
	stp	x19, x20, [x23]
	str	x28, [x23, #16]
	cbz	x21, LBB45_22
; %bb.21:
	mov	x0, x21
	bl	__ZdlPv
LBB45_22:
	mov	x19, x22
	b	LBB45_38
LBB45_23:
	mov	x9, x26
LBB45_24:
	str	x9, [x0, #8]
	sub	x0, x26, x2
	b	LBB45_37
LBB45_25:
	mov	x8, x26
	b	LBB45_28
LBB45_26:
	mov	x8, x26
LBB45_27:                               ; =>This Inner Loop Header: Depth=1
	ldr	w11, [x10], #4
	str	w11, [x8], #4
	cmp	x10, x3
	b.ne	LBB45_27
LBB45_28:
	str	x8, [x0, #8]
	cmp	x9, #1
	b.lt	LBB45_38
; %bb.29:
	lsl	x13, x4, #2
	add	x9, x19, x13
	sub	x2, x8, x9
	sub	x9, x8, x13
	cmp	x9, x26
	b.hs	LBB45_35
; %bb.30:
	add	x10, x9, #4
	cmp	x26, x10
	csel	x10, x26, x10, hi
	mvn	x11, x8
	add	x11, x13, x11
	add	x11, x10, x11
	and	x10, x4, #0x3ffffffffffffff0
	cmp	x11, #60
	ccmp	x10, #0, #4, hs
	mov	x10, x8
	b.eq	LBB45_34
; %bb.31:
	lsr	x10, x11, #2
	add	x11, x10, #1
	and	x12, x11, #0x7ffffffffffffff0
	lsl	x10, x12, #2
	add	x9, x9, x10
	add	x10, x8, x10
	add	x14, x8, #32
	sub	x13, x14, x13
	mov	x15, x12
LBB45_32:                               ; =>This Inner Loop Header: Depth=1
	ldp	q0, q1, [x13, #-32]
	ldp	q2, q3, [x13], #64
	stp	q0, q1, [x14, #-32]
	stp	q2, q3, [x14], #64
	subs	x15, x15, #16
	b.ne	LBB45_32
; %bb.33:
	cmp	x11, x12
	b.eq	LBB45_36
LBB45_34:                               ; =>This Inner Loop Header: Depth=1
	ldr	w11, [x9], #4
	str	w11, [x10], #4
	cmp	x9, x26
	b.lo	LBB45_34
	b	LBB45_36
LBB45_35:
	mov	x10, x8
LBB45_36:
	str	x10, [x0, #8]
	sub	x0, x8, x2
LBB45_37:
	mov	x1, x19
	bl	_memmove
	mov	x0, x19
	mov	x1, x20
	mov	x2, x21
	bl	_memmove
LBB45_38:
	mov	x0, x19
	ldp	x29, x30, [sp, #96]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #80]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #64]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #48]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #32]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #112
	ret
LBB45_39:
	bl	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
LBB45_40:
	bl	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.cfi_endproc
                                        ; -- End function
	.globl	__ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv ; -- Begin function _ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv
	.weak_def_can_be_hidden	__ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv
	.p2align	2
__ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv: ; @_ZZNSt3__16vectorIjNS_9allocatorIjEEE12emplace_backIJRKjEEERjDpOT_ENKUlvE0_clEv
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-96]!           ; 16-byte Folded Spill
	stp	x26, x25, [sp, #16]             ; 16-byte Folded Spill
	stp	x24, x23, [sp, #32]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #48]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #64]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #80]             ; 16-byte Folded Spill
	add	x29, sp, #80
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	ldr	x23, [x0, #16]
	ldp	x20, x8, [x23]
	sub	x21, x8, x20
	asr	x24, x21, #2
	add	x8, x24, #1
	lsr	x9, x8, #62
	cbnz	x9, LBB46_5
; %bb.1:
	ldr	x9, [x23, #16]
	mov	x10, #9223372036854775804       ; =0x7ffffffffffffffc
	sub	x9, x9, x20
	asr	x11, x9, #1
	cmp	x11, x8
	csel	x8, x11, x8, hi
	cmp	x9, x10
	mov	x9, #4611686018427387903        ; =0x3fffffffffffffff
	csel	x8, x8, x9, lo
	lsr	x9, x8, #62
	cbnz	x9, LBB46_6
; %bb.2:
	mov	x19, x0
	ldr	x26, [x0, #8]
	lsl	x22, x8, #2
	mov	x0, x22
	bl	__Znwm
	add	x25, x0, x21
	add	x27, x0, x22
	ldr	w8, [x26]
	sub	x22, x25, x24, lsl #2
	str	w8, [x25], #4
	mov	x0, x22
	mov	x1, x20
	mov	x2, x21
	bl	_memcpy
	stp	x22, x25, [x23]
	str	x27, [x23, #16]
	cbz	x20, LBB46_4
; %bb.3:
	mov	x0, x20
	bl	__ZdlPv
LBB46_4:
	ldr	x8, [x19]
	str	x25, [x8]
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #64]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #48]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #32]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #96             ; 16-byte Folded Reload
	ret
LBB46_5:
	bl	__ZNSt3__16vectorIjNS_9allocatorIjEEE20__throw_length_errorB9nqe220106Ev
LBB46_6:
	bl	__ZSt28__throw_bad_array_new_lengthB9nqe220106v
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__const
	.globl	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE ; @_ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.weak_def_can_be_hidden	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE:
	.quad	0
	.quad	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED1Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEED0Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE16__on_zero_sharedEv
	.quad	__ZNKSt3__119__shared_weak_count13__get_deleterERKSt9type_info
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEE21__on_zero_shared_weakEv

	.private_extern	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE ; @_ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.globl	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.weak_definition	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE:
	.quad	__ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE-9223372036854775808
	.quad	__ZTINSt3__119__shared_weak_countE

	.private_extern	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE ; @_ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.section	__TEXT,__const
	.globl	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
	.weak_definition	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE
__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE:
	.asciz	"NSt3__120__shared_ptr_emplaceINS_6vectorIhNS_9allocatorIhEEEENS2_IS4_EEEE"

	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"vector"

	.section	__DATA,__const
	.globl	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE ; @_ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.weak_def_can_be_hidden	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE:
	.quad	0
	.quad	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED1Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEED0Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE16__on_zero_sharedEv
	.quad	__ZNKSt3__119__shared_weak_count13__get_deleterERKSt9type_info
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEE21__on_zero_shared_weakEv

	.private_extern	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE ; @_ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.globl	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.weak_definition	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTINSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE:
	.quad	__ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE-9223372036854775808
	.quad	__ZTINSt3__119__shared_weak_countE

	.private_extern	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE ; @_ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.section	__TEXT,__const
	.globl	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
	.weak_definition	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE
__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE:
	.asciz	"NSt3__120__shared_ptr_emplaceINS_6vectorItNS_9allocatorItEEEENS2_IS4_EEEE"

	.section	__DATA,__const
	.globl	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE ; @_ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.weak_def_can_be_hidden	__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTVNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE:
	.quad	0
	.quad	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED1Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEED0Ev
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE16__on_zero_sharedEv
	.quad	__ZNKSt3__119__shared_weak_count13__get_deleterERKSt9type_info
	.quad	__ZNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEE21__on_zero_shared_weakEv

	.private_extern	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE ; @_ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.globl	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.weak_definition	__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.p2align	3, 0x0
__ZTINSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE:
	.quad	__ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE-9223372036854775808
	.quad	__ZTINSt3__119__shared_weak_countE

	.private_extern	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE ; @_ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.section	__TEXT,__const
	.globl	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
	.weak_definition	__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE
__ZTSNSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE:
	.asciz	"NSt3__120__shared_ptr_emplaceINS_6vectorIjNS_9allocatorIjEEEENS2_IS4_EEEE"

.subsections_via_symbols
