000000010002ab74 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)>:
10002ab74:     	sub	sp, sp, #0xc0
10002ab78:     	stp	x26, x25, [sp, #0x70]
10002ab7c:     	stp	x24, x23, [sp, #0x80]
10002ab80:     	stp	x22, x21, [sp, #0x90]
10002ab84:     	stp	x20, x19, [sp, #0xa0]
10002ab88:     	stp	x29, x30, [sp, #0xb0]
10002ab8c:     	add	x29, sp, #0xb0
10002ab90:     	mov	x21, x2
10002ab94:     	mov	x20, x1
10002ab98:     	mov	x19, x0
10002ab9c:     	adrp	x8, 0x100044000 <GD::Bc::MessageData::bc_strs_>
10002aba0:     	ldr	x8, [x8, #0xb18]
10002aba4:     	ldr	x8, [x8]
10002aba8:     	stur	x8, [x29, #-0x48]
10002abac:     	mov	x22, x1
10002abb0:     	ldr	x8, [x22, #0x10]!
10002abb4:     	cbz	x8, 0x10002abd8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x64>
10002abb8:     	ldp	x9, x8, [x8]
10002abbc:     	sub	x8, x8, x9
10002abc0:     	cbz	x8, 0x10002abd4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
10002abc4:     	ldr	w10, [x9], #0x4
10002abc8:     	cbnz	w10, 0x10002abf8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x84>
10002abcc:     	subs	x8, x8, #0x4
10002abd0:     	b.ne	0x10002abc4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x50>
10002abd4:     	bl	0x10002ce60 <void GD::Bc::Details::error<>(GD::Bc::Msg)>
10002abd8:     	ldrb	w8, [x20, #0xc]
10002abdc:     	cmp	x8, #0xff
10002abe0:     	lsl	x8, x8, #2
10002abe4:     	ccmp	x8, #0x0, #0x4, ne
10002abe8:     	b.eq	0x10002abd4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
10002abec:     	mov	x9, x20
10002abf0:     	ldr	w10, [x9], #0x4
10002abf4:     	cbz	w10, 0x10002abcc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x58>
10002abf8:     	ldr	w8, [x19, #0x24]
10002abfc:     	ldr	w9, [x20, #0x24]
10002ac00:     	subs	w1, w8, w9
10002ac04:     	b.ls	0x10002ac10 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9c>
10002ac08:     	mov	x0, x20
10002ac0c:     	b	0x10002ac1c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8>
10002ac10:     	b.hs	0x10002ac20 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xac>
10002ac14:     	sub	w1, w9, w8
10002ac18:     	mov	x0, x19
10002ac1c:     	bl	0x100006490 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
10002ac20:     	mov	x0, x19
10002ac24:     	mov	x1, x21
10002ac28:     	bl	0x100006490 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
10002ac2c:     	ldr	x12, [x19, #0x10]
10002ac30:     	cbz	x12, 0x10002ad18 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1a4>
10002ac34:     	ldp	x9, x8, [x12]
10002ac38:     	sub	x8, x8, x9
10002ac3c:     	asr	x8, x8, #2
10002ac40:     	ldr	x9, [x22]
10002ac44:     	cbz	x9, 0x10002ad2c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1b8>
10002ac48:     	ldp	x11, x10, [x9]
10002ac4c:     	sub	x10, x10, x11
10002ac50:     	asr	x10, x10, #2
10002ac54:     	mov	x11, x19
10002ac58:     	cbz	x12, 0x10002ac60 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
10002ac5c:     	ldr	x11, [x12]
10002ac60:     	mov	x12, x20
10002ac64:     	cbz	x9, 0x10002ac6c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xf8>
10002ac68:     	ldr	x12, [x9]
10002ac6c:     	cmp	x8, x10
10002ac70:     	csel	x13, x8, x10, hi
10002ac74:     	cbz	x13, 0x10002acc8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x154>
10002ac78:     	sub	x13, x13, #0x1
10002ac7c:     	cmp	x13, x8
10002ac80:     	b.hs	0x10002aca0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x12c>
10002ac84:     	ldr	w14, [x11, x13, lsl #2]
10002ac88:     	cmp	x13, x10
10002ac8c:     	b.lo	0x10002acac <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x138>
10002ac90:     	mov	w15, #0x0               ; =0
10002ac94:     	cmp	w14, w15
10002ac98:     	b.hs	0x10002acb8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x144>
10002ac9c:     	b	0x10002ad44 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
10002aca0:     	mov	w14, #0x0               ; =0
10002aca4:     	cmp	x13, x10
10002aca8:     	b.hs	0x10002ac90 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x11c>
10002acac:     	ldr	w15, [x12, x13, lsl #2]
10002acb0:     	cmp	w14, w15
10002acb4:     	b.lo	0x10002ad44 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
10002acb8:     	b.hi	0x10002ad84 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x210>
10002acbc:     	sub	x13, x13, #0x1
10002acc0:     	cmn	x13, #0x1
10002acc4:     	b.ne	0x10002ac7c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x108>
10002acc8:     	add	x22, sp, #0x20
10002accc:     	add	x0, sp, #0x20
10002acd0:     	mov	w1, #0x1                ; =1
10002acd4:     	bl	0x100025eac <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::BasicDigits(unsigned int)>
10002acd8:     	cmp	x19, x22
10002acdc:     	b.eq	0x10002ae04 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x290>
10002ace0:     	ldr	q0, [sp, #0x30]
10002ace4:     	stp	xzr, xzr, [sp, #0x30]
10002ace8:     	ldr	x22, [x19, #0x18]
10002acec:     	str	q0, [x19, #0x10]
10002acf0:     	cbz	x22, 0x10002ada0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x22c>
10002acf4:     	add	x8, x22, #0x8
10002acf8:     	mov	x9, #-0x1               ; =-1
10002acfc:     	ldaddal	x9, x8, [x8]
10002ad00:     	cbz	x8, 0x10002ae10 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x29c>
10002ad04:     	ldr	x8, [x19, #0x10]
10002ad08:     	cbz	x8, 0x10002ada8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
10002ad0c:     	mov	w8, #0xff               ; =255
10002ad10:     	strb	w8, [x19, #0xc]
10002ad14:     	b	0x10002adc8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
10002ad18:     	ldrb	w8, [x19, #0xc]
10002ad1c:     	cmp	w8, #0xff
10002ad20:     	csel	w8, wzr, w8, eq
10002ad24:     	ldr	x9, [x22]
10002ad28:     	cbnz	x9, 0x10002ac48 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xd4>
10002ad2c:     	ldrb	w10, [x20, #0xc]
10002ad30:     	cmp	w10, #0xff
10002ad34:     	csel	w10, wzr, w10, eq
10002ad38:     	mov	x11, x19
10002ad3c:     	cbnz	x12, 0x10002ac5c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xe8>
10002ad40:     	b	0x10002ac60 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
10002ad44:     	ldr	x22, [x19, #0x18]
10002ad48:     	stp	xzr, xzr, [x19, #0x10]
10002ad4c:     	cbz	x22, 0x10002ad78 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
10002ad50:     	add	x8, x22, #0x8
10002ad54:     	mov	x9, #-0x1               ; =-1
10002ad58:     	ldaddal	x9, x8, [x8]
10002ad5c:     	cbnz	x8, 0x10002ad78 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
10002ad60:     	ldr	x8, [x22]
10002ad64:     	ldr	x8, [x8, #0x10]
10002ad68:     	mov	x0, x22
10002ad6c:     	blr	x8
10002ad70:     	mov	x0, x22
10002ad74:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002ad78:     	mov	w8, #0xff               ; =255
10002ad7c:     	strb	w8, [x19, #0xc]
10002ad80:     	b	0x10002b5b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002ad84:     	ldr	x8, [x20, #0x18]
10002ad88:     	stp	x9, x8, [sp, #0x10]
10002ad8c:     	stp	xzr, xzr, [x22]
10002ad90:     	cbz	x9, 0x10002ae34 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2c0>
10002ad94:     	mov	w8, #0xff               ; =255
10002ad98:     	strb	w8, [sp, #0xc]
10002ad9c:     	b	0x10002ae54 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
10002ada0:     	fmov	x8, d0
10002ada4:     	cbnz	x8, 0x10002ad0c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
10002ada8:     	ldrb	w8, [sp, #0x2c]
10002adac:     	strb	w8, [x19, #0xc]
10002adb0:     	cmp	x8, #0xff
10002adb4:     	b.eq	0x10002adc8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
10002adb8:     	lsl	x2, x8, #2
10002adbc:     	add	x1, sp, #0x20
10002adc0:     	mov	x0, x19
10002adc4:     	bl	0x10003b35c <_vfprintf+0x10003b35c>
10002adc8:     	ldr	x22, [sp, #0x38]
10002adcc:     	stp	xzr, xzr, [sp, #0x30]
10002add0:     	cbz	x22, 0x10002adfc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
10002add4:     	add	x8, x22, #0x8
10002add8:     	mov	x9, #-0x1               ; =-1
10002addc:     	ldaddal	x9, x8, [x8]
10002ade0:     	cbnz	x8, 0x10002adfc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
10002ade4:     	ldr	x8, [x22]
10002ade8:     	ldr	x8, [x8, #0x10]
10002adec:     	mov	x0, x22
10002adf0:     	blr	x8
10002adf4:     	mov	x0, x22
10002adf8:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002adfc:     	mov	w8, #0xff               ; =255
10002ae00:     	strb	w8, [sp, #0x2c]
10002ae04:     	ldr	x22, [sp, #0x38]
10002ae08:     	cbnz	x22, 0x10002b58c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa18>
10002ae0c:     	b	0x10002b5b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002ae10:     	ldr	x8, [x22]
10002ae14:     	ldr	x8, [x8, #0x10]
10002ae18:     	mov	x0, x22
10002ae1c:     	blr	x8
10002ae20:     	mov	x0, x22
10002ae24:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002ae28:     	ldr	x8, [x19, #0x10]
10002ae2c:     	cbnz	x8, 0x10002ad0c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
10002ae30:     	b	0x10002ada8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
10002ae34:     	ldrb	w8, [x20, #0xc]
10002ae38:     	strb	w8, [sp, #0xc]
10002ae3c:     	cmp	x8, #0xff
10002ae40:     	b.eq	0x10002ae54 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
10002ae44:     	lsl	x2, x8, #2
10002ae48:     	mov	x0, sp
10002ae4c:     	mov	x1, x20
10002ae50:     	bl	0x10003b35c <_vfprintf+0x10003b35c>
10002ae54:     	stp	xzr, xzr, [x22]
10002ae58:     	mov	w8, #0xff               ; =255
10002ae5c:     	strb	w8, [x20, #0xc]
10002ae60:     	mov	x0, x19
10002ae64:     	bl	0x1000059cc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002ae68:     	mov	x0, x19
10002ae6c:     	bl	0x100006654 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002ae70:     	mov	x0, sp
10002ae74:     	bl	0x1000059cc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002ae78:     	mov	x0, sp
10002ae7c:     	bl	0x100006654 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002ae80:     	ldr	x8, [sp, #0x10]
10002ae84:     	cbz	x8, 0x10002aee4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x370>
10002ae88:     	ldp	x9, x10, [x8]
10002ae8c:     	sub	x11, x10, x9
10002ae90:     	cmp	x11, #0x4
10002ae94:     	b.ne	0x10002af18 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
10002ae98:     	cmp	x10, x9
10002ae9c:     	b.ne	0x10002aef4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x380>
10002aea0:     	mov	w0, #0x10               ; =16
10002aea4:     	bl	0x10003b1f4 <_vfprintf+0x10003b1f4>
10002aea8:     	mov	x20, x0
10002aeac:     	adrp	x1, 0x10003d000 <_.str+0x294>
10002aeb0:     	add	x1, x1, #0x618
10002aeb4:     	bl	0x10003b020 <_vfprintf+0x10003b020>
10002aeb8:     	adrp	x8, 0x100044000 <GD::Bc::MessageData::bc_strs_>
10002aebc:     	ldr	x8, [x8, #0xaa0]
10002aec0:     	add	x8, x8, #0x10
10002aec4:     	str	x8, [x20]
10002aec8:     	adrp	x1, 0x100044000 <GD::Bc::MessageData::bc_strs_>
10002aecc:     	ldr	x1, [x1, #0xc58]
10002aed0:     	adrp	x2, 0x100044000 <GD::Bc::MessageData::bc_strs_>
10002aed4:     	ldr	x2, [x2, #0x908]
10002aed8:     	mov	x0, x20
10002aedc:     	bl	0x10003b254 <_vfprintf+0x10003b254>
10002aee0:     	brk	#0x1
10002aee4:     	ldrb	w9, [sp, #0xc]
10002aee8:     	cmp	w9, #0x1
10002aeec:     	b.ne	0x10002af18 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
10002aef0:     	mov	x9, sp
10002aef4:     	ldr	w22, [x9]
10002aef8:     	mov	x0, x19
10002aefc:     	bl	0x1000059cc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002af00:     	ldr	x8, [x19, #0x10]
10002af04:     	cbz	x8, 0x10002afa8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x434>
10002af08:     	ldp	x10, x8, [x8]
10002af0c:     	sub	x8, x8, x10
10002af10:     	cbnz	x8, 0x10002afc8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x454>
10002af14:     	b	0x10002aff8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
10002af18:     	ldr	x22, [x19, #0x10]
10002af1c:     	cbz	x22, 0x10002af4c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3d8>
10002af20:     	ldp	x10, x9, [x22]
10002af24:     	sub	x9, x9, x10
10002af28:     	cmp	x9, #0x8
10002af2c:     	b.ne	0x10002af5c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3e8>
10002af30:     	mov	w9, #0xca00             ; =51712
10002af34:     	movk	w9, #0x3b9a, lsl #16
10002af38:     	ldp	w11, w10, [x10]
10002af3c:     	umaddl	x10, w10, w9, x11
10002af40:     	cbz	x8, 0x10002b004 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x490>
10002af44:     	ldr	x8, [x8]
10002af48:     	b	0x10002b018 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a4>
10002af4c:     	ldrb	w9, [x19, #0xc]
10002af50:     	mov	x10, x19
10002af54:     	cmp	w9, #0x2
10002af58:     	b.eq	0x10002af30 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3bc>
10002af5c:     	cbz	x8, 0x10002b0ac <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x538>
10002af60:     	ldp	x10, x9, [x8]
10002af64:     	sub	x9, x9, x10
10002af68:     	asr	x23, x9, #2
10002af6c:     	cbz	x22, 0x10002b0bc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x548>
10002af70:     	ldp	x10, x9, [x22]
10002af74:     	sub	x9, x9, x10
10002af78:     	asr	x24, x9, #2
10002af7c:     	cbz	x8, 0x10002b0cc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x558>
10002af80:     	ldp	x9, x10, [x8]
10002af84:     	sub	x8, x10, x9
10002af88:     	asr	x8, x8, #2
10002af8c:     	mov	w10, #0x64ff            ; =25855
10002af90:     	movk	w10, #0x1dcd, lsl #16
10002af94:     	add	x8, x9, x8, lsl #2
10002af98:     	ldur	w8, [x8, #-0x4]
10002af9c:     	cmp	w8, w10
10002afa0:     	b.ls	0x10002b0fc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x588>
10002afa4:     	b	0x10002b124 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
10002afa8:     	ldrb	w8, [x19, #0xc]
10002afac:     	cmp	w8, #0xff
10002afb0:     	b.ne	0x10002afbc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x448>
10002afb4:     	mov	w8, #0x0                ; =0
10002afb8:     	strb	wzr, [x19, #0xc]
10002afbc:     	ubfiz	x8, x8, #2, #32
10002afc0:     	mov	x10, x19
10002afc4:     	cbz	x8, 0x10002aff8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
10002afc8:     	mov	x11, #0x0               ; =0
10002afcc:     	mov	w9, #0xca00             ; =51712
10002afd0:     	movk	w9, #0x3b9a, lsl #16
10002afd4:     	sub	x10, x10, #0x4
10002afd8:     	ldr	w12, [x10, x8]
10002afdc:     	add	x11, x11, w12, uxtw
10002afe0:     	udiv	x12, x11, x22
10002afe4:     	str	w12, [x10, x8]
10002afe8:     	msub	x11, x12, x22, x11
10002afec:     	mul	x11, x11, x9
10002aff0:     	subs	x8, x8, #0x4
10002aff4:     	b.ne	0x10002afd8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x464>
10002aff8:     	mov	x0, x19
10002affc:     	bl	0x100006654 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002b000:     	b	0x10002b584 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b004:     	ldrb	w8, [sp, #0xc]
10002b008:     	cmp	w8, #0xff
10002b00c:     	b.ne	0x10002b014 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a0>
10002b010:     	strb	wzr, [sp, #0xc]
10002b014:     	mov	x8, sp
10002b018:     	ldp	w11, w8, [x8]
10002b01c:     	umaddl	x8, w8, w9, x11
10002b020:     	udiv	x8, x10, x8
10002b024:     	lsr	x10, x8, #9
10002b028:     	mov	x11, #0x5a53            ; =23123
10002b02c:     	movk	x11, #0xa09b, lsl #16
10002b030:     	movk	x11, #0xb82f, lsl #32
10002b034:     	movk	x11, #0x44, lsl #48
10002b038:     	umulh	x10, x10, x11
10002b03c:     	lsr	x10, x10, #11
10002b040:     	msub	w8, w10, w9, w8
10002b044:     	stp	w8, w10, [sp, #0x48]
10002b048:     	add	x8, sp, #0x48
10002b04c:     	add	x22, sp, #0x20
10002b050:     	add	x0, sp, #0x20
10002b054:     	add	x1, sp, #0x48
10002b058:     	add	x2, x8, #0x8
10002b05c:     	bl	0x100005c60 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::DigitStorage(unsigned int const*, unsigned int const*)>
10002b060:     	cmp	x19, x22
10002b064:     	b.eq	0x10002b554 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
10002b068:     	ldr	q0, [sp, #0x30]
10002b06c:     	stp	xzr, xzr, [sp, #0x30]
10002b070:     	ldr	x22, [x19, #0x18]
10002b074:     	str	q0, [x19, #0x10]
10002b078:     	cbz	x22, 0x10002b200 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x68c>
10002b07c:     	add	x8, x22, #0x8
10002b080:     	mov	x9, #-0x1               ; =-1
10002b084:     	ldaddal	x9, x8, [x8]
10002b088:     	cbnz	x8, 0x10002b0a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x530>
10002b08c:     	ldr	x8, [x22]
10002b090:     	ldr	x8, [x8, #0x10]
10002b094:     	mov	x0, x22
10002b098:     	blr	x8
10002b09c:     	mov	x0, x22
10002b0a0:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002b0a4:     	ldr	x8, [x19, #0x10]
10002b0a8:     	b	0x10002b204 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x690>
10002b0ac:     	ldrb	w9, [sp, #0xc]
10002b0b0:     	cmp	w9, #0xff
10002b0b4:     	csel	w23, wzr, w9, eq
10002b0b8:     	cbnz	x22, 0x10002af70 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3fc>
10002b0bc:     	ldrb	w9, [x19, #0xc]
10002b0c0:     	cmp	w9, #0xff
10002b0c4:     	csel	w24, wzr, w9, eq
10002b0c8:     	cbnz	x8, 0x10002af80 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x40c>
10002b0cc:     	ldrb	w8, [sp, #0xc]
10002b0d0:     	cmp	w8, #0xff
10002b0d4:     	csel	w8, wzr, w8, eq
10002b0d8:     	b.ne	0x10002b0e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x56c>
10002b0dc:     	strb	wzr, [sp, #0xc]
10002b0e0:     	mov	x9, sp
10002b0e4:     	mov	w10, #0x64ff            ; =25855
10002b0e8:     	movk	w10, #0x1dcd, lsl #16
10002b0ec:     	add	x8, x9, x8, lsl #2
10002b0f0:     	ldur	w8, [x8, #-0x4]
10002b0f4:     	cmp	w8, w10
10002b0f8:     	b.hi	0x10002b124 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
10002b0fc:     	udiv	w22, w10, w8
10002b100:     	add	w1, w22, #0x1
10002b104:     	mov	x0, x19
10002b108:     	mov	w2, #0x0                ; =0
10002b10c:     	bl	0x100005828 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
10002b110:     	mov	x0, sp
10002b114:     	add	w1, w22, #0x1
10002b118:     	mov	w2, #0x0                ; =0
10002b11c:     	bl	0x100005828 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
10002b120:     	ldr	x22, [x19, #0x10]
10002b124:     	str	wzr, [sp, #0x44]
10002b128:     	cbz	x22, 0x10002b158 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5e4>
10002b12c:     	ldp	x8, x9, [x22, #0x8]
10002b130:     	str	x8, [sp, #0x48]
10002b134:     	add	x10, sp, #0x48
10002b138:     	add	x11, sp, #0x44
10002b13c:     	stp	x10, x11, [sp, #0x20]
10002b140:     	str	x22, [sp, #0x30]
10002b144:     	cmp	x8, x9
10002b148:     	b.hs	0x10002b614 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xaa0>
10002b14c:     	str	wzr, [x8], #0x4
10002b150:     	str	x8, [x22, #0x8]
10002b154:     	b	0x10002b190 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x61c>
10002b158:     	ldrb	w8, [x19, #0xc]
10002b15c:     	cmp	w8, #0x3
10002b160:     	b.ne	0x10002b17c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x608>
10002b164:     	mov	x0, x19
10002b168:     	mov	w1, #0x6                ; =6
10002b16c:     	bl	0x100005ed0 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
10002b170:     	ldr	x22, [x19, #0x10]
10002b174:     	cbnz	x22, 0x10002b12c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b8>
10002b178:     	ldrb	w8, [x19, #0xc]
10002b17c:     	cmp	w8, #0xff
10002b180:     	csel	w8, wzr, w8, eq
10002b184:     	add	w9, w8, #0x1
10002b188:     	strb	w9, [x19, #0xc]
10002b18c:     	str	wzr, [x19, w8, uxtw #2]
10002b190:     	mov	x8, #0x0                ; =0
10002b194:     	sub	x25, x24, x23
10002b198:     	stp	xzr, xzr, [sp, #0x30]
10002b19c:     	mov	w9, #0xff               ; =255
10002b1a0:     	strb	w9, [sp, #0x2c]
10002b1a4:     	add	x22, x25, #0x1
10002b1a8:     	strb	wzr, [sp, #0x54]
10002b1ac:     	add	x26, sp, #0x48
10002b1b0:     	stp	xzr, xzr, [sp, #0x58]
10002b1b4:     	str	wzr, [sp, #0x44]
10002b1b8:     	cmp	x22, #0x4
10002b1bc:     	b.lo	0x10002b288 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x714>
10002b1c0:     	add	x0, sp, #0x48
10002b1c4:     	mov	x1, x22
10002b1c8:     	bl	0x100005ed0 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
10002b1cc:     	ldr	x0, [sp, #0x58]
10002b1d0:     	cbz	x0, 0x10002b274 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x700>
10002b1d4:     	add	x2, sp, #0x44
10002b1d8:     	mov	x1, x22
10002b1dc:     	bl	0x100006750 <std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::resize(unsigned long, unsigned int const&)>
10002b1e0:     	ldur	q0, [sp, #0x58]
10002b1e4:     	stp	xzr, xzr, [x26, #0x10]
10002b1e8:     	str	q0, [sp, #0x30]
10002b1ec:     	fmov	x8, d0
10002b1f0:     	cbz	x8, 0x10002b600 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8c>
10002b1f4:     	mov	w8, #0xff               ; =255
10002b1f8:     	strb	w8, [sp, #0x2c]
10002b1fc:     	b	0x10002b2d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b200:     	fmov	x8, d0
10002b204:     	cbz	x8, 0x10002b214 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6a0>
10002b208:     	mov	w8, #0xff               ; =255
10002b20c:     	strb	w8, [x19, #0xc]
10002b210:     	b	0x10002b234 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
10002b214:     	ldrb	w8, [sp, #0x2c]
10002b218:     	strb	w8, [x19, #0xc]
10002b21c:     	cmp	x8, #0xff
10002b220:     	b.eq	0x10002b234 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
10002b224:     	lsl	x2, x8, #2
10002b228:     	add	x1, sp, #0x20
10002b22c:     	mov	x0, x19
10002b230:     	bl	0x10003b35c <_vfprintf+0x10003b35c>
10002b234:     	ldr	x22, [sp, #0x38]
10002b238:     	stp	xzr, xzr, [sp, #0x30]
10002b23c:     	cbz	x22, 0x10002b268 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
10002b240:     	add	x8, x22, #0x8
10002b244:     	mov	x9, #-0x1               ; =-1
10002b248:     	ldaddal	x9, x8, [x8]
10002b24c:     	cbnz	x8, 0x10002b268 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
10002b250:     	ldr	x8, [x22]
10002b254:     	ldr	x8, [x8, #0x10]
10002b258:     	mov	x0, x22
10002b25c:     	blr	x8
10002b260:     	mov	x0, x22
10002b264:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002b268:     	mov	w8, #0xff               ; =255
10002b26c:     	strb	w8, [sp, #0x2c]
10002b270:     	b	0x10002b554 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
10002b274:     	ldrb	w8, [sp, #0x54]
10002b278:     	mov	w9, #0xff               ; =255
10002b27c:     	bics	wzr, w9, w8
10002b280:     	csel	w8, wzr, w8, eq
10002b284:     	and	x8, x8, #0xff
10002b288:     	subs	x9, x22, x8
10002b28c:     	b.ls	0x10002b2a8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
10002b290:     	cmp	x9, #0x1
10002b294:     	b.lt	0x10002b2a8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
10002b298:     	add	x10, sp, #0x48
10002b29c:     	add	x0, x10, x8, lsl #2
10002b2a0:     	lsl	x1, x9, #2
10002b2a4:     	bl	0x10003b2a8 <_vfprintf+0x10003b2a8>
10002b2a8:     	and	w8, w22, #0xff
10002b2ac:     	strb	w22, [sp, #0x54]
10002b2b0:     	ldr	x9, [sp, #0x60]
10002b2b4:     	stp	xzr, xzr, [x26, #0x10]
10002b2b8:     	stp	xzr, x9, [sp, #0x30]
10002b2bc:     	strb	w8, [sp, #0x2c]
10002b2c0:     	cmp	w8, #0xff
10002b2c4:     	b.eq	0x10002b2d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b2c8:     	ubfiz	x2, x8, #2, #32
10002b2cc:     	add	x0, sp, #0x20
10002b2d0:     	add	x1, sp, #0x48
10002b2d4:     	bl	0x10003b35c <_vfprintf+0x10003b35c>
10002b2d8:     	ldr	x8, [x19, #0x10]
10002b2dc:     	cbz	x8, 0x10002b2e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x774>
10002b2e0:     	ldr	x8, [x8]
10002b2e4:     	b	0x10002b300 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
10002b2e8:     	ldrb	w9, [x19, #0xc]
10002b2ec:     	mov	x8, x19
10002b2f0:     	cmp	w9, #0xff
10002b2f4:     	b.ne	0x10002b300 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
10002b2f8:     	strb	wzr, [x19, #0xc]
10002b2fc:     	mov	x8, x19
10002b300:     	ldr	x9, [sp, #0x10]
10002b304:     	cbz	x9, 0x10002b334 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7c0>
10002b308:     	ldr	x14, [x9]
10002b30c:     	ldr	x9, [sp, #0x30]
10002b310:     	cbz	x9, 0x10002b350 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7dc>
10002b314:     	ldr	x9, [x9]
10002b318:     	mov	w10, #0xca00            ; =51712
10002b31c:     	movk	w10, #0x3b9a, lsl #16
10002b320:     	sub	x11, x8, #0x4
10002b324:     	sub	x12, x8, #0x8
10002b328:     	add	x13, x14, x23, lsl #2
10002b32c:     	cbnz	x23, 0x10002b37c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x808>
10002b330:     	b	0x10002b4f4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
10002b334:     	ldrb	w9, [sp, #0xc]
10002b338:     	cmp	w9, #0xff
10002b33c:     	b.ne	0x10002b344 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7d0>
10002b340:     	strb	wzr, [sp, #0xc]
10002b344:     	mov	x14, sp
10002b348:     	ldr	x9, [sp, #0x30]
10002b34c:     	cbnz	x9, 0x10002b314 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7a0>
10002b350:     	ldrb	w9, [sp, #0x2c]
10002b354:     	cmp	w9, #0xff
10002b358:     	b.ne	0x10002b360 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7ec>
10002b35c:     	strb	wzr, [sp, #0x2c]
10002b360:     	add	x9, sp, #0x20
10002b364:     	mov	w10, #0xca00            ; =51712
10002b368:     	movk	w10, #0x3b9a, lsl #16
10002b36c:     	sub	x11, x8, #0x4
10002b370:     	sub	x12, x8, #0x8
10002b374:     	add	x13, x14, x23, lsl #2
10002b378:     	cbz	x23, 0x10002b4f4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
10002b37c:     	sub	x15, x8, x23, lsl #2
10002b380:     	add	x15, x15, x24, lsl #2
10002b384:     	mov	x16, #0x5a53            ; =23123
10002b388:     	movk	x16, #0xa09b, lsl #16
10002b38c:     	movk	x16, #0xb82f, lsl #32
10002b390:     	movk	x16, #0x44, lsl #48
10002b394:     	mov	x17, #0xe827            ; =59431
10002b398:     	movk	x17, #0x2e0b, lsl #16
10002b39c:     	movk	x17, #0x11, lsl #32
10002b3a0:     	b	0x10002b3b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x840>
10002b3a4:     	str	w1, [x9, x0, lsl #2]
10002b3a8:     	sub	x25, x0, #0x1
10002b3ac:     	sub	x15, x15, #0x4
10002b3b0:     	cbz	x0, 0x10002b548 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
10002b3b4:     	mov	x0, x25
10002b3b8:     	add	x2, x25, x23
10002b3bc:     	ldr	w1, [x8, x2, lsl #2]
10002b3c0:     	ldr	w3, [x11, x2, lsl #2]
10002b3c4:     	umaddl	x4, w1, w10, x3
10002b3c8:     	ldur	w3, [x13, #-0x4]
10002b3cc:     	udiv	x1, x4, x3
10002b3d0:     	msub	x4, x1, x3, x4
10002b3d4:     	b	0x10002b3e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x874>
10002b3d8:     	sub	x1, x1, #0x1
10002b3dc:     	add	x4, x4, x3
10002b3e0:     	cmp	x4, x10
10002b3e4:     	b.hs	0x10002b408 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x894>
10002b3e8:     	cmp	x1, x10
10002b3ec:     	b.eq	0x10002b3d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
10002b3f0:     	ldur	w5, [x13, #-0x8]
10002b3f4:     	ldr	w6, [x12, x2, lsl #2]
10002b3f8:     	madd	x6, x4, x10, x6
10002b3fc:     	mul	x5, x1, x5
10002b400:     	cmp	x5, x6
10002b404:     	b.hi	0x10002b3d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
10002b408:     	mov	x4, #0x0                ; =0
10002b40c:     	mov	x3, #0x0                ; =0
10002b410:     	b	0x10002b428 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8b4>
10002b414:     	sub	w5, w6, w5
10002b418:     	str	w5, [x15, x4, lsl #2]
10002b41c:     	add	x4, x4, #0x1
10002b420:     	cmp	x23, w4, uxtw
10002b424:     	b.ls	0x10002b45c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8e8>
10002b428:     	ldr	w5, [x14, x4, lsl #2]
10002b42c:     	madd	x5, x1, x5, x3
10002b430:     	lsr	x3, x5, #9
10002b434:     	umulh	x3, x3, x16
10002b438:     	lsr	x3, x3, #11
10002b43c:     	msub	w5, w3, w10, w5
10002b440:     	ldr	w6, [x15, x4, lsl #2]
10002b444:     	cmp	w6, w5
10002b448:     	b.hs	0x10002b414 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
10002b44c:     	add	w6, w6, w10
10002b450:     	str	w6, [x15, x4, lsl #2]
10002b454:     	add	x3, x3, #0x1
10002b458:     	b	0x10002b414 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
10002b45c:     	cbz	x3, 0x10002b3a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b460:     	umulh	x4, x3, x17
10002b464:     	lsr	w4, w4, #2
10002b468:     	msub	w4, w4, w10, w3
10002b46c:     	ldr	w5, [x8, x2, lsl #2]
10002b470:     	subs	w4, w5, w4
10002b474:     	b.hs	0x10002b484 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x910>
10002b478:     	add	w3, w4, w10
10002b47c:     	str	w3, [x8, x2, lsl #2]
10002b480:     	b	0x10002b490 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x91c>
10002b484:     	str	w4, [x8, x2, lsl #2]
10002b488:     	cmp	x3, x10
10002b48c:     	b.lo	0x10002b3a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b490:     	mov	x3, #0x0                ; =0
10002b494:     	mov	x4, #0x0                ; =0
10002b498:     	ldr	w5, [x14, x3, lsl #2]
10002b49c:     	ldr	w6, [x15, x3, lsl #2]
10002b4a0:     	add	w5, w6, w5
10002b4a4:     	add	x5, x4, w5, uxtw
10002b4a8:     	lsr	x4, x5, #9
10002b4ac:     	umulh	x4, x4, x16
10002b4b0:     	lsr	x4, x4, #11
10002b4b4:     	msub	w5, w4, w10, w5
10002b4b8:     	str	w5, [x15, x3, lsl #2]
10002b4bc:     	add	x3, x3, #0x1
10002b4c0:     	cmp	x23, x3
10002b4c4:     	b.ne	0x10002b498 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x924>
10002b4c8:     	ldr	w3, [x8, x2, lsl #2]
10002b4cc:     	add	x3, x4, x3
10002b4d0:     	umulh	x4, x3, x17
10002b4d4:     	lsr	w4, w4, #2
10002b4d8:     	msub	w3, w4, w10, w3
10002b4dc:     	str	w3, [x8, x2, lsl #2]
10002b4e0:     	sub	x1, x1, #0x1
10002b4e4:     	b	0x10002b3a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b4e8:     	str	w15, [x9, x14, lsl #2]
10002b4ec:     	sub	x25, x14, #0x1
10002b4f0:     	cbz	x14, 0x10002b548 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
10002b4f4:     	mov	x14, x25
10002b4f8:     	ldr	w15, [x8, x25, lsl #2]
10002b4fc:     	ldr	w16, [x11, x25, lsl #2]
10002b500:     	umaddl	x17, w15, w10, x16
10002b504:     	ldur	w16, [x13, #-0x4]
10002b508:     	udiv	x15, x17, x16
10002b50c:     	msub	x17, x15, x16, x17
10002b510:     	b	0x10002b524 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9b0>
10002b514:     	sub	x15, x15, #0x1
10002b518:     	add	x17, x17, x16
10002b51c:     	cmp	x17, x10
10002b520:     	b.hs	0x10002b4e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
10002b524:     	cmp	x15, x10
10002b528:     	b.eq	0x10002b514 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
10002b52c:     	ldur	w0, [x13, #-0x8]
10002b530:     	ldr	w1, [x12, x14, lsl #2]
10002b534:     	madd	x1, x17, x10, x1
10002b538:     	mul	x0, x15, x0
10002b53c:     	cmp	x0, x1
10002b540:     	b.hi	0x10002b514 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
10002b544:     	b	0x10002b4e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
10002b548:     	add	x0, sp, #0x20
10002b54c:     	mov	x1, x19
10002b550:     	bl	0x10002c230 <std::__1::enable_if<is_move_constructible<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value && is_move_assignable<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value, void>::type std::__1::swap[abi:nqe220106]<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>(GD::Bc::Details::DigitStorage<unsigned int, 3ul>&, GD::Bc::Details::DigitStorage<unsigned int, 3ul>&)>
10002b554:     	ldr	x22, [sp, #0x38]
10002b558:     	cbz	x22, 0x10002b584 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b55c:     	add	x8, x22, #0x8
10002b560:     	mov	x9, #-0x1               ; =-1
10002b564:     	ldaddal	x9, x8, [x8]
10002b568:     	cbnz	x8, 0x10002b584 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b56c:     	ldr	x8, [x22]
10002b570:     	ldr	x8, [x8, #0x10]
10002b574:     	mov	x0, x22
10002b578:     	blr	x8
10002b57c:     	mov	x0, x22
10002b580:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002b584:     	ldr	x22, [sp, #0x18]
10002b588:     	cbz	x22, 0x10002b5b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b58c:     	add	x8, x22, #0x8
10002b590:     	mov	x9, #-0x1               ; =-1
10002b594:     	ldaddal	x9, x8, [x8]
10002b598:     	cbnz	x8, 0x10002b5b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b59c:     	ldr	x8, [x22]
10002b5a0:     	ldr	x8, [x8, #0x10]
10002b5a4:     	mov	x0, x22
10002b5a8:     	blr	x8
10002b5ac:     	mov	x0, x22
10002b5b0:     	bl	0x10003b0d4 <_vfprintf+0x10003b0d4>
10002b5b4:     	str	w21, [x19, #0x24]
10002b5b8:     	ldr	w8, [x19, #0x20]
10002b5bc:     	ldr	w9, [x20, #0x20]
10002b5c0:     	cmp	w8, w9
10002b5c4:     	cset	w8, ne
10002b5c8:     	str	w8, [x19, #0x20]
10002b5cc:     	ldur	x8, [x29, #-0x48]
10002b5d0:     	adrp	x9, 0x100044000 <GD::Bc::MessageData::bc_strs_>
10002b5d4:     	ldr	x9, [x9, #0xb18]
10002b5d8:     	ldr	x9, [x9]
10002b5dc:     	cmp	x9, x8
10002b5e0:     	b.ne	0x10002b624 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xab0>
10002b5e4:     	ldp	x29, x30, [sp, #0xb0]
10002b5e8:     	ldp	x20, x19, [sp, #0xa0]
10002b5ec:     	ldp	x22, x21, [sp, #0x90]
10002b5f0:     	ldp	x24, x23, [sp, #0x80]
10002b5f4:     	ldp	x26, x25, [sp, #0x70]
10002b5f8:     	add	sp, sp, #0xc0
10002b5fc:     	ret
10002b600:     	ldrb	w8, [sp, #0x54]
10002b604:     	strb	w8, [sp, #0x2c]
10002b608:     	cmp	w8, #0xff
10002b60c:     	b.ne	0x10002b2c8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x754>
10002b610:     	b	0x10002b2d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b614:     	add	x0, sp, #0x20
10002b618:     	bl	0x1000063b4 <unsigned int& std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::emplace_back<unsigned int const&>(unsigned int const&)::'lambda0'()::operator()() const>
10002b61c:     	ldr	x8, [sp, #0x48]
10002b620:     	b	0x10002b150 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5dc>
10002b624:     	bl	0x10003b26c <_vfprintf+0x10003b26c>
10002b628:     	mov	x19, x0
10002b62c:     	mov	x0, x20
10002b630:     	bl	0x10003b224 <_vfprintf+0x10003b224>
10002b634:     	mov	x0, sp
10002b638:     	bl	0x1000045b4 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b63c:     	mov	x0, x19
10002b640:     	bl	0x10003aff0 <_vfprintf+0x10003aff0>
10002b644:     	mov	x19, x0
10002b648:     	add	x0, sp, #0x48
10002b64c:     	bl	0x1000045b4 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b650:     	add	x0, sp, #0x20
10002b654:     	bl	0x1000045b4 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b658:     	mov	x0, sp
10002b65c:     	bl	0x1000045b4 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b660:     	mov	x0, x19
10002b664:     	bl	0x10003aff0 <_vfprintf+0x10003aff0>
10002b668:     	bl	0x10003aff0 <_vfprintf+0x10003aff0>
10002b66c:     	mov	x19, x0
10002b670:     	mov	x0, sp
10002b674:     	bl	0x1000045b4 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b678:     	mov	x0, x19
10002b67c:     	bl	0x10003aff0 <_vfprintf+0x10003aff0>
