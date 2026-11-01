000000010002aed4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)>:
10002aed4:     	sub	sp, sp, #0xc0
10002aed8:     	stp	x26, x25, [sp, #0x70]
10002aedc:     	stp	x24, x23, [sp, #0x80]
10002aee0:     	stp	x22, x21, [sp, #0x90]
10002aee4:     	stp	x20, x19, [sp, #0xa0]
10002aee8:     	stp	x29, x30, [sp, #0xb0]
10002aeec:     	add	x29, sp, #0xb0
10002aef0:     	mov	x21, x2
10002aef4:     	mov	x20, x1
10002aef8:     	mov	x19, x0
10002aefc:     	adrp	x8, 0x100048000 <GD::Bc::MessageData::bc_strs_>
10002af00:     	ldr	x8, [x8, #0xb18]
10002af04:     	ldr	x8, [x8]
10002af08:     	stur	x8, [x29, #-0x48]
10002af0c:     	mov	x22, x1
10002af10:     	ldr	x8, [x22, #0x10]!
10002af14:     	cbz	x8, 0x10002af38 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x64>
10002af18:     	ldp	x9, x8, [x8]
10002af1c:     	sub	x8, x8, x9
10002af20:     	cbz	x8, 0x10002af34 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
10002af24:     	ldr	w10, [x9], #0x4
10002af28:     	cbnz	w10, 0x10002af58 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x84>
10002af2c:     	subs	x8, x8, #0x4
10002af30:     	b.ne	0x10002af24 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x50>
10002af34:     	bl	0x10002d1c0 <void GD::Bc::Details::error<>(GD::Bc::Msg)>
10002af38:     	ldrb	w8, [x20, #0xc]
10002af3c:     	cmp	x8, #0xff
10002af40:     	lsl	x8, x8, #2
10002af44:     	ccmp	x8, #0x0, #0x4, ne
10002af48:     	b.eq	0x10002af34 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
10002af4c:     	mov	x9, x20
10002af50:     	ldr	w10, [x9], #0x4
10002af54:     	cbz	w10, 0x10002af2c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x58>
10002af58:     	ldr	w8, [x19, #0x24]
10002af5c:     	ldr	w9, [x20, #0x24]
10002af60:     	subs	w1, w8, w9
10002af64:     	b.ls	0x10002af70 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9c>
10002af68:     	mov	x0, x20
10002af6c:     	b	0x10002af7c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8>
10002af70:     	b.hs	0x10002af80 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xac>
10002af74:     	sub	w1, w9, w8
10002af78:     	mov	x0, x19
10002af7c:     	bl	0x1000067f0 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
10002af80:     	mov	x0, x19
10002af84:     	mov	x1, x21
10002af88:     	bl	0x1000067f0 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
10002af8c:     	ldr	x12, [x19, #0x10]
10002af90:     	cbz	x12, 0x10002b078 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1a4>
10002af94:     	ldp	x9, x8, [x12]
10002af98:     	sub	x8, x8, x9
10002af9c:     	asr	x8, x8, #2
10002afa0:     	ldr	x9, [x22]
10002afa4:     	cbz	x9, 0x10002b08c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1b8>
10002afa8:     	ldp	x11, x10, [x9]
10002afac:     	sub	x10, x10, x11
10002afb0:     	asr	x10, x10, #2
10002afb4:     	mov	x11, x19
10002afb8:     	cbz	x12, 0x10002afc0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
10002afbc:     	ldr	x11, [x12]
10002afc0:     	mov	x12, x20
10002afc4:     	cbz	x9, 0x10002afcc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xf8>
10002afc8:     	ldr	x12, [x9]
10002afcc:     	cmp	x8, x10
10002afd0:     	csel	x13, x8, x10, hi
10002afd4:     	cbz	x13, 0x10002b028 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x154>
10002afd8:     	sub	x13, x13, #0x1
10002afdc:     	cmp	x13, x8
10002afe0:     	b.hs	0x10002b000 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x12c>
10002afe4:     	ldr	w14, [x11, x13, lsl #2]
10002afe8:     	cmp	x13, x10
10002afec:     	b.lo	0x10002b00c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x138>
10002aff0:     	mov	w15, #0x0               ; =0
10002aff4:     	cmp	w14, w15
10002aff8:     	b.hs	0x10002b018 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x144>
10002affc:     	b	0x10002b0a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
10002b000:     	mov	w14, #0x0               ; =0
10002b004:     	cmp	x13, x10
10002b008:     	b.hs	0x10002aff0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x11c>
10002b00c:     	ldr	w15, [x12, x13, lsl #2]
10002b010:     	cmp	w14, w15
10002b014:     	b.lo	0x10002b0a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
10002b018:     	b.hi	0x10002b0e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x210>
10002b01c:     	sub	x13, x13, #0x1
10002b020:     	cmn	x13, #0x1
10002b024:     	b.ne	0x10002afdc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x108>
10002b028:     	add	x22, sp, #0x20
10002b02c:     	add	x0, sp, #0x20
10002b030:     	mov	w1, #0x1                ; =1
10002b034:     	bl	0x10002620c <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::BasicDigits(unsigned int)>
10002b038:     	cmp	x19, x22
10002b03c:     	b.eq	0x10002b164 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x290>
10002b040:     	ldr	q0, [sp, #0x30]
10002b044:     	stp	xzr, xzr, [sp, #0x30]
10002b048:     	ldr	x22, [x19, #0x18]
10002b04c:     	str	q0, [x19, #0x10]
10002b050:     	cbz	x22, 0x10002b100 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x22c>
10002b054:     	add	x8, x22, #0x8
10002b058:     	mov	x9, #-0x1               ; =-1
10002b05c:     	ldaddal	x9, x8, [x8]
10002b060:     	cbz	x8, 0x10002b170 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x29c>
10002b064:     	ldr	x8, [x19, #0x10]
10002b068:     	cbz	x8, 0x10002b108 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
10002b06c:     	mov	w8, #0xff               ; =255
10002b070:     	strb	w8, [x19, #0xc]
10002b074:     	b	0x10002b128 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
10002b078:     	ldrb	w8, [x19, #0xc]
10002b07c:     	cmp	w8, #0xff
10002b080:     	csel	w8, wzr, w8, eq
10002b084:     	ldr	x9, [x22]
10002b088:     	cbnz	x9, 0x10002afa8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xd4>
10002b08c:     	ldrb	w10, [x20, #0xc]
10002b090:     	cmp	w10, #0xff
10002b094:     	csel	w10, wzr, w10, eq
10002b098:     	mov	x11, x19
10002b09c:     	cbnz	x12, 0x10002afbc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xe8>
10002b0a0:     	b	0x10002afc0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
10002b0a4:     	ldr	x22, [x19, #0x18]
10002b0a8:     	stp	xzr, xzr, [x19, #0x10]
10002b0ac:     	cbz	x22, 0x10002b0d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
10002b0b0:     	add	x8, x22, #0x8
10002b0b4:     	mov	x9, #-0x1               ; =-1
10002b0b8:     	ldaddal	x9, x8, [x8]
10002b0bc:     	cbnz	x8, 0x10002b0d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
10002b0c0:     	ldr	x8, [x22]
10002b0c4:     	ldr	x8, [x8, #0x10]
10002b0c8:     	mov	x0, x22
10002b0cc:     	blr	x8
10002b0d0:     	mov	x0, x22
10002b0d4:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b0d8:     	mov	w8, #0xff               ; =255
10002b0dc:     	strb	w8, [x19, #0xc]
10002b0e0:     	b	0x10002b914 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b0e4:     	ldr	x8, [x20, #0x18]
10002b0e8:     	stp	x9, x8, [sp, #0x10]
10002b0ec:     	stp	xzr, xzr, [x22]
10002b0f0:     	cbz	x9, 0x10002b194 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2c0>
10002b0f4:     	mov	w8, #0xff               ; =255
10002b0f8:     	strb	w8, [sp, #0xc]
10002b0fc:     	b	0x10002b1b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
10002b100:     	fmov	x8, d0
10002b104:     	cbnz	x8, 0x10002b06c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
10002b108:     	ldrb	w8, [sp, #0x2c]
10002b10c:     	strb	w8, [x19, #0xc]
10002b110:     	cmp	x8, #0xff
10002b114:     	b.eq	0x10002b128 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
10002b118:     	lsl	x2, x8, #2
10002b11c:     	add	x1, sp, #0x20
10002b120:     	mov	x0, x19
10002b124:     	bl	0x10003b6bc <_vfprintf+0x10003b6bc>
10002b128:     	ldr	x22, [sp, #0x38]
10002b12c:     	stp	xzr, xzr, [sp, #0x30]
10002b130:     	cbz	x22, 0x10002b15c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
10002b134:     	add	x8, x22, #0x8
10002b138:     	mov	x9, #-0x1               ; =-1
10002b13c:     	ldaddal	x9, x8, [x8]
10002b140:     	cbnz	x8, 0x10002b15c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
10002b144:     	ldr	x8, [x22]
10002b148:     	ldr	x8, [x8, #0x10]
10002b14c:     	mov	x0, x22
10002b150:     	blr	x8
10002b154:     	mov	x0, x22
10002b158:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b15c:     	mov	w8, #0xff               ; =255
10002b160:     	strb	w8, [sp, #0x2c]
10002b164:     	ldr	x22, [sp, #0x38]
10002b168:     	cbnz	x22, 0x10002b8ec <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa18>
10002b16c:     	b	0x10002b914 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b170:     	ldr	x8, [x22]
10002b174:     	ldr	x8, [x8, #0x10]
10002b178:     	mov	x0, x22
10002b17c:     	blr	x8
10002b180:     	mov	x0, x22
10002b184:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b188:     	ldr	x8, [x19, #0x10]
10002b18c:     	cbnz	x8, 0x10002b06c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
10002b190:     	b	0x10002b108 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
10002b194:     	ldrb	w8, [x20, #0xc]
10002b198:     	strb	w8, [sp, #0xc]
10002b19c:     	cmp	x8, #0xff
10002b1a0:     	b.eq	0x10002b1b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
10002b1a4:     	lsl	x2, x8, #2
10002b1a8:     	mov	x0, sp
10002b1ac:     	mov	x1, x20
10002b1b0:     	bl	0x10003b6bc <_vfprintf+0x10003b6bc>
10002b1b4:     	stp	xzr, xzr, [x22]
10002b1b8:     	mov	w8, #0xff               ; =255
10002b1bc:     	strb	w8, [x20, #0xc]
10002b1c0:     	mov	x0, x19
10002b1c4:     	bl	0x100005d2c <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002b1c8:     	mov	x0, x19
10002b1cc:     	bl	0x100006bfc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002b1d0:     	mov	x0, sp
10002b1d4:     	bl	0x100005d2c <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002b1d8:     	mov	x0, sp
10002b1dc:     	bl	0x100006bfc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002b1e0:     	ldr	x8, [sp, #0x10]
10002b1e4:     	cbz	x8, 0x10002b244 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x370>
10002b1e8:     	ldp	x9, x10, [x8]
10002b1ec:     	sub	x11, x10, x9
10002b1f0:     	cmp	x11, #0x4
10002b1f4:     	b.ne	0x10002b278 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
10002b1f8:     	cmp	x10, x9
10002b1fc:     	b.ne	0x10002b254 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x380>
10002b200:     	mov	w0, #0x10               ; =16
10002b204:     	bl	0x10003b554 <_vfprintf+0x10003b554>
10002b208:     	mov	x20, x0
10002b20c:     	adrp	x1, 0x10003d000 <_.str.1+0x12e>
10002b210:     	add	x1, x1, #0x976
10002b214:     	bl	0x10003b380 <_vfprintf+0x10003b380>
10002b218:     	adrp	x8, 0x100048000 <GD::Bc::MessageData::bc_strs_>
10002b21c:     	ldr	x8, [x8, #0xaa0]
10002b220:     	add	x8, x8, #0x10
10002b224:     	str	x8, [x20]
10002b228:     	adrp	x1, 0x100048000 <GD::Bc::MessageData::bc_strs_>
10002b22c:     	ldr	x1, [x1, #0xc58]
10002b230:     	adrp	x2, 0x100048000 <GD::Bc::MessageData::bc_strs_>
10002b234:     	ldr	x2, [x2, #0x908]
10002b238:     	mov	x0, x20
10002b23c:     	bl	0x10003b5b4 <_vfprintf+0x10003b5b4>
10002b240:     	brk	#0x1
10002b244:     	ldrb	w9, [sp, #0xc]
10002b248:     	cmp	w9, #0x1
10002b24c:     	b.ne	0x10002b278 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
10002b250:     	mov	x9, sp
10002b254:     	ldr	w22, [x9]
10002b258:     	mov	x0, x19
10002b25c:     	bl	0x100005d2c <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
10002b260:     	ldr	x8, [x19, #0x10]
10002b264:     	cbz	x8, 0x10002b308 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x434>
10002b268:     	ldp	x10, x8, [x8]
10002b26c:     	sub	x8, x8, x10
10002b270:     	cbnz	x8, 0x10002b328 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x454>
10002b274:     	b	0x10002b358 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
10002b278:     	ldr	x22, [x19, #0x10]
10002b27c:     	cbz	x22, 0x10002b2ac <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3d8>
10002b280:     	ldp	x10, x9, [x22]
10002b284:     	sub	x9, x9, x10
10002b288:     	cmp	x9, #0x8
10002b28c:     	b.ne	0x10002b2bc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3e8>
10002b290:     	mov	w9, #0xca00             ; =51712
10002b294:     	movk	w9, #0x3b9a, lsl #16
10002b298:     	ldp	w11, w10, [x10]
10002b29c:     	umaddl	x10, w10, w9, x11
10002b2a0:     	cbz	x8, 0x10002b364 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x490>
10002b2a4:     	ldr	x8, [x8]
10002b2a8:     	b	0x10002b378 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a4>
10002b2ac:     	ldrb	w9, [x19, #0xc]
10002b2b0:     	mov	x10, x19
10002b2b4:     	cmp	w9, #0x2
10002b2b8:     	b.eq	0x10002b290 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3bc>
10002b2bc:     	cbz	x8, 0x10002b40c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x538>
10002b2c0:     	ldp	x10, x9, [x8]
10002b2c4:     	sub	x9, x9, x10
10002b2c8:     	asr	x23, x9, #2
10002b2cc:     	cbz	x22, 0x10002b41c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x548>
10002b2d0:     	ldp	x10, x9, [x22]
10002b2d4:     	sub	x9, x9, x10
10002b2d8:     	asr	x24, x9, #2
10002b2dc:     	cbz	x8, 0x10002b42c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x558>
10002b2e0:     	ldp	x9, x10, [x8]
10002b2e4:     	sub	x8, x10, x9
10002b2e8:     	asr	x8, x8, #2
10002b2ec:     	mov	w10, #0x64ff            ; =25855
10002b2f0:     	movk	w10, #0x1dcd, lsl #16
10002b2f4:     	add	x8, x9, x8, lsl #2
10002b2f8:     	ldur	w8, [x8, #-0x4]
10002b2fc:     	cmp	w8, w10
10002b300:     	b.ls	0x10002b45c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x588>
10002b304:     	b	0x10002b484 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
10002b308:     	ldrb	w8, [x19, #0xc]
10002b30c:     	cmp	w8, #0xff
10002b310:     	b.ne	0x10002b31c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x448>
10002b314:     	mov	w8, #0x0                ; =0
10002b318:     	strb	wzr, [x19, #0xc]
10002b31c:     	ubfiz	x8, x8, #2, #32
10002b320:     	mov	x10, x19
10002b324:     	cbz	x8, 0x10002b358 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
10002b328:     	mov	x11, #0x0               ; =0
10002b32c:     	mov	w9, #0xca00             ; =51712
10002b330:     	movk	w9, #0x3b9a, lsl #16
10002b334:     	sub	x10, x10, #0x4
10002b338:     	ldr	w12, [x10, x8]
10002b33c:     	add	x11, x11, w12, uxtw
10002b340:     	udiv	x12, x11, x22
10002b344:     	str	w12, [x10, x8]
10002b348:     	msub	x11, x12, x22, x11
10002b34c:     	mul	x11, x11, x9
10002b350:     	subs	x8, x8, #0x4
10002b354:     	b.ne	0x10002b338 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x464>
10002b358:     	mov	x0, x19
10002b35c:     	bl	0x100006bfc <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
10002b360:     	b	0x10002b8e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b364:     	ldrb	w8, [sp, #0xc]
10002b368:     	cmp	w8, #0xff
10002b36c:     	b.ne	0x10002b374 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a0>
10002b370:     	strb	wzr, [sp, #0xc]
10002b374:     	mov	x8, sp
10002b378:     	ldp	w11, w8, [x8]
10002b37c:     	umaddl	x8, w8, w9, x11
10002b380:     	udiv	x8, x10, x8
10002b384:     	lsr	x10, x8, #9
10002b388:     	mov	x11, #0x5a53            ; =23123
10002b38c:     	movk	x11, #0xa09b, lsl #16
10002b390:     	movk	x11, #0xb82f, lsl #32
10002b394:     	movk	x11, #0x44, lsl #48
10002b398:     	umulh	x10, x10, x11
10002b39c:     	lsr	x10, x10, #11
10002b3a0:     	msub	w8, w10, w9, w8
10002b3a4:     	stp	w8, w10, [sp, #0x48]
10002b3a8:     	add	x8, sp, #0x48
10002b3ac:     	add	x22, sp, #0x20
10002b3b0:     	add	x0, sp, #0x20
10002b3b4:     	add	x1, sp, #0x48
10002b3b8:     	add	x2, x8, #0x8
10002b3bc:     	bl	0x100005fc0 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::DigitStorage(unsigned int const*, unsigned int const*)>
10002b3c0:     	cmp	x19, x22
10002b3c4:     	b.eq	0x10002b8b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
10002b3c8:     	ldr	q0, [sp, #0x30]
10002b3cc:     	stp	xzr, xzr, [sp, #0x30]
10002b3d0:     	ldr	x22, [x19, #0x18]
10002b3d4:     	str	q0, [x19, #0x10]
10002b3d8:     	cbz	x22, 0x10002b560 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x68c>
10002b3dc:     	add	x8, x22, #0x8
10002b3e0:     	mov	x9, #-0x1               ; =-1
10002b3e4:     	ldaddal	x9, x8, [x8]
10002b3e8:     	cbnz	x8, 0x10002b404 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x530>
10002b3ec:     	ldr	x8, [x22]
10002b3f0:     	ldr	x8, [x8, #0x10]
10002b3f4:     	mov	x0, x22
10002b3f8:     	blr	x8
10002b3fc:     	mov	x0, x22
10002b400:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b404:     	ldr	x8, [x19, #0x10]
10002b408:     	b	0x10002b564 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x690>
10002b40c:     	ldrb	w9, [sp, #0xc]
10002b410:     	cmp	w9, #0xff
10002b414:     	csel	w23, wzr, w9, eq
10002b418:     	cbnz	x22, 0x10002b2d0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3fc>
10002b41c:     	ldrb	w9, [x19, #0xc]
10002b420:     	cmp	w9, #0xff
10002b424:     	csel	w24, wzr, w9, eq
10002b428:     	cbnz	x8, 0x10002b2e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x40c>
10002b42c:     	ldrb	w8, [sp, #0xc]
10002b430:     	cmp	w8, #0xff
10002b434:     	csel	w8, wzr, w8, eq
10002b438:     	b.ne	0x10002b440 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x56c>
10002b43c:     	strb	wzr, [sp, #0xc]
10002b440:     	mov	x9, sp
10002b444:     	mov	w10, #0x64ff            ; =25855
10002b448:     	movk	w10, #0x1dcd, lsl #16
10002b44c:     	add	x8, x9, x8, lsl #2
10002b450:     	ldur	w8, [x8, #-0x4]
10002b454:     	cmp	w8, w10
10002b458:     	b.hi	0x10002b484 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
10002b45c:     	udiv	w22, w10, w8
10002b460:     	add	w1, w22, #0x1
10002b464:     	mov	x0, x19
10002b468:     	mov	w2, #0x0                ; =0
10002b46c:     	bl	0x100005b88 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
10002b470:     	mov	x0, sp
10002b474:     	add	w1, w22, #0x1
10002b478:     	mov	w2, #0x0                ; =0
10002b47c:     	bl	0x100005b88 <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
10002b480:     	ldr	x22, [x19, #0x10]
10002b484:     	str	wzr, [sp, #0x44]
10002b488:     	cbz	x22, 0x10002b4b8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5e4>
10002b48c:     	ldp	x8, x9, [x22, #0x8]
10002b490:     	str	x8, [sp, #0x48]
10002b494:     	add	x10, sp, #0x48
10002b498:     	add	x11, sp, #0x44
10002b49c:     	stp	x10, x11, [sp, #0x20]
10002b4a0:     	str	x22, [sp, #0x30]
10002b4a4:     	cmp	x8, x9
10002b4a8:     	b.hs	0x10002b974 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xaa0>
10002b4ac:     	str	wzr, [x8], #0x4
10002b4b0:     	str	x8, [x22, #0x8]
10002b4b4:     	b	0x10002b4f0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x61c>
10002b4b8:     	ldrb	w8, [x19, #0xc]
10002b4bc:     	cmp	w8, #0x3
10002b4c0:     	b.ne	0x10002b4dc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x608>
10002b4c4:     	mov	x0, x19
10002b4c8:     	mov	w1, #0x6                ; =6
10002b4cc:     	bl	0x100006230 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
10002b4d0:     	ldr	x22, [x19, #0x10]
10002b4d4:     	cbnz	x22, 0x10002b48c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b8>
10002b4d8:     	ldrb	w8, [x19, #0xc]
10002b4dc:     	cmp	w8, #0xff
10002b4e0:     	csel	w8, wzr, w8, eq
10002b4e4:     	add	w9, w8, #0x1
10002b4e8:     	strb	w9, [x19, #0xc]
10002b4ec:     	str	wzr, [x19, w8, uxtw #2]
10002b4f0:     	mov	x8, #0x0                ; =0
10002b4f4:     	sub	x25, x24, x23
10002b4f8:     	stp	xzr, xzr, [sp, #0x30]
10002b4fc:     	mov	w9, #0xff               ; =255
10002b500:     	strb	w9, [sp, #0x2c]
10002b504:     	add	x22, x25, #0x1
10002b508:     	strb	wzr, [sp, #0x54]
10002b50c:     	add	x26, sp, #0x48
10002b510:     	stp	xzr, xzr, [sp, #0x58]
10002b514:     	str	wzr, [sp, #0x44]
10002b518:     	cmp	x22, #0x4
10002b51c:     	b.lo	0x10002b5e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x714>
10002b520:     	add	x0, sp, #0x48
10002b524:     	mov	x1, x22
10002b528:     	bl	0x100006230 <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
10002b52c:     	ldr	x0, [sp, #0x58]
10002b530:     	cbz	x0, 0x10002b5d4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x700>
10002b534:     	add	x2, sp, #0x44
10002b538:     	mov	x1, x22
10002b53c:     	bl	0x1000069b4 <std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::resize(unsigned long, unsigned int const&)>
10002b540:     	ldur	q0, [sp, #0x58]
10002b544:     	stp	xzr, xzr, [x26, #0x10]
10002b548:     	str	q0, [sp, #0x30]
10002b54c:     	fmov	x8, d0
10002b550:     	cbz	x8, 0x10002b960 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8c>
10002b554:     	mov	w8, #0xff               ; =255
10002b558:     	strb	w8, [sp, #0x2c]
10002b55c:     	b	0x10002b638 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b560:     	fmov	x8, d0
10002b564:     	cbz	x8, 0x10002b574 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6a0>
10002b568:     	mov	w8, #0xff               ; =255
10002b56c:     	strb	w8, [x19, #0xc]
10002b570:     	b	0x10002b594 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
10002b574:     	ldrb	w8, [sp, #0x2c]
10002b578:     	strb	w8, [x19, #0xc]
10002b57c:     	cmp	x8, #0xff
10002b580:     	b.eq	0x10002b594 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
10002b584:     	lsl	x2, x8, #2
10002b588:     	add	x1, sp, #0x20
10002b58c:     	mov	x0, x19
10002b590:     	bl	0x10003b6bc <_vfprintf+0x10003b6bc>
10002b594:     	ldr	x22, [sp, #0x38]
10002b598:     	stp	xzr, xzr, [sp, #0x30]
10002b59c:     	cbz	x22, 0x10002b5c8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
10002b5a0:     	add	x8, x22, #0x8
10002b5a4:     	mov	x9, #-0x1               ; =-1
10002b5a8:     	ldaddal	x9, x8, [x8]
10002b5ac:     	cbnz	x8, 0x10002b5c8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
10002b5b0:     	ldr	x8, [x22]
10002b5b4:     	ldr	x8, [x8, #0x10]
10002b5b8:     	mov	x0, x22
10002b5bc:     	blr	x8
10002b5c0:     	mov	x0, x22
10002b5c4:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b5c8:     	mov	w8, #0xff               ; =255
10002b5cc:     	strb	w8, [sp, #0x2c]
10002b5d0:     	b	0x10002b8b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
10002b5d4:     	ldrb	w8, [sp, #0x54]
10002b5d8:     	mov	w9, #0xff               ; =255
10002b5dc:     	bics	wzr, w9, w8
10002b5e0:     	csel	w8, wzr, w8, eq
10002b5e4:     	and	x8, x8, #0xff
10002b5e8:     	subs	x9, x22, x8
10002b5ec:     	b.ls	0x10002b608 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
10002b5f0:     	cmp	x9, #0x1
10002b5f4:     	b.lt	0x10002b608 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
10002b5f8:     	add	x10, sp, #0x48
10002b5fc:     	add	x0, x10, x8, lsl #2
10002b600:     	lsl	x1, x9, #2
10002b604:     	bl	0x10003b608 <_vfprintf+0x10003b608>
10002b608:     	and	w8, w22, #0xff
10002b60c:     	strb	w22, [sp, #0x54]
10002b610:     	ldr	x9, [sp, #0x60]
10002b614:     	stp	xzr, xzr, [x26, #0x10]
10002b618:     	stp	xzr, x9, [sp, #0x30]
10002b61c:     	strb	w8, [sp, #0x2c]
10002b620:     	cmp	w8, #0xff
10002b624:     	b.eq	0x10002b638 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b628:     	ubfiz	x2, x8, #2, #32
10002b62c:     	add	x0, sp, #0x20
10002b630:     	add	x1, sp, #0x48
10002b634:     	bl	0x10003b6bc <_vfprintf+0x10003b6bc>
10002b638:     	ldr	x8, [x19, #0x10]
10002b63c:     	cbz	x8, 0x10002b648 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x774>
10002b640:     	ldr	x8, [x8]
10002b644:     	b	0x10002b660 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
10002b648:     	ldrb	w9, [x19, #0xc]
10002b64c:     	mov	x8, x19
10002b650:     	cmp	w9, #0xff
10002b654:     	b.ne	0x10002b660 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
10002b658:     	strb	wzr, [x19, #0xc]
10002b65c:     	mov	x8, x19
10002b660:     	ldr	x9, [sp, #0x10]
10002b664:     	cbz	x9, 0x10002b694 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7c0>
10002b668:     	ldr	x14, [x9]
10002b66c:     	ldr	x9, [sp, #0x30]
10002b670:     	cbz	x9, 0x10002b6b0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7dc>
10002b674:     	ldr	x9, [x9]
10002b678:     	mov	w10, #0xca00            ; =51712
10002b67c:     	movk	w10, #0x3b9a, lsl #16
10002b680:     	sub	x11, x8, #0x4
10002b684:     	sub	x12, x8, #0x8
10002b688:     	add	x13, x14, x23, lsl #2
10002b68c:     	cbnz	x23, 0x10002b6dc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x808>
10002b690:     	b	0x10002b854 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
10002b694:     	ldrb	w9, [sp, #0xc]
10002b698:     	cmp	w9, #0xff
10002b69c:     	b.ne	0x10002b6a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7d0>
10002b6a0:     	strb	wzr, [sp, #0xc]
10002b6a4:     	mov	x14, sp
10002b6a8:     	ldr	x9, [sp, #0x30]
10002b6ac:     	cbnz	x9, 0x10002b674 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7a0>
10002b6b0:     	ldrb	w9, [sp, #0x2c]
10002b6b4:     	cmp	w9, #0xff
10002b6b8:     	b.ne	0x10002b6c0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7ec>
10002b6bc:     	strb	wzr, [sp, #0x2c]
10002b6c0:     	add	x9, sp, #0x20
10002b6c4:     	mov	w10, #0xca00            ; =51712
10002b6c8:     	movk	w10, #0x3b9a, lsl #16
10002b6cc:     	sub	x11, x8, #0x4
10002b6d0:     	sub	x12, x8, #0x8
10002b6d4:     	add	x13, x14, x23, lsl #2
10002b6d8:     	cbz	x23, 0x10002b854 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
10002b6dc:     	sub	x15, x8, x23, lsl #2
10002b6e0:     	add	x15, x15, x24, lsl #2
10002b6e4:     	mov	x16, #0x5a53            ; =23123
10002b6e8:     	movk	x16, #0xa09b, lsl #16
10002b6ec:     	movk	x16, #0xb82f, lsl #32
10002b6f0:     	movk	x16, #0x44, lsl #48
10002b6f4:     	mov	x17, #0xe827            ; =59431
10002b6f8:     	movk	x17, #0x2e0b, lsl #16
10002b6fc:     	movk	x17, #0x11, lsl #32
10002b700:     	b	0x10002b714 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x840>
10002b704:     	str	w1, [x9, x0, lsl #2]
10002b708:     	sub	x25, x0, #0x1
10002b70c:     	sub	x15, x15, #0x4
10002b710:     	cbz	x0, 0x10002b8a8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
10002b714:     	mov	x0, x25
10002b718:     	add	x2, x25, x23
10002b71c:     	ldr	w1, [x8, x2, lsl #2]
10002b720:     	ldr	w3, [x11, x2, lsl #2]
10002b724:     	umaddl	x4, w1, w10, x3
10002b728:     	ldur	w3, [x13, #-0x4]
10002b72c:     	udiv	x1, x4, x3
10002b730:     	msub	x4, x1, x3, x4
10002b734:     	b	0x10002b748 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x874>
10002b738:     	sub	x1, x1, #0x1
10002b73c:     	add	x4, x4, x3
10002b740:     	cmp	x4, x10
10002b744:     	b.hs	0x10002b768 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x894>
10002b748:     	cmp	x1, x10
10002b74c:     	b.eq	0x10002b738 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
10002b750:     	ldur	w5, [x13, #-0x8]
10002b754:     	ldr	w6, [x12, x2, lsl #2]
10002b758:     	madd	x6, x4, x10, x6
10002b75c:     	mul	x5, x1, x5
10002b760:     	cmp	x5, x6
10002b764:     	b.hi	0x10002b738 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
10002b768:     	mov	x4, #0x0                ; =0
10002b76c:     	mov	x3, #0x0                ; =0
10002b770:     	b	0x10002b788 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8b4>
10002b774:     	sub	w5, w6, w5
10002b778:     	str	w5, [x15, x4, lsl #2]
10002b77c:     	add	x4, x4, #0x1
10002b780:     	cmp	x23, w4, uxtw
10002b784:     	b.ls	0x10002b7bc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8e8>
10002b788:     	ldr	w5, [x14, x4, lsl #2]
10002b78c:     	madd	x5, x1, x5, x3
10002b790:     	lsr	x3, x5, #9
10002b794:     	umulh	x3, x3, x16
10002b798:     	lsr	x3, x3, #11
10002b79c:     	msub	w5, w3, w10, w5
10002b7a0:     	ldr	w6, [x15, x4, lsl #2]
10002b7a4:     	cmp	w6, w5
10002b7a8:     	b.hs	0x10002b774 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
10002b7ac:     	add	w6, w6, w10
10002b7b0:     	str	w6, [x15, x4, lsl #2]
10002b7b4:     	add	x3, x3, #0x1
10002b7b8:     	b	0x10002b774 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
10002b7bc:     	cbz	x3, 0x10002b704 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b7c0:     	umulh	x4, x3, x17
10002b7c4:     	lsr	w4, w4, #2
10002b7c8:     	msub	w4, w4, w10, w3
10002b7cc:     	ldr	w5, [x8, x2, lsl #2]
10002b7d0:     	subs	w4, w5, w4
10002b7d4:     	b.hs	0x10002b7e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x910>
10002b7d8:     	add	w3, w4, w10
10002b7dc:     	str	w3, [x8, x2, lsl #2]
10002b7e0:     	b	0x10002b7f0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x91c>
10002b7e4:     	str	w4, [x8, x2, lsl #2]
10002b7e8:     	cmp	x3, x10
10002b7ec:     	b.lo	0x10002b704 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b7f0:     	mov	x3, #0x0                ; =0
10002b7f4:     	mov	x4, #0x0                ; =0
10002b7f8:     	ldr	w5, [x14, x3, lsl #2]
10002b7fc:     	ldr	w6, [x15, x3, lsl #2]
10002b800:     	add	w5, w6, w5
10002b804:     	add	x5, x4, w5, uxtw
10002b808:     	lsr	x4, x5, #9
10002b80c:     	umulh	x4, x4, x16
10002b810:     	lsr	x4, x4, #11
10002b814:     	msub	w5, w4, w10, w5
10002b818:     	str	w5, [x15, x3, lsl #2]
10002b81c:     	add	x3, x3, #0x1
10002b820:     	cmp	x23, x3
10002b824:     	b.ne	0x10002b7f8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x924>
10002b828:     	ldr	w3, [x8, x2, lsl #2]
10002b82c:     	add	x3, x4, x3
10002b830:     	umulh	x4, x3, x17
10002b834:     	lsr	w4, w4, #2
10002b838:     	msub	w3, w4, w10, w3
10002b83c:     	str	w3, [x8, x2, lsl #2]
10002b840:     	sub	x1, x1, #0x1
10002b844:     	b	0x10002b704 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
10002b848:     	str	w15, [x9, x14, lsl #2]
10002b84c:     	sub	x25, x14, #0x1
10002b850:     	cbz	x14, 0x10002b8a8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
10002b854:     	mov	x14, x25
10002b858:     	ldr	w15, [x8, x25, lsl #2]
10002b85c:     	ldr	w16, [x11, x25, lsl #2]
10002b860:     	umaddl	x17, w15, w10, x16
10002b864:     	ldur	w16, [x13, #-0x4]
10002b868:     	udiv	x15, x17, x16
10002b86c:     	msub	x17, x15, x16, x17
10002b870:     	b	0x10002b884 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9b0>
10002b874:     	sub	x15, x15, #0x1
10002b878:     	add	x17, x17, x16
10002b87c:     	cmp	x17, x10
10002b880:     	b.hs	0x10002b848 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
10002b884:     	cmp	x15, x10
10002b888:     	b.eq	0x10002b874 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
10002b88c:     	ldur	w0, [x13, #-0x8]
10002b890:     	ldr	w1, [x12, x14, lsl #2]
10002b894:     	madd	x1, x17, x10, x1
10002b898:     	mul	x0, x15, x0
10002b89c:     	cmp	x0, x1
10002b8a0:     	b.hi	0x10002b874 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
10002b8a4:     	b	0x10002b848 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
10002b8a8:     	add	x0, sp, #0x20
10002b8ac:     	mov	x1, x19
10002b8b0:     	bl	0x10002c590 <std::__1::enable_if<is_move_constructible<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value && is_move_assignable<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value, void>::type std::__1::swap[abi:nqe220106]<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>(GD::Bc::Details::DigitStorage<unsigned int, 3ul>&, GD::Bc::Details::DigitStorage<unsigned int, 3ul>&)>
10002b8b4:     	ldr	x22, [sp, #0x38]
10002b8b8:     	cbz	x22, 0x10002b8e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b8bc:     	add	x8, x22, #0x8
10002b8c0:     	mov	x9, #-0x1               ; =-1
10002b8c4:     	ldaddal	x9, x8, [x8]
10002b8c8:     	cbnz	x8, 0x10002b8e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
10002b8cc:     	ldr	x8, [x22]
10002b8d0:     	ldr	x8, [x8, #0x10]
10002b8d4:     	mov	x0, x22
10002b8d8:     	blr	x8
10002b8dc:     	mov	x0, x22
10002b8e0:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b8e4:     	ldr	x22, [sp, #0x18]
10002b8e8:     	cbz	x22, 0x10002b914 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b8ec:     	add	x8, x22, #0x8
10002b8f0:     	mov	x9, #-0x1               ; =-1
10002b8f4:     	ldaddal	x9, x8, [x8]
10002b8f8:     	cbnz	x8, 0x10002b914 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
10002b8fc:     	ldr	x8, [x22]
10002b900:     	ldr	x8, [x8, #0x10]
10002b904:     	mov	x0, x22
10002b908:     	blr	x8
10002b90c:     	mov	x0, x22
10002b910:     	bl	0x10003b434 <_vfprintf+0x10003b434>
10002b914:     	str	w21, [x19, #0x24]
10002b918:     	ldr	w8, [x19, #0x20]
10002b91c:     	ldr	w9, [x20, #0x20]
10002b920:     	cmp	w8, w9
10002b924:     	cset	w8, ne
10002b928:     	str	w8, [x19, #0x20]
10002b92c:     	ldur	x8, [x29, #-0x48]
10002b930:     	adrp	x9, 0x100048000 <GD::Bc::MessageData::bc_strs_>
10002b934:     	ldr	x9, [x9, #0xb18]
10002b938:     	ldr	x9, [x9]
10002b93c:     	cmp	x9, x8
10002b940:     	b.ne	0x10002b984 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xab0>
10002b944:     	ldp	x29, x30, [sp, #0xb0]
10002b948:     	ldp	x20, x19, [sp, #0xa0]
10002b94c:     	ldp	x22, x21, [sp, #0x90]
10002b950:     	ldp	x24, x23, [sp, #0x80]
10002b954:     	ldp	x26, x25, [sp, #0x70]
10002b958:     	add	sp, sp, #0xc0
10002b95c:     	ret
10002b960:     	ldrb	w8, [sp, #0x54]
10002b964:     	strb	w8, [sp, #0x2c]
10002b968:     	cmp	w8, #0xff
10002b96c:     	b.ne	0x10002b628 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x754>
10002b970:     	b	0x10002b638 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
10002b974:     	add	x0, sp, #0x20
10002b978:     	bl	0x100006714 <unsigned int& std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::emplace_back<unsigned int const&>(unsigned int const&)::'lambda0'()::operator()() const>
10002b97c:     	ldr	x8, [sp, #0x48]
10002b980:     	b	0x10002b4b0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5dc>
10002b984:     	bl	0x10003b5cc <_vfprintf+0x10003b5cc>
10002b988:     	mov	x19, x0
10002b98c:     	mov	x0, x20
10002b990:     	bl	0x10003b584 <_vfprintf+0x10003b584>
10002b994:     	mov	x0, sp
10002b998:     	bl	0x100004914 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b99c:     	mov	x0, x19
10002b9a0:     	bl	0x10003b350 <_vfprintf+0x10003b350>
10002b9a4:     	mov	x19, x0
10002b9a8:     	add	x0, sp, #0x48
10002b9ac:     	bl	0x100004914 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b9b0:     	add	x0, sp, #0x20
10002b9b4:     	bl	0x100004914 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b9b8:     	mov	x0, sp
10002b9bc:     	bl	0x100004914 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b9c0:     	mov	x0, x19
10002b9c4:     	bl	0x10003b350 <_vfprintf+0x10003b350>
10002b9c8:     	bl	0x10003b350 <_vfprintf+0x10003b350>
10002b9cc:     	mov	x19, x0
10002b9d0:     	mov	x0, sp
10002b9d4:     	bl	0x100004914 <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
10002b9d8:     	mov	x0, x19
10002b9dc:     	bl	0x10003b350 <_vfprintf+0x10003b350>
