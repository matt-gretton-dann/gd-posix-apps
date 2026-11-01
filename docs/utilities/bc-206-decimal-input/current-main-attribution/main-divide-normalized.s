sub	sp, sp, #0xc0
stp	x26, x25, [sp, #0x70]
stp	x24, x23, [sp, #0x80]
stp	x22, x21, [sp, #0x90]
stp	x20, x19, [sp, #0xa0]
stp	x29, x30, [sp, #0xb0]
add	x29, sp, #0xb0
mov	x21, x2
mov	x20, x1
mov	x19, x0
adrp	x8, ADDR <GD::Bc::MessageData::bc_strs_>
ldr	x8, [x8, #0xb18]
ldr	x8, [x8]
stur	x8, [x29, #-0x48]
mov	x22, x1
ldr	x8, [x22, #0x10]!
cbz	x8, REL:0x64 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x64>
ldp	x9, x8, [x8]
sub	x8, x8, x9
cbz	x8, REL:0x60 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
ldr	w10, [x9], #0x4
cbnz	w10, REL:0x84 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x84>
subs	x8, x8, #0x4
b.ne	REL:0x50 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x50>
bl	ADDR <void GD::Bc::Details::error<>(GD::Bc::Msg)>
ldrb	w8, [x20, #0xc]
cmp	x8, #0xff
lsl	x8, x8, #2
ccmp	x8, #0x0, #0x4, ne
b.eq	REL:0x60 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x60>
mov	x9, x20
ldr	w10, [x9], #0x4
cbz	w10, REL:0x58 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x58>
ldr	w8, [x19, #0x24]
ldr	w9, [x20, #0x24]
subs	w1, w8, w9
b.ls	REL:0x9c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9c>
mov	x0, x20
b	REL:0xa8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8>
b.hs	REL:0xac <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xac>
sub	w1, w9, w8
mov	x0, x19
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
mov	x0, x19
mov	x1, x21
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mul_pow10(unsigned int)>
ldr	x12, [x19, #0x10]
cbz	x12, REL:0x1a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1a4>
ldp	x9, x8, [x12]
sub	x8, x8, x9
asr	x8, x8, #2
ldr	x9, [x22]
cbz	x9, REL:0x1b8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1b8>
ldp	x11, x10, [x9]
sub	x10, x10, x11
asr	x10, x10, #2
mov	x11, x19
cbz	x12, REL:0xec <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
ldr	x11, [x12]
mov	x12, x20
cbz	x9, REL:0xf8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xf8>
ldr	x12, [x9]
cmp	x8, x10
csel	x13, x8, x10, hi
cbz	x13, REL:0x154 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x154>
sub	x13, x13, #0x1
cmp	x13, x8
b.hs	REL:0x12c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x12c>
ldr	w14, [x11, x13, lsl #2]
cmp	x13, x10
b.lo	REL:0x138 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x138>
mov	w15, #0x0               ; =0
cmp	w14, w15
b.hs	REL:0x144 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x144>
b	REL:0x1d0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
mov	w14, #0x0               ; =0
cmp	x13, x10
b.hs	REL:0x11c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x11c>
ldr	w15, [x12, x13, lsl #2]
cmp	w14, w15
b.lo	REL:0x1d0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x1d0>
b.hi	REL:0x210 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x210>
sub	x13, x13, #0x1
cmn	x13, #0x1
b.ne	REL:0x108 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x108>
add	x22, sp, #0x20
add	x0, sp, #0x20
mov	w1, #0x1                ; =1
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::BasicDigits(unsigned int)>
cmp	x19, x22
b.eq	REL:0x290 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x290>
ldr	q0, [sp, #0x30]
stp	xzr, xzr, [sp, #0x30]
ldr	x22, [x19, #0x18]
str	q0, [x19, #0x10]
cbz	x22, REL:0x22c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x22c>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbz	x8, REL:0x29c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x29c>
ldr	x8, [x19, #0x10]
cbz	x8, REL:0x234 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
mov	w8, #0xff               ; =255
strb	w8, [x19, #0xc]
b	REL:0x254 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
ldrb	w8, [x19, #0xc]
cmp	w8, #0xff
csel	w8, wzr, w8, eq
ldr	x9, [x22]
cbnz	x9, REL:0xd4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xd4>
ldrb	w10, [x20, #0xc]
cmp	w10, #0xff
csel	w10, wzr, w10, eq
mov	x11, x19
cbnz	x12, REL:0xe8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xe8>
b	REL:0xec <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xec>
ldr	x22, [x19, #0x18]
stp	xzr, xzr, [x19, #0x10]
cbz	x22, REL:0x204 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0x204 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x204>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
mov	w8, #0xff               ; =255
strb	w8, [x19, #0xc]
b	REL:0xa40 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
ldr	x8, [x20, #0x18]
stp	x9, x8, [sp, #0x10]
stp	xzr, xzr, [x22]
cbz	x9, REL:0x2c0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2c0>
mov	w8, #0xff               ; =255
strb	w8, [sp, #0xc]
b	REL:0x2e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
fmov	x8, d0
cbnz	x8, REL:0x198 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
ldrb	w8, [sp, #0x2c]
strb	w8, [x19, #0xc]
cmp	x8, #0xff
b.eq	REL:0x254 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x254>
lsl	x2, x8, #2
add	x1, sp, #0x20
mov	x0, x19
bl	ADDR <_vfprintf+ADDR>
ldr	x22, [sp, #0x38]
stp	xzr, xzr, [sp, #0x30]
cbz	x22, REL:0x288 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0x288 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x288>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
mov	w8, #0xff               ; =255
strb	w8, [sp, #0x2c]
ldr	x22, [sp, #0x38]
cbnz	x22, REL:0xa18 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa18>
b	REL:0xa40 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
ldr	x8, [x19, #0x10]
cbnz	x8, REL:0x198 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x198>
b	REL:0x234 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x234>
ldrb	w8, [x20, #0xc]
strb	w8, [sp, #0xc]
cmp	x8, #0xff
b.eq	REL:0x2e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x2e0>
lsl	x2, x8, #2
mov	x0, sp
mov	x1, x20
bl	ADDR <_vfprintf+ADDR>
stp	xzr, xzr, [x22]
mov	w8, #0xff               ; =255
strb	w8, [x20, #0xc]
mov	x0, x19
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
mov	x0, x19
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
mov	x0, sp
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
mov	x0, sp
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
ldr	x8, [sp, #0x10]
cbz	x8, REL:0x370 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x370>
ldp	x9, x10, [x8]
sub	x11, x10, x9
cmp	x11, #0x4
b.ne	REL:0x3a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
cmp	x10, x9
b.ne	REL:0x380 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x380>
mov	w0, #0x10               ; =16
bl	ADDR <_vfprintf+ADDR>
mov	x20, x0
adrp	x1, ADDR <_.str+0x294>
add	x1, x1, #0x618
bl	ADDR <_vfprintf+ADDR>
adrp	x8, ADDR <GD::Bc::MessageData::bc_strs_>
ldr	x8, [x8, #0xaa0]
add	x8, x8, #0x10
str	x8, [x20]
adrp	x1, ADDR <GD::Bc::MessageData::bc_strs_>
ldr	x1, [x1, #0xc58]
adrp	x2, ADDR <GD::Bc::MessageData::bc_strs_>
ldr	x2, [x2, #0x908]
mov	x0, x20
bl	ADDR <_vfprintf+ADDR>
brk	#0x1
ldrb	w9, [sp, #0xc]
cmp	w9, #0x1
b.ne	REL:0x3a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3a4>
mov	x9, sp
ldr	w22, [x9]
mov	x0, x19
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::copy_on_write()>
ldr	x8, [x19, #0x10]
cbz	x8, REL:0x434 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x434>
ldp	x10, x8, [x8]
sub	x8, x8, x10
cbnz	x8, REL:0x454 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x454>
b	REL:0x484 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
ldr	x22, [x19, #0x10]
cbz	x22, REL:0x3d8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3d8>
ldp	x10, x9, [x22]
sub	x9, x9, x10
cmp	x9, #0x8
b.ne	REL:0x3e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3e8>
mov	w9, #0xca00             ; =51712
movk	w9, #0x3b9a, lsl #16
ldp	w11, w10, [x10]
umaddl	x10, w10, w9, x11
cbz	x8, REL:0x490 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x490>
ldr	x8, [x8]
b	REL:0x4a4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a4>
ldrb	w9, [x19, #0xc]
mov	x10, x19
cmp	w9, #0x2
b.eq	REL:0x3bc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3bc>
cbz	x8, REL:0x538 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x538>
ldp	x10, x9, [x8]
sub	x9, x9, x10
asr	x23, x9, #2
cbz	x22, REL:0x548 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x548>
ldp	x10, x9, [x22]
sub	x9, x9, x10
asr	x24, x9, #2
cbz	x8, REL:0x558 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x558>
ldp	x9, x10, [x8]
sub	x8, x10, x9
asr	x8, x8, #2
mov	w10, #0x64ff            ; =25855
movk	w10, #0x1dcd, lsl #16
add	x8, x9, x8, lsl #2
ldur	w8, [x8, #-0x4]
cmp	w8, w10
b.ls	REL:0x588 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x588>
b	REL:0x5b0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
ldrb	w8, [x19, #0xc]
cmp	w8, #0xff
b.ne	REL:0x448 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x448>
mov	w8, #0x0                ; =0
strb	wzr, [x19, #0xc]
ubfiz	x8, x8, #2, #32
mov	x10, x19
cbz	x8, REL:0x484 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x484>
mov	x11, #0x0               ; =0
mov	w9, #0xca00             ; =51712
movk	w9, #0x3b9a, lsl #16
sub	x10, x10, #0x4
ldr	w12, [x10, x8]
add	x11, x11, w12, uxtw
udiv	x12, x11, x22
str	w12, [x10, x8]
msub	x11, x12, x22, x11
mul	x11, x11, x9
subs	x8, x8, #0x4
b.ne	REL:0x464 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x464>
mov	x0, x19
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::tidy()>
b	REL:0xa10 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
ldrb	w8, [sp, #0xc]
cmp	w8, #0xff
b.ne	REL:0x4a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x4a0>
strb	wzr, [sp, #0xc]
mov	x8, sp
ldp	w11, w8, [x8]
umaddl	x8, w8, w9, x11
udiv	x8, x10, x8
lsr	x10, x8, #9
mov	x11, #0x5a53            ; =23123
movk	x11, #0xa09b, lsl #16
movk	x11, #0xb82f, lsl #32
movk	x11, #0x44, lsl #48
umulh	x10, x10, x11
lsr	x10, x10, #11
msub	w8, w10, w9, w8
stp	w8, w10, [sp, #0x48]
add	x8, sp, #0x48
add	x22, sp, #0x20
add	x0, sp, #0x20
add	x1, sp, #0x48
add	x2, x8, #0x8
bl	ADDR <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::DigitStorage(unsigned int const*, unsigned int const*)>
cmp	x19, x22
b.eq	REL:0x9e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
ldr	q0, [sp, #0x30]
stp	xzr, xzr, [sp, #0x30]
ldr	x22, [x19, #0x18]
str	q0, [x19, #0x10]
cbz	x22, REL:0x68c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x68c>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0x530 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x530>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
ldr	x8, [x19, #0x10]
b	REL:0x690 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x690>
ldrb	w9, [sp, #0xc]
cmp	w9, #0xff
csel	w23, wzr, w9, eq
cbnz	x22, REL:0x3fc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x3fc>
ldrb	w9, [x19, #0xc]
cmp	w9, #0xff
csel	w24, wzr, w9, eq
cbnz	x8, REL:0x40c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x40c>
ldrb	w8, [sp, #0xc]
cmp	w8, #0xff
csel	w8, wzr, w8, eq
b.ne	REL:0x56c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x56c>
strb	wzr, [sp, #0xc]
mov	x9, sp
mov	w10, #0x64ff            ; =25855
movk	w10, #0x1dcd, lsl #16
add	x8, x9, x8, lsl #2
ldur	w8, [x8, #-0x4]
cmp	w8, w10
b.hi	REL:0x5b0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b0>
udiv	w22, w10, w8
add	w1, w22, #0x1
mov	x0, x19
mov	w2, #0x0                ; =0
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
mov	x0, sp
add	w1, w22, #0x1
mov	w2, #0x0                ; =0
bl	ADDR <GD::Bc::Details::BasicDigits<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::mac(unsigned int, unsigned int)>
ldr	x22, [x19, #0x10]
str	wzr, [sp, #0x44]
cbz	x22, REL:0x5e4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5e4>
ldp	x8, x9, [x22, #0x8]
str	x8, [sp, #0x48]
add	x10, sp, #0x48
add	x11, sp, #0x44
stp	x10, x11, [sp, #0x20]
str	x22, [sp, #0x30]
cmp	x8, x9
b.hs	REL:0xaa0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xaa0>
str	wzr, [x8], #0x4
str	x8, [x22, #0x8]
b	REL:0x61c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x61c>
ldrb	w8, [x19, #0xc]
cmp	w8, #0x3
b.ne	REL:0x608 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x608>
mov	x0, x19
mov	w1, #0x6                ; =6
bl	ADDR <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
ldr	x22, [x19, #0x10]
cbnz	x22, REL:0x5b8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5b8>
ldrb	w8, [x19, #0xc]
cmp	w8, #0xff
csel	w8, wzr, w8, eq
add	w9, w8, #0x1
strb	w9, [x19, #0xc]
str	wzr, [x19, w8, uxtw #2]
mov	x8, #0x0                ; =0
sub	x25, x24, x23
stp	xzr, xzr, [sp, #0x30]
mov	w9, #0xff               ; =255
strb	w9, [sp, #0x2c]
add	x22, x25, #0x1
strb	wzr, [sp, #0x54]
add	x26, sp, #0x48
stp	xzr, xzr, [sp, #0x58]
str	wzr, [sp, #0x44]
cmp	x22, #0x4
b.lo	REL:0x714 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x714>
add	x0, sp, #0x48
mov	x1, x22
bl	ADDR <GD::Bc::Details::DigitStorage<unsigned int, 3ul>::promote(unsigned long)>
ldr	x0, [sp, #0x58]
cbz	x0, REL:0x700 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x700>
add	x2, sp, #0x44
mov	x1, x22
bl	ADDR <std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::resize(unsigned long, unsigned int const&)>
ldur	q0, [sp, #0x58]
stp	xzr, xzr, [x26, #0x10]
str	q0, [sp, #0x30]
fmov	x8, d0
cbz	x8, REL:0xa8c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa8c>
mov	w8, #0xff               ; =255
strb	w8, [sp, #0x2c]
b	REL:0x764 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
fmov	x8, d0
cbz	x8, REL:0x6a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6a0>
mov	w8, #0xff               ; =255
strb	w8, [x19, #0xc]
b	REL:0x6c0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
ldrb	w8, [sp, #0x2c]
strb	w8, [x19, #0xc]
cmp	x8, #0xff
b.eq	REL:0x6c0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6c0>
lsl	x2, x8, #2
add	x1, sp, #0x20
mov	x0, x19
bl	ADDR <_vfprintf+ADDR>
ldr	x22, [sp, #0x38]
stp	xzr, xzr, [sp, #0x30]
cbz	x22, REL:0x6f4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0x6f4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x6f4>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
mov	w8, #0xff               ; =255
strb	w8, [sp, #0x2c]
b	REL:0x9e0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9e0>
ldrb	w8, [sp, #0x54]
mov	w9, #0xff               ; =255
bics	wzr, w9, w8
csel	w8, wzr, w8, eq
and	x8, x8, #0xff
subs	x9, x22, x8
b.ls	REL:0x734 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
cmp	x9, #0x1
b.lt	REL:0x734 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x734>
add	x10, sp, #0x48
add	x0, x10, x8, lsl #2
lsl	x1, x9, #2
bl	ADDR <_vfprintf+ADDR>
and	w8, w22, #0xff
strb	w22, [sp, #0x54]
ldr	x9, [sp, #0x60]
stp	xzr, xzr, [x26, #0x10]
stp	xzr, x9, [sp, #0x30]
strb	w8, [sp, #0x2c]
cmp	w8, #0xff
b.eq	REL:0x764 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
ubfiz	x2, x8, #2, #32
add	x0, sp, #0x20
add	x1, sp, #0x48
bl	ADDR <_vfprintf+ADDR>
ldr	x8, [x19, #0x10]
cbz	x8, REL:0x774 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x774>
ldr	x8, [x8]
b	REL:0x78c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
ldrb	w9, [x19, #0xc]
mov	x8, x19
cmp	w9, #0xff
b.ne	REL:0x78c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x78c>
strb	wzr, [x19, #0xc]
mov	x8, x19
ldr	x9, [sp, #0x10]
cbz	x9, REL:0x7c0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7c0>
ldr	x14, [x9]
ldr	x9, [sp, #0x30]
cbz	x9, REL:0x7dc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7dc>
ldr	x9, [x9]
mov	w10, #0xca00            ; =51712
movk	w10, #0x3b9a, lsl #16
sub	x11, x8, #0x4
sub	x12, x8, #0x8
add	x13, x14, x23, lsl #2
cbnz	x23, REL:0x808 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x808>
b	REL:0x980 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
ldrb	w9, [sp, #0xc]
cmp	w9, #0xff
b.ne	REL:0x7d0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7d0>
strb	wzr, [sp, #0xc]
mov	x14, sp
ldr	x9, [sp, #0x30]
cbnz	x9, REL:0x7a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7a0>
ldrb	w9, [sp, #0x2c]
cmp	w9, #0xff
b.ne	REL:0x7ec <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x7ec>
strb	wzr, [sp, #0x2c]
add	x9, sp, #0x20
mov	w10, #0xca00            ; =51712
movk	w10, #0x3b9a, lsl #16
sub	x11, x8, #0x4
sub	x12, x8, #0x8
add	x13, x14, x23, lsl #2
cbz	x23, REL:0x980 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x980>
sub	x15, x8, x23, lsl #2
add	x15, x15, x24, lsl #2
mov	x16, #0x5a53            ; =23123
movk	x16, #0xa09b, lsl #16
movk	x16, #0xb82f, lsl #32
movk	x16, #0x44, lsl #48
mov	x17, #0xe827            ; =59431
movk	x17, #0x2e0b, lsl #16
movk	x17, #0x11, lsl #32
b	REL:0x840 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x840>
str	w1, [x9, x0, lsl #2]
sub	x25, x0, #0x1
sub	x15, x15, #0x4
cbz	x0, REL:0x9d4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
mov	x0, x25
add	x2, x25, x23
ldr	w1, [x8, x2, lsl #2]
ldr	w3, [x11, x2, lsl #2]
umaddl	x4, w1, w10, x3
ldur	w3, [x13, #-0x4]
udiv	x1, x4, x3
msub	x4, x1, x3, x4
b	REL:0x874 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x874>
sub	x1, x1, #0x1
add	x4, x4, x3
cmp	x4, x10
b.hs	REL:0x894 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x894>
cmp	x1, x10
b.eq	REL:0x864 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
ldur	w5, [x13, #-0x8]
ldr	w6, [x12, x2, lsl #2]
madd	x6, x4, x10, x6
mul	x5, x1, x5
cmp	x5, x6
b.hi	REL:0x864 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x864>
mov	x4, #0x0                ; =0
mov	x3, #0x0                ; =0
b	REL:0x8b4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8b4>
sub	w5, w6, w5
str	w5, [x15, x4, lsl #2]
add	x4, x4, #0x1
cmp	x23, w4, uxtw
b.ls	REL:0x8e8 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8e8>
ldr	w5, [x14, x4, lsl #2]
madd	x5, x1, x5, x3
lsr	x3, x5, #9
umulh	x3, x3, x16
lsr	x3, x3, #11
msub	w5, w3, w10, w5
ldr	w6, [x15, x4, lsl #2]
cmp	w6, w5
b.hs	REL:0x8a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
add	w6, w6, w10
str	w6, [x15, x4, lsl #2]
add	x3, x3, #0x1
b	REL:0x8a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x8a0>
cbz	x3, REL:0x830 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
umulh	x4, x3, x17
lsr	w4, w4, #2
msub	w4, w4, w10, w3
ldr	w5, [x8, x2, lsl #2]
subs	w4, w5, w4
b.hs	REL:0x910 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x910>
add	w3, w4, w10
str	w3, [x8, x2, lsl #2]
b	REL:0x91c <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x91c>
str	w4, [x8, x2, lsl #2]
cmp	x3, x10
b.lo	REL:0x830 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
mov	x3, #0x0                ; =0
mov	x4, #0x0                ; =0
ldr	w5, [x14, x3, lsl #2]
ldr	w6, [x15, x3, lsl #2]
add	w5, w6, w5
add	x5, x4, w5, uxtw
lsr	x4, x5, #9
umulh	x4, x4, x16
lsr	x4, x4, #11
msub	w5, w4, w10, w5
str	w5, [x15, x3, lsl #2]
add	x3, x3, #0x1
cmp	x23, x3
b.ne	REL:0x924 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x924>
ldr	w3, [x8, x2, lsl #2]
add	x3, x4, x3
umulh	x4, x3, x17
lsr	w4, w4, #2
msub	w3, w4, w10, w3
str	w3, [x8, x2, lsl #2]
sub	x1, x1, #0x1
b	REL:0x830 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x830>
str	w15, [x9, x14, lsl #2]
sub	x25, x14, #0x1
cbz	x14, REL:0x9d4 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9d4>
mov	x14, x25
ldr	w15, [x8, x25, lsl #2]
ldr	w16, [x11, x25, lsl #2]
umaddl	x17, w15, w10, x16
ldur	w16, [x13, #-0x4]
udiv	x15, x17, x16
msub	x17, x15, x16, x17
b	REL:0x9b0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9b0>
sub	x15, x15, #0x1
add	x17, x17, x16
cmp	x17, x10
b.hs	REL:0x974 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
cmp	x15, x10
b.eq	REL:0x9a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
ldur	w0, [x13, #-0x8]
ldr	w1, [x12, x14, lsl #2]
madd	x1, x17, x10, x1
mul	x0, x15, x0
cmp	x0, x1
b.hi	REL:0x9a0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x9a0>
b	REL:0x974 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x974>
add	x0, sp, #0x20
mov	x1, x19
bl	ADDR <std::__1::enable_if<is_move_constructible<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value && is_move_assignable<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>::value, void>::type std::__1::swap[abi:nqe220106]<GD::Bc::Details::DigitStorage<unsigned int, 3ul>>(GD::Bc::Details::DigitStorage<unsigned int, 3ul>&, GD::Bc::Details::DigitStorage<unsigned int, 3ul>&)>
ldr	x22, [sp, #0x38]
cbz	x22, REL:0xa10 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0xa10 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa10>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
ldr	x22, [sp, #0x18]
cbz	x22, REL:0xa40 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
add	x8, x22, #0x8
mov	x9, #-0x1               ; =-1
ldaddal	x9, x8, [x8]
cbnz	x8, REL:0xa40 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xa40>
ldr	x8, [x22]
ldr	x8, [x8, #0x10]
mov	x0, x22
blr	x8
mov	x0, x22
bl	ADDR <_vfprintf+ADDR>
str	w21, [x19, #0x24]
ldr	w8, [x19, #0x20]
ldr	w9, [x20, #0x20]
cmp	w8, w9
cset	w8, ne
str	w8, [x19, #0x20]
ldur	x8, [x29, #-0x48]
adrp	x9, ADDR <GD::Bc::MessageData::bc_strs_>
ldr	x9, [x9, #0xb18]
ldr	x9, [x9]
cmp	x9, x8
b.ne	REL:0xab0 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0xab0>
ldp	x29, x30, [sp, #0xb0]
ldp	x20, x19, [sp, #0xa0]
ldp	x22, x21, [sp, #0x90]
ldp	x24, x23, [sp, #0x80]
ldp	x26, x25, [sp, #0x70]
add	sp, sp, #0xc0
ret
ldrb	w8, [sp, #0x54]
strb	w8, [sp, #0x2c]
cmp	w8, #0xff
b.ne	REL:0x754 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x754>
b	REL:0x764 <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x764>
add	x0, sp, #0x20
bl	ADDR <unsigned int& std::__1::vector<unsigned int, std::__1::allocator<unsigned int>>::emplace_back<unsigned int const&>(unsigned int const&)::'lambda0'()::operator()() const>
ldr	x8, [sp, #0x48]
b	REL:0x5dc <GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>::divide(GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, unsigned int)+0x5dc>
bl	ADDR <_vfprintf+ADDR>
mov	x19, x0
mov	x0, x20
bl	ADDR <_vfprintf+ADDR>
mov	x0, sp
bl	ADDR <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
mov	x0, x19
bl	ADDR <_vfprintf+ADDR>
mov	x19, x0
add	x0, sp, #0x48
bl	ADDR <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
add	x0, sp, #0x20
bl	ADDR <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
mov	x0, sp
bl	ADDR <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
mov	x0, x19
bl	ADDR <_vfprintf+ADDR>
bl	ADDR <_vfprintf+ADDR>
mov	x19, x0
mov	x0, sp
bl	ADDR <std::__1::pair<GD::Bc::BasicNumber<GD::Bc::NumberTraitsBase<unsigned int, unsigned long long, 9u>>, bool>::~pair()>
mov	x0, x19
bl	ADDR <_vfprintf+ADDR>
