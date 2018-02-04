#include <stdio.h>

#define N 65536

/*

  For this microbenchmark, we used L1 data cache with our open-ended prefetcher (Address correlator using index table and global history buffer)
  To keep things simple, our configuration is a fully associative cache with 2 ways and block size 64 bytes.
  It uses LRU replacement policy.

  The microbenchmark was compiled using the following command:

  	ssbig-na-sstrix-gcc mbq6.c -O1 -o mbq6

  and run using:

	./sim-cache -config q6.cfg mbq6

*/

void foo (int n)
{

	//For Loop iterator variable
	int i, j;

	//2 Arrays
	int a[n], b[n];

	//We expect these accesses to all be Misses (Access every 16th element to avoid spatial locality hits)
	//We also expect the index table and GHB to record these misses in the order
	// older <- a[i], b[i], a[i+16], b[i+16], a[i+32], b[i+32], etc. <- newer
	// Expect 2 * n / 16 misses in total from this loop
	for (i = 0; i < n; i+= 16)
	{
		a[i] = 0;
		b[i] = 1;
	}

	//Similar to first loop, expect a[i] to miss (always) generating n/16 misses
	//However, because address correlator prefetcher should recognize that after seeing a[i] miss, it should prefetch b[i]
	//So, b[i]'s should hit, resulting in n/16 hits 
	for (i = 0; i < n; i+= 16)
	{
		a[i] = 1;
		b[i] = 2;
	}

}

int main ()
{

	foo(N);

	return 0;

}

/*

Assembly code for foo function

004001f0 <foo>:
foo():
mbq6.c:6
  4001f0:	43 00 00 00 	addiu $29,$29,-40
  4001f4:	d8 ff 1d 1d 
  4001f8:	34 00 00 00 	sw $30,32($29)
  4001fc:	20 00 1e 1d 
  400200:	42 00 00 00 	addu $30,$0,$29
  400204:	00 1e 1d 00 
  400208:	55 00 00 00 	sll $2,$4,0x2
  40020c:	02 02 04 00 
  400210:	43 00 00 00 	addiu $2,$2,14
  400214:	0e 00 02 02 
  400218:	57 00 00 00 	srl $2,$2,0x3
  40021c:	03 02 02 00 
  400220:	55 00 00 00 	sll $2,$2,0x3
  400224:	03 02 02 00 
  400228:	45 00 00 00 	subu $29,$29,$2
  40022c:	00 1d 02 1d 
  400230:	28 00 00 00 	lw $3,0($29)
  400234:	00 00 03 1d 
  400238:	42 00 00 00 	addu $7,$0,$29
  40023c:	00 07 1d 00 
  400240:	45 00 00 00 	subu $29,$29,$2
  400244:	00 1d 02 1d 
  400248:	28 00 00 00 	lw $2,0($29)
  40024c:	00 00 02 1d 
  400250:	42 00 00 00 	addu $5,$0,$0
  400254:	00 05 00 00 
  400258:	42 00 00 00 	addu $6,$0,$29
  40025c:	00 06 1d 00 
  400260:	07 00 00 00 	blez $4,4002b0 <foo+0xc0>
  400264:	12 00 00 04 
  400268:	43 00 00 00 	addiu $8,$0,1
  40026c:	01 00 08 00 
  400270:	55 00 00 00 	sll $2,$5,0x2					*** beginning of first for loop
  400274:	02 02 05 00 
  400278:	42 00 00 00 	addu $3,$2,$7
  40027c:	00 03 07 02 
  400280:	42 00 00 00 	addu $2,$2,$6
  400284:	00 02 06 02 
  400288:	43 00 00 00 	addiu $5,$5,16
  40028c:	10 00 05 05 
  400290:	34 00 00 00 	sw $0,0($3)					*** Memory access to a[i]
  400294:	00 00 00 03 
  400298:	34 00 00 00 	sw $8,0($2)					*** Memory access to b[i]
  40029c:	00 00 08 02 
  4002a0:	5b 00 00 00 	slt $2,$5,$4
  4002a4:	00 02 04 05 
  4002a8:	06 00 00 00 	bne $2,$0,400270 <foo+0x80>			*** Branch first for loop
  4002ac:	f0 ff 00 02 
  4002b0:	42 00 00 00 	addu $5,$0,$0
  4002b4:	00 05 00 00 
  4002b8:	07 00 00 00 	blez $4,400310 <foo+0x120>
  4002bc:	14 00 00 04 
  4002c0:	43 00 00 00 	addiu $9,$0,1
  4002c4:	01 00 09 00 
  4002c8:	43 00 00 00 	addiu $8,$0,2
  4002cc:	02 00 08 00 
  4002d0:	55 00 00 00 	sll $2,$5,0x2					*** Beginning of second for loop
  4002d4:	02 02 05 00 
  4002d8:	42 00 00 00 	addu $3,$2,$7
  4002dc:	00 03 07 02 
  4002e0:	42 00 00 00 	addu $2,$2,$6
  4002e4:	00 02 06 02 
  4002e8:	43 00 00 00 	addiu $5,$5,16
  4002ec:	10 00 05 05 
  4002f0:	34 00 00 00 	sw $9,0($3)					*** Memory access to a[i]
  4002f4:	00 00 09 03 
  4002f8:	34 00 00 00 	sw $8,0($2)					*** Memory access to b[i]
  4002fc:	00 00 08 02 
  400300:	5b 00 00 00 	slt $2,$5,$4
  400304:	00 02 04 05 
  400308:	06 00 00 00 	bne $2,$0,4002d0 <foo+0xe0>			*** Branch second for loop
  40030c:	f0 ff 00 02 
  400310:	42 00 00 00 	addu $29,$0,$30
  400314:	00 1d 1e 00 
  400318:	28 00 00 00 	lw $30,32($29)
  40031c:	20 00 1e 1d 
  400320:	43 00 00 00 	addiu $29,$29,40
  400324:	28 00 1d 1d 
  400328:	03 00 00 00 	jr $31
  40032c:	00 00 00 1f 

*/
