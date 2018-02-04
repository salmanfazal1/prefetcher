#include <stdio.h>

#define N 600000

/*

  For this microbenchmark, we use only L1 data cache with next line prefetching. (No data L2)
  To keep things simple, our configuration is a fully associative cache with 2 ways and block size 8 bytes.
  It uses LRU replacement policy.

  The microbenchmark was compiled using the following command:

  	ssbig-na-sstrix-gcc mbq1.c -O1 -o mbq1

  and run using:
	./sim-cache -config q1.cfg mbq1

*/


void foo(int n)
{	
	//For Loop iterator variable
	int i;

	//Instantiate an array of size 'n'
	int a[n];

	//Access every second element in the array
	//We access every second element, because an int is 4 bytes, though minimum support cache block size is 8 bytes
	//This way, we are not getting hits because of accessing the same cache block, but in fact due to the next line prefetcher
	//Next-line prefetcher should ensure ~100% hit rate
	//Because of this expect ~n/2 cache hits
	for (i = 0; i < n; i+=2)
	{
		a[i] = i;
	}

	//Access every fourth element in the array (Next-line prefetcher should always fail)
	//This ensures that prefetcher only fetches next line (and not the line after as well)
	//Because of this expect ~n/4 cache misses
	for (i = 0; i < n; i+=4)
	{
		a[i] = i;
	}

}

int main()
{

	foo(N);

	return 0;
}

/*

Assembly code for microbenchmark, important lines denoted with *** and comments

foo():
mbq1.c:15
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
  400230:	28 00 00 00 	lw $2,0($29)
  400234:	00 00 02 1d 
  400238:	42 00 00 00 	addu $5,$0,$29
  40023c:	00 05 1d 00 
  400240:	42 00 00 00 	addu $3,$0,$0
  400244:	00 03 00 00 
  400248:	07 00 00 00 	blez $4,400280 <foo+0x90>
  40024c:	0c 00 00 04 
  400250:	55 00 00 00 	sll $2,$3,0x2					*** Start of first loop
  400254:	02 02 03 00 
  400258:	42 00 00 00 	addu $2,$2,$5
  40025c:	00 02 05 02 
  400260:	34 00 00 00 	sw $3,0($2)					*** Memory access to array a[i] in the first loop
  400264:	00 00 03 02 
  400268:	43 00 00 00 	addiu $3,$3,2
  40026c:	02 00 03 03 
  400270:	5b 00 00 00 	slt $2,$3,$4
  400274:	00 02 04 03 
  400278:	06 00 00 00 	bne $2,$0,400250 <foo+0x60>			*** Branch for first loop
  40027c:	f4 ff 00 02 
  400280:	42 00 00 00 	addu $3,$0,$0
  400284:	00 03 00 00 
  400288:	07 00 00 00 	blez $4,4002c0 <foo+0xd0>
  40028c:	0c 00 00 04 
  400290:	55 00 00 00 	sll $2,$3,0x2					*** Start of second loop
  400294:	02 02 03 00 
  400298:	42 00 00 00 	addu $2,$2,$5
  40029c:	00 02 05 02 
  4002a0:	34 00 00 00 	sw $3,0($2)					*** Memory access to array a[i] in the second loop
  4002a4:	00 00 03 02 
  4002a8:	43 00 00 00 	addiu $3,$3,4
  4002ac:	04 00 03 03 
  4002b0:	5b 00 00 00 	slt $2,$3,$4
  4002b4:	00 02 04 03 
  4002b8:	06 00 00 00 	bne $2,$0,400290 <foo+0xa0>			*** Branch for second loop
  4002bc:	f4 ff 00 02 
  4002c0:	42 00 00 00 	addu $29,$0,$30
  4002c4:	00 1d 1e 00 
  4002c8:	28 00 00 00 	lw $30,32($29)
  4002cc:	20 00 1e 1d 
  4002d0:	43 00 00 00 	addiu $29,$29,40
  4002d4:	28 00 1d 1d 
  4002d8:	03 00 00 00 	jr $31
  4002dc:	00 00 00 1f 
*/
