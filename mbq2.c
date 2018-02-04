#include <stdio.h>
#define N 2000000
           

/*

  For this microbenchmark, we use only L1 data cache with next line prefetching. (No data L2)
  To keep things simple, our configuration is a fully associative cache with 2 ways and block size 8 bytes.
  It uses LRU replacement policy.

  The microbenchmark was compiled using the following command:

  	ssbig-na-sstrix-gcc mbq2.c -O1 -o mbq2

  and run using:
	./sim-cache -config q2.cfg mbq2

*/




int main()
{
	int i;
	int j;
	int array[N];
	int barray[N/2];
	//access with stride 1 so all of these are hits ...> N hits 
	for(i = 0; i < N ; i++){
				
		array[i] = i;
		
	}

	
	
	//variable stride throughout so all are misses .....> 20 accesses , 20 misses 
	for(j = 0; j < N/2; j += 1){
		
		barray[j] = array[(N)%(1+rand())];
		
		//j = j + j/2;
		
	}

	
	


	return 0;
}


