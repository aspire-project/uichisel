#include <stdio.h>
int main() { 
	 FILE* __file_out;
 	 __file_out = fopen("tmp", "w");
 	 fprintf(__file_out, "hello");
 	 fflush(__file_out);
 	 fclose(__file_out);
 return 0;}
