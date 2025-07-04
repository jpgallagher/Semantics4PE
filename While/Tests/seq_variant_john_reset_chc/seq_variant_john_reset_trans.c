extern int rand();
void assert(int e);

void main() {
   int i;
   int j;
   assert(i>=0 && j>=0);
   while (i>=1){
      j=10;
      i=i-1;
   }
   while (j>=1){
      j=j-1;
   }
}
