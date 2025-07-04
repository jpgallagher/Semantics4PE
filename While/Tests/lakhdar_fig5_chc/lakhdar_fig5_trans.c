extern int rand();
void assert(int e);

void main() {
   int i;
   int j;
   i=0;
   j=0;
   while (i<=9){
      j=0;
      while (j<=9){
         j=j+1;
      }
      i=i+1;
   }
}
