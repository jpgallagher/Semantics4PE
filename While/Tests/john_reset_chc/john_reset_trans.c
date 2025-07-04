extern int rand();
void assert(int e);

void main() {
   int i;
   int j;
   assert(i>=0 && j>=0);
   while (i>0 && !j<=0){
      j=j-1;
   }
   while (i>0 && j<=0){
      j=10;
      i=i-1;
      while (i>0 && !j<=0){
         j=j-1;
      }
   }
}
