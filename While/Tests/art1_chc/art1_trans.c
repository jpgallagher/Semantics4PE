extern int rand();
void assert(int e);

void main() {
   int a;
   int b;
   assert(a>=0 && b>=0);
   while (a>=1 && !b>=1){
      a=a-1;
   }
   while (a>=1 && b>=1){
      b=b-1;
      while (a>=1 && !b>=1){
         a=a-1;
      }
   }
}
