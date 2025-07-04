extern int rand();
void assert(int e);

void main() {
   int a;
   int b;
   while (a>=1 && !b>=1){
      a=a-1;
   }
   while (a>=1 && b>=1){
      a=a+b;
      b=b-1;
      while (a>=1 && !b>=1){
         a=a-1;
      }
   }
}
