extern int rand();
void assert(int e);

void main() {
   int a;
   int b;
   int c;
   assert(a>=0 && b>=0);
   while (a>0 && b>0 && !rand()){
      a=a+1;
      b=b-1;
   }
   while (a>0 && b>0 && rand()){
      a=a-1;
      while (a>0 && b>0 && !rand()){
         a=a+1;
         b=b-1;
      }
   }
}
