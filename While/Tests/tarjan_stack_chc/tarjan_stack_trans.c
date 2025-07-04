extern int rand();
void assert(int e);

void main() {
   int m;
   int i;
   int n=0;
   assert(m>=0);
   i=m;
   while (i>=1){
      i=i-1;
      if (rand()){
         n=n+1;
      }
      else {
         while (n>=1 && rand()){
            n=n-1;
         }
      }
   }
}
