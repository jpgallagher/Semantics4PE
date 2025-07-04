extern int rand();
void assert(int e);

void main() {
   int x;
   int z;
   int n;
   assert(x<=n && n>=0 && x<=z);
   while (x<=n-1 && !z>x){
      z=z+1;
   }
   while (x<=n-1 && z>x){
      x=x+1;
      while (x<=n-1 && !z>x){
         z=z+1;
      }
   }
}
