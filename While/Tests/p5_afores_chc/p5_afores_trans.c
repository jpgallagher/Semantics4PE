extern int rand();
void assert(int e);

void main() {
   int i;
   int n;
   int fwd;
   assert(fwd==0 || fwd==1);
   while (0<i && i<n && !fwd){
      i=i-1;
   }
   while (0<i && i<n && fwd){
      i=i+1;
      while (0<i && i<n && !fwd){
         i=i-1;
      }
   }
}
