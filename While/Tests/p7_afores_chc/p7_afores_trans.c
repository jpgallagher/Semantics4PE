extern int rand();
void assert(int e);

void main() {
   int i;
   int n;
   int r;
   int nondet;
   while (i<n && !r>0 && rand()){
      i=i+1;
   }
   while (i<n && r>0 && rand()){
      i=0;
      r=r-1;
      while (i<n && !r>0 && rand()){
         i=i+1;
      }
   }
}
