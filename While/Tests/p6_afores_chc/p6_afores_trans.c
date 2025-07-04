extern int rand();
void assert(int e);

void main() {
   int i;
   int n;
   int r;
   while (i<n && !r>0){
      i=i+1;
   }
   while (i<n && r>0){
      i=0;
      r=r-1;
      while (i<n && !r>0){
         i=i+1;
      }
   }
}
