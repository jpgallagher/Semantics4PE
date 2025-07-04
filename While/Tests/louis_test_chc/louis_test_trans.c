extern int rand();
void assert(int e);

void main() {
   int x=0;
   int y=20;
   while (y>=1 && rand() && !x<=9){
      x=x-1;
      y=y-1;
   }
   while (y>=1 && rand() && x<=9){
      x=x+1;
      y=y-1;
      while (y>=1 && rand() && !x<=9){
         x=x-1;
         y=y-1;
      }
   }
}
