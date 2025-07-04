extern int rand();
void assert(int e);

void main() {
   int x;
   int y;
   int m;
   assert(x>=0 && y>=0 && y<=m);
   while (x>=1 && !y<=m-1){
      x=x-1;
   }
   while (x>=1 && y<=m-1){
      y=y+1;
      while (x>=1 && !y<=m-1){
         x=x-1;
      }
   }
}
