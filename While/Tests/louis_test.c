extern int rand();


int main(){
 int x = 0;
 int y = 20;
 while (y>=1 && rand()){
   if (x<=9)
     x++;
   else
     x--;
   y--;
 }
}
