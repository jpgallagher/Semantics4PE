extern int rand();


int main(){
 int x = 0;
 int y = 20;
 while (y>0 && rand()){
   if (x<10)
     x++;
   else
     x--;
   y--;
 }
}
