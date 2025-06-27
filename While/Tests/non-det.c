extern int rand();
void assert(int e);

void main() {
	int a,b,c;
	assert(a>=0 && b>=0);
	while(a>0 && b>0){
    	if(rand())
        	a--;
    	else
        	a++;
        b--;
	}
}
