void assert(int e);

void main() {
	int a,b;
	assert(a>=0 && b>=0);
	while(a>=1){
    	if(b>=1)
        	b--;
    	else
        	a--;
	}
}

