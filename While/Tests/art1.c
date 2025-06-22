void assert(int e);

void main() {
	int a,b;
	assert(a>=0 && b>=0);
	while(a>0){
    	if(b>0)
        	b--;
    	else
        	a--;
	}
}

