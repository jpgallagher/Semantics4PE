extern int rand();

void main() {
	int i,n,r,nondet;
	while (i<n) {
    	if (r>0 && rand()){
        	i=0;
        	r--;
    	}
    		else i++;
	}
}
