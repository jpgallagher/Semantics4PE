void assert(int e);

void main() {
	int i,n,fwd;
	assert(fwd==0 || fwd==1);
	while (0<i && i<n) {
		if(fwd)
			i++;
		else
			i--;
	}
}
