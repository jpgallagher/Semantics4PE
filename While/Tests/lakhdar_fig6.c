
void main() {	
	int i,j,b;
	i=0; 
	j=0;
	b=0;
	while (b==0) {
		if (i<=50)
			j=j+1;
		else
			j=j-1;
		if (j<0) 
			b=1;
		else
			i=i+1;
	}
}
