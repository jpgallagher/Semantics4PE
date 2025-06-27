extern int rand();

void main() {	
	int i,j;
	i=0; 
	j=0;
	while (i<=9 || j<=9) {
		if (rand() && j<=9)
			j=j+1;
		if (rand() && i<=9)
			i=i+1;
	}
}
