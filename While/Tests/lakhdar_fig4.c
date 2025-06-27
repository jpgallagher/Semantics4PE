extern int rand();

void main() {	
	int i;
	i=0; 
	while (1) {
		if (rand()) {
			i=i+1;
			if (i>=100)	
				i=0;
		}
	}
}
