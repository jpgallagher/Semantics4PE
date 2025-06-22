extern int rand();
int m;

void main() {
	int i,m;
	int n = 0;
	i = m;
	while (i>0) {
		i--;
		if (rand())
			n++;
		else {
			while(n>0 && rand()) {
			   n--;
			}
		}
	}
	
}

