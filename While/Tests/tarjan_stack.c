extern int rand();
void assert(int e);
int m;

void main() {
	int i;
	int n = 0;
	assert(m>=0);
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

