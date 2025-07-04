void assert(int e);

void main() {
	int i,j;
	assert(i>=0 && j>=0);
	while (i>=1) {
   		if (j<=0) 
      		{j=1-j;
       		i--;}
   		else
      		{j--;}
   	}
}
