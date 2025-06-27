void assert(int e);

void main() {
	int i,j;
	assert(i>=0 && j>=0);
	while (i>0)
   	{
   		if (j<=0)
      		{j=10;
       		i--;}
   		else
      		{j--;}
   }
}
