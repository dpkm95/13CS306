#include<stdio.h>
#include<stdlib.h>

int sum(int, int)
;
int mul(int, int);
int sub(int, int);
int divd(int, int);

int main()
{
	int a, b;
	printf("Enter two numbers : ");
	scanf("%d %d", &a, &b);
	int s = sum(a,b);
	printf("The sum of the two numbers is : %d\n", s);
	int m = mul(a, b);
	printf("The product of the two numbers is : %d\n", m);
	int d = sub(a, b);
	printf("The difference of the two numbers is : %d\n", d);
	int di = divd(a, b);
	printf("The division of the two numbers is : %d\n", di);
	return 0;
}

int sum(int a, int b)

{
	return a+b;
}

int sub(int a, int b)
{
	return a - b;
}

int mul(int a, int b)
{
	return a * b;
}

int divd(int a, int b)
{
	if (b == 0)
		return -1;
	else
		return a / b;
}
