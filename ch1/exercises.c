
#include <stdio.h>

void ex_1_6_1(void)
{
    int w, x, y, z;
    int i = 4; int j = 5;
    {
        int j = 7;
        i = 6;
        w = i + j;
    }
    x = i + j;
    {
        int i = 8;
        y = i + j;
    }
    z = i + j;
    printf("Exercise 1.6.1\n");
    printf("w = %d\n", w);
    printf("x = %d\n", x);
    printf("y = %d\n", y);
    printf("z = %d\n", z);
}

void ex_1_6_2(void)
{
    int w, x, y, z;
    int i = 3; int j = 4;
    {
        int i = 5;
        w = i + j;
        }
    x = i + j;
    {
        int j = 6;
        i = 7;
        y = i + j;
    }
    z = i + j;
    printf("Exercise 1.6.2\n");
    printf("w = %d\n", w);
    printf("x = %d\n", x);
    printf("y = %d\n", y);
    printf("z = %d\n", z);
}

#define a (x + 1)
int x = 2;
void b() { x = a; printf("%d\n", x); }
void c() { int x = 1; printf("%d\n", a); }

void ex_1_6_4(void)
{
    printf("Exercise 1.6.4\n");
    b(); c();
}

int main(void)
{
    ex_1_6_1();
    ex_1_6_2();
    ex_1_6_4();
}
