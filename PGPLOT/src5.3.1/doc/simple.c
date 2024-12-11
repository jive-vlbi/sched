#include "cpgplot.h"

int main(void)
{
  int i;
  float xs[5] = {1., 2., 3., 4., 5.};
  float ys[5] = {1., 4., 9., 16., 25.};
  float x, xr[100], yr[100];

  if (cpgopen("?") < 1)
    return 1;
  cpgenv(0., 10., 0., 20., 0, 1);
  cpglab("(x)", "(y)", "A Simple Graph");
  cpgpt(5, xs, ys, 9);
  for (i=1; i<=60; i++) {
    x = 0.1*i;
    xr[i-1] = x;
    yr[i-1] = x*x;
  }
  cpgline(60, xr, yr);
  cpgclos();
  return 0;
}
