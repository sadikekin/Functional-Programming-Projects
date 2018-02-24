#include <iostream>

using namespace std;

int main() {
  int sum = 0;
  for(int i=2018;i<=2418;i++){
    sum += 365;
    if(i%4 == 0)
      sum += 1;
  }
  printf("number of weeks in 400 years %f\n", sum/7.0);
  return 0;
}
