#include <iostream>

using namespace std;

int main()
{
  cout << "Hello, World!\n";

  double initamount = 10000;
  double target = initamount * 2;
  int year = 0;
  for (year = 0; initamount < target; year++) {
    initamount = initamount * 1.05;
  }
  cout << year << " years to accumulate " << target << " credits.\n";

  return 0;
}
