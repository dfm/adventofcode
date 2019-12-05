#include <cmath>
#include <iostream>

inline int get_fuel(int mass) {
  int fuel_mass = int(double(mass) / 3) - 2;
  if (fuel_mass > 0) {
    return fuel_mass + get_fuel(fuel_mass);
  } else {
    return 0;
  }
}

int main() {
  std::string line;
  int fuel = 0;
  while (std::getline(std::cin, line)) {
    try {
      fuel += get_fuel(std::stoi(line));
    } catch (const std::invalid_argument &exception) { std::cout << "invalid mass" << std::endl; };
  }
  std::cout << fuel << std::endl;
}
