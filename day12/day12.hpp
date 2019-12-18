#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <vector>

struct Particle {
  long position;
  long velocity;

  bool operator!=(const Particle &other) const { return (position != other.position) || (velocity != other.velocity); }
};

class SubSystem {
  public:
  SubSystem(){};
  SubSystem(const SubSystem &other) : particles(other.size()) {
    size_t num = other.size();
    for (size_t n = 0; n < num; ++n) {
      particles[n].position = other.particles[n].position;
      particles[n].velocity = other.particles[n].velocity;
    }
  }
  size_t size() const { return particles.size(); }
  void add_particle(long position, long velocity = 0) { particles.push_back({position, velocity}); }
  void update_velocity() {
    size_t num = size();
    for (size_t i = 0; i < num; ++i) {
      for (size_t j = i + 1; j < num; ++j) {
        Particle &p1 = particles[i];
        Particle &p2 = particles[j];
        long delta   = (p1.position < p2.position) - (p1.position > p2.position);
        p1.velocity += delta;
        p2.velocity -= delta;
      }
    }
  }

  void update_position() {
    for (Particle &p : particles) { p.position += p.velocity; }
  }

  void step() {
    update_velocity();
    update_position();
  }

  size_t find_period() {
    SubSystem initial(*this);
    size_t count = 0;
    while (true) {
      step();
      count++;
      if (*this == initial) break;
    }
    return count;
  }

  const Particle &operator()(size_t index) const { return particles[index]; }
  bool operator==(const SubSystem &other) const {
    size_t num = other.size();
    if (num != size()) return false;
    for (size_t n = 0; n < num; ++n) {
      if (particles[n] != other.particles[n]) return false;
    }
    return true;
  }

  private:
  std::vector<Particle> particles;
};

struct System {
  SubSystem x;
  SubSystem y;
  SubSystem z;

  size_t size() const { return x.size(); }

  System(char *filename) {
    std::ifstream stream(filename);
    if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }

    long x0, y0, z0;
    std::string line;
    while (std::getline(stream, line)) {
      int number = std::sscanf(line.c_str(), "<x=%ld, y=%ld, z=%ld>", &x0, &y0, &z0);
      if (number != 3) break;
      x.add_particle(x0);
      y.add_particle(y0);
      z.add_particle(z0);
    }
  }

  void step() {
    x.step();
    y.step();
    z.step();
  }

  long steps(size_t num) {
    for (size_t i = 0; i < num; ++i) { step(); }
    return energy();
  }

  long energy() const {
    long energy = 0;
    for (size_t n = 0; n < size(); ++n) {
      auto px = x(n);
      auto py = y(n);
      auto pz = z(n);
      long E  = std::abs(px.position) + std::abs(py.position) + std::abs(pz.position);
      E *= std::abs(px.velocity) + std::abs(py.velocity) + std::abs(pz.velocity);
      energy += E;
    }
    return energy;
  }

  friend std::ostream &operator<<(std::ostream &os, const System &system) {
    for (size_t n = 0; n < system.size(); ++n) {
      os << "<x=" << system.x(n).position;
      os << ", y=" << system.y(n).position;
      os << ", z=" << system.z(n).position;
      os << ", vx=" << system.x(n).velocity;
      os << ", vy=" << system.y(n).velocity;
      os << ", vz=" << system.z(n).velocity;
      os << ">" << std::endl;
    }
    return os;
  }
};

template <typename T>
T gcd(const T &a, const T &b) {
  if (b == 0) return a;
  return gcd(b, a % b);
}

template <typename T>
T lcm(const T &a, const T &b) {
  return a * b / gcd(a, b);
}
