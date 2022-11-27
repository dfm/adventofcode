#include "aoc/registry.hpp"

namespace aoc {
namespace registry {

registry_t &get_registry() {
  static registry_t registry;
  return registry;
}

}  // namespace registry
}  // namespace aoc