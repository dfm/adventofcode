#[derive(Default, Debug, Clone)]
pub struct CharGrid {
  pub(crate) width: usize,
  pub(crate) height: usize,
  pub(crate) data: Vec<u8>,
}

impl CharGrid {
  pub fn new(data: &str) -> Self {
    let mut grid: Self = Default::default();
    for line in data.lines() {
      grid.width = line.as_bytes().iter().map(|&c| grid.data.push(c)).count();
      grid.height += 1;
    }
    grid
  }

  pub fn to_coords(&self, idx: usize) -> (usize, usize) {
    let y = idx / self.width;
    (idx - y * self.width, y)
  }

  pub fn to_index(&self, x: usize, y: usize) -> usize {
    y * self.width + x
  }

  pub fn get(&self, x: usize, y: usize) -> u8 {
    self.data[self.to_index(x, y)]
  }
  
  pub fn get_mut(&mut self, x: usize, y: usize) -> &mut u8 {
    let idx = self.to_index(x, y);
    &mut self.data[idx]
  }
}
