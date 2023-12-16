#[derive(Default, Clone)]
pub struct CharGrid<T = u8> {
  pub(crate) width: usize,
  pub(crate) height: usize,
  pub(crate) data: Vec<T>,
}

impl CharGrid<u8> {
  pub fn new(data: &str) -> Self {
    let mut grid: Self = Default::default();
    for line in data.lines() {
      grid.width = line.as_bytes().iter().map(|&c| grid.data.push(c)).count();
      grid.height += 1;
    }
    grid
  }
}

impl<T: Copy> CharGrid<T> {
  pub fn to_coords(&self, idx: usize) -> (usize, usize) {
    let y = idx / self.width;
    (idx - y * self.width, y)
  }

  pub fn to_index(&self, x: usize, y: usize) -> usize {
    y * self.width + x
  }

  pub fn in_bounds(&self, x: i64, y: i64) -> bool {
    0 <= x && x < self.width as i64 && 0 <= y && y < self.height as i64
  }

  pub fn get(&self, x: usize, y: usize) -> T {
    self.data[self.to_index(x, y)]
  }

  pub fn get_mut(&mut self, x: usize, y: usize) -> &mut T {
    let idx = self.to_index(x, y);
    &mut self.data[idx]
  }

  pub fn get_checked(&self, x: i64, y: i64) -> Option<T> {
    if self.in_bounds(x, y) {
      Some(self.data[self.to_index(x as usize, y as usize)])
    } else {
      None
    }
  }

  pub fn get_mut_checked(&mut self, x: i64, y: i64) -> Option<&mut T> {
    if self.in_bounds(x, y) {
      let idx = self.to_index(x as usize, y as usize);
      Some(&mut self.data[idx])
    } else {
      None
    }
  }
}

impl std::fmt::Debug for CharGrid<u8> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for y in 0..self.height {
      for x in 0..self.width {
        write!(f, "{}", self.get(x, y) as char)?;
      }
      writeln!(f)?;
    }
    Ok(())
  }
}

impl std::fmt::Debug for CharGrid<bool> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for y in 0..self.height {
      for x in 0..self.width {
        if self.get(x, y) {
          write!(f, "*")?;
        } else {
          write!(f, ".")?;
        }
      }
      writeln!(f)?;
    }
    Ok(())
  }
}
