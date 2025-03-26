use std::fmt;
use std::ops::{Add, Mul, Sub};

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct HexCoord(i8, i8);

impl Add for HexCoord {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        HexCoord(self.0 + other.0, self.1 + other.1)
    }
}

impl Sub for HexCoord {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        HexCoord(self.0 - other.0, self.1 - other.1)
    }
}

impl Mul<i8> for HexCoord {
    type Output = Self;

    fn mul(self, other: i8) -> Self {
        HexCoord(self.0 * other, self.1 * other)
    }
}

impl fmt::Display for HexCoord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hex({}, {})", self.0, self.1)
    }
}

pub const HEXDIR: [HexCoord; 6] = [
    HexCoord(1, -1),
    HexCoord(1, 0),
    HexCoord(0, 1),
    HexCoord(-1, 1),
    HexCoord(-1, 0),
    HexCoord(0, -1),
];

impl HexCoord {
    pub fn origin() -> Self {
        HexCoord(0, 0)
    }

    pub fn neighbors(&self) -> impl Iterator<Item = Self> {
        let origin = *self;
        HEXDIR.iter().map(move |&x| x + origin)
    }

    pub fn to_offset(&self) -> (i8, i8) {
        // convert to row/column offset coordinates

        let col = self.0 + (self.1 - (self.1 & 1)) / 2;
        let row = self.1;
        (row, col)
    }

    pub fn dist(&self, other: &Self) -> i8 {
        let v = *self - *other;
        ((v.0).abs() + (v.0 + v.1).abs() + (v.1).abs()) / 2
    }

    pub fn shared_neighbors(&self, other: &Self) -> Option<(Self, Self)> {
        // return the two shared neighbors of two coords, if any
        let dir = match *other - *self {
            HexCoord(1, 0) => (HexCoord(1, -1), HexCoord(0, 1)),
            HexCoord(0, 1) => (HexCoord(1, 0), HexCoord(-1, 1)),
            HexCoord(-1, 1) => (HexCoord(0, 1), HexCoord(-1, 0)),
            HexCoord(-1, 0) => (HexCoord(-1, 1), HexCoord(0, -1)),
            HexCoord(0, -1) => (HexCoord(-1, 0), HexCoord(1, -1)),
            HexCoord(1, -1) => (HexCoord(0, -1), HexCoord(1, 0)),
            _ => return None,
        };

        Some((*self + dir.0, *self + dir.1))
    }
}

// replacement for HashMap<HexCoord, T> using a linear spiral buffer for storage
#[derive(Default, Debug, Clone)]
pub struct SpiralBufMap<T> {
    buf: Vec<Option<(HexCoord, T)>>,
    len: usize,
}

impl<T> SpiralBufMap<T> {
    fn idx(c: HexCoord) -> usize {
        let n = c.dist(&HexCoord::origin()); // radius
        let HexCoord(q, r) = c;
        let (q, r, n) = (q as i16, r as i16, n as i16); // so later ops don't overflow

        // let ring_start = if n == 0 {
        //     0
        // } else {
        //     //(3 * n * n) - (3 * n) + 1
        //     (3 * n * (n - 1)) + 1
        // };

        let ring_start = match n {
            0 => 0,
            1 => 1,
            2 => 7,
            3 => 19,
            4 => 37,
            5 => 61,
            6 => 91,
            x => (3 * x * (x - 1)) + 1,
        };

        let ring_idx = if r == 0 - n {
            q
        } else if r == n {
            n + 1 - q
        } else {
            let sign = if q >= 0 { 1 } else { 0 };
            (2 * r) + (4 * n) + sign
        };

        (ring_start + ring_idx) as usize
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get<'a>(&'a self, c: &HexCoord) -> Option<&'a T> {
        let idx = Self::idx(*c);
        let (coord, val) = self.buf.get(idx)?.as_ref()?;

        assert_eq!(c, coord);

        Some(val)
    }

    pub fn contains(&self, c: &HexCoord) -> bool {
        self.get(c).is_some()
    }

    pub fn get_mut<'a>(&'a mut self, c: &HexCoord) -> Option<&'a mut T> {
        let idx = Self::idx(*c);
        let (coord, val) = self.buf.get_mut(idx)?.as_mut()?;

        assert_eq!(c, coord);

        Some(val)
    }

    pub fn insert(&mut self, c: HexCoord, v: T) -> Option<T> {
        self.len += 1;

        let idx = Self::idx(c);

        if idx >= self.buf.len() {
            self.buf
                .resize_with((idx + 1).next_power_of_two(), Default::default);
        }

        let old = std::mem::replace(&mut self.buf[idx], None);
        self.buf[idx] = Some((c, v));

        if let Some((coord, val)) = old {
            assert_eq!(c, coord);
            Some(val)
        } else {
            None
        }
    }

    pub fn remove(&mut self, c: &HexCoord) -> Option<T> {
        let idx = Self::idx(*c);

        if idx >= self.buf.len() {
            return None;
        }

        let val = std::mem::replace(&mut self.buf[idx], None);

        if let Some((_, v)) = val {
            self.len -= 1;
            Some(v)
        } else {
            None
        }
    }

    pub fn entry<'a>(&'a mut self, c: HexCoord) -> BufMapEntry<'a, T> {
        let occupied = self.get(&c).is_some();

        BufMapEntry {
            occupied,
            key: c,
            map: self,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&HexCoord, &T)> {
        self.buf.iter().flatten().map(|(a, b)| (a, b))
    }
}

impl<T> IntoIterator for SpiralBufMap<T> {
    type Item = (HexCoord, T);
    type IntoIter = std::iter::Flatten<std::vec::IntoIter<Option<Self::Item>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.buf.into_iter().flatten()
    }
}

impl<T: fmt::Display> fmt::Display for SpiralBufMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        for x in self.iter() {
            write!(f, "({}: {}), ", x.0, x.1)?;
        }

        write!(f, "]")
    }
}

pub struct BufMapEntry<'a, V> {
    occupied: bool,
    key: HexCoord,
    map: &'a mut SpiralBufMap<V>,
}

impl<'a, V> BufMapEntry<'a, V> {
    pub fn or_insert(self, default: V) -> &'a mut V {
        if self.occupied {
            self.map.get_mut(&self.key).unwrap()
        } else {
            self.map.insert(self.key, default);
            self.map.get_mut(&self.key).unwrap()
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct SpiralBufSet(SpiralBufMap<()>);

impl SpiralBufSet {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, c: HexCoord) -> bool {
        if self.contains(&c) {
            return false;
        }

        self.0.insert(c, ());
        true
    }

    pub fn contains(&self, c: &HexCoord) -> bool {
        self.0.contains(c)
    }

    pub fn remove(&mut self, c: &HexCoord) -> bool {
        self.0.remove(c).is_some()
    }

    pub fn iter(&self) -> impl Iterator<Item = &HexCoord> {
        self.0.iter().map(|(a, b)| a)
    }
}

impl fmt::Display for SpiralBufSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        for x in self.iter() {
            write!(f, "{}, ", x)?;
        }

        write!(f, "]")
    }
}
