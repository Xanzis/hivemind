use std::collections::{HashMap, HashSet};
use std::ops::{Add, Mul};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct HexCoord(i16, i16, i16);

impl Add for HexCoord {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        HexCoord(self.0 + other.0, self.1 + other.1, self.2 + other.2)
    }
}

impl Mul<i16> for HexCoord {
    type Output = Self;

    fn mul(self, other: i16) -> Self {
        HexCoord(self.0 * other, self.1 * other, self.2 * other)
    }
}

const HEXDIR: [HexCoord; 6] = [
    HexCoord(1, -1, 0),
    HexCoord(1, 0, -1),
    HexCoord(0, 1, -1),
    HexCoord(-1, 1, 0),
    HexCoord(-1, 0, 1),
    HexCoord(0, -1, 1),
];

impl HexCoord {
    fn origin() -> Self {
        HexCoord(0, 0, 0)
    }

    fn neighbors<'a>(&'a self) -> impl Iterator<Item = Self> + 'a {
        HEXDIR.iter().map(|&x| x + *self)
    }

    fn neighbor_set(&self) -> HashSet<HexCoord> {
        self.neighbors().collect()
    }

    fn to_offset(&self) -> (i16, i16) {
        // convert to row/column offset coordinates

        let col = self.0 + (self.1 - (self.1 & 1)) / 2;
        let row = self.1;
        (row, col)
    }
}

// the vec is kept empty for references to empty cells
#[derive(Debug, Clone)]
struct HexBoard<T> {
    map: HashMap<HexCoord, Vec<T>>,
    empty: Vec<T>,

    perimeter_cache: Option<HashSet<HexCoord>>,
    occupied_cache: Option<HashSet<HexCoord>>,
}

impl<T> HexBoard<T> {
    fn new() -> Self {
        HexBoard {
            map: HashMap::new(),
            empty: Vec::new(),
            perimeter_cache: None,
            occupied_cache: None,
        }
    }

    fn get<'a>(&'a self, coord: HexCoord) -> &'a Vec<T> {
        self.map.get(&coord).unwrap_or(&self.empty)
    }

    fn get_top<'a>(&'a self, coord: HexCoord) -> Option<&'a T> {
        // get the top of the cell, if it exists
        self.get(coord).last()
    }

    fn all_top<'a>(&'a self) -> impl Iterator<Item = (&'a HexCoord, &'a T)> {
        self.map
            .iter()
            .filter_map(|(x, v)| v.last().map(|p| (x, p)))
    }

    fn place(&mut self, coord: HexCoord, piece: T) {
        self.map.entry(coord).or_insert(Vec::new()).push(piece);

        self.perimeter_cache = Some(self.find_perimeter());
        self.occupied_cache = Some(self.find_occupied());
    }

    fn mov(&mut self, from: HexCoord, to: HexCoord) -> bool {
        let mut res = false;

        if let Some(piece) = self.map.get_mut(&from).map(|v| v.pop()).flatten() {
            self.map.entry(to).or_insert(Vec::new()).push(piece);
            res = true;
        }

        self.perimeter_cache = Some(self.find_perimeter());
        self.occupied_cache = Some(self.find_occupied());

        res
    }

    fn is_empty(&self, coord: HexCoord) -> bool {
        self.map.get(&coord).map(|v| v.is_empty()).unwrap_or(true)
    }

    fn perimeter(&self) -> HashSet<HexCoord> {
        if let Some(p) = self.perimeter_cache.clone() {
            p
        } else {
            self.find_perimeter()
        }
    }

    fn find_perimeter(&self) -> HashSet<HexCoord> {
        let mut res = HashSet::new();

        for &coord in self.map.keys() {
            if self.is_empty(coord) {
                continue;
            }

            for neighbor in coord.neighbors() {
                if self.is_empty(neighbor) {
                    res.insert(neighbor);
                }
            }
        }

        res
    }

    fn occupied(&self) -> HashSet<HexCoord> {
        if let Some(o) = self.occupied_cache.clone() {
            o
        } else {
            self.find_occupied()
        }
    }

    fn find_occupied(&self) -> HashSet<HexCoord> {
        let mut res = HashSet::new();

        res.extend(self.map.keys().filter(|&&c| !self.is_empty(c)));

        res
    }

    fn neighbor_cells<'a>(&'a self, coord: HexCoord) -> impl Iterator<Item = HexCoord> + 'a {
        let nb: Vec<_> = coord.neighbors().collect();
        nb.into_iter().filter(|&x| self.get_top(x).is_some())
    }

    fn neighbor_pieces<'a>(&'a self, coord: HexCoord) -> impl Iterator<Item = &'a T> {
        let nb: Vec<_> = coord.neighbors().collect();
        nb.into_iter().filter_map(|x| self.get_top(x))
    }

    fn reachable_without(&self, from: HexCoord, without: HexCoord) -> HashSet<HexCoord> {
        let mut to_check = vec![from];
        let mut visited = HashSet::new();

        while let Some(cur) = to_check.pop() {
            to_check.extend(
                self.neighbor_cells(cur)
                    .filter(|&c| c != without)
                    .filter(|&c| visited.insert(c)),
            );
        }

        visited
    }

    fn passable_coords(&self, from: HexCoord) -> Vec<HexCoord> {
        // return every cell on the perimeter that isn't occupied and isn't impassible
        let from_neighbors = from.neighbor_set();

        // the destination is passable if:
        // - the dest is not occupied
        // - the source and dest share exactly one populated neighbor cell
        //   - 0 and the dest has hopped a peninsula or left the hive
        //   - 2 and the passage is blocked
        from.neighbors()
            .filter(|c| !self.occupied().contains(c))
            .filter(|c| {
                c.neighbor_set()
                    .intersection(&from_neighbors)
                    .filter(|&&x| !self.is_empty(x))
                    .count()
                    == 1
            })
            .collect()
    }

    fn is_bridge(&self, coord: HexCoord) -> bool {
        // if the piece is not a bridge between disjoint hives the number of cells reachable
        // from one of its neighbors (minus the piece in question) should be occupied-1

        // but also it's not a bridge if it's a stack
        if self.get(coord).len() > 1 {
            return false;
        }

        let nb = self.neighbor_cells(coord).next().unwrap();
        self.occupied().len() - self.reachable_without(nb, coord).len() != 1
    }
}

impl<T: Into<char> + Clone + std::fmt::Debug> HexBoard<T> {
    fn disp(&self) -> String {
        // display as a monospace hex grid

        if self.map.len() == 0 {
            return String::new();
        }

        let map_offset: HashMap<(i16, i16), Option<T>> = self
            .map
            .clone()
            .into_iter()
            .map(|(k, v)| (k.to_offset(), v.last().cloned()))
            .collect();

        let max_r = map_offset.keys().map(|&(r, _)| r).max().unwrap();
        let min_r = map_offset.keys().map(|&(r, _)| r).min().unwrap();
        let max_c = map_offset.keys().map(|&(_, c)| c).max().unwrap();
        let min_c = map_offset.keys().map(|&(_, c)| c).min().unwrap();

        let mut res = String::new();

        for r in min_r..=max_r {
            if (r % 2).abs() == 1 {
                res.push(' ');
            }

            for c in min_c..=max_c {
                res.push(
                    map_offset
                        .get(&(r, c))
                        .cloned()
                        .flatten()
                        .map(|x| x.into())
                        .unwrap_or('.'),
                );
                res.push(' ');
            }

            res.push('\n');
        }

        res
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum HiveBug {
    Queen,
    Beetle,
    Grasshopper,
    Spider,
    Ant,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct HivePiece {
    color: bool,
    bug: HiveBug,
}

impl From<HivePiece> for char {
    fn from(x: HivePiece) -> char {
        use HiveBug::*;

        if x.color {
            match x.bug {
                Queen => 'Q',
                Beetle => 'B',
                Grasshopper => 'G',
                Spider => 'S',
                Ant => 'A',
            }
        } else {
            match x.bug {
                Queen => 'q',
                Beetle => 'b',
                Grasshopper => 'g',
                Spider => 's',
                Ant => 'a',
            }
        }
    }
}

impl HivePiece {
    fn valid_dests(&self, board: &HexBoard<HivePiece>, coord: HexCoord) -> Vec<HexCoord> {
        use HiveBug::*;

        assert_eq!(board.get_top(coord), Some(self));

        if board.is_bridge(coord) {
            // not allowed to move a bug that is the only connecting piece
            return Vec::new();
        }

        match self.bug {
            Queen => board
                .passable_coords(coord)
                .into_iter()
                .filter(|&c| board.neighbor_cells(c).count() > 1)
                .collect(),
            Beetle => {
                let perimeter = board.perimeter();
                let occupied = board.occupied();
                // >1 check needed bc otherwise the beetle counts itself and wanders off
                coord
                    .neighbors()
                    .filter(|c| perimeter.contains(c) || occupied.contains(c))
                    .filter(|&c| board.neighbor_cells(c).count() > 1)
                    .collect()
            }
            Grasshopper => {
                let mut res = Vec::new();
                let perimeter = board.perimeter();
                let occupied = board.occupied();

                for dir in HEXDIR {
                    if !occupied.contains(&(coord + dir)) {
                        continue;
                    }

                    for step in 2.. {
                        let dest = coord + (dir * step);
                        if perimeter.contains(&dest) {
                            res.push(dest);
                            break;
                        } else if occupied.contains(&dest) {
                            continue;
                        } else {
                        }
                    }
                }

                res
            }
            Spider => {
                let one_away: HashSet<HexCoord> =
                    board.passable_coords(coord).into_iter().collect();
                let two_away: HashSet<HexCoord> = one_away
                    .iter()
                    .flat_map(|&c| board.passable_coords(c))
                    .filter(|&c| c != coord)
                    .collect();
                two_away
                    .iter()
                    .flat_map(|&c| board.passable_coords(c))
                    .filter(|&c| c != coord)
                    .filter(|c| !one_away.contains(c))
                    .filter(|&c| board.neighbor_cells(c).count() > 1)
                    .collect()
            }
            Ant => {
                let mut passable: HashSet<HexCoord> =
                    board.passable_coords(coord).into_iter().collect();
                let mut to_explore: Vec<HexCoord> = passable.iter().cloned().collect();

                while let Some(c) = to_explore.pop() {
                    // filtering by c != coord first also stops it from going in passible
                    to_explore.extend(
                        board
                            .passable_coords(c)
                            .into_iter()
                            .filter(|&c| c != coord)
                            .filter(|&c| passable.insert(c)),
                    );
                }

                // gotta filter out moves to its own perimeter (stop ant from wandering off)
                passable
                    .into_iter()
                    .filter(|&c| board.neighbor_cells(c).count() > 1)
                    .collect()
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum HiveMove {
    Place(HivePiece, HexCoord),
    Move(HivePiece, HexCoord, HexCoord),
}

#[derive(Clone, Debug)]
pub enum HiveResult {
    Cont(HiveGame),
    WinW,
    WinB,
    Draw,
    Invalid,
}

impl HiveResult {
    pub fn game(self) -> Option<HiveGame> {
        match self {
            HiveResult::Cont(g) => Some(g),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct HiveGame {
    board: HexBoard<HivePiece>,
    hand_w: HashMap<HiveBug, u8>,
    hand_b: HashMap<HiveBug, u8>,
    queen_loc_w: Option<HexCoord>,
    queen_loc_b: Option<HexCoord>,
    turn: bool,
    round: usize,
}

impl HiveGame {
    pub fn new() -> Self {
        use HiveBug::*;

        let hand_w: HashMap<HiveBug, u8> = [
            (Queen, 1),
            (Beetle, 2),
            (Grasshopper, 3),
            (Spider, 2),
            (Ant, 3),
        ]
        .into_iter()
        .collect();
        let hand_b = hand_w.clone();

        HiveGame {
            board: HexBoard::new(),
            hand_w,
            hand_b,
            queen_loc_w: None,
            queen_loc_b: None,
            turn: true,
            round: 0,
        }
    }

    fn queen_surrounded(&self, color: bool) -> bool {
        if color {
            if let Some(loc) = self.queen_loc_w {
                self.board.neighbor_pieces(loc).count() == 6
            } else {
                false
            }
        } else {
            if let Some(loc) = self.queen_loc_b {
                self.board.neighbor_pieces(loc).count() == 6
            } else {
                false
            }
        }
    }

    pub fn valid_moves(&self) -> Vec<HiveMove> {
        use HiveBug::*;
        let mut res = Vec::new();

        // check win conditions
        if self.queen_surrounded(true) || self.queen_surrounded(false) {
            return res;
        }

        res.extend(self.valid_placements());

        // no move allowed if queen isn't placed yet
        if self.turn {
            if self.queen_loc_w.is_none() {
                return res;
            }
        } else {
            if self.queen_loc_b.is_none() {
                return res;
            }
        }

        for (&c, &p) in self.board.all_top().filter(|(_, x)| x.color == self.turn) {
            //println!("Checking out {:?}", (c, p));
            let dests = p.valid_dests(&self.board, c);
            //println!("It can move to {:?}", dests);
            res.extend(dests.into_iter().map(|d| HiveMove::Move(p, c, d)));
        }

        res
    }

    fn valid_placements(&self) -> Vec<HiveMove> {
        // queen must be placed on or before turn 3
        // turn 0: first placement must be on origin, second may (must) border opponent piece

        let queen_placed = if self.turn {
            self.queen_loc_w.is_some()
        } else {
            self.queen_loc_b.is_some()
        };

        let hand = if self.turn {
            &self.hand_w
        } else {
            &self.hand_b
        };

        let pieces = if self.round == 3 && !queen_placed {
            vec![HivePiece {
                color: self.turn,
                bug: HiveBug::Queen,
            }]
        } else {
            hand.keys()
                .cloned()
                .map(|b| HivePiece {
                    color: self.turn,
                    bug: b,
                })
                .collect()
        };

        let dests = if self.round == 0 {
            if self.turn {
                vec![HexCoord::origin()]
            } else {
                vec![HexCoord::origin() + HEXDIR[0]]
            }
        } else {
            self.board
                .perimeter()
                .iter()
                .cloned()
                .filter(|&c| self.board.neighbor_pieces(c).all(|p| p.color == self.turn))
                .collect()
        };

        let mut res = Vec::new();

        for p in pieces {
            for &d in dests.iter() {
                res.push(HiveMove::Place(p, d))
            }
        }

        res
    }

    pub fn disp(&self) -> String {
        format!(
            "Round {}\nTurn {}\nBoard:\n{}",
            self.round,
            self.turn,
            self.board.disp()
        )
    }

    pub fn disp_board(&self) -> String {
        self.board.disp()
    }

    pub fn make_move(&self, mov: HiveMove) -> HiveResult {
        if !self.valid_moves().into_iter().any(|m| m == mov) {
            return HiveResult::Invalid;
        }

        let mut res = self.clone();

        let hand = if self.turn {
            &mut res.hand_w
        } else {
            &mut res.hand_b
        };

        let queen_loc = if self.turn {
            &mut res.queen_loc_w
        } else {
            &mut res.queen_loc_b
        };

        match mov {
            HiveMove::Place(p, c) => {
                *hand.get_mut(&p.bug).unwrap() -= 1;
                if hand.get(&p.bug) == Some(&0) {
                    hand.remove(&p.bug);
                }

                if p.bug == HiveBug::Queen {
                    queen_loc.replace(c);
                }

                res.board.place(c, p);
            }
            HiveMove::Move(p, s, d) => {
                if p.bug == HiveBug::Queen {
                    queen_loc.replace(d);
                }

                res.board.mov(s, d);
            }
        }

        res.turn = !res.turn;
        if res.turn {
            res.round += 1;
        }

        match (res.queen_surrounded(false), res.queen_surrounded(true)) {
            (true, false) => HiveResult::WinW,
            (false, true) => HiveResult::WinB,
            (true, true) => HiveResult::Draw,
            (false, false) => HiveResult::Cont(res),
        }
    }
}
