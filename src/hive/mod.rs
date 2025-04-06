use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::mem::MaybeUninit;

use rustc_hash::FxHashSet;

mod hex;
pub use hex::{HexCoord, SpiralBufMap, SpiralBufSet, HEXDIR};

#[derive(Clone, Copy)]
struct Pile<T: Copy> {
    i: u8,
    arr: [MaybeUninit<T>; 5],
}

impl<T: Copy> Pile<T> {
    fn new() -> Self {
        Self {
            i: 0,
            arr: [MaybeUninit::uninit(); 5],
        }
    }

    fn push(&mut self, item: T) {
        self.arr[self.i as usize].write(item);
        self.i += 1;
    }

    fn pop(&mut self) -> Option<T> {
        if self.i == 0 {
            return None;
        }

        self.i -= 1;
        unsafe {
            let res = self.arr[self.i as usize].assume_init_read();
            self.arr[self.i as usize].assume_init_drop();
            Some(res)
        }
    }

    fn len(&self) -> usize {
        self.i as usize
    }

    fn last<'a>(&'a self) -> Option<&'a T> {
        if self.i == 0 {
            None
        } else {
            unsafe { Some(self.arr[(self.i - 1) as usize].assume_init_ref()) }
        }
    }

    fn is_empty(&self) -> bool {
        self.i == 0
    }
}

impl<T: Hash + Copy> Hash for Pile<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.i.hash(state);
        for x in 0..self.i {
            unsafe {
                self.arr[x as usize].assume_init_ref().hash(state);
            }
        }
    }
}

impl<T: fmt::Debug + Copy> fmt::Debug for Pile<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            let iter = (0..self.len()).map(|i| self.arr[i].assume_init_ref());
            fmt.debug_list().entries(iter).finish()
        }
    }
}

impl<T: Copy> Default for Pile<T> {
    fn default() -> Self {
        Self::new()
    }
}

// the vec is kept empty for references to empty cells
#[derive(Debug, Clone)]
pub struct HexBoard<T: Copy> {
    //map: FxHashMap<HexCoord, Pile<T>>,
    map: SpiralBufMap<Pile<T>>,
    empty: Pile<T>,

    perimeter: FxHashSet<HexCoord>,
    occupied: SpiralBufSet,
    bridges: RefCell<Option<SpiralBufSet>>,
}

impl<T: Copy> HexBoard<T> {
    fn new() -> Self {
        HexBoard {
            map: Default::default(),
            empty: Pile::new(),
            perimeter: FxHashSet::default(),
            occupied: SpiralBufSet::default(),
            bridges: RefCell::new(None),
        }
    }

    fn get<'a>(&'a self, coord: HexCoord) -> &'a Pile<T> {
        self.map.get(&coord).unwrap_or(&self.empty)
    }

    pub fn get_top<'a>(&'a self, coord: HexCoord) -> Option<&'a T> {
        // get the top of the cell, if it exists
        self.get(coord).last()
    }

    pub fn all_top<'a>(&'a self) -> impl Iterator<Item = (&'a HexCoord, &'a T)> {
        self.map
            .iter()
            .filter_map(|(x, v)| v.last().map(|p| (x, p)))
    }

    fn place(&mut self, coord: HexCoord, piece: T) {
        if !self.is_empty(coord) {
            panic!("aaa");
        }

        self.map.entry(coord).or_insert(Pile::new()).push(piece);

        let nb: Vec<_> = self.neighbor_space(coord).collect();
        self.perimeter.extend(nb);
        self.perimeter.remove(&coord);
        self.occupied.insert(coord);

        //invalidate bridge cache
        self.bridges.take();
    }

    fn mov(&mut self, from: HexCoord, to: HexCoord) -> bool {
        if let Some(piece) = self.map.get_mut(&from).map(|v| v.pop()).flatten() {
            self.map.entry(to).or_insert(Pile::new()).push(piece);

            let dead_nb: Vec<_> = self
                .neighbor_space(from)
                .filter(|&x| self.neighbor_cells(x).count() == 0)
                .collect();
            for nb in dead_nb {
                self.perimeter.remove(&nb);
            }

            if self.is_empty(from) {
                // might not be if a beetle is leaving a pile
                self.perimeter.insert(from);
                self.occupied.remove(&from);
            }

            let nb: Vec<_> = self.neighbor_space(to).collect();
            self.perimeter.extend(nb);
            self.perimeter.remove(&to);
            self.occupied.insert(to);

            //invalidate bridge cache
            self.bridges.take();

            true
        } else {
            false
        }
    }

    fn is_empty(&self, coord: HexCoord) -> bool {
        self.map.get(&coord).map(|v| v.is_empty()).unwrap_or(true)
    }

    pub fn perimeter(&self) -> &FxHashSet<HexCoord> {
        &self.perimeter
    }

    pub fn occupied(&self) -> &SpiralBufSet {
        &self.occupied
    }

    #[allow(dead_code)]
    pub fn disp_perimeter(&self) -> String {
        let mut res: HexBoard<char> = HexBoard::new();
        for &c in self.perimeter() {
            res.place(c, 'P');
        }
        res.disp()
    }

    #[allow(dead_code)]
    pub fn disp_occupied(&self) -> String {
        let mut res: HexBoard<char> = HexBoard::new();
        for &c in self.occupied().iter() {
            res.place(c, 'O');
        }
        res.disp()
    }

    pub fn neighbor_space<'a>(&'a self, coord: HexCoord) -> impl Iterator<Item = HexCoord> + 'a {
        coord.neighbors().filter(|&x| self.is_empty(x))
    }

    pub fn neighbor_cells<'a>(&'a self, coord: HexCoord) -> impl Iterator<Item = HexCoord> + 'a {
        coord.neighbors().filter(|&x| self.get_top(x).is_some())
    }

    pub fn neighbor_pieces<'a>(&'a self, coord: HexCoord) -> impl Iterator<Item = &'a T> {
        coord.neighbors().filter_map(|x| self.get_top(x))
    }

    fn passable_coords<'a>(
        &'a self,
        from: HexCoord,
        without: Option<HexCoord>,
    ) -> impl Iterator<Item = HexCoord> + 'a {
        // return every cell on the perimeter that isn't occupied and isn't impassible
        // optionally, treat 'without' as an empty cell (so spiders and ants don't block themselves)

        // the destination is passable if:
        // - the dest is not occupied
        // - the source and dest share exactly one populated neighbor cell
        //   - 0 and the dest has hopped a peninsula or left the hive
        //   - 2 and the passage is blocked
        self.neighbor_space(from).filter(move |to| {
            let (nb1, nb2) = to.shared_neighbors(&from).unwrap();
            if let Some(w) = without {
                let nb1_is_empty = self.is_empty(nb1) || (nb1 == w);
                let nb2_is_empty = self.is_empty(nb2) || (nb2 == w);
                nb1_is_empty ^ nb2_is_empty
            } else {
                self.is_empty(nb1) ^ self.is_empty(nb2)
            }
        })
    }

    fn passable_climber_coords(&self, from: HexCoord) -> Vec<HexCoord> {
        // return cells passable to climbers

        // the destination is passable if:
        // - the source and dest share one or two populated neighbor cells, or the dest is populated
        //   - 0 and the dest has hopped a peninsula or left the hive
        //   - unless the dest is populated, in which case the dest has not left the hive
        from.neighbors()
            .filter(|&c| {
                (c.neighbors()
                    .filter(|&x| from.neighbors().any(|y| x == y))
                    .filter(|&x| !self.is_empty(x))
                    .count()
                    >= 1)
                    || !self.is_empty(c)
            })
            .collect()
    }

    pub fn is_bridge(&self, coord: HexCoord) -> bool {
        // need to compute bridges lazily, using an interior mutability cache
        // cell seems fine as a spiral map is 3 usizes (including the vec pointer)
        let bridges = self.bridges.take().unwrap_or_else(|| self.find_bridges());

        let res = bridges.contains(&coord);
        self.bridges.replace(Some(bridges));
        res
    }

    fn find_bridges(&self) -> SpiralBufSet {
        let mut bridges: SpiralBufSet = Default::default();
        let mut dfs_idx: SpiralBufMap<usize> = Default::default();
        let mut dfs_low: SpiralBufMap<usize> = Default::default(); // lowest idx in tree reachable from descendant nodes
        let mut parent: SpiralBufMap<HexCoord> = Default::default();
        let mut visited: SpiralBufSet = Default::default();

        let mut count: usize = 0;

        let root = *self.occupied().iter().next().unwrap();

        self.bridge_search(
            root,
            &mut count,
            &mut bridges,
            &mut dfs_idx,
            &mut dfs_low,
            &mut parent,
            &mut visited,
        );

        bridges
    }

    fn bridge_search(
        &self,
        node: HexCoord,
        count: &mut usize,
        bridges: &mut SpiralBufSet,
        dfs_idx: &mut SpiralBufMap<usize>,
        dfs_low: &mut SpiralBufMap<usize>,
        parent: &mut SpiralBufMap<HexCoord>,
        visited: &mut SpiralBufSet,
    ) {
        visited.insert(node);

        dfs_idx.insert(node, *count);
        dfs_low.insert(node, *count);
        *count += 1;

        let mut children = 0;

        let node_parent = parent.get(&node).cloned();

        for adj in self.neighbor_cells(node) {
            if !visited.contains(&adj) {
                children += 1;
                parent.insert(adj, node);
                self.bridge_search(adj, count, bridges, dfs_idx, dfs_low, parent, visited);

                // update earliest ancestor reachable from node subtree to earliest ancestor reachable from child subtree if earlier
                dfs_low.insert(
                    node,
                    *dfs_low.get(&node).unwrap().min(dfs_low.get(&adj).unwrap()),
                );

                // if ANY child has a subtree with earliest reachable ancestor >= node, node is a bridge
                if dfs_low.get(&adj).unwrap() >= dfs_idx.get(&node).unwrap()
                    && node_parent.is_some()
                {
                    bridges.insert(node);
                }
            } else if Some(adj) != node_parent {
                // already visited, so this is a backedge, update earliest ancestor reachable from subtree to adj index if earlier
                dfs_low.insert(
                    node,
                    *dfs_low.get(&node).unwrap().min(dfs_idx.get(&adj).unwrap()),
                );
            }
        }

        // if node_parent.is_some() && dfs_low.get(&node) == dfs_idx.get(&node) && children != 0 {
        //     bridges.insert(node);
        // }

        if node_parent.is_none() && children > 1 {
            // node is root and multiple children
            bridges.insert(node);
        }
    }
}

impl<T: Copy + Hash> HexBoard<T> {
    pub fn default_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl<T: Copy + Hash> Hash for HexBoard<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // no longer have to sort the cells as SpiralBufMap has a deterministic order
        for (coord, content) in self.map.iter() {
            // self.map may contain cells that were inserted but that pieces have since been removed from
            if content.is_empty() {
                continue;
            }

            coord.hash(state);
            content.hash(state);
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct DbgUsize(usize);

impl From<usize> for DbgUsize {
    fn from(x: usize) -> Self {
        DbgUsize(x)
    }
}

impl From<DbgUsize> for char {
    fn from(x: DbgUsize) -> char {
        char::from_digit((x.0 % 10) as u32, 10).unwrap()
    }
}

impl<T: Into<char> + Clone + Copy + std::fmt::Debug> HexBoard<T> {
    #[allow(dead_code)]
    fn disp(&self) -> String {
        // display as a monospace hex grid

        if self.map.len() == 0 {
            return String::new();
        }

        let map_offset: HashMap<(i8, i8), Option<T>> = self
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
pub enum HiveBug {
    Queen,
    Beetle,
    Grasshopper,
    Spider,
    Ant,
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub struct HivePiece {
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
    pub fn color(&self) -> bool {
        self.color
    }

    pub fn bug(&self) -> HiveBug {
        self.bug
    }

    fn valid_dests(&self, board: &HexBoard<HivePiece>, coord: HexCoord) -> Vec<HexCoord> {
        use HiveBug::*;

        assert_eq!(board.get_top(coord), Some(self));

        if board.is_bridge(coord) {
            // not allowed to move a bug that is the only connecting piece
            return Vec::new();
        }

        match self.bug {
            Queen => board.passable_coords(coord, None).collect(),
            Beetle => board.passable_climber_coords(coord),
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
                let one_away: HashSet<HexCoord> = board.passable_coords(coord, None).collect();
                let two_away: HashSet<HexCoord> = one_away
                    .iter()
                    .flat_map(|&c| board.passable_coords(c, Some(coord)))
                    .filter(|&c| c != coord)
                    .collect();
                two_away
                    .iter()
                    .flat_map(|&c| board.passable_coords(c, Some(coord)))
                    .filter(|&c| c != coord)
                    .filter(|c| !one_away.contains(c))
                    .filter(|&c| board.neighbor_cells(c).filter(|&x| x != coord).count() != 0) // stop spider from climbing off the hive on itself
                    .collect()
            }
            Ant => {
                let mut passable: HashSet<HexCoord> = board.passable_coords(coord, None).collect();

                // to_explore includes the originating node (second element) for to avoid hashset lookup on backtrack
                let mut to_explore: Vec<(HexCoord, HexCoord)> = Vec::with_capacity(board.map.len());
                to_explore.extend(passable.iter().cloned().map(|c| (c, coord)));

                while let Some((cur, from)) = to_explore.pop() {
                    // filtering by c != coord first also stops it from going in passible
                    // filtering by c != from catches some backtracking before incurring a hashset lookup
                    to_explore.extend(
                        board
                            .passable_coords(cur, Some(coord))
                            .filter(|&c| c != coord)
                            .filter(|&c| c != from)
                            .filter(|&c| passable.insert(c))
                            .map(|c| (c, cur)),
                    );
                }

                // gotta filter out moves to its own perimeter (stop ant from wandering off)
                passable
                    .into_iter()
                    .filter(|&c| board.neighbor_cells(c).filter(|&x| x != coord).count() != 0) // stop ant from climbing off the hive on itself
                    .collect()
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum MoveInner {
    Place(HivePiece, HexCoord),
    Move(HivePiece, HexCoord, HexCoord),
    Pass,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HiveMove(MoveInner);

impl HiveMove {
    pub fn pass() -> Self {
        Self(MoveInner::Pass)
    }

    pub fn piece(&self) -> Option<HivePiece> {
        match &self.0 {
            &MoveInner::Place(p, _) => Some(p),
            &MoveInner::Move(p, _, _) => Some(p),
            &MoveInner::Pass => None,
        }
    }

    pub fn dest(&self) -> Option<HexCoord> {
        match &self.0 {
            &MoveInner::Place(_, c) => Some(c),
            &MoveInner::Move(_, _, c) => Some(c),
            &MoveInner::Pass => None,
        }
    }

    pub fn is_place(&self) -> bool {
        match &self.0 {
            &MoveInner::Place(_, _) => true,
            &MoveInner::Move(_, _, _) => false,
            &MoveInner::Pass => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_move(&self) -> bool {
        match &self.0 {
            &MoveInner::Place(_, _) => false,
            &MoveInner::Move(_, _, _) => true,
            &MoveInner::Pass => false,
        }
    }

    pub fn is_pass(&self) -> bool {
        match &self.0 {
            &MoveInner::Place(_, _) => false,
            &MoveInner::Move(_, _, _) => false,
            &MoveInner::Pass => true,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HiveResult<'a> {
    Cont(HiveGame<'a>),
    WinW(HiveGame<'a>),
    WinB(HiveGame<'a>),
    Draw(HiveGame<'a>),
    OutOfMoves(HiveGame<'a>),
    #[allow(dead_code)]
    Invalid,
}

impl<'a> HiveResult<'a> {
    pub fn game(self) -> Option<HiveGame<'a>> {
        match self {
            HiveResult::Cont(g) => Some(g),
            HiveResult::WinW(g) => Some(g),
            HiveResult::WinB(g) => Some(g),
            HiveResult::Draw(g) => Some(g),
            HiveResult::OutOfMoves(g) => Some(g),
            _ => None,
        }
    }

    pub fn game_ref<'b>(&'b self) -> Option<&'b HiveGame<'a>> {
        match self {
            HiveResult::Cont(g) => Some(&g),
            HiveResult::WinW(g) => Some(&g),
            HiveResult::WinB(g) => Some(&g),
            HiveResult::Draw(g) => Some(&g),
            HiveResult::OutOfMoves(g) => Some(&g),
            _ => None,
        }
    }

    pub fn default_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(Clone, Debug)]
pub struct HiveGame<'a> {
    board: HexBoard<HivePiece>,
    hand_w: HashMap<HiveBug, u8>,
    hand_b: HashMap<HiveBug, u8>,
    queen_loc_w: Option<HexCoord>,
    queen_loc_b: Option<HexCoord>,
    turn: bool,
    round: usize,

    seen_boards: HashMap<u64, u32>,

    move_budget: Option<&'a Cell<u32>>,
}

impl<'c> HiveGame<'c> {
    pub fn new() -> HiveGame<'static> {
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

            seen_boards: HashMap::new(),

            move_budget: None,
        }
    }

    pub fn with_budget<'a>(self, budget: &'a Cell<u32>) -> HiveGame<'a> {
        HiveGame {
            move_budget: Some(budget),
            ..self
        }
    }

    pub fn round(&self) -> usize {
        self.round
    }

    pub fn turn(&self) -> bool {
        self.turn
    }

    pub fn queen_loc(&self, color: bool) -> Option<HexCoord> {
        if color {
            self.queen_loc_w
        } else {
            self.queen_loc_b
        }
    }

    pub fn board<'a>(&'a self) -> &'a HexBoard<HivePiece> {
        &self.board
    }

    pub fn default_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
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
            let dests = p.valid_dests(&self.board, c);

            res.extend(
                dests
                    .into_iter()
                    .map(|d| HiveMove(MoveInner::Move(p, c, d))),
            );
        }

        res.push(HiveMove(MoveInner::Pass));

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
                res.push(HiveMove(MoveInner::Place(p, d)));
            }
        }

        res
    }

    #[allow(dead_code)]
    pub fn disp(&self) -> String {
        format!(
            "Round {}\nTurn {}\nBoard:\n{}",
            self.round,
            self.turn,
            self.board.disp()
        )
    }

    #[allow(dead_code)]
    pub fn disp_board(&self) -> String {
        self.board.disp()
    }

    pub fn make_move(&self, mov: HiveMove) -> HiveResult<'c> {
        // no longer checking if mov is valid
        // as moves are only generated by a HiveGame
        // TODO validate the game instance / turn number are correct I guess
        //if !self.valid_moves().into_iter().any(|m| m == mov) {
        //    return HiveResult::Invalid;
        //}

        if let Some(b) = self.move_budget {
            let remaining = b.take();
            if remaining == 0 {
                return HiveResult::OutOfMoves(self.clone());
                // b.take() replaced with 0 so no need to put new value in
            }

            b.set(remaining - 1);
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

        match mov.0 {
            MoveInner::Place(p, c) => {
                *hand.get_mut(&p.bug).unwrap() -= 1;
                if hand.get(&p.bug) == Some(&0) {
                    hand.remove(&p.bug);
                }

                if p.bug == HiveBug::Queen {
                    queen_loc.replace(c);
                }

                res.board.place(c, p);
            }
            MoveInner::Move(p, s, d) => {
                if p.bug == HiveBug::Queen {
                    queen_loc.replace(d);
                }

                res.board.mov(s, d);
            }
            MoveInner::Pass => {}
        }

        res.turn = !res.turn;
        if res.turn {
            res.round += 1;
        }

        let seen_count = res.seen_boards.entry(res.board.default_hash()).or_insert(0);
        if *seen_count > 2 {
            // board repeated too many times
            // call it a draw, not strictly part of hive rules but seems reasonable
            return HiveResult::Draw(res);
        }
        *seen_count += 1;

        match (res.queen_surrounded(false), res.queen_surrounded(true)) {
            (true, false) => HiveResult::WinW(res),
            (false, true) => HiveResult::WinB(res),
            (true, true) => HiveResult::Draw(res),
            (false, false) => HiveResult::Cont(res),
        }
    }

    pub fn move_budget(&self) -> u32 {
        self.move_budget.map(|b| b.get()).unwrap_or(0)
    }
}

impl Hash for HiveGame<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.round.hash(state);
        self.turn.hash(state);

        let mut board_map: Vec<_> = self.board.map.iter().collect();
        board_map.sort_by(|a, b| a.0.cmp(b.0));
        board_map.hash(state);
    }
}

impl PartialEq for HiveGame<'_> {
    fn eq(&self, other: &Self) -> bool {
        let self_hash = self.default_hash();
        let other_hash = other.default_hash();

        self_hash == other_hash
    }
}

impl Eq for HiveGame<'_> {}
