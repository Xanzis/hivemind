use crate::hive::{HiveGame, HiveMove};

pub mod min_move;
pub mod min_queen_move;
pub mod multisearch;
pub mod random;
pub mod search;
pub mod swarm;

pub trait Player {
    fn make_move(&mut self, game: HiveGame) -> HiveMove;
}
