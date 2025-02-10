use super::Player;
use crate::hive::{HiveGame, HiveMove};

#[derive(Default)]
pub struct RandomPlayer();

impl Player for RandomPlayer {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let moves = game.valid_moves();
        let i = (9087901 + game.round()) % moves.len();
        moves[i]
    }
}
