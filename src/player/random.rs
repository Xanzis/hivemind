use super::Player;
use crate::hive::{HiveGame, HiveMove};

#[derive(Default)]
pub struct Random();

impl Player for Random {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let moves = game.valid_moves();
        let i = (9087901 + game.round()) % moves.len();
        moves[i]
    }

    fn ident(&self) -> &'static str {
        "random"
    }
}
