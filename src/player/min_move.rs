use super::Player;
use crate::hive::{HiveGame, HiveMove};

#[derive(Default)]
pub struct MinMove();

impl Player for MinMove {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        // minimize opponent's next moves
        let moves = game.valid_moves();

        moves
            .into_iter()
            .min_by_key(|&m| {
                let res = game.make_move(m);
                if let Some(g) = res.game() {
                    g.valid_moves().len()
                } else {
                    0
                }
            })
            .unwrap()
    }
}
