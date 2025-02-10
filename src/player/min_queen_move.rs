use super::Player;
use crate::hive::{HiveBug, HiveGame, HiveMove};

#[derive(Default)]
pub struct MinQueenMovePlayer();

impl Player for MinQueenMovePlayer {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let moves = game.valid_moves();

        moves
            .into_iter()
            .min_by_key(|&m| {
                let res = game.make_move(m);
                if let Some(g) = res.game() {
                    let opp_moves = g.valid_moves();
                    opp_moves
                        .into_iter()
                        .filter(|x| x.piece().map(|p| p.bug()) == Some(HiveBug::Queen))
                        .count()
                } else {
                    0
                }
            })
            .unwrap()
    }
}
