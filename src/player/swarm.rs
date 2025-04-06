use super::Player;
use crate::hive::{HiveGame, HiveMove};

#[derive(Default)]
pub struct Swarm();

impl Player for Swarm {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let moves = game.valid_moves();
        let own_color = game.turn();

        let mut value = i32::MAX;
        let mut mov = moves[0];

        let opp_queen = if let Some(l) = game.queen_loc(!own_color) {
            l
        } else {
            return mov;
        };

        for m in moves {
            let res = game.make_move(m);
            if let Some(g) = res.game() {
                // mean square distance to opp queen times 50
                let v: i32 = g
                    .board()
                    .all_top()
                    .filter_map(|(x, p)| {
                        if p.color() == own_color {
                            Some(x.dist(&opp_queen) as i32)
                        } else {
                            None
                        }
                    })
                    .map(|d| d.pow(2))
                    .sum();

                let own_piece_count = g
                    .board()
                    .all_top()
                    .filter(|(_, p)| p.color() == own_color)
                    .count() as i32;

                let v = (v * 50) / (own_piece_count + 1);

                // subtract a bonus for placing more pieces
                let v = v - 10 * own_piece_count;

                if v < value {
                    value = v;
                    mov = m;
                }
            }
        }

        mov
    }

    fn ident(&self) -> &'static str {
        "swarm"
    }
}
