use super::{Heuristic, SearchPlayer};
use crate::hive::{HiveBug, HiveGame};

pub type Search = SearchPlayer<SearchHeuristic>;

#[derive(Default)]
pub struct SearchHeuristic();

impl Heuristic for SearchHeuristic {
    fn leaf_val(&mut self, game: &HiveGame, color: bool) -> i32 {
        search_val(game, color)
    }

    fn ident() -> &'static str {
        "search"
    }
}

fn search_val(game: &HiveGame, color: bool) -> i32 {
    // heuristic value of node
    let board = game.board();

    let own_queen = game.queen_loc(color);
    let opp_queen = game.queen_loc(!color);

    let mut res = 0;

    // queen surrounded is bad
    if let Some(l) = own_queen {
        // queen surrounded is bad
        res -= (board.neighbor_cells(l).count().pow(2) * 10) as i32;

        //beetles neighboring queen is bad
        res -= (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 50) as i32;

        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 10;
        }
    }

    if let Some(l) = opp_queen {
        // other queen surrounded is good
        res += (board.neighbor_cells(l).count().pow(2) * 12) as i32;

        //beetles neighboring other queen is good
        res += (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 60) as i32;

        // what the heck, beetle on the other queen is good
        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 10;
        }
    }

    // own pieces not being bridges is good (freer to move)
    // removing for now, bridge finding is super expensive
    for (&c, &p) in board.all_top() {
        if p.color() == color {
            if !board.is_bridge(c) {
                res += 2;

                if p.bug() == HiveBug::Ant {
                    res += 2;
                }
            }
        }
    }

    res
}
