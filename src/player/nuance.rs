use super::{Heuristic, SearchPlayer};
use crate::hive::{HiveBug, HiveGame};

pub type Nuance = SearchPlayer<NuanceHeuristic>;

#[derive(Default)]
pub struct NuanceHeuristic();

impl Heuristic for NuanceHeuristic {
    fn leaf_val(&mut self, game: &HiveGame, color: bool) -> i32 {
        search_val(game, color)
    }

    fn ident() -> &'static str {
        "nuance"
    }
}

fn search_val(game: &HiveGame, color: bool) -> i32 {
    // heuristic value of node
    let board = game.board();

    let own_queen = game.queen_loc(color);
    let opp_queen = game.queen_loc(!color);

    let mut res = 0;

    let own_hand = game.hand(color);

    // reward development, beetles and grasshoppers better to play first
    res -= (own_hand.get(&HiveBug::Beetle).copied().unwrap_or(0) as i32) * 4;
    res -= (own_hand.get(&HiveBug::Grasshopper).copied().unwrap_or(0) as i32) * 3;
    res -= (own_hand.get(&HiveBug::Spider).copied().unwrap_or(0) as i32) * 2;
    res -= (own_hand.get(&HiveBug::Ant).copied().unwrap_or(0) as i32) * 2;

    // own queen logic
    if let Some(l) = own_queen {
        res -= (board.neighbor_cells(l).count().pow(2) * 10) as i32;
        res -= (board
            .neighbor_pieces(l)
            .filter(|p| p.color() != color)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 50) as i32;

        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res -= 10;
        }

        let mut sum_dist: i32 = 0;
        for c in board.occupied().iter() {
            sum_dist += l.dist(c) as i32;
        }
        res += (sum_dist / board.occupied().len() as i32) * 3;
    }

    // other queen logic
    if let Some(l) = opp_queen {
        res += (board.neighbor_cells(l).count().pow(2) * 20) as i32;
        res += (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 80) as i32;
        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 20;
        }

        let l_c = l.to_cube();
        for g in board
            .all_top()
            .filter_map(|(c, p)| (p.bug() == HiveBug::Grasshopper).then_some(c))
        {
            let g_c = g.to_cube();
            if g_c.0 == l_c.0 || g_c.1 == l_c.1 || g_c.2 == l_c.2 {
                res += 5;
            }
        }
    }

    res
}
