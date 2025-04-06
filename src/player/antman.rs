use super::{Heuristic, SearchPlayer};
use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};
use std::collections::HashMap;

pub type AntMan = SearchPlayer<AntManHeuristic>;

#[derive(Default)]
pub struct AntManHeuristic {
    ant_book: HashMap<u64, i32>, // book of hashed hive results against node values
}

impl Heuristic for AntManHeuristic {
    fn leaf_val(&mut self, game: &HiveGame, color: bool) -> i32 {
        search_val(game, color)
    }

    fn nonrecurse_val(
        &mut self,
        game: &HiveGame,
        mov: &HiveMove,
        _res: &HiveResult,
        _color: bool,
    ) -> Option<i32> {
        // optionally preempt recursion with a node evalation
        // antman preempts if the move is an ant move to a board state it's seen before

        let is_ant_move = mov
            .piece()
            .map(|p| p.bug() == HiveBug::Ant)
            .unwrap_or(false);

        if is_ant_move {
            self.ant_book.get(&game.default_hash()).cloned() // None if the game is not in book
        } else {
            None
        }
    }

    fn track_val(&mut self, game: &HiveGame, mov: &HiveMove, val: i32) {
        let is_ant_move = mov
            .piece()
            .map(|p| p.bug() == HiveBug::Ant)
            .unwrap_or(false);

        if is_ant_move {
            self.ant_book.insert(game.default_hash(), val);
        }
    }

    fn ident() -> &'static str {
        "antman"
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
