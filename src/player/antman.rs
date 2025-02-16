use super::Player;
use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};
use std::collections::HashMap;

#[derive(Default)]
pub struct AntMan {
    ant_book: HashMap<HiveResult, i32>,
    visited_nodes: usize,
    ant_hits: usize,
}

impl Player for AntMan {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let own_color = game.turn();

        self.visited_nodes = 0;
        self.ant_hits = 0;

        let res = self.eval(HiveResult::Cont(game), 5, i32::MIN, i32::MAX, own_color);

        println!(
            "AntMan processed {} nodes, value {}, ant book hits {}",
            self.visited_nodes, res.0, self.ant_hits
        );

        res.1
    }
}

impl AntMan {
    fn eval(
        &mut self,
        res: HiveResult,
        depth: usize,
        mut alpha: i32,
        mut beta: i32,
        color: bool,
    ) -> (i32, HiveMove) {
        self.visited_nodes += 1;

        let game = match res {
            HiveResult::WinW(_) => {
                return if color {
                    (i32::MAX, HiveMove::Pass)
                } else {
                    (i32::MIN, HiveMove::Pass)
                }
            }
            HiveResult::WinB(_) => {
                return if !color {
                    (i32::MAX, HiveMove::Pass)
                } else {
                    (i32::MIN, HiveMove::Pass)
                }
            }
            HiveResult::Draw(_) => return (0, HiveMove::Pass),
            HiveResult::Cont(g) => g,
            _ => panic!("eek"),
        };

        if depth == 0 {
            return (search_val(&game, color), HiveMove::Pass);
        }

        if game.turn() == color {
            // maximizing player
            let mut value = i32::MIN;
            let mut mov = HiveMove::Pass;

            // sort by heuristic value for better pruning
            let mut moves = game.valid_moves();
            moves.sort_by_cached_key(|&m| {
                -1 * search_val(&game.make_move(m).game().unwrap(), color)
            });

            for m in moves {
                let node = game.make_move(m);

                let node_val = {
                    let is_ant_move = m.piece().map(|p| p.bug() == HiveBug::Ant).unwrap_or(false);

                    if is_ant_move {
                        if let Some(&v) = self.ant_book.get(&node) {
                            self.ant_hits += 1;
                            v
                        } else {
                            let v = self
                                .eval(node.clone(), depth - 1, alpha, beta, color)
                                .0
                                .saturating_sub(1);
                            self.ant_book.insert(node, v);
                            v
                        }
                    } else {
                        self.eval(node, depth - 1, alpha, beta, color)
                            .0
                            .saturating_sub(1)
                    }
                };

                if node_val > value {
                    value = node_val;
                    mov = m;
                }

                if value > beta {
                    break;
                }

                alpha = alpha.max(value);
            }
            return (value, mov);
        } else {
            // minimizing player
            let mut value = i32::MAX;
            let mut mov = HiveMove::Pass;

            // sort by heuristic value for better pruning
            let mut moves = game.valid_moves();
            moves.sort_by_cached_key(|&m| search_val(&game.make_move(m).game().unwrap(), color));

            for m in moves {
                let node = game.make_move(m);

                let node_val = {
                    let is_ant_move = m.piece().map(|p| p.bug() == HiveBug::Ant).unwrap_or(false);

                    if is_ant_move {
                        if let Some(&v) = self.ant_book.get(&node) {
                            v
                        } else {
                            let v = self
                                .eval(node.clone(), depth - 1, alpha, beta, color)
                                .0
                                .saturating_sub(1);
                            self.ant_book.insert(node, v);
                            v
                        }
                    } else {
                        self.eval(node, depth - 1, alpha, beta, color)
                            .0
                            .saturating_sub(1)
                    }
                };

                if node_val < value {
                    value = node_val;
                    mov = m;
                }

                if value < alpha {
                    break;
                }

                beta = beta.min(value);
            }
            return (value, mov);
        }
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
