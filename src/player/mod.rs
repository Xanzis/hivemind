use crate::hive::{HiveGame, HiveMove, HiveResult};

pub mod antman;
pub mod min_move;
pub mod min_queen_move;
pub mod multisearch;
pub mod random;
pub mod search;
pub mod swarm;

pub trait Player {
    fn make_move(&mut self, game: HiveGame) -> HiveMove;
}

pub trait Heuristic {
    fn leaf_val(&mut self, game: &HiveGame, color: bool) -> i32;

    fn moves_to_search<'a>(
        &mut self,
        game: &HiveGame,
        moves: Vec<(HiveMove, HiveResult<'a>)>,
        color: bool,
    ) -> Vec<(HiveMove, HiveResult<'a>)> {
        // need this to allow the heuristic to make the moves without duplicating requests
        moves
    }

    fn nonrecurse_val(
        &mut self,
        game: &HiveGame,
        mov: &HiveMove,
        res: &HiveResult,
        color: bool,
    ) -> Option<i32> {
        // optionally preempt recursion with a node evalation
        None
    }

    fn track_val(&mut self, game: &HiveGame, mov: &HiveMove, val: i32) {
        return;
    }
}

// dfs player with generic heuristic, iterative deepening, and alpha-beta pruning
#[derive(Default)]
pub struct SearchPlayer<T: Heuristic + Default>(T);

impl<T: Heuristic + Default> SearchPlayer<T> {
    fn eval(
        &mut self,
        res: HiveResult,
        depth: usize,
        mut alpha: i32,
        mut beta: i32,
        color: bool,
    ) -> (i32, HiveMove) {
        let game = match res {
            HiveResult::WinW(_) => {
                return if color {
                    (i32::MAX, HiveMove::pass())
                } else {
                    (i32::MIN, HiveMove::pass())
                }
            }
            HiveResult::WinB(_) => {
                return if !color {
                    (i32::MAX, HiveMove::pass())
                } else {
                    (i32::MIN, HiveMove::pass())
                }
            }
            HiveResult::Draw(_) => return (0, HiveMove::pass()),
            HiveResult::OutOfMoves(g) => {
                // out of moves, return heuristic value
                let val = self.0.leaf_val(&g, color);
                return (val, HiveMove::pass());
            }
            HiveResult::Cont(g) => g,
            _ => panic!("eek"),
        };

        if depth == 0 {
            return (self.0.leaf_val(&game, color), HiveMove::pass());
        }

        let mut value = if game.turn() == color {
            i32::MIN
        } else {
            i32::MAX
        };
        let mut mov = HiveMove::pass();

        let mut moves = game.valid_moves();
        let mut results: Vec<(HiveMove, HiveResult)> =
            moves.into_iter().map(|m| (m, game.make_move(m))).collect();

        // standard move processor, for reference when reimplementing
        // results.sort_by_cached_key(|(m, r)| -1 * search_val(r.game_ref().unwrap(), game.turn()));

        for (m, r) in self.0.moves_to_search(&game, results, color) {
            let node_val = if let Some(v) = self.0.nonrecurse_val(&game, &m, &r, color) {
                v
            } else {
                self.eval(r, depth - 1, alpha, beta, color)
                    .0
                    .saturating_sub(1)
            };

            self.0.track_val(&game, &m, node_val);

            if game.turn() == color {
                // maximizing player
                if node_val > value {
                    value = node_val;
                    mov = m;
                }

                if value > beta {
                    break;
                }

                alpha = alpha.max(value);
            } else {
                // minimizing player
                if node_val < value {
                    value = node_val;
                    mov = m;
                }

                if value < alpha {
                    break;
                }

                beta = beta.min(value);
            }
        }

        (value, mov)
    }
}

impl<T: Heuristic + Default> Player for SearchPlayer<T> {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let own_color = game.turn();

        let mut depth = 1; // current depth to search to

        let res = loop {
            let cur_res = self.eval(
                HiveResult::Cont(game.clone()),
                depth,
                i32::MIN,
                i32::MAX,
                own_color,
            );

            //println!("depth {} move {:?}", depth, cur_res);

            if game.move_budget() == 0 {
                // out of budget to expand nodes
                // return the best move at the latest depth
                break cur_res;
            }

            depth += 1;
        };

        //println!("Search processed {} nodes, value {}", nodes, res.0);
        res.1
    }
}
