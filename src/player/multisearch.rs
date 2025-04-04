use super::Player;
use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};

#[derive(Default)]
pub struct MultiSearch();

impl Player for MultiSearch {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let own_color = game.turn();

        let mut nodes: u32 = 0;

        let mut depth = 1;

        let res = loop {
            let cur_res = eval_search(
                HiveResult::Cont(game.clone()),
                depth,
                i32::MIN,
                i32::MAX,
                own_color,
                &mut nodes,
            );

            //println!("depth {} move {:?}", depth, cur_res);

            if game.move_budget() == 0 {
                // out of budget to expand nodes
                // return the best move at the latest depth
                break cur_res;
            }

            depth += 1;
        };

        //println!("MultiSearch processed {} nodes, value {}", nodes, res.0);
        res.1
    }
}

fn eval_search(
    res: HiveResult,
    depth: usize,
    mut alpha: i32,
    mut beta: i32,
    color: bool,
    nodes: &mut u32,
) -> (i32, HiveMove) {
    *nodes += 1;

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
        HiveResult::OutOfMoves(g) => return (search_val(&g, color), HiveMove::pass()), // out of moves, return heuristic value
        HiveResult::Cont(g) => g,
        _ => panic!("eek"),
    };

    if depth == 0 {
        return (search_val(&game, color), HiveMove::pass());
    }

    let mut value = if game.turn() == color {
        i32::MIN
    } else {
        i32::MAX
    };
    let mut mov = HiveMove::pass();

    let mut moves = moves_to_search(&game, depth, color);
    let mut results: Vec<(HiveMove, HiveResult)> =
        moves.into_iter().map(|m| (m, game.make_move(m))).collect();

    results.sort_by_cached_key(|(m, r)| -1 * search_val(r.game_ref().unwrap(), game.turn()));

    for (m, r) in results {
        let node_val = eval_search(r, depth - 1, alpha, beta, color, nodes)
            .0
            .saturating_sub(1);

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

fn moves_to_search(game: &HiveGame, depth: usize, color: bool) -> Vec<HiveMove> {
    let valid_moves = game.valid_moves();

    // fully search up to a near depth
    if depth > 3 {
        return valid_moves;
    }

    let mut res = Vec::new();

    let own_queen = game.queen_loc(color);
    let opp_queen = game.queen_loc(!color);

    for m in valid_moves {
        // ugh
        if m.is_pass() {
            res.push(m);
            continue;
        }

        // if move is to queen, consider it
        if let Some(d) = m.dest() {
            if let Some(l) = own_queen {
                if d.dist(&l) == 1 {
                    res.push(m);
                    continue;
                }
            }

            if let Some(l) = opp_queen {
                if d.dist(&l) == 1 {
                    res.push(m);
                    continue;
                }
            }
        }

        // stop adding lower-value moves farther down the tree
        if depth < 2 {
            continue;
        }

        // placements are fine to check out
        if m.is_place() {
            res.push(m);
            continue;
        }

        // if move is not an ant, consider it
        // should cut search space way down, probably ok to ignore non-queen ant moves in the far future
        // future work: maybe consider ant moves if they trap a piece?
        // ie if they have one neighbor which is the opposite color as the current move
        if m.piece().unwrap().bug() != HiveBug::Ant {
            res.push(m);
            continue;
        }
    }

    res
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
    // for (&c, &p) in board.all_top() {
    //     if p.color() == color {
    //         if !board.is_bridge(c) {
    //             res += 2;

    //             if p.bug() == HiveBug::Ant {
    //                 res += 2;
    //             }
    //         }
    //     }
    // }

    res
}
