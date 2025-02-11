use super::Player;
use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};

#[derive(Default)]
pub struct Search();

impl Player for Search {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        let own_color = game.turn();

        let mut nodes: u32 = 0;

        let res = eval_search(
            HiveResult::Cont(game),
            3,
            i32::MIN,
            i32::MAX,
            own_color,
            &mut nodes,
        );

        println!("Search processed {} nodes, value {}", nodes, res.0);

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
        moves.sort_by_cached_key(|&m| -1 * search_val(&game.make_move(m).game().unwrap(), color));

        for m in moves {
            let node_val = eval_search(game.make_move(m), depth - 1, alpha, beta, color, nodes)
                .0
                .saturating_sub(1);

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
            let node_val = eval_search(game.make_move(m), depth - 1, alpha, beta, color, nodes)
                .0
                .saturating_sub(1);

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
