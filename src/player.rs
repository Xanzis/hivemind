use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};

pub fn random(game: HiveGame) -> HiveMove {
    let moves = game.valid_moves();
    let i = (9087901 + game.round()) % moves.len();
    moves[i]
}

pub fn min_move(game: HiveGame) -> HiveMove {
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

pub fn min_queen_move(game: HiveGame) -> HiveMove {
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

pub fn search(game: HiveGame) -> HiveMove {
    let own_color = game.turn();

    game.valid_moves()
        .into_iter()
        .max_by_key(|&m| eval_search(game.make_move(m), 1, i32::MIN, i32::MAX, own_color))
        .expect("ope")
}

pub fn eval_search(
    res: HiveResult,
    depth: usize,
    mut alpha: i32,
    mut beta: i32,
    color: bool,
) -> i32 {
    let game = match res {
        HiveResult::WinW(_) => return if color { i32::MAX } else { i32::MIN },
        HiveResult::WinB(_) => return if !color { i32::MAX } else { i32::MIN },
        HiveResult::Draw(_) => return 0,
        HiveResult::Cont(g) => g,
        _ => panic!("eek"),
    };

    if depth == 0 {
        // heuristic value of node
        let board = game.board();

        let own_queen = game.queen_loc(color);
        let opp_queen = game.queen_loc(!color);

        let own_q_neighbors = if let Some(l) = own_queen {
            board.neighbor_cells(l).count()
        } else {
            0
        };
        let opp_q_neighbors = if let Some(l) = opp_queen {
            board.neighbor_cells(l).count()
        } else {
            0
        };

        return opp_q_neighbors.pow(2) as i32 - own_q_neighbors.pow(2) as i32;
    }

    if game.turn() == color {
        let mut value = i32::MIN;
        for m in game.valid_moves() {
            value = value.max(eval_search(
                game.make_move(m),
                depth - 1,
                alpha,
                beta,
                color,
            ));
            if value > beta {
                break;
            }
            alpha = alpha.max(value);
        }
        return value;
    } else {
        let mut value = i32::MAX;
        for m in game.valid_moves() {
            value = value.min(eval_search(
                game.make_move(m),
                depth - 1,
                alpha,
                beta,
                color,
            ));
            if value < alpha {
                break;
            }
            beta = beta.min(value)
        }
        return value;
    }
}
