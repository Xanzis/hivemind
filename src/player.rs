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

pub fn swarm(game: HiveGame) -> HiveMove {
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
            let v = v - 5 * own_piece_count;

            if v < value {
                value = v;
                mov = m;
            }
        }
    }

    mov
}

pub fn search(game: HiveGame) -> HiveMove {
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

    println!("Processed {} nodes, value {}", nodes, res.0);

    res.1
}

pub fn eval_search(
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
        for m in game.valid_moves() {
            let node_val =
                eval_search(game.make_move(m), depth - 1, alpha, beta, color, nodes).0 - 1;

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
        for m in game.valid_moves() {
            let node_val =
                eval_search(game.make_move(m), depth - 1, alpha, beta, color, nodes).0 - 1;

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
        res -= board.neighbor_cells(l).count().pow(2) as i32;

        //beetles neighboring queen is bad
        res -= (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 2) as i32;
    }

    if let Some(l) = opp_queen {
        // other queen surrounded is good
        res += board.neighbor_cells(l).count().pow(2) as i32;

        //beetles neighboring other queen is good
        res += (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 2) as i32;

        //what the heck, beetle on the other queen is good
        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 3;
        }
    }

    res
}
