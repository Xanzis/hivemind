use crate::hive::{HiveGame, HiveResult};
use crate::player::Player;

use std::cell::Cell;
use std::collections::HashMap;

pub type PlayerConstructor = fn() -> Box<dyn Player>;

pub fn default_player<T>() -> Box<dyn Player>
where
    T: Player + Default + 'static,
{
    Box::new(T::default())
}

pub fn run_match(
    cons_a: PlayerConstructor,
    cons_b: PlayerConstructor,
    elos: &mut HashMap<&'static str, f32>,
    rounds: usize,
) {
    let ident_a = cons_a().ident();
    let ident_b = cons_b().ident();

    let elo_a = *elos.entry(ident_a).or_insert(1000.0);
    let elo_b = *elos.entry(ident_b).or_insert(1000.0);

    let q_a = 10.0f32.powf(elo_a / 400.0);
    let q_b = 10.0f32.powf(elo_b / 400.0);

    let e_a = q_a / (q_a + q_b); // expected result for player a
    let e_b = 1.0 - e_a;

    let mut s_a = 0.0;
    let mut s_b = 0.0;

    for _ in 0..rounds {
        let round_score = run_game(cons_a, cons_b);
        s_a += round_score.0 / (rounds as f32);
        s_b += round_score.1 / (rounds as f32);
    }

    let k = 32.0; // reasonable choice, lower changes ratings more slower

    *elos.get_mut(&ident_a).unwrap() += k * (s_a - e_a);
    *elos.get_mut(&ident_b).unwrap() += k * (s_b - e_b);
}

pub fn run_game(cons_a: PlayerConstructor, cons_b: PlayerConstructor) -> (f32, f32) {
    let mut player_a = cons_a();
    let mut player_b = cons_b();

    let node_limit: u32 = 40_000;

    let mut game = HiveGame::new();

    for _ in 0..100 {
        let pa_nodes = Cell::new(node_limit);
        let next = player_a.make_move(game.clone().with_budget(&pa_nodes));
        let res = game.make_move(next);

        game = match res {
            HiveResult::Cont(g) => g,
            HiveResult::WinW(_) => return (1.0, 0.0),
            HiveResult::WinB(_) => return (0.0, 1.0),
            HiveResult::Draw(_) => return (0.5, 0.5),
            HiveResult::Invalid => panic!("invalid move"),
            HiveResult::OutOfMoves(_) => {
                panic!("Should never happen, top level game has no move budget")
            }
        };

        let pb_nodes = Cell::new(node_limit);
        let next = player_b.make_move(game.clone().with_budget(&pb_nodes));
        let res = game.make_move(next);

        game = match res {
            HiveResult::Cont(g) => g,
            HiveResult::WinW(_) => return (1.0, 0.0),
            HiveResult::WinB(_) => return (0.0, 1.0),
            HiveResult::Draw(_) => return (0.5, 0.5),
            HiveResult::Invalid => panic!("invalid move"),
            HiveResult::OutOfMoves(_) => {
                panic!("Should never happen, top level game has no move budget")
            }
        };
    }

    // a little dumb but treat a 100 move game as a draw
    (0.5, 0.5)
}
