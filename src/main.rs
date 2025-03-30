mod hive;
mod player;

use hive::{HiveBug, HiveGame, HiveResult};
use player::Player;

use std::cell::Cell;

fn main() {
    let mut player1 = player::search::Search::default();
    let mut player2 = player::multisearch::MultiSearch::default();

    let node_limit: u32 = 10_000;

    let mut game = HiveGame::new();
    println!("\nGame state:\n{}", game.disp());

    for i in 0..100 {
        let p1_nodes = Cell::new(node_limit);
        let next = player1.make_move(game.clone().with_budget(&p1_nodes));
        let res = game.make_move(next);

        if let Some(g) = process_result(res) {
            game = g;
        } else {
            break;
        }

        println!("\nGame state:\n{}", game.disp());
        println!("Possible next moves: {}", game.valid_moves().iter().count());
        //println!("Board occupied:\n{}", game.board().disp_occupied());
        //println!("Board perimeter:\n{}", game.board().disp_perimeter());

        let p2_nodes = Cell::new(node_limit);
        let next = player2.make_move(game.clone().with_budget(&p2_nodes));
        let res = game.make_move(next);

        if let Some(g) = process_result(res) {
            game = g;
        } else {
            break;
        }

        println!("Game disp:\n{}", game.disp());
        //println!("Board occupied:\n{}", game.board().disp_occupied());
        //println!("Board perimeter:\n{}", game.board().disp_perimeter());
    }
}

fn process_result(res: HiveResult) -> Option<HiveGame> {
    match res {
        HiveResult::Cont(g) => Some(g),
        HiveResult::WinW(g) => {
            println!("WinW with:\n{}", g.disp_board());
            None
        }
        HiveResult::WinB(g) => {
            println!("WinB with:\n{}", g.disp_board());
            None
        }
        HiveResult::Draw(g) => {
            println!("Draw with:\n{}", g.disp_board());
            None
        }
        HiveResult::Invalid => {
            println!("Invalid");
            None
        }
        HiveResult::OutOfMoves(_) => {
            panic!("Should never happen, top level game has no move budget")
        }
    }
}
