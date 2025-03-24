mod hive;
mod player;

use hive::{HiveBug, HiveGame, HiveResult};
use player::Player;

fn main() {
    let mut player1 = player::multisearch::MultiSearch::default();
    let mut player2 = player::search::Search::default();

    let mut game = HiveGame::new();
    println!("\nGame state:\n{}", game.disp());

    for i in 0..100 {
        let next = player1.make_move(game.clone());
        let res = game.make_move(next);

        match res {
            HiveResult::Cont(g) => {
                game = g;
            }
            HiveResult::WinW(g) => {
                println!("WinW with:\n{}", g.disp_board());
                break;
            }
            HiveResult::WinB(g) => {
                println!("WinB with:\n{}", g.disp_board());
                break;
            }
            HiveResult::Draw(g) => {
                println!("Draw with:\n{}", g.disp_board());
                break;
            }
            HiveResult::Invalid => {
                println!("Invalid");
                break;
            }
        }

        println!("\nGame state:\n{}", game.disp());
        println!("Possible next moves: {}", game.valid_moves().iter().count());
        //println!("Board occupied:\n{}", game.board().disp_occupied());
        //println!("Board perimeter:\n{}", game.board().disp_perimeter());

        let next = player2.make_move(game.clone());
        let res = game.make_move(next);

        match res {
            HiveResult::Cont(g) => {
                game = g;
            }
            HiveResult::WinW(g) => {
                println!("WinW with:\n{}", g.disp_board());
                break;
            }
            HiveResult::WinB(g) => {
                println!("WinB with:\n{}", g.disp_board());
                break;
            }
            HiveResult::Draw(g) => {
                println!("Draw with:\n{}", g.disp_board());
                break;
            }
            HiveResult::Invalid => {
                println!("Invalid");
                break;
            }
        }

        println!("Game disp:\n{}", game.disp());
        //println!("Board occupied:\n{}", game.board().disp_occupied());
        //println!("Board perimeter:\n{}", game.board().disp_perimeter());
    }
}
