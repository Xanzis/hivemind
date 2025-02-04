mod hive;
mod player;

use hive::{HiveBug, HiveGame, HiveResult};

fn main() {
    let mut game = HiveGame::new();
    println!("Game disp:\n{}", game.disp());

    for i in 0..100 {
        let next = player::random(game.clone());
        let res = game.make_move(next);

        match res {
            HiveResult::Cont(g) => {
                game = g;
            }
            HiveResult::WinW(_) => {
                println!("WinW");
                break;
            }
            HiveResult::WinB(_) => {
                println!("WinB");
                break;
            }
            HiveResult::Draw(_) => {
                println!("Draw");
                break;
            }
            HiveResult::Invalid => {
                println!("Invalid");
                break;
            }
        }

        println!("Game disp:\n{}", game.disp());

        let next = player::search(game.clone());
        let res = game.make_move(next);

        match res {
            HiveResult::Cont(g) => {
                game = g;
            }
            HiveResult::WinW(_) => {
                println!("WinW");
                break;
            }
            HiveResult::WinB(_) => {
                println!("WinB");
                break;
            }
            HiveResult::Draw(_) => {
                println!("Draw");
                break;
            }
            HiveResult::Invalid => {
                println!("Invalid");
                break;
            }
        }

        println!("Game disp:\n{}", game.disp());
    }
}
