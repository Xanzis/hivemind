mod hive;

use hive::{HiveBug, HiveGame, HiveResult};

fn main() {
    let mut game = HiveGame::new();

    for i in 0..100 {
        //println!("Game state:\n{:?}", game);
        println!("Game disp:\n{}", game.disp());

        let next = game.valid_moves();
        //println!("Valid next moves:\n{:?}\n", next);

        let i = 9469876982721 % next.len();

        println!("Move: {:?}", next[i]);

        let res = game.make_move(next[i]);

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
    }

    let next = game.valid_moves();
    println!("Game disp:\n{}", game.disp());
    println!("Spider moves:");
    for m in next.iter().filter(|m| m.piece().bug() == HiveBug::Spider) {
        println!("{:?}:", m);
        let res = game.make_move(*m);
        if let HiveResult::Cont(g) = res {
            println!("{}", g.disp_board());
        }
    }
}
