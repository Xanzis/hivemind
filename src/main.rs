mod hive;

fn main() {
    let mut game = hive::HiveGame::new();

    for i in 0..200 {
        //println!("Game state:\n{:?}", game);
        println!("Game disp:\n{}", game.disp());

        let next = game.valid_moves();
        //println!("Valid next moves:\n{:?}\n", next);

        let i = 9469876982721 % next.len();

        println!("Move: {:?}", next[i]);

        let res = game.make_move(next[i]);

        match res {
            hive::HiveResult::Cont(g) => {
                game = g;
            },
            hive::HiveResult::WinW => {
                println!("WinW");
                break
            },
            hive::HiveResult::WinB => {
                println!("WinB");
                break
            },
            hive::HiveResult::Draw => {
                println!("Draw");
                break
            },
            hive::HiveResult::Invalid => {
                println!("Invalid");
                break
            },
        }
    }
}
