mod hive;

fn main() {
    let mut game = hive::HiveGame::new();

    for i in 0..100 {
        //println!("Game state:\n{:?}", game);
        println!("Game disp:\n{}", game.disp());

        let next = game.valid_moves();
        //println!("Valid next moves:\n{:?}\n", next);

        let i = 9469876982721 % next.len();

        let res = game.make_move(next[i]);

        if let Some(g) = res.game() {
            game = g
        } else {
            println!("Game over");
            break;
        }
    }
}
