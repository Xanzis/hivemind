mod hive;
mod player;
mod tourny;

use tourny::default_player;

use hive::{HiveBug, HiveGame, HiveResult};
use player::Player;

use std::cell::Cell;
use std::collections::HashMap;

fn main() {
    let players = vec![
        default_player::<player::search::SearchNew>,
        default_player::<player::multisearch::MultiSearch>,
    ];

    let mut elos = HashMap::new();

    tourny::run_match(players[0], players[1], &mut elos, 5);

    println!("{:?}", elos);
}
