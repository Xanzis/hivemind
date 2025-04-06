mod hive;
mod player;
mod tourny;

use hive::{HiveBug, HiveGame, HiveResult};
use player::Player;
use tourny::{default_player, PlayerConstructor};

use clap::{Args, Parser, Subcommand};

use std::cell::Cell;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about)]
/// A tournament engine for the Hive board game
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a tournament between all available players
    Tournament,

    /// Run a game between a given pair of players
    Game(GameArgs),

    /// List available players
    ListPlayers,
}

#[derive(Args)]
struct GameArgs {
    player_a: String,
    player_b: String,
}

fn main() {
    let cli = Cli::parse();

    let players = vec![
        default_player::<player::antman::AntMan>,
        default_player::<player::min_move::MinMove>,
        default_player::<player::min_queen_move::MinQueenMove>,
        default_player::<player::multisearch::MultiSearch>,
        default_player::<player::random::Random>,
        default_player::<player::search::Search>,
        default_player::<player::swarm::Swarm>,
    ];

    match cli.command {
        Commands::Tournament => {
            run_tournament(&players);
        }
        Commands::Game(args) => {
            let a = players
                .iter()
                .position(|p| p().ident() == &args.player_a)
                .unwrap();
            let b = players
                .iter()
                .position(|p| p().ident() == &args.player_b)
                .unwrap();

            run_game(players[a], players[b]);
        }
        Commands::ListPlayers => {
            players.iter().for_each(|p| println!("{}", p().ident()));
        }
    }
}

fn run_tournament(players: &[PlayerConstructor]) {
    let mut elos = HashMap::new();

    for a in 0..players.len() {
        for b in (0..players.len()).filter(|x| *x != a) {
            let ident_a = players[a]().ident();
            let ident_b = players[b]().ident();
            println!("running match between {} and {} ...", ident_a, ident_b);
            tourny::run_match(players[a], players[b], &mut elos, 5);
            println!(
                "... done, elos ({}, {}), ({}, {})",
                ident_a,
                elos.get(&ident_a).unwrap(),
                ident_b,
                elos.get(&ident_b).unwrap()
            );
        }
    }

    println!("{:?}", elos);
}

fn run_game(a: PlayerConstructor, b: PlayerConstructor) {
    let mut player1 = a();
    let mut player2 = b();

    let node_limit: u32 = 40_000;

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
