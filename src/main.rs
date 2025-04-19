mod hive;
mod player;
mod tourny;

use hive::{HiveGame, HiveResult};
use tourny::{default_player, PlayerConstructor};

use clap::{Args, Parser, Subcommand};

use std::cell::Cell;
use std::collections::HashMap;
use std::fs;
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
    Tournament(TournamentArgs),

    /// Run a single player against all other players
    Simul(SimulArgs),

    /// Run a game between a given pair of players
    Game(GameArgs),

    /// List available players
    ListPlayers,
}

#[derive(Args)]
struct TournamentArgs {
    elo_path: PathBuf,
}

#[derive(Args)]
struct SimulArgs {
    elo_path: PathBuf,
    player_a: String,
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
        default_player::<player::nuance::Nuance>,
    ];

    match cli.command {
        Commands::Tournament(args) => {
            run_tournament(&players, args.elo_path);
        }
        Commands::Simul(args) => {
            let a = players
                .iter()
                .find(|p| p().ident() == &args.player_a)
                .unwrap();
            run_simul(&players, args.elo_path, *a);
        }
        Commands::Game(args) => {
            // in a 1:1 game, user can be a player
            let mut players = players;
            players.push(default_player::<player::me::Me>);

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

fn run_tournament(players: &[PlayerConstructor], elo_path: PathBuf) {
    let mut elos = load_elos(elo_path.clone()).unwrap_or(HashMap::new());

    for a in 0..players.len() {
        for b in (0..players.len()).filter(|x| *x != a) {
            let ident_a = players[a]().ident();
            let ident_b = players[b]().ident();
            println!("running match between {} and {} ...", ident_a, ident_b);
            tourny::run_match(players[a], players[b], &mut elos, 5);
            println!(
                "... done, elos ({}, {}), ({}, {})",
                ident_a,
                elos.get(ident_a).unwrap(),
                ident_b,
                elos.get(ident_b).unwrap()
            );
        }
    }

    println!("{:?}", elos);

    let _ = save_elos(elo_path, &elos);
}

fn run_simul(players: &[PlayerConstructor], elo_path: PathBuf, player_a: PlayerConstructor) {
    let mut elos = load_elos(elo_path.clone()).unwrap_or(HashMap::new());

    for player_b in players.iter().copied() {
        let ident_a = player_a().ident();
        let ident_b = player_b().ident();
        println!("running match between {} and {} ...", ident_a, ident_b);
        tourny::run_match(player_a, player_b, &mut elos, 5);
        println!(
            "... done, elos ({}, {}), ({}, {})",
            ident_a,
            elos.get(ident_a).unwrap(),
            ident_b,
            elos.get(ident_b).unwrap()
        );
    }

    println!("{:?}", elos);

    let _ = save_elos(elo_path, &elos);
}

fn run_game(a: PlayerConstructor, b: PlayerConstructor) {
    let mut player1 = a();
    let mut player2 = b();

    let node_limit: u32 = 40_000;

    let mut game = HiveGame::new();
    println!("\nGame state:\n{}", game.disp());

    for _ in 0..100 {
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

        let p2_nodes = Cell::new(node_limit);
        let next = player2.make_move(game.clone().with_budget(&p2_nodes));
        let res = game.make_move(next);

        if let Some(g) = process_result(res) {
            game = g;
        } else {
            break;
        }

        println!("Game disp:\n{}", game.disp());
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

fn load_elos(path: PathBuf) -> std::io::Result<HashMap<String, f32>> {
    let file = fs::read_to_string(path)?;
    Ok(serde_json::from_str(&file).unwrap())
}

fn save_elos(path: PathBuf, elos: &HashMap<String, f32>) -> std::io::Result<()> {
    let json = serde_json::to_string(elos).unwrap();
    fs::write(path, json)
}
