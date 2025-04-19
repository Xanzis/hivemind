use super::Player;
use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};
use std::io;

#[derive(Default)]
pub struct Me();

impl Player for Me {
    fn make_move(&mut self, game: HiveGame) -> HiveMove {
        // minimize opponent's next moves

        println!("your turn");

        let moves = game.valid_moves();

        let mut choice = select_move(&game, moves.clone());

        while choice.is_none() {
            println!("invalid selection, repeat selection");
            println!("board state is\n{}", game.disp_board());
            choice = select_move(&game, moves.clone())
        }

        choice.unwrap()
    }

    fn ident(&self) -> &'static str {
        "me"
    }
}

fn select_move(game: &HiveGame, mut moves: Vec<HiveMove>) -> Option<HiveMove> {
    let mut buf = String::new();

    println!("select move:");
    println!("place [p], move [m], or pass [-]?");
    io::stdin().read_line(&mut buf).expect("io error");

    moves = match buf.trim() {
        "p" => moves.into_iter().filter(|m| m.is_place()).collect(),
        "m" => moves.into_iter().filter(|&m| m.is_move()).collect(),
        "-" => return Some(HiveMove::pass()),
        _ => return None,
    };

    println!("queen [q], beetle [b], spider [s], grasshopper [g], or ant [a]?");
    buf.clear();
    io::stdin().read_line(&mut buf).expect("io error");

    moves = match buf.trim() {
        "q" => moves
            .into_iter()
            .filter(|m| matches!(m.piece().map(|p| p.bug()), Some(HiveBug::Queen)))
            .collect(),
        "b" => moves
            .into_iter()
            .filter(|m| matches!(m.piece().map(|p| p.bug()), Some(HiveBug::Beetle)))
            .collect(),
        "s" => moves
            .into_iter()
            .filter(|m| matches!(m.piece().map(|p| p.bug()), Some(HiveBug::Spider)))
            .collect(),
        "g" => moves
            .into_iter()
            .filter(|m| matches!(m.piece().map(|p| p.bug()), Some(HiveBug::Grasshopper)))
            .collect(),
        "a" => moves
            .into_iter()
            .filter(|m| matches!(m.piece().map(|p| p.bug()), Some(HiveBug::Ant)))
            .collect(),
        _ => return None,
    };

    println!("available moves:");

    for (i, m) in moves.iter().enumerate() {
        println!("[{}] {:?} results in:", i, m);

        match game.make_move(m.clone()) {
            HiveResult::WinW(_) => println!("win for white"),
            HiveResult::WinB(_) => println!("win for black"),
            HiveResult::Draw(_) => println!("draw"),
            HiveResult::Cont(g) => println!("continue with board:\n{}", g.disp_board()),
            _ => unreachable!(),
        }
    }

    println!("enter index of move to play:");
    buf.clear();
    io::stdin().read_line(&mut buf).expect("io error");

    if let Ok(i) = buf.trim().parse::<usize>() {
        if i < moves.len() {
            return Some(moves[i]);
        }
    }

    return None;
}
