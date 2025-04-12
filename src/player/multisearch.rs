use crate::hive::{HiveBug, HiveGame, HiveMove, HiveResult};
use crate::player::{Heuristic, SearchPlayer};

pub type MultiSearch = SearchPlayer<MultiSearchHeuristic>;

#[derive(Default)]
pub struct MultiSearchHeuristic();

impl Heuristic for MultiSearchHeuristic {
    fn leaf_val(&mut self, game: &HiveGame, color: bool) -> i32 {
        search_val(game, color)
    }

    fn moves_to_search<'a>(
        &mut self,
        game: &HiveGame,
        moves: Vec<(HiveMove, HiveResult<'a>)>,
        depth: usize,
        color: bool,
    ) -> Vec<(HiveMove, HiveResult<'a>)> {
        let own_queen = game.queen_loc(color);
        let opp_queen = game.queen_loc(!color);

        let mut m = match depth {
            0..=1 => {
                // at bottom of tree, consider only queen moves and moves to/from queen
                moves
                    .into_iter()
                    .filter(|&(ref m, _)| {
                        let move_bug: Option<HiveBug> = m.piece().map(|p| p.bug());

                        m.is_pass()
                            || move_bug == Some(HiveBug::Queen)
                            || match (m.dest(), own_queen, opp_queen) {
                                (Some(a), Some(b), Some(c)) => a.dist(&b) == 1 || a.dist(&c) == 1,
                                (Some(a), Some(b), None) => a.dist(&b) == 1,
                                (Some(a), None, Some(b)) => a.dist(&b) == 1,
                                _ => false,
                            }
                            || match (m.orig(), own_queen, opp_queen) {
                                (Some(a), Some(b), Some(c)) => a.dist(&b) == 1 || a.dist(&c) == 1,
                                (Some(a), Some(b), None) => a.dist(&b) == 1,
                                (Some(a), None, Some(b)) => a.dist(&b) == 1,
                                _ => false,
                            }
                    })
                    .collect()
            }
            2..=4 => {
                // near bottom of the tree, search placements and moves that aren't ants
                moves
                    .into_iter()
                    .filter(|&(ref m, _)| {
                        m.is_pass()
                            || m.piece().map(|p| p.bug()) != Some(HiveBug::Ant)
                            || m.is_place()
                    })
                    .collect()
            }
            _ => moves,
        };

        m.sort_by_cached_key(|(_, r)| -1 * self.leaf_val(r.game_ref().unwrap(), game.turn()));
        m
    }

    fn ident() -> &'static str {
        "multisearch"
    }
}

fn search_val(game: &HiveGame, color: bool) -> i32 {
    // heuristic value of node
    let board = game.board();

    let own_queen = game.queen_loc(color);
    let opp_queen = game.queen_loc(!color);

    let mut res = 0;

    // queen surrounded is bad
    if let Some(l) = own_queen {
        // queen surrounded is bad
        res -= (board.neighbor_cells(l).count().pow(2) * 10) as i32;

        //beetles neighboring queen is bad
        res -= (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 50) as i32;

        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 10;
        }
    }

    if let Some(l) = opp_queen {
        // other queen surrounded is good
        res += (board.neighbor_cells(l).count().pow(2) * 12) as i32;

        //beetles neighboring other queen is good
        res += (board
            .neighbor_pieces(l)
            .filter(|p| p.bug() == HiveBug::Beetle)
            .count()
            * 60) as i32;

        // what the heck, beetle on the other queen is good
        if board.get_top(l).unwrap().bug() == HiveBug::Beetle {
            res += 10;
        }
    }

    res
}
