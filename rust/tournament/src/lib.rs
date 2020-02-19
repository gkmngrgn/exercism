use std::cmp::Ordering;
use std::collections::HashMap;

const STATUS_WIN: &str = "win";
const STATUS_DRAW: &str = "draw";
const STATUS_LOSS: &str = "loss";

#[derive(Eq, Clone)]
struct Team {
    name: String,
    win: u8,
    draw: u8,
    loss: u8,
    point: u8,
    match_played: u8,
}

impl Team {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            win: 0,
            draw: 0,
            loss: 0,
            match_played: 0,
            point: 0,
        }
    }

    fn add_score(&mut self, status: &str) {
        match status {
            STATUS_WIN => {
                self.win += 1;
                self.point += 3;
            }
            STATUS_DRAW => {
                self.draw += 1;
                self.point += 1;
            }
            STATUS_LOSS => self.loss += 1,
            _ => panic!("unexpected status."),
        }
        self.match_played += 1;
    }
}

impl Ord for Team {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.point == other.point {
            self.name.cmp(&other.name)
        } else {
            other.point.cmp(&self.point)
        }
    }
}

impl PartialOrd for Team {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Team {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

pub fn tally(match_results: &str) -> String {
    let mut teams: HashMap<&str, Team> = HashMap::new();

    for contest in match_results.trim().split("\n") {
        let status: Vec<&str> = contest.split(";").collect();
        if status.len() != 3 {
            continue;
        }

        let (team_1_name, team_2_name, result) = (status[0], status[1], status[2]);

        let team_1 = teams.entry(team_1_name).or_insert(Team::new(team_1_name));
        team_1.add_score(result);

        let team_2 = teams.entry(team_2_name).or_insert(Team::new(team_2_name));
        team_2.add_score(match result {
            STATUS_WIN => STATUS_LOSS,
            STATUS_DRAW => STATUS_DRAW,
            STATUS_LOSS => STATUS_WIN,
            _ => panic!("unexpected status."),
        });
    }

    let mut scoreboard: Vec<Team> = teams.values().cloned().collect();
    scoreboard.sort();

    let mut table = vec![];
    table.push(format!("{title:30} | MP |  W |  D |  L |  P", title = "Team").to_string());
    for team in scoreboard {
        table.push(format!(
            "{team:30} | {mp:2} | {sw:2} | {sd:2} | {sl:2} | {pt:2}",
            team = team.name,
            mp = team.match_played,
            sw = team.win,
            sd = team.draw,
            sl = team.loss,
            pt = team.point,
        ))
    }
    table.join("\n")
}
