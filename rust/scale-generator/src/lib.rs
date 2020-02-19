use failure::Error;

const SCALE_SHARP: &'static str = "A|A#|B|C|C#|D|D#|E|F|F#|G|G#";
const SCALE_FLAT: &'static str = "A|Bb|B|C|Db|D|Eb|E|F|Gb|G|Ab";

pub struct Scale {
    tonic: String,
    tone_map: Vec<bool>,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        let tonic_map: Vec<bool> = intervals
            .chars()
            .map(|m| match m {
                'A' => vec![true, false, false],
                'M' => vec![true, false],
                'm' => vec![true],
                _ => vec![],
            })
            .flatten()
            .collect();
        Ok(Scale {
            tonic: tonic.to_string(),
            tone_map: tonic_map,
        })
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        Scale::new(tonic, &'m'.to_string().repeat(12))
    }

    pub fn enumerate(&self) -> Vec<String> {
        let capitalized_tonic = self.capitalize(&self.tonic);
        let scale = match self.tonic.as_str() {
            t if t.ends_with("#") => SCALE_SHARP,
            "A" | "B" | "C" | "D" | "E" | "G" | "a" | "b" | "e" => SCALE_SHARP,
            _ => SCALE_FLAT,
        }
        .split("|")
        .cycle()
        .skip_while(|&n| n != &capitalized_tonic);
        self.tone_map
            .iter()
            .zip(scale)
            .filter(|(&i, _)| i)
            .map(|(_, t)| t.to_string())
            .collect()
    }

    fn capitalize(&self, tonic: &str) -> String {
        format!(
            "{}{}",
            tonic.chars().nth(0).unwrap().to_uppercase(),
            tonic.chars().skip(1).collect::<String>()
        )
    }
}
