use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct LetterLocation {
    letter: char,
    index: usize,
    is_yes: bool,
}
#[derive(Clone, Debug, Eq, PartialEq)]
struct LetterLocations {
    letters: HashSet<LetterLocation>,
}
impl LetterLocations {
    pub fn new() -> Self {
        LetterLocations {
            letters: HashSet::new(),
        }
    }

    pub fn insert(&mut self, letter_location: LetterLocation) -> bool {
        self.letters.insert(letter_location)
    }

    pub fn extend<I: IntoIterator<Item = LetterLocation>>(&mut self, iter: I) {
        self.letters.extend(iter);
    }

    pub fn iter(&self) -> impl Iterator<Item = &LetterLocation> {
        self.letters.iter()
    }
}
impl<'a> IntoIterator for &'a LetterLocations {
    type Item = &'a LetterLocation;
    type IntoIter = std::collections::hash_set::Iter<'a, LetterLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.letters.iter()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct ExclusionLetter {
    letter: char,
}
#[derive(Clone, Debug, Eq, PartialEq)]
struct ExclusionLetters {
    letters: HashSet<ExclusionLetter>,
}

impl ExclusionLetters {
    pub fn new() -> Self {
        ExclusionLetters {
            letters: HashSet::new(),
        }
    }

    pub fn insert(&mut self, exclusion_letter: ExclusionLetter) -> bool {
        self.letters.insert(exclusion_letter)
    }

    pub fn insert_letter(&mut self, letter: char) -> bool {
        self.insert(ExclusionLetter { letter })
    }

    pub fn extend<I: IntoIterator<Item = ExclusionLetter>>(&mut self, iter: I) {
        self.letters.extend(iter);
    }

    pub fn extend_letters<I: IntoIterator<Item = char>>(&mut self, iter: I) {
        self.extend(iter.into_iter().map(|letter| ExclusionLetter { letter }));
    }

    pub fn is_empty(&self) -> bool {
        self.letters.is_empty()
    }

    pub fn contains_letter(&self, letter: char) -> bool {
        self.letters.contains(&ExclusionLetter { letter })
    }
}

struct DoubleFilters {
    excludes: ExclusionLetters,
    locations: LetterLocations,
}

impl DoubleFilters {
    pub fn new() -> Self {
        DoubleFilters {
            locations: LetterLocations::new(),
            excludes: ExclusionLetters::new(),
        }
    }

    pub fn extend(&mut self, new_filters: DoubleFilters) {
        self.locations.extend(new_filters.locations.letters);
        self.excludes.extend(new_filters.excludes.letters);
    }

    pub fn extend_filters(&mut self, (locations, excludes): (LetterLocations, ExclusionLetters)) {
        self.locations.extend(locations.letters);
        self.excludes.extend(excludes.letters);
    }
    pub fn is_empty(&self) -> bool {
        self.locations.letters.is_empty() && self.excludes.is_empty()
    }
}

const WORDLE_LENGTH: usize = 5;

#[inline]
fn expected_letter_filter(word: &str, location: &LetterLocation) -> bool {
    let actual = word.as_bytes()[location.index] as char;
    word.contains(location.letter) && (location.is_yes == (actual == location.letter))
}

fn read_args() -> (DoubleFilters, String, bool, bool) {
    let mut filters = DoubleFilters::new();
    let mut target = String::new();
    let mut show_all = false;
    let mut solve_all = false;

    let mut args = env::args().skip(1); // skip program name (1st arg)
    while let Some(arg) = args.next() {
        match &arg[..] {
            "-a" | "--show-all" => {
                show_all = true;
            }
            "-t" | "--target-word" => {
                target = args.next().expect("need an argument for solve");
            }
            "-e" | "--everything" => {
                solve_all = true;
            }
            _ => {
                if arg.starts_with('-') {
                    eprintln!("Skipping unknown flag argument {}", arg);
                } else {
                    // Parse wordle-solving clues given as an argument:
                    filters.extend_filters(parse_location_arg(&arg));
                }
            }
        }
    }
    (filters, target, show_all, solve_all)
}

fn parse_location_arg(arg: &str) -> (LetterLocations, ExclusionLetters) {
    let mut locations = LetterLocations::new();
    let mut excludes = ExclusionLetters::new();

    // Compile regex only once
    let user_args_re = Regex::new(r"([a-z]+)\+?(\d+)?-?(\d+)?").unwrap();
    if let Some(cap) = user_args_re.captures(arg) {
        let letters: Vec<char> = cap[1].chars().collect();

        let locs_yes = cap.get(2).map_or("", |m| m.as_str());
        let locs_no = cap.get(3).map_or("", |m| m.as_str());

        let is_exclusion_arg = locs_yes.is_empty() && locs_no.is_empty();
        if is_exclusion_arg {
            excludes.extend_letters(letters);
        } else {
            assert!(
                letters.len() == 1,
                "location letter can only be one character"
            );
            let letter = letters[0];

            // Process "yes" and "no" arguments in a loop
            let locs_yes_no = vec![(true, locs_yes), (false, locs_no)];
            for (is_yes, locs) in locs_yes_no.iter() {
                for loc_str in locs.chars() {
                    let loc: usize = loc_str.to_digit(10).unwrap_or(0) as usize;
                    assert!(loc > 0 && loc <= WORDLE_LENGTH, "char positions are 1 - 5");
                    locations.insert(LetterLocation {
                        letter: letter,
                        index: loc - 1,
                        is_yes: *is_yes,
                    });
                }
            }
        }
    }

    (locations, excludes)
}

fn get_words(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("loading file {filename}");
    let reader = BufReader::new(file);

    let mut words: Vec<String> = reader
        .lines()
        .filter_map(Result::ok)
        .filter(|word| word.len() == WORDLE_LENGTH)
        .filter(|word| !word.contains('\'')) // TODO: extend this to exclude any punctuation?
        .filter(|word| word.is_ascii())
        .map(|word| word.to_lowercase())
        .collect();
    words.sort();
    words
}

fn score_letters(words: &[String]) -> HashMap<char, f32> {
    // Count letter frequency
    let mut letter_counts: HashMap<char, u32> = HashMap::with_capacity(26);
    let mut total_count: u32 = 0;

    for word in words {
        for letter in word.chars() {
            *letter_counts.entry(letter).or_insert(0) += 1;
            total_count += 1;
        }
    }

    // Calculate scores
    let mut letter_scores: HashMap<char, f32> = HashMap::with_capacity(26);
    let mut max_score: f32 = 0.0;

    for (letter, count) in letter_counts {
        let score = count as f32 / total_count as f32;
        letter_scores.insert(letter, score);
        max_score = max_score.max(score);
    }

    // Normalize scores
    for score in letter_scores.values_mut() {
        *score /= max_score;
    }

    letter_scores
}

fn score_words(words: &[String]) -> HashMap<String, f32> {
    let letter_scores = score_letters(words);
    let mut word_scores = HashMap::with_capacity(words.len());

    for word in words {
        let unique_letters: HashSet<char> = word.chars().collect();
        let unique_ratio = unique_letters.len() as f32 / word.len() as f32;

        let frequency_score: f32 = word
            .chars()
            .map(|letter| *letter_scores.get(&letter).unwrap_or(&0.0))
            .sum::<f32>()
            / word.len() as f32;

        let combined_score = (frequency_score + unique_ratio) / 2.0;
        word_scores.insert(word.clone(), combined_score);
    }

    word_scores
}

fn get_highest_scoring_word<'a>(
    word_scores: &'a HashMap<String, f32>,
    words: &'a [String],
) -> String {
    words
        .iter()
        .max_by(|a, b| {
            let score_a = word_scores.get(*a).unwrap_or(&f32::NEG_INFINITY);
            let score_b = word_scores.get(*b).unwrap_or(&f32::NEG_INFINITY);
            score_a
                .partial_cmp(score_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .map(String::as_str)
        .unwrap_or("fail: done with search")
        .to_string()
}

fn filter_words(words: &[String], filters: &DoubleFilters) -> Vec<String> {
    words
        .iter()
        .filter(|word| {
            // Check exclusions first (faster rejection)
            for ch in word.chars() {
                if filters.excludes.contains_letter(ch) {
                    return false;
                }
            }
            // Then check locations
            filters
                .locations
                .iter()
                .all(|loc| expected_letter_filter(word, loc))
        })
        .cloned()
        .collect()
}

fn get_filters_from_guess(target: &str, guess: &str) -> DoubleFilters {
    let mut filters = DoubleFilters::new();
    let target_chars: Vec<char> = target.chars().collect();

    for (iguess, guess_char) in guess.chars().enumerate() {
        if target.contains(guess_char) {
            filters.locations.insert(LetterLocation {
                letter: guess_char,
                index: iguess,
                is_yes: guess_char == target_chars[iguess],
            });
        } else {
            filters.excludes.insert_letter(guess_char);
        }
    }
    filters
}

fn solve_for_targets(targets: &[String], words: &[String], word_scores: &HashMap<String, f32>) {
    // Solve the wordle for one or more target words. Print more output if
    // there's just one user-supplied word.
    let mut itarget = 0;
    for target in targets {
        itarget += 1;
        let mut filtered_words: Vec<String> = words.to_vec(); //words.clone();
        let mut filters = DoubleFilters::new();
        let mut guess = String::new();
        let mut iguess = 0;
        while target != &guess {
            iguess += 1;
            guess = get_highest_scoring_word(word_scores, &filtered_words);
            if targets.len() == 1 {
                println!("{guess} guess {iguess}");
            }

            // Analyze results from last guess and make new filters
            let new_filters = get_filters_from_guess(target, &guess);
            filters.extend(new_filters);
            filtered_words = filter_words(&filtered_words, &filters);

            if filtered_words.is_empty() {
                println!("Can't find '{target}' after {iguess} guesses");
                break;
            }
        }

        if target == &guess && itarget % 1500 == 0 {
            println!("{itarget}: Found \"{target}\" on guess {guess}");
        }
    }
}

fn format_sorted(words: &[String], scores: &HashMap<String, f32>) -> String {
    // Make a formatted string of sorted words/scores
    let mut sorted_word_scores: Vec<(&String, &f32)> = words
        .iter()
        .map(|w| (w, scores.get(w).unwrap_or(&0.0)))
        .collect();
    sorted_word_scores.sort_by(|a, b| a.1.partial_cmp(b.1).unwrap_or(std::cmp::Ordering::Equal));
    sorted_word_scores
        .iter()
        .map(|(w, s)| format!("{w}: {s:.3}"))
        .collect::<Vec<String>>()
        .join("\n")
}

fn main() -> std::io::Result<()> {
    // Read in arguments and solve wordle

    let (filters, target, show_all, solve_all) = read_args();
    let filename = "/usr/share/dict/american-english";
    let words = get_words(filename);
    let scores = score_words(&words);

    // If requested, solve for a user-supplied target (!target.is_empty()) or
    // all words (solve_all) and exit.
    if !target.is_empty() || solve_all {
        let targets = if solve_all { words.clone() } else { vec![target] };
        solve_for_targets(&targets, &words, &scores);
        return Ok(());
    }

    // Otherwise, give the best guess word for today's wordle:
    // either with no filters provided (just return the best scoring word)
    // or find the best word for the given filters
    let message: String;
    if filters.is_empty() {
        message = if show_all { format_sorted(&words, &scores) } else { get_highest_scoring_word(&scores, &words) };
    } else {
        let filtered_words = filter_words(&words, &filters);
        if filtered_words.is_empty() {
            message = format!(
                "No matches found with {:?} {:?}",
                filters.locations, filters.excludes
            );
        } else {
            let guess = get_highest_scoring_word(&scores, &filtered_words);
            message = if show_all { format_sorted(&filtered_words, &scores) } else { guess.clone() }
        }
    }
    println!("{message}");
    Ok(())
}
