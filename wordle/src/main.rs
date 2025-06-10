use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum FilterRule {
    Include {
        letter: char,
        index: usize,
        right_spot: bool,
    },
    Exclude {
        letter: char,
    },
}

const WORDLE_LENGTH: usize = 5;

#[inline]
fn matches_filter(word: &str, rule: &FilterRule) -> bool {
    match rule {
        FilterRule::Include {
            letter,
            index,
            right_spot,
        } => word.contains(*letter) && *right_spot == (word.chars().nth(*index) == Some(*letter)),
        FilterRule::Exclude { letter } => !word.contains(*letter),
    }
}

struct Args {
    filters: HashSet<FilterRule>,
    target: String,
    show_all: bool,
    solve_all: bool,
}

fn read_args() -> Args {
    let mut filters = HashSet::<FilterRule>::new();
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
                    filters.extend(parse_filter_arg(&arg));
                }
            }
        }
    }
    Args {
        filters,
        target,
        show_all,
        solve_all,
    }
}

fn parse_position_args(cap: &regex::Captures) -> Vec<(bool, char)> {
    let yes_positions = cap.get(2).map_or("", |m| m.as_str());
    let no_positions = cap.get(3).map_or("", |m| m.as_str());

    let mut positions = Vec::with_capacity(yes_positions.len() + no_positions.len());
    positions.extend(yes_positions.chars().map(|p| (true, p)));
    positions.extend(no_positions.chars().map(|p| (false, p)));
    positions
}

fn parse_exclude_args(letters: &[char]) -> HashSet<FilterRule> {
    letters
        .iter()
        .map(|&letter| FilterRule::Exclude { letter })
        .collect()
}

fn parse_include_args(letter: char, positions: &[(bool, char)]) -> HashSet<FilterRule> {
    // Process "yes" and "no" arguments in a loop
    let mut filters = HashSet::<FilterRule>::new();
    for &(right_spot, position) in positions.iter() {
        let index: usize = position.to_digit(10).unwrap_or(0) as usize - 1;
        assert!(index < WORDLE_LENGTH, "char positions are 1 - 5");

        let wordle_filter = FilterRule::Include {
            letter,
            index,
            right_spot,
        };
        filters.insert(wordle_filter);
    }
    filters
}

fn parse_filter_arg(arg: &str) -> HashSet<FilterRule> {
    let mut filters = HashSet::<FilterRule>::new();

    let user_args_re = Regex::new(r"([a-z]+)\+?(\d+)?-?(\d+)?").unwrap();

    match user_args_re.captures(arg) {
        Some(cap) => {
            let letters: Vec<char> = cap[1].chars().collect();
            let positions = parse_position_args(&cap);
            if positions.is_empty() {
                filters = parse_exclude_args(&letters);
            } else {
                assert!(
                    !letters.is_empty(),
                    "Location letter can only be one character"
                );
                filters = parse_include_args(letters[0], &positions);
            }
        }
        None => {
            eprintln!("Bad specification for '{arg}', can't parse");
        }
    }

    filters
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
        for ch in word.chars() {
            *letter_counts.entry(ch).or_insert(0) += 1;
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

fn calculate_word_scores(words: &[String]) -> HashMap<String, f32> {
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

fn filter_words<'a>(words: &'a [String], rules: &HashSet<FilterRule>) -> Vec<&'a String> {
    words
        .iter()
        .filter(|word| rules.iter().all(|fr| matches_filter(word, fr)))
        .collect()
}

fn highest_scoring_word<'a>(
    word_scores: &HashMap<String, f32>,
    words: &[&'a String],
) -> &'a String {
    words
        .iter()
        .max_by(|a, b| {
            let score_a = word_scores.get(**a).unwrap_or(&f32::NEG_INFINITY);
            let score_b = word_scores.get(**b).unwrap_or(&f32::NEG_INFINITY);
            score_a
                .partial_cmp(score_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .map(|&s| s)
        .expect("No words available")
}

fn get_filters_from_guess(target: &str, guess: &str) -> HashSet<FilterRule> {
    let mut rules = HashSet::<FilterRule>::new();
    let target_chars: Vec<char> = target.chars().collect();

    for (index, letter) in guess.chars().enumerate() {
        let rule = if target.contains(letter) {
            FilterRule::Include {
                letter,
                index,
                right_spot: letter == target_chars[index],
            }
        } else {
            FilterRule::Exclude { letter }
        };

        rules.insert(rule);
    }
    rules
}

fn solve_for_targets(
    word_scores: &HashMap<String, f32>,
    words: &[String],
    targets: &[String],
) -> String {
    // Solve the wordle for one or more target words. Print more output if
    // there's just one user-supplied word.
    let mut itarget = 0;
    let mut count_histogram: HashMap<usize, usize> = HashMap::with_capacity(15);
    let mut message = "".to_string();
    for target in targets {
        itarget += 1;
        let mut filtered_words: Vec<&String> = words.iter().collect();
        let mut filters = HashSet::<FilterRule>::new();
        let mut guess = "";
        let mut iguess = 0;

        while target != guess {
            iguess += 1;
            guess = highest_scoring_word(word_scores, &filtered_words).as_str();

            if targets.len() == 1 {
                message += format!("{guess} guess {iguess}\n").as_str();
            }

            // Analyze results from last guess and make new filters
            filters.extend(get_filters_from_guess(target, guess));
            filtered_words = filter_words(words, &filters);

            if filtered_words.is_empty() {
                message += format!("Can't find '{target}' after {iguess} guesses\n").as_str();
                break;
            }
        }
        *count_histogram.entry(iguess).or_insert(0) += 1;

        if target == guess && itarget % 1500 == 0 {
            message += format!("{itarget}: Found \"{target}\" on guess {iguess}\n").as_str();
        }
    }
    if targets.len() > 1 {
        let mut guess_nums: Vec<usize> = count_histogram.keys().copied().collect();
        let tot_counts = count_histogram.values().sum::<usize>();
        guess_nums.sort_unstable();
        for guess_num in guess_nums {
            message += format!(
                "{:2}: {:4} ({:.2})\n",
                guess_num,
                count_histogram[&guess_num],
                count_histogram[&guess_num] as f32 / tot_counts as f32,
            )
            .as_str();
        }
    }
    message
}

fn sorted_scored_words(scores: &HashMap<String, f32>, words: &[&String]) -> String {
    // Make a formatted string of sorted words/scores
    let mut sorted_word_scores: Vec<(&String, &f32)> = words
        .iter()
        .map(|&w| (w, scores.get(w).unwrap_or(&0.0)))
        .collect();
    sorted_word_scores.sort_by(|a, b| a.1.partial_cmp(b.1).unwrap_or(std::cmp::Ordering::Equal));
    sorted_word_scores
        .iter()
        .map(|(w, s)| format!("{w}: {s:.3}"))
        .collect::<Vec<String>>()
        .join("\n")
}

fn pretty_print_filters(filter_rules: &HashSet<FilterRule>) {
    let mut pretty_rules = filter_rules.iter().collect::<Vec<_>>();
    // Sort: exclude rules before includes, and then alphabetize . "+" include rules sort before "-" include rules.
    pretty_rules.sort_by(|a, b| match (a, b) {
        (
            FilterRule::Exclude { letter: _ },
            FilterRule::Include {
                letter: _,
                index: _,
                right_spot: _,
            },
        ) => std::cmp::Ordering::Less,
        (
            FilterRule::Include {
                letter: _,
                index: _,
                right_spot: _,
            },
            FilterRule::Exclude { letter: _ },
        ) => std::cmp::Ordering::Greater,
        (FilterRule::Exclude { letter: la }, FilterRule::Exclude { letter: lb }) => la.cmp(lb),
        (
            FilterRule::Include {
                letter: la,
                index: ia,
                right_spot: ra,
            },
            FilterRule::Include {
                letter: lb,
                index: ib,
                right_spot: rb,
            },
        ) => {
            let by_right_spot = rb.cmp(ra);
            let by_alpha = la.cmp(lb);
            let by_number = ia.cmp(ib);
            by_right_spot.then(by_alpha).then(by_number)
        }
    });

    let mut seen = HashSet::<String>::new();
    for pr in pretty_rules.iter() {
        match pr {
            FilterRule::Include {
                letter,
                index,
                right_spot,
            } => {
                let letter_string = format!("{letter}");
                let letter_right_spot = format!("{letter}{right_spot}");
                if !seen.contains(&letter_string) {
                    print!(" {letter}");
                }
                if !seen.contains(&letter_right_spot) {
                    print!("{}", if *right_spot { "+" } else { "-" });
                }
                print!("{}", index + 1);
                seen.insert(letter_string);
                seen.insert(letter_right_spot);
            }
            FilterRule::Exclude { letter } => print!("{}", letter),
        };
    }
    println!("");
}

fn main() -> std::io::Result<()> {
    // Read in arguments and solve wordle
    let args = read_args();
    let filename = "/usr/share/dict/american-english";
    let words = get_words(filename);
    let scores = calculate_word_scores(&words);

    // Solve for a list of words (user-supplied or all words), and exit
    let is_list = !args.target.is_empty() || args.solve_all;
    let message = if is_list {
        let targets = if args.solve_all {
            words.clone()
        } else {
            vec![args.target]
        };
        solve_for_targets(&scores, &words, &targets)
    } else {
        // If the user has entered filters, return the matching word(s) and exit:
        let filtered_words = filter_words(&words, &args.filters);
        if filtered_words.is_empty() {
            format!("No matches found with {:?} ", args.filters)
        } else if args.show_all {
            sorted_scored_words(&scores, &filtered_words)
        } else {
            pretty_print_filters(&args.filters);
            highest_scoring_word(&scores, &filtered_words).clone()
        }
    };

    println!("{message}");
    Ok(())
}
