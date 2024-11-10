use std::{
    env,
    fs::{read_dir, DirEntry},
    iter::once,
    path::{Path, PathBuf},
};

use clap::{arg, Parser};
use exif::{self, DateTime, In, Tag, Value};

#[derive(Parser)]
struct Cli {
    scan_dir: PathBuf,
    target_dir: Option<PathBuf>,
    #[arg(long, short = 'n')]
    dry_run: bool,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    let target_dir = cli
        .target_dir
        .unwrap_or(env::current_dir().map_err(|err| err.to_string())?);

    if cli.dry_run {
        DryRunner::run(&cli.scan_dir, &target_dir)
    } else {
        IORunner::run(&cli.scan_dir, &target_dir)
    }
}

#[derive(Debug)]
struct MonthOfYear {
    year: u16,
    month: u8,
}

impl From<MonthOfYear> for PathBuf {
    fn from(month_of_year: MonthOfYear) -> Self {
        PathBuf::from(month_of_year.year.to_string())
            .join(PathBuf::from(format!("{:02}", month_of_year.month)))
    }
}

#[test]
fn test_path_buf_from_year_of_month() {
    let yom = MonthOfYear {
        year: 2024,
        month: 11,
    };
    assert_eq!(PathBuf::from("2024/11"), PathBuf::from(yom));

    let yom = MonthOfYear {
        year: 2024,
        month: 2,
    };
    assert_eq!(PathBuf::from("2024/02"), PathBuf::from(yom));
}

impl From<DateTime> for MonthOfYear {
    fn from(date_time: DateTime) -> Self {
        Self {
            year: date_time.year,
            month: date_time.month,
        }
    }
}

fn read_month_of_year<P>(path: P) -> Result<MonthOfYear, String>
where
    P: AsRef<Path>,
{
    let file = std::fs::File::open(path).map_err(|err| err.to_string())?;
    let mut bufreader = std::io::BufReader::new(&file);
    let exifreader = exif::Reader::new();
    let exif = exifreader
        .read_from_container(&mut bufreader)
        .map_err(|err| err.to_string())?;

    match exif.get_field(Tag::DateTime, In::PRIMARY) {
        Some(field) => match field.value {
            Value::Ascii(ref vec) if !vec.is_empty() => DateTime::from_ascii(&vec[0])
                .map(MonthOfYear::from)
                .map_err(|err| err.to_string()),
            _ => Err(String::from("no DateTime field present")),
        },
        None => Err(String::from("no DateTime field present")),
    }
}

fn scan_dir(dir: &PathBuf) -> std::io::Result<Box<dyn Iterator<Item = std::io::Result<DirEntry>>>> {
    let read_dirs = read_dir(dir)?;

    let new_iter = read_dirs.flat_map(|entry_result| match entry_result {
        Err(err) => Box::new(once(Err(err))),
        Ok(entry) => match entry.file_type() {
            Err(err) => Box::new(once(Err(err))),
            Ok(file_type) => {
                if file_type.is_dir() {
                    match scan_dir(&entry.path()) {
                        Err(err) => Box::new(once(Err(err))),
                        Ok(iter) => iter,
                    }
                } else {
                    Box::new(once(Ok(entry)))
                }
            }
        },
    });

    Ok(Box::new(new_iter))
}

trait Runner {
    fn create_dir_all<P: AsRef<Path>>(path: P) -> std::io::Result<()>;

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()>;

    fn run(input_dir: &PathBuf, targed_dir: &Path) -> Result<(), String> {
        let scan_results = scan_dir(input_dir)
            .map_err(|err| format!("can't scan {dir:?}: {err}", dir = input_dir,))?;

        for result in scan_results {
            match result {
                Err(err) => eprintln!("{}", err),
                Ok(scan) => {
                    let path = scan.path();
                    match path.parent() {
                        None => eprintln!("can't read parent for {path:?}"),
                        Some(parent) => match Self::create_dir_all(parent) {
                            Err(err) => eprintln!("{err}"),
                            Ok(()) => {
                                let month_of_year = read_month_of_year(&path)?;
                                let to = targed_dir.join(PathBuf::from(month_of_year)).join(
                                    path.file_name().ok_or_else(|| {
                                        format!("unable to read file name from {path:?}")
                                    })?,
                                );
                                match Self::rename(path, to) {
                                    Ok(()) => {}
                                    Err(err) => eprintln!("{err}"),
                                }
                            }
                        },
                    }
                }
            }
        }

        Ok(())
    }
}

struct IORunner {}

impl Runner for IORunner {
    fn create_dir_all<P: AsRef<Path>>(path: P) -> std::io::Result<()> {
        std::fs::create_dir_all(path)
    }

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()> {
        std::fs::rename(from, to)
    }
}

struct DryRunner {}

impl Runner for DryRunner {
    fn create_dir_all<P: AsRef<Path>>(path: P) -> std::io::Result<()> {
        println!("create directory {path:?}", path = path.as_ref());
        Ok(())
    }

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()> {
        println!("{from:?} -> {to:?}", from = from.as_ref(), to = to.as_ref());
        Ok(())
    }
}
