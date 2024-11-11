use std::{
    env,
    fs::{read_dir, DirEntry},
    iter::once,
    path::{Path, PathBuf},
};

use chrono::Datelike;
use clap::{arg, Parser};
use nom_exif::{
    EntryValue, Exif, ExifIter, ExifTag, MediaParser, MediaSource, TrackInfo, TrackInfoTag,
};

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
    year: i32,
    month: u32,
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

impl TryFrom<&EntryValue> for MonthOfYear {
    type Error = String;

    fn try_from(value: &EntryValue) -> Result<Self, Self::Error> {
        match value.as_time() {
            Some(date_time) => Ok(Self {
                year: date_time.year(),
                month: date_time.month(),
            }),
            None => Err(format!("Cannot read {value:?} as time")),
        }
    }
}

fn read_month_of_year<P>(path: P) -> Result<MonthOfYear, String>
where
    P: AsRef<Path>,
{
    let mut parser = MediaParser::new();
    let ms = MediaSource::file_path(path.as_ref())
        .map_err(|err| format!("Error opining {path}: {err}", path = path_string(&path)))?;

    if ms.has_exif() {
        // Parse the file as an Exif-compatible file
        let iter: ExifIter = parser
            .parse(ms)
            .map_err(|err| format!("Error reading exif for: {err}"))?;
        let exif: Exif = iter.into();
        exif.get(ExifTag::DateTimeOriginal)
            .ok_or_else(|| String::from("No DateTime field present"))
            .and_then(MonthOfYear::try_from)
    } else if ms.has_track() {
        // Parse the file as a track
        let info: TrackInfo = parser
            .parse(ms)
            .map_err(|err| format!("Error reading tracke info: {err}"))?;
        info.get(TrackInfoTag::CreateDate)
            .ok_or_else(|| String::from("No DateTime field present"))
            .and_then(MonthOfYear::try_from)
    } else {
        Err(format!("Unknown media type"))
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
    type Event;

    fn create_dir_all<P: AsRef<Path>>(path: P) -> std::io::Result<()>;

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()>;

    fn run(input_dir: &PathBuf, targed_dir: &Path) -> Result<(), String> {
        let scan_results = scan_dir(input_dir)
            .map_err(|err| format!("Can't scan {dir}: {err}", dir = path_string(input_dir)))?;

        for result in scan_results {
            match result {
                Err(err) => eprintln!("{}", err),
                Ok(scan) => {
                    let path = scan.path();
                    let month_of_year = read_month_of_year(&path)
                        .map_err(|err| {
                            format!(
                                "Unable to read date from {path}: {err}",
                                path = path_string(&path)
                            )
                        })
                        .map(PathBuf::from)
                        .unwrap_or_else(|err| {
                            eprintln!("{}", err);
                            PathBuf::from("unsorted")
                        });
                    let to = targed_dir
                        .join(month_of_year)
                        .join(path.file_name().ok_or_else(|| {
                            format!(
                                "Unable to read file name from {path}",
                                path = path_string(&path)
                            )
                        })?);
                    if to.exists() {
                        eprintln!("{to:?} already exists");
                    } else {
                        match to.parent() {
                            None => {
                                eprintln!("Can't read parent for {path}", path = path_string(&path))
                            }
                            Some(parent) => match Self::create_dir_all(parent) {
                                Err(err) => eprintln!("{err}"),
                                Ok(()) => match Self::rename(path, to) {
                                    Ok(()) => {}
                                    Err(err) => eprintln!("{err}"),
                                },
                            },
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

struct IORunner {}

impl Runner for IORunner {
    type Event = ();

    fn create_dir_all<P: AsRef<Path>>(path: P) -> std::io::Result<()> {
        std::fs::create_dir_all(path)
    }

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()> {
        std::fs::rename(from, to)
    }
}

enum DryRunEvent {}

struct DryRunner {}

impl Runner for DryRunner {
    type Event = DryRunEvent;

    fn create_dir_all<P: AsRef<Path>>(_path: P) -> std::io::Result<()> {
        // println!("create directory {path:?}", path = path.as_ref());
        Ok(())
    }

    fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> std::io::Result<()> {
        println!(
            "{from} -> {to}",
            from = path_string(from),
            to = path_string(to)
        );
        Ok(())
    }
}

fn path_string<P: AsRef<Path>>(path: P) -> String {
    let os_string: &std::ffi::OsStr = path.as_ref().as_ref();
    <&str>::try_from(os_string)
        .map(String::from)
        .unwrap_or_else(|_| format!("{path:?}", path = path.as_ref()))
}
