use regex::Regex;
use std::env;
use std::env::current_dir;
use std::env::var_os;
use std::ffi::OsString;
use std::os::unix::process::CommandExt;
use std::process::Command;

fn main() {
    let address = env::args_os().nth(1).unwrap();
    let config = Config::from_env();
    let error = parse_resource_identifier(address.to_str().unwrap())
        .do_the_thing(config)
        .exec();
    panic!("{error}");
}

macro_rules! vec_of_strings {
    ($($x:expr),* $(,)?) => (vec![$(String::from($x)),*]);
}

#[derive(Debug, PartialEq)]
enum Editor {
    Plumb,
    EmacsClient,
    Vis,
    VsCodium,
    Nine(Box<Editor>),
    Unknown(OsString),
}

impl Editor {
    fn command(&self) -> Command {
        match self {
            Editor::Plumb => Command::new("plumb"),
            Editor::EmacsClient => Command::new("emacsclient"),
            Editor::Vis => Command::new("vis"),
            Editor::VsCodium => Command::new("codium"),
            Editor::Nine(_) => Command::new("9"),
            Editor::Unknown(unknown_cmd) => Command::new(unknown_cmd),
        }
    }

    fn open_file_args(&self, cmd: &mut Command, file_address: FilePathAddress) {
        match self {
            Editor::Plumb => match file_address {
                FilePathAddress::NoAddress { path } => {
                    cmd.args(["-d", "edit"]);
                    cmd.arg(current_dir().unwrap().join(path));
                }
                FilePathAddress::Line { path, line } => {
                    cmd.args(vec_of_strings![
                        "-d",
                        "edit",
                        "-a",
                        format!("addr={line}"),
                        format!("{}/{}", current_dir().unwrap().to_str().unwrap(), path)
                    ]);
                }
                FilePathAddress::LineColumn {
                    path,
                    line,
                    column: 0,
                } => {
                    cmd.args(vec_of_strings![
                        "-d",
                        "edit",
                        "-a",
                        format!("addr={line}-#0"),
                        format!("{}/{}", current_dir().unwrap().to_str().unwrap(), path)
                    ]);
                }
                FilePathAddress::LineColumn { path, line, column } => {
                    cmd.args(vec_of_strings![
                        "-d",
                        "edit",
                        "-a",
                        format!("addr={line}-#0+#{}", column - 1),
                        format!("{}/{}", current_dir().unwrap().to_str().unwrap(), path)
                    ]);
                }
                FilePathAddress::Range {
                    path,
                    from_line,
                    from_column,
                    to_line,
                    to_column,
                } => {
                    cmd.args(vec_of_strings![
                        "-d",
                        "edit",
                        "-a",
                        format!(
                            "addr={from_line}-#0+#{from_column},{to_line}-#0+#{to_column}",
                            from_column = from_column - 1,
                        ),
                        format!("{}/{}", current_dir().unwrap().to_str().unwrap(), path)
                    ]);
                }
            },
            Editor::EmacsClient => match file_address {
                FilePathAddress::NoAddress { path } => {
                    cmd.args(vec_of_strings!["-n", "-a", "''"]);
                    cmd.arg(path);
                }
                FilePathAddress::Line { path, line } => {
                    cmd.args(vec_of_strings!["-n", "-a", "''", format!("+{line}"), path]);
                }
                FilePathAddress::LineColumn { path, line, column } => {
                    cmd.args(vec_of_strings![
                        "-n",
                        "-a",
                        "''",
                        format!("+{line}:{column}"),
                        path
                    ]);
                }
                FilePathAddress::Range {
                    path,
                    from_line: line,
                    from_column: column,
                    ..
                } => {
                    cmd.args(vec_of_strings![
                        "-n",
                        "-a",
                        "''",
                        format!("+{line}:{column}"),
                        path
                    ]);
                }
            },
            Editor::Vis => match file_address {
                FilePathAddress::NoAddress { path } => {
                    cmd.arg(path);
                }
                FilePathAddress::Line { path, line } => {
                    cmd.args(vec![format!("+{line}-#0"), path]);
                }
                FilePathAddress::LineColumn {
                    path,
                    line,
                    column: 0,
                } => {
                    cmd.args(vec![format!("+{line}-#0"), path]);
                }
                FilePathAddress::LineColumn { path, line, column } => {
                    cmd.args(vec![format!("+{line}-#0+#{}", column - 1,), path]);
                }
                FilePathAddress::Range {
                    path,
                    from_line,
                    from_column,
                    to_line,
                    to_column,
                } => {
                    cmd.args([
                        format!(
                            "+{from_line}-#0+#{from_column},{to_line}-#0+#{to_column}",
                            from_line = from_line,
                            from_column = from_column - 1,
                            to_line = to_line,
                            to_column = to_column
                        ),
                        path,
                    ]);
                }
            },
            Editor::VsCodium => match file_address {
                FilePathAddress::NoAddress { path } => {
                    cmd.arg("-g");
                    cmd.arg(path);
                }
                FilePathAddress::Line { path, line } => {
                    cmd.args(vec_of_strings!["-g", format!("{path}:{line}")]);
                }
                FilePathAddress::LineColumn { path, line, column } => {
                    cmd.args(vec_of_strings!["-g", format!("{path}:{line}:{column}")]);
                }
                FilePathAddress::Range {
                    path,
                    from_line,
                    from_column,
                    ..
                } => {
                    cmd.args(vec_of_strings![
                        "-g",
                        format!("{path}:{from_line}:{from_column}")
                    ]);
                }
            },
            Editor::Nine(editor) => {
                cmd.arg(editor.command().get_program());
                editor.open_file_args(cmd, file_address);
            }
            Editor::Unknown(_) => match file_address {
                FilePathAddress::NoAddress { path } => {
                    cmd.arg(path);
                }
                FilePathAddress::Line { path, .. } => {
                    cmd.arg(path);
                }
                FilePathAddress::LineColumn { path, .. } => {
                    cmd.arg(path);
                }
                FilePathAddress::Range { path, .. } => {
                    cmd.arg(path);
                }
            },
        }
    }
}

#[derive(Debug, PartialEq)]
enum ResourceIdentifier {
    File(FilePathAddress),
    ManPage { page: String, section: String },
    GitCommit(String),
    Url(String),
}

#[derive(Debug, PartialEq)]
enum FilePathAddress {
    NoAddress {
        path: OsString,
    },
    Line {
        path: String,
        line: u32,
    },
    LineColumn {
        path: String,
        line: u32,
        column: u32,
    },
    Range {
        path: String,
        from_line: u32,
        from_column: u32,
        to_line: u32,
        to_column: u32,
    },
}

struct Config {
    editor: Editor,
    browser: String,
}

impl Config {
    fn from_env() -> Self {
        let editor = match var_os("EDITOR") {
            Some(editor_command) => {
                let editor_parts = editor_command.to_str().unwrap().split_ascii_whitespace();
                get_editor(editor_parts).unwrap_or_else(|| Editor::Unknown(editor_command))
            }
            None => Editor::Vis,
        };
        let browser = var_os("BROWSER")
            .and_then(|x| x.to_owned().into_string().ok())
            .unwrap_or("firefox".into());
        Config { editor, browser }
    }
}

fn get_editor(mut editor_parts: std::str::SplitAsciiWhitespace<'_>) -> Option<Editor> {
    match editor_parts.next().unwrap() {
        "vis" => Some(Editor::Vis),
        "editinacme" | "sam" | "B" | "E" => Some(Editor::Plumb),
        "emacsclient" => Some(Editor::EmacsClient),
        "codium" => Some(Editor::VsCodium),
        "9" => get_editor(editor_parts).map(|inner| Editor::Nine(Box::new(inner))),
        _ => None,
    }
}

fn parse_resource_identifier(line: &str) -> ResourceIdentifier {
    parse_man_page(line)
        .or_else(|| parse_url(line))
        .or_else(|| parse_git_commit(line))
        .or_else(|| parse_file_path_multi_line_range_address(line).map(ResourceIdentifier::File))
        .or_else(|| parse_file_path_single_line_range_address(line).map(ResourceIdentifier::File))
        .or_else(|| parse_file_path_address(line).map(ResourceIdentifier::File))
        .unwrap_or_else(|| {
            ResourceIdentifier::File(FilePathAddress::NoAddress {
                path: OsString::from(line),
            })
        })
}

fn parse_file_path_address(line: &str) -> Option<FilePathAddress> {
    let re = Regex::new(r"(?<path>[^:]+)(:(?<line>[0-9]*)(:(?<column>[0-9]*))?)?:?").unwrap();
    let captures = re.captures(line)?;
    match (
        captures.name("path").map(|x| x.as_str()),
        captures
            .name("line")
            .and_then(|x| x.as_str().parse::<_>().ok()),
        captures
            .name("column")
            .and_then(|x| x.as_str().parse::<_>().ok()),
    ) {
        (Some(path), None, None) => Some(FilePathAddress::NoAddress {
            path: OsString::from(path),
        }),
        (Some(path), Some(line), None) => Some(FilePathAddress::Line {
            path: String::from(path),
            line,
        }),
        (Some(path), Some(line), Some(column)) => Some(FilePathAddress::LineColumn {
            path: String::from(path),
            line,
            column,
        }),

        _ => None,
    }
}

fn parse_file_path_single_line_range_address(line: &str) -> Option<FilePathAddress> {
    let re =
        Regex::new(r"(?<path>[^:]+):(?<line>[0-9]+):(?<from_column>[0-9]+)-(?<to_column>[0-9]+):?")
            .unwrap();
    let captures = re.captures(line)?;
    let path = captures.name("path").map(|x| x.as_str())?.to_string();
    let line = captures
        .name("line")
        .and_then(|x| x.as_str().parse::<_>().ok())?;
    let from_column = captures
        .name("from_column")
        .and_then(|x| x.as_str().parse::<_>().ok())?;
    let to_column = captures
        .name("to_column")
        .and_then(|x| x.as_str().parse::<_>().ok())?;

    Some(FilePathAddress::Range {
        path,
        from_line: line,
        from_column,
        to_line: line,
        to_column,
    })
}

fn parse_file_path_multi_line_range_address(line: &str) -> Option<FilePathAddress> {
    let re = Regex::new(r"(?<path>[^:]+):\((?<from_line>[0-9]+),(?<from_column>[0-9]+)\)-\((?<to_line>[0-9]+),(?<to_column>[0-9]+)\):?").unwrap();
    let captures = re.captures(line)?;
    let path = captures.name("path").map(|x| x.as_str())?.to_string();
    let from_line = captures
        .name("from_line")
        .and_then(|x| x.as_str().parse::<_>().ok())?;
    let from_column = captures
        .name("from_column")
        .and_then(|x| x.as_str().parse::<_>().ok())?;
    let to_line = captures
        .name("to_line")
        .and_then(|x| x.as_str().parse::<_>().ok())?;
    let to_column = captures
        .name("to_column")
        .and_then(|x| x.as_str().parse::<_>().ok())?;

    Some(FilePathAddress::Range {
        path,
        from_line,
        from_column,
        to_line,
        to_column,
    })
}

fn parse_man_page(line: &str) -> Option<ResourceIdentifier> {
    let re = Regex::new(r"^(?<page>.+)\((?<section>[0-9]+)\)$").unwrap();
    let captures = re.captures(line)?;
    let page = captures.name("page").map(|x| x.as_str())?.to_string();
    let section = captures.name("section").map(|x| x.as_str())?.to_string();

    Some(ResourceIdentifier::ManPage { page, section })
}

fn parse_url(line: &str) -> Option<ResourceIdentifier> {
    Regex::new(r"(https?|file)://.+$")
        .unwrap()
        .is_match(line)
        .then(|| ResourceIdentifier::Url(String::from(line)))
}

fn parse_git_commit(line: &str) -> Option<ResourceIdentifier> {
    Regex::new(r"^[a-f0-9]{7}$|^[a-f0-9]{40}$")
        .unwrap()
        .is_match(line)
        .then(|| ResourceIdentifier::GitCommit(String::from(line)))
}

impl ResourceIdentifier {
    fn do_the_thing(self, config: Config) -> Command {
        match self {
            ResourceIdentifier::File(file_path_address) => {
                let mut command = config.editor.command();
                config
                    .editor
                    .open_file_args(&mut command, file_path_address);
                command
            }
            ResourceIdentifier::ManPage { page, section } => {
                let mut command = Command::new("man");
                command.args([section, page]);
                command
            }
            ResourceIdentifier::GitCommit(rev) => {
                let mut command = Command::new("git");
                command.args(["show".into(), rev]);
                command
            }
            ResourceIdentifier::Url(url) => {
                let mut command = Command::new(config.browser);
                command.arg(url);
                command
            }
        }
    }
}

#[cfg(test)]
mod tests {

    mod parse_resource_identifier {

        use super::super::*;

        #[test]
        fn matches_simple_filenames() {
            assert_eq!(
                parse_resource_identifier("README.md"),
                ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md")
                })
            )
        }

        #[test]
        fn matches_filenames_with_trailing_colon() {
            assert_eq!(
                parse_resource_identifier("README.md:"),
                ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md")
                })
            )
        }

        #[test]
        fn matches_filenames_with_line() {
            assert_eq!(
                parse_resource_identifier("README.md:4"),
                ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 4,
                })
            )
        }

        #[test]
        fn matches_filenames_with_line_and_trailing_colon() {
            assert_eq!(
                parse_resource_identifier("README.md:4:"),
                ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 4,
                })
            )
        }
        #[test]
        fn matches_filenames_with_line_and_trailing_word() {
            assert_eq!(
                parse_resource_identifier("README.md:4:match"),
                ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 4,
                })
            );
        }

        #[test]
        fn matches_simple_filenames_with_line_and_column() {
            assert_eq!(
                parse_resource_identifier("README.md:4:5"),
                ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 4,
                    column: 5,
                })
            )
        }

        #[test]
        fn matches_filenames_with_line_column_and_trailing_colon() {
            assert_eq!(
                parse_resource_identifier("README.md:4:5:"),
                ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 4,
                    column: 5,
                })
            )
        }

        #[test]
        fn matches_hlint_multi_line_output() {
            assert_eq!(
                parse_resource_identifier("README.md:(30,5)-(31,62):"),
                ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 30,
                    from_column: 5,
                    to_line: 31,
                    to_column: 62,
                })
            )
        }

        #[test]
        fn matches_hlint_single_line_output() {
            assert_eq!(
                parse_resource_identifier("README.md:42:117-119:"),
                ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 42,
                    from_column: 117,
                    to_line: 42,
                    to_column: 119,
                })
            )
        }

        #[test]
        fn matches_man_pages() {
            assert_eq!(
                parse_resource_identifier("ls(1)"),
                ResourceIdentifier::ManPage {
                    page: String::from("ls"),
                    section: String::from("1"),
                }
            )
        }

        #[test]
        fn matches_git_short_commit_hashes() {
            assert_eq!(
                parse_resource_identifier("7437dd8"),
                ResourceIdentifier::GitCommit(String::from("7437dd8"))
            )
        }

        #[test]
        fn matches_git_long_commit_hashes() {
            assert_eq!(
                parse_resource_identifier("7437dd88ba8ffb7648ab1bb32fe1465851f2804f"),
                ResourceIdentifier::GitCommit(String::from(
                    "7437dd88ba8ffb7648ab1bb32fe1465851f2804f"
                ))
            )
        }

        #[test]
        fn not_matches_filenames_with_timestamps_as_git_hashes() {
            assert_eq!(
                parse_resource_identifier("20210113T005730.txt"),
                ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("20210113T005730.txt")
                })
            )
        }

        #[test]
        fn matches_http_url() {
            assert_eq!(
                parse_resource_identifier("http://www.rust-lang.org"),
                ResourceIdentifier::Url(String::from("http://www.rust-lang.org"))
            )
        }

        #[test]
        fn matches_https_url() {
            assert_eq!(
                parse_resource_identifier("https://www.rust-lang.org"),
                ResourceIdentifier::Url(String::from("https://www.rust-lang.org"))
            )
        }
        #[test]
        fn matches_file_url() {
            assert_eq!(
                parse_resource_identifier("file:///foo.bar"),
                ResourceIdentifier::Url(String::from("file:///foo.bar"))
            )
        }
    }

    mod do_the_thing {
        use crate::ResourceIdentifier;

        mod unknown_editor {
            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::Unknown(OsString::from("some_editor")),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "some_editor");
                assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["README.md"]);
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::Unknown(OsString::from("some_editor")),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 1,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "some_editor");
                assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["README.md"]);
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::Unknown(OsString::from("some_editor")),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 1,
                    column: 2,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "some_editor");
                assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["README.md"]);
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::Unknown(OsString::from("some_editor")),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "some_editor");
                assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["README.md"]);
            }
        }

        mod vis {
            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::Vis,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "vis");
                assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["README.md"]);
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::Vis,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 5,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "vis");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["+5-#0", "README.md"]
                );
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::Vis,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 7,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "vis");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["+5-#0+#6", "README.md"]
                );
            }

            #[test]
            fn file_path_line_column_zero_address() {
                let config = Config {
                    editor: Editor::Vis,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 0,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "vis");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["+5-#0", "README.md"]
                );
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::Vis,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "vis");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["+5-#0+#6,6-#0+#10", "README.md"]
                );
            }
        }

        mod plumb {
            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::Plumb,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "plumb");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "-d",
                        "edit",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::Plumb,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 5,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "plumb");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "-d",
                        "edit",
                        "-a",
                        "addr=5",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::Plumb,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 7,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "plumb");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "-d",
                        "edit",
                        "-a",
                        "addr=5-#0+#6",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_column_zero_address() {
                let config = Config {
                    editor: Editor::Plumb,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 0,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "plumb");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "-d",
                        "edit",
                        "-a",
                        "addr=5-#0",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::Plumb,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "plumb");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "-d",
                        "edit",
                        "-a",
                        "addr=5-#0+#6,6-#0+#10",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }
        }

        mod nine {
            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::Nine(Box::new(Editor::Plumb)),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "9");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "plumb",
                        "-d",
                        "edit",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::Nine(Box::new(Editor::Plumb)),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 5,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "9");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "plumb",
                        "-d",
                        "edit",
                        "-a",
                        "addr=5",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::Nine(Box::new(Editor::Plumb)),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 7,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "9");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "plumb",
                        "-d",
                        "edit",
                        "-a",
                        "addr=5-#0+#6",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::Nine(Box::new(Editor::Plumb)),
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "9");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec![
                        "plumb",
                        "-d",
                        "edit",
                        "-a",
                        "addr=5-#0+#6,6-#0+#10",
                        format!("{}/README.md", current_dir().unwrap().to_str().unwrap()).as_str()
                    ]
                );
            }
        }

        mod emacsclient {

            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::EmacsClient,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "emacsclient");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-n", "-a", "''", "README.md"]
                );
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::EmacsClient,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 5,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "emacsclient");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-n", "-a", "''", "+5", "README.md"]
                );
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::EmacsClient,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 7,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "emacsclient");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-n", "-a", "''", "+5:7", "README.md"]
                );
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::EmacsClient,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "emacsclient");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-n", "-a", "''", "+5:7", "README.md"]
                );
            }
        }

        mod vscodium {
            use super::super::super::*;

            #[test]
            fn file_path_no_address() {
                let config = Config {
                    editor: Editor::VsCodium,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::NoAddress {
                    path: OsString::from("README.md"),
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "codium");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-g", "README.md"]
                );
            }

            #[test]
            fn file_path_line_address() {
                let config = Config {
                    editor: Editor::VsCodium,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Line {
                    path: String::from("README.md"),
                    line: 5,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "codium");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-g", "README.md:5"]
                );
            }

            #[test]
            fn file_path_line_column_address() {
                let config = Config {
                    editor: Editor::VsCodium,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::LineColumn {
                    path: String::from("README.md"),
                    line: 5,
                    column: 7,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "codium");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-g", "README.md:5:7"]
                );
            }

            #[test]
            fn file_path_line_range_address() {
                let config = Config {
                    editor: Editor::VsCodium,
                    browser: String::from("?"),
                };
                let address = ResourceIdentifier::File(FilePathAddress::Range {
                    path: String::from("README.md"),
                    from_line: 5,
                    from_column: 7,
                    to_line: 6,
                    to_column: 10,
                });
                let command = address.do_the_thing(config);
                assert_eq!(command.get_program(), "codium");
                assert_eq!(
                    command.get_args().collect::<Vec<_>>(),
                    vec!["-g", "README.md:5:7"]
                );
            }
        }

        use super::super::*;

        #[test]
        fn man_pages() {
            let config = Config {
                editor: Editor::EmacsClient,
                browser: String::from("?"),
            };
            let address = ResourceIdentifier::ManPage {
                page: String::from("foo"),
                section: String::from("7"),
            };
            let command = address.do_the_thing(config);
            assert_eq!(command.get_program(), "man");
            assert_eq!(command.get_args().collect::<Vec<_>>(), vec!["7", "foo"]);
        }

        #[test]
        fn git_commit() {
            let config = Config {
                editor: Editor::EmacsClient,
                browser: String::from("?"),
            };
            let address = ResourceIdentifier::GitCommit(String::from(
                "7437dd88ba8ffb7648ab1bb32fe1465851f2804f",
            ));
            let command = address.do_the_thing(config);
            assert_eq!(command.get_program(), "git");
            assert_eq!(
                command.get_args().collect::<Vec<_>>(),
                vec!["show", "7437dd88ba8ffb7648ab1bb32fe1465851f2804f"]
            );
        }

        #[test]
        fn url() {
            let config = Config {
                editor: Editor::EmacsClient,
                browser: String::from("chromium"),
            };
            let address = ResourceIdentifier::Url(String::from("https://www.haskell.org/"));
            let command = address.do_the_thing(config);
            assert_eq!(command.get_program(), "chromium");
            assert_eq!(
                command.get_args().collect::<Vec<_>>(),
                vec!["https://www.haskell.org/"]
            );
        }
    }
}
