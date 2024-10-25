mod cli;
use cli::print_err;
use nazmc_ast::{FileKey, PkgPoolBuilder};
use nazmc_data_pool::typed_index_collections::{ti_vec, TiVec};
use nazmc_data_pool::{IdPoolBuilder, StrPoolBuilder};
use nazmc_diagnostics::file_info::FileInfo;
use nazmc_lexer::LexerIter;
use nazmc_parser::parse;
use owo_colors::OwoColorize;
use serde::Deserialize;
use serde_yaml::Value;
use std::io;
use std::io::Write;
use std::{
    collections::HashMap,
    fs, panic,
    process::{exit, Command},
};
use thin_vec::ThinVec;

fn collect_paths(paths: Vec<Value>, prefix: &str, collected_paths: &mut Vec<String>) {
    for path in paths {
        match path {
            Value::String(s) => {
                if prefix.is_empty() {
                    collected_paths.push(s.clone());
                } else {
                    collected_paths.push(format!("{}/{}", prefix, s));
                }
            }
            Value::Mapping(mapping) => {
                for (key, value) in mapping {
                    if let Value::String(key_str) = key {
                        let new_prefix = if prefix.is_empty() {
                            key_str.clone()
                        } else {
                            format!("{}/{}", prefix, key_str)
                        };

                        if let Value::Sequence(nested_paths) = value {
                            collect_paths(nested_paths, &new_prefix, collected_paths);
                        }
                    }
                }
            }
            s => todo!("{:?}", s),
        }
    }
}

#[derive(Deserialize)]
struct NazmYaml {
    الاسم: Option<String>,
    الإصدار: Option<String>,
    المسارات: Vec<Value>,
}

fn get_file_paths() -> Vec<String> {
    let nazm_yaml = match fs::read_to_string("nazm.yaml") {
        Ok(content) => content,
        Err(_) => {
            print_err(format!("{}", "لم يتم العثور على ملف nazm.yaml".bold(),));
            exit(1);
        }
    };

    let mut val = serde_yaml::from_str::<Value>(&nazm_yaml).unwrap();

    val.apply_merge().unwrap();

    let Ok(NazmYaml {
        الاسم,
        الإصدار,
        المسارات,
    }) = serde_yaml::from_value(val)
    else {
        print_err(format!(
            "{}",
            "ملف nazm.yaml يجب أن يحتوي على خاصية `المسارات` مع مسار ملف واحد على الأقل".bold(),
        ));
        exit(1)
    };

    let mut collected_paths = Vec::new();

    collect_paths(المسارات, "", &mut collected_paths);

    if collected_paths.is_empty() {
        print_err(format!(
            "{}",
            "ملف nazm.yaml يجب أن يحتوي على خاصية `المسارات` مع مسار ملف واحد على الأقل".bold(),
        ));
        exit(1)
    };

    // println!("الاسم: {}", الاسم.unwrap_or("".to_string()));
    // println!("الإصدار: {}", الإصدار.unwrap_or("".to_string()));
    // println!("المسارات:");
    // for path in &collected_paths {
    //     println!("\t{}", path);
    // }

    collected_paths
}

fn main() {
    // RTL printing
    let output = Command::new("printf").arg(r#""\e[2 k""#).output().unwrap();
    let output = &output.stdout[1..output.stdout.len() - 1];
    io::stdout().write_all(output).unwrap();
    io::stderr().write_all(output).unwrap();

    let files_paths = get_file_paths();
    let mut id_pool = IdPoolBuilder::new();
    let mut str_pool = StrPoolBuilder::new();
    let mut pkgs = PkgPoolBuilder::new();
    let mut files_infos = TiVec::<FileKey, FileInfo>::new();
    let mut ast = nazmc_ast::AST::default();
    let mut diagnostics: Vec<String> = vec![];
    let mut fail_after_parsing = false;
    let mut name_conflicts = nazmc_parser::NameConflicts::new();

    // Register the unit type name to index 0
    // main fn id to index 1
    // the implicit lambda param name to index 2
    id_pool.get_key(&"()".to_string());
    id_pool.get_key(&"البداية".to_string());
    id_pool.get_key(&"س".to_string());

    files_paths
        .into_iter()
        .enumerate()
        .for_each(|(file_idx, file_path)| {
            let mut pkg_path = file_path
                .split_terminator('/')
                .map(|s| id_pool.get_key(&s.to_string()))
                .collect::<ThinVec<_>>();

            pkg_path.pop(); // remove the actual file

            let pkg_key = pkgs.get_key(&pkg_path);

            let path = format!("{file_path}.نظم");
            let Ok(file_content) = fs::read_to_string(&path) else {
                panic::set_hook(Box::new(|_| {}));
                print_err(format!(
                    "{} {}{}",
                    "لا يمكن قراءة الملف".bold(),
                    path.bright_red().bold(),
                    " أو أنه غير موجود".bold()
                ));
                panic!()
            };

            let (tokens, lines, lexer_errors) =
                LexerIter::new(&file_content, &mut id_pool, &mut str_pool).collect_all();

            let file_info = FileInfo { path, lines };

            match parse(
                tokens,
                &file_info,
                &file_content,
                lexer_errors,
                &mut ast,
                &mut name_conflicts,
                pkg_key,
                file_idx.into(),
            ) {
                Ok(_) => {
                    // if pkg_idx >= pkgs_to_files_indexes.len() {
                    //     pkgs_to_files_indexes.resize(pkg_idx + 1, vec![]);
                    // }

                    files_infos.push(file_info);
                    // files_asts.push(nazmc_resolve::ParsedFile { path, lines, ast });
                    // pkgs_to_files_indexes[pkg_idx].push(file_idx);
                }
                Err(d) => {
                    diagnostics.push(d);
                    fail_after_parsing = true;
                }
            }
        });

    if fail_after_parsing {
        let last_idx = diagnostics.len() - 1;
        for (i, d) in diagnostics.iter().enumerate() {
            eprint!("{d}");
            if i != last_idx {
                eprintln!();
            }
        }
        exit(1)
    }

    let pkgs = pkgs.build();

    // let resolver = nazmc_resolve::NameResolver::new(
    //     &id_pool,
    //     &pkgs,
    //     &pkgs_names,
    //     &pkgs_to_files_indexes,
    //     &files_asts,
    // );

    // let mut nrt = resolver.resolve();

    // let nir_builder = NIRBuilder::new(
    //     &id_pool,
    //     pkgs,
    //     pkgs_names,
    //     pkgs_to_files_indexes,
    //     files_asts,
    //     nrt,
    // );
    // let (file_path, file_content) = cli::read_file();

    // nazmc_parser::parse_file(&file_path, &file_content, &mut id_pool, &mut str_pool);

    // for Token { span, val, kind } in tokens {
    //     let color = match kind {
    //         TokenKind::LineComment | TokenKind::DelimitedComment => XtermColors::BrightTurquoise,
    //         TokenKind::Symbol(_) => XtermColors::UserBrightYellow,
    //         TokenKind::Id => XtermColors::LightAnakiwaBlue,
    //         TokenKind::Keyword(_) | TokenKind::Literal(LiteralKind::Bool(_)) => {
    //             XtermColors::FlushOrange
    //         }
    //         TokenKind::Literal(LiteralKind::Str(_) | LiteralKind::Char(_)) => {
    //             XtermColors::PinkSalmon
    //         }
    //         TokenKind::Literal(_) => XtermColors::ChelseaCucumber,
    //         _ => XtermColors::UserWhite,
    //     };

    //     let mut val = format!("{}", val.color(color));

    //     if matches!(kind, TokenKind::Keyword(_) | TokenKind::Symbol(_)) {
    //         val = format!("{}", val.bold());
    //     }

    //     print!("{}", val);
    // }
}
