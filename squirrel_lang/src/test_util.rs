use std::{
    fs,
    path::{Path, PathBuf},
};

use serde::{de::DeserializeOwned, Serialize};

pub fn exchange_data<'a, T: Serialize + DeserializeOwned>(
    test_type: &str,
    test_file: &str,
    actual_data: &T,
) -> T {
    let actual_path: PathBuf = [
        "../target/tmp",
        test_type,
        format!("{test_file}-actual.json").as_str(),
    ]
    .iter()
    .collect();
    let actual_str = serde_json::to_string_pretty(actual_data).expect(
        format!(
            "Failed to serialize actual data to {}",
            actual_path.display()
        )
        .as_str(),
    );

    let expect_path: PathBuf = [
        "../resources",
        test_type,
        format!("{test_file}-expect.json").as_str(),
    ]
    .iter()
    .collect();
    let expect_str = exchange_internal(test_type, test_file, &actual_str, &actual_path, &expect_path);
    let expect_data: T = serde_json::from_str(&expect_str).expect(
        format!(
            "Failed to parse expected data from {}",
            expect_path.display()
        )
        .as_str(),
    );

    expect_data
}

pub fn exchange_str<'a>(
    test_type: &str,
    test_file: &str,
    actual_str: &str,
) -> String {
    let actual_path: PathBuf = [
        "../target/tmp",
        test_type,
        format!("{test_file}-actual.txt").as_str(),
    ]
    .iter()
    .collect();

    let expect_path: PathBuf = [
        "../resources",
        test_type,
        format!("{test_file}-expect.txt").as_str(),
    ]
    .iter()
    .collect();

    exchange_internal(test_type, test_file, actual_str, &actual_path, &expect_path)

}

fn exchange_internal<'a>(
    test_type: &str,
    test_file: &str,
    actual_str: &str,
    actual_path: &Path,
    expect_path: &Path,
) -> String {
    fs::create_dir_all(actual_path.parent().unwrap())
        .expect(format!("Failed to create directory {}", actual_path.display()).as_str());
    fs::write(&actual_path, actual_str)
        .expect(format!("Failed to write to {}", actual_path.display()).as_str());

    let expect_str = match fs::read_to_string(&expect_path) {
        other => other.expect(
            format!(
                "Failed to read expected data from {}",
                expect_path.display()
            )
            .as_str(),
        ),
    };

    expect_str
}