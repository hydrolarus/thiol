// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use goldentests::{TestConfig, TestResult};

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/thiolc", "../tests", "// ")?;
    config.run_tests()
}
