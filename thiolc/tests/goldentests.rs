use goldentests::{TestConfig, TestResult};

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/thiolc", "../tests", "// ")?;
    config.run_tests()
}
