test_that("import_data validates input correctly", {
    expect_error(import_data("nonexistent.file"), "File does not exist")
    expect_error(import_data("test.csv", format = "unknown"), "should be one of")
}) 