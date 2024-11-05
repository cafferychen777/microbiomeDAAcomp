test_that("import_data validates input correctly", {
    expect_error(import_data("nonexistent.file"), "File does not exist")
    expect_error(import_data("test.csv", format = "unknown"), "should be one of")
})

test_that("import_data handles CSV format correctly", {
    # Create a temporary CSV file for testing
    test_data <- data.frame(
        Sample1 = c(10, 20, 30),
        Sample2 = c(15, 25, 35),
        Kingdom = c("Bacteria", "Bacteria", "Bacteria"),
        Phylum = c("Firmicutes", "Bacteroidetes", "Proteobacteria"),
        row.names = c("OTU1", "OTU2", "OTU3")
    )
    
    temp_file <- tempfile(fileext = ".csv")
    write.csv(test_data, temp_file)
    
    # Test import
    result <- import_data(temp_file, 
                         format = "csv",
                         taxonomy_cols = c("Kingdom", "Phylum"))
    
    # Cleanup
    unlink(temp_file)
    
    # Verify results
    expect_s4_class(result, "phyloseq")
    expect_equal(nrow(phyloseq::otu_table(result)), 3)
    expect_equal(ncol(phyloseq::otu_table(result)), 2)
    expect_equal(nrow(phyloseq::tax_table(result)), 3)
    expect_equal(ncol(phyloseq::tax_table(result)), 2)
}) 