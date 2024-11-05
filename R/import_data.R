#' Import Microbiome Data
#' 
#' @title Import Microbiome Data from Various Formats
#' @description Import microbiome data from different file formats and convert to a standardized phyloseq object
#' 
#' @param data_path Character string specifying the path to the input data file
#' @param format Character string specifying the format of input data ("biom", "phyloseq", or "csv")
#' @param taxonomy_cols Character vector specifying taxonomy column names for CSV format
#' @param sample_meta_data Data frame or path to sample metadata file (optional)
#' @param ... Additional arguments passed to the import function
#'
#' @return A phyloseq object containing the imported microbiome data
#' @export
#'
#' @examples
#' \dontrun{
#' # Import from BIOM format
#' data <- import_data("path/to/data.biom", format = "biom")
#' 
#' # Import from CSV with taxonomy columns
#' data <- import_data("path/to/data.csv", 
#'                    format = "csv",
#'                    taxonomy_cols = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"))
#' }
#' 
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data
#' @importFrom biomformat read_biom
#' @importFrom utils read.csv
import_data <- function(data_path, 
                       format = c("biom", "phyloseq", "csv"),
                       taxonomy_cols = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"),
                       sample_meta_data = NULL,
                       ...) {
    
    format <- match.arg(format)
    
    # Input validation
    if (!file.exists(data_path)) {
        stop("File does not exist: ", data_path)
    }
    
    # Import based on format
    result <- switch(format,
        biom = import_from_biom(data_path),
        phyloseq = import_from_phyloseq(data_path),
        csv = import_from_csv(data_path, taxonomy_cols)
    )
    
    # Add sample metadata if provided
    if (!is.null(sample_meta_data)) {
        if (is.character(sample_meta_data) && file.exists(sample_meta_data)) {
            sample_meta_data <- read.csv(sample_meta_data, row.names = 1, ...)
        }
        if (is.data.frame(sample_meta_data)) {
            sample_data <- phyloseq::sample_data(sample_meta_data)
            result <- phyloseq::merge_phyloseq(result, sample_data)
        }
    }
    
    return(result)
}

# Helper function to import from BIOM format
import_from_biom <- function(file_path) {
    biom_data <- biomformat::read_biom(file_path)
    return(phyloseq::import_biom(biom_data))
}

# Helper function to import from phyloseq format
import_from_phyloseq <- function(file_path) {
    load(file_path)
    if (!exists("physeq") || !inherits(physeq, "phyloseq")) {
        stop("The file does not contain a valid phyloseq object")
    }
    return(physeq)
}

# Helper function to import from CSV format
import_from_csv <- function(file_path, taxonomy_cols) {
    # Read CSV file
    data <- read.csv(file_path, row.names = 1)
    
    # Separate abundance and taxonomy data
    tax_idx <- match(taxonomy_cols, colnames(data))
    if (any(is.na(tax_idx))) {
        missing_cols <- taxonomy_cols[is.na(tax_idx)]
        stop("Missing taxonomy columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Create OTU table
    otu_mat <- as.matrix(data[, -tax_idx])
    otu <- phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
    
    # Create taxonomy table
    tax_mat <- as.matrix(data[, tax_idx])
    colnames(tax_mat) <- taxonomy_cols
    tax <- phyloseq::tax_table(tax_mat)
    
    # Combine into phyloseq object
    return(phyloseq::phyloseq(otu, tax))
} 