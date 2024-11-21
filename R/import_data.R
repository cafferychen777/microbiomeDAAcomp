#' Import Microbiome Data
#' 
#' @title Import Microbiome Data from Various Formats
#' @description Import and standardize microbiome data from different file formats into a phyloseq object.
#' This function supports multiple input formats including BIOM, phyloseq, and CSV files, making it
#' flexible for various data sources and analysis workflows.
#' 
#' @param data_path Character string specifying the path to the input data file
#' @param format Character string specifying the format of input data. Options:
#'        \itemize{
#'          \item "biom": Biological Observation Matrix format
#'          \item "phyloseq": Saved phyloseq object
#'          \item "csv": Comma-separated values file
#'        }
#' @param taxonomy_cols Character vector specifying taxonomy column names for CSV format.
#'        Default: c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
#' @param sample_meta_data Data frame or path to sample metadata file (optional).
#'        If provided as a path, the file should be a CSV with samples as rows
#' @param ... Additional arguments passed to read.csv() when reading metadata
#'
#' @return A phyloseq object containing:
#'         \itemize{
#'           \item OTU table (abundance matrix)
#'           \item Taxonomy table
#'           \item Sample metadata (if provided)
#'         }
#'
#' @details
#' For CSV format, the input file should have:
#' - First column: OTU/ASV IDs
#' - Middle columns: Sample abundance values
#' - Last columns: Taxonomy information matching taxonomy_cols
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import from BIOM format
#' data <- import_data("path/to/data.biom", format = "biom")
#' 
#' # Import from CSV with taxonomy columns and metadata
#' data <- import_data(
#'   data_path = "path/to/data.csv",
#'   format = "csv",
#'   taxonomy_cols = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"),
#'   sample_meta_data = "path/to/metadata.csv"
#' )
#' 
#' # Import saved phyloseq object
#' data <- import_data("path/to/saved_phyloseq.RData", format = "phyloseq")
#' }
#' 
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data merge_phyloseq import_biom
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

#' Import Data from BIOM Format
#'
#' @param file_path Character string specifying the path to the BIOM file
#' @param ... Additional arguments passed to the import function
#'
#' @return A phyloseq object containing the imported data
#'
#' @examples
#' \dontrun{
#' data <- import_from_biom("path/to/file.biom")
#' }
#'
#' @importFrom biomformat read_biom
#' @keywords internal
import_from_biom <- function(file_path, ...) {
    biom_data <- biomformat::read_biom(file_path)
    return(phyloseq::import_biom(biom_data))
}

#' Import Data from Phyloseq Format
#'
#' @param file_path Character string specifying the path to the saved phyloseq object
#'
#' @return A phyloseq object containing the imported data
#'
#' @examples
#' \dontrun{
#' data <- import_from_phyloseq("path/to/saved_phyloseq.RData")
#' }
#'
#' @keywords internal
import_from_phyloseq <- function(file_path) {
    load(file_path)
    if (!exists("physeq") || !inherits(physeq, "phyloseq")) {
        stop("The file does not contain a valid phyloseq object")
    }
    return(physeq)
}

#' Import Data from CSV Format
#'
#' @param file_path Character string specifying the path to the CSV file
#' @param taxonomy_cols Character vector specifying which columns contain taxonomy information
#' @param ... Additional arguments passed to read.csv
#'
#' @return A phyloseq object containing the imported data
#'
#' @examples
#' \dontrun{
#' data <- import_from_csv("path/to/file.csv", 
#'                        taxonomy_cols = c("Kingdom", "Phylum", "Class"))
#' }
#'
#' @importFrom utils read.csv
#' @keywords internal
import_from_csv <- function(file_path, taxonomy_cols = NULL, ...) {
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