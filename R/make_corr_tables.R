#' Make correlation tables
#'
#' @description
#' Takes raw data and creates a correlation table and a correlation p value table. Outputs these tables to csv files.
#'
#' @param input_data data.frame. The raw data.
#' @param path_name character. Where to put the correlation tables output csv files.
#' @param data_name character. What to name the correlation tables output csv files.
#'
#' @return corrs psych
#' @export
#' @examples
#' raw_data <- as.data.frame(readr::read_csv(
#'           system.file("extdata", "raw_data_for_correlation_calculation.csv", package = "nicertables")))
#' make_corr_tables(input_data = raw_data)
#' make_corr_tables(input_data = raw_data, path_name = "~/", data_name = "test_data")$r
#'
make_corr_tables <- function (input_data, path_name = "~/", data_name = "data"){
	# data_name sets a string to use for naming the output files

	# Ensure correct input format
	stopifnot(is.data.frame(input_data))
	stopifnot(is.character(path_name))
	stopifnot(is.character(data_name))

	# Make the correlations (corrs variable type = psych)
	corrs <- psych::corr.test(input_data)

	# Write the correlation table and correlation significant table to files
	readr::write_csv(as.data.frame(corrs$r), stringr::str_c(path_name, "corrs_mat_", data_name, ".csv"))
	readr::write_csv(as.data.frame(corrs$p), stringr::str_c(path_name, "corrs_sig_mat_", data_name, ".csv"))

	# Return the correlations variable in case I want to use it for something else
	return (corrs)
}
