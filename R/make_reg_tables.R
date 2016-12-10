#' Make regression tables
#'
#' @description
#' Takes raw regression analysis output, formated in Mplus style, and makes it into a
#' nicely formatted regression results table. Note: this function is *very* specific to a
#' particular type and format of analysis output, and assumes a certain naming convention
#' for the variables. It would likely need to be modified to be remotely useful to anyone
#' other than the author.
#'
#' @param mplus_lcr_table data.frame
#' @param table_style character. Table style options are: "multiline", "grid", "simple", and "rmarkdown".
#' @param table_split_cells integer. Max cell length (number of characters).
#' @param table_split_table integer. Max row length (number of characters).
#' @param table_alignment character. Table alignment options are: "left", "right", "centre".
#' @param table_alignment_rownames character. Rowname alignment options are: "left", "right", "centre".
#'
#' @return split_lcr_table data.frame
#' @export
#' @examples
#' # Use the function without setting any arguments
#' input_table_1 <- as.data.frame(readr::read_lines(
#'           system.file("extdata", "messy_mplus_regression_output_1.txt", package = "nicertables")))
#' make_reg_tables(mplus_lcr_table = input_table_1)
#' # Use the function with some different arguments (to improve the formatting for this particular data file)
#' input_table_2 <- as.data.frame(readr::read_lines(
#'           system.file("extdata", "messy_mplus_regression_output_2.txt", package = "nicertables")))
#' make_reg_tables(mplus_lcr_table = input_table_2, table_style = "grid", table_split_cells = 20, table_split_table = 80,
#' 								 table_alignment_rownames = "centre")
#'
make_reg_tables <- function (mplus_lcr_table, table_style = "multiline", table_split_cells = 50, table_split_table = 100,
														table_alignment = "left", table_alignment_rownames = "left"){
	# Sets a bunch of argument defaults for table formatting

	# Ensure correct input format
	stopifnot(is.data.frame(mplus_lcr_table))
	stopifnot(is.numeric(table_split_cells))
	stopifnot(is.numeric(table_split_table))
	# Definitely a long-winded way of doing this, but ok for now...
	stopifnot(sum(stringr::str_count(table_style, c("multiline","grid","simple","rmarkdown"))) == 1)
	stopifnot(sum(stringr::str_count(table_alignment, c("left","right","centre","center"))) == 1)
	stopifnot(sum(stringr::str_count(table_alignment, c("left","right","centre","center"))) == 1)

	# Deal with the table column names
	colnames(mplus_lcr_table) <- c("Values")
	split_lcr_table <- tidyr::separate(mplus_lcr_table, "Values",
																		 into = c("Level", "Parameter", "Coef", "SE", "Estimate_SE", "p_value"),
																		 sep = " +", remove = TRUE, extra = "merge")
	split_lcr_table <-  dplyr::select(split_lcr_table, -Estimate_SE)

	# Change the variable names to friendlier names (this covers a variety of variables that might turn up)
	# Note to self: BE CAREFUL - make sure C#1 and C#2 are being interpreted the way I intended (not reversed)
	split_lcr_table <-  dplyr::mutate(split_lcr_table,
					 Level = stringr::str_replace(Level, "Within", "**Within level**"),
					 Level = stringr::str_replace(Level, "Between", "**Between level**"),
					 Parameter = stringr::str_replace(Parameter, "Level", ""),
					 Parameter = stringr::str_replace(Parameter, "C#1", "Log-odds of solitude class 1 ('good') / class 2 ('bad')"),
					 Parameter = stringr::str_replace(Parameter, "C#2", "Log-odds of solitude class 1 ('good') / class 2 ('bad')"),
					 Parameter = stringr::str_replace(Parameter, "Intercepts", "**INTERCEPT**"),
					 Parameter = stringr::str_replace(Parameter, "Residual", "**RESIDUAL VARIANCE**"),
					 Coef = stringr::str_replace(Coef, "ON", ""),
					 Coef = stringr::str_replace(Coef, "Variances", ""),
					 Parameter = stringr::str_replace(Parameter, "IS_EURO", "Ethnicity (0 = non-European, 1 = European)"),
					 Parameter = stringr::str_replace(Parameter, "GENDER", "Gender (0 = male, 1 = female)"),
					 Parameter = stringr::str_replace(Parameter, "ISRELATI", "In relationship (0 = no, 1 = yes)"),
					 Parameter = stringr::str_replace(Parameter, "ISMARRIE", "Marital status (0 = not married, 1 = married)"),
					 Parameter = stringr::str_replace(Parameter, "EDUC", "Education level (1-9)"),
					 Parameter = stringr::str_replace(Parameter, "EXTRA", "Extraversion (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "NEUR", "Neuroticism (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "REFL", "Trait Self-reflection (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "RUMI", "Trait Self-rumination (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "PSSE", "Perceived social self-efficacy (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "SIAS", "Social interaction anxiety (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "LONELI", "Trait loneliness (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "ANIM", "Animal naming task score"),
					 Parameter = stringr::str_replace(Parameter, "TOT_NETW", "Total social network size"),
					 Parameter = stringr::str_replace(Parameter, "NETW_1", "Close social network size (circle 1)"),
					 Parameter = stringr::str_replace(Parameter, "PRESOLC", "Preference for solitude scale score"),
					 Parameter = stringr::str_replace(Parameter, "MACARTH", "MacArthur scale score (1-10)"),
					 Parameter = stringr::str_replace(Parameter, "RFF_RELA", "Position relations with others (1-5)"),
					 Parameter = stringr::str_replace(Parameter, "M_ISALO", "Person-average time alone (proportion of beeps, 0-1)"),
					 Parameter = stringr::str_replace(Parameter, "M_ISSOL", "Person-average time in solitude (proportion of beeps, 0-1)"),
					 Parameter = stringr::str_replace(Parameter, "M_ISSOL2", "Person-average time in solitude (proportion of beeps, 0-1)"),
					 Parameter = stringr::str_replace(Parameter, "M_DESALO", "Person-average desire to be alone (proportion of beeps, 0-1)"),
					 Parameter = stringr::str_replace(Parameter, "M_DESSOL", "Person-average desire for solitude (proportion of beeps, 0-1)"),
					 Parameter = stringr::str_replace(Parameter, "ISALO", "Momentary aloneness (0 = not alone, 1 = alone)"),
					 Parameter = stringr::str_replace(Parameter, "DESALO", "Momentary desire to be alone (0 = no, 1 = yes)"),
					 Parameter = stringr::str_replace(Parameter, "ISSOL2", "Momentary solitude (0 = not solitude, 1 = solitude)"),
					 Parameter = stringr::str_replace(Parameter, "DESSOL", "Momentary desire for solitude (0 = no, 1 = yes)"),
					 Parameter = stringr::str_replace(Parameter, "T22_OUTS", "Currently outside (0 = no, 1 = yes)"),
					 Parameter = stringr::str_replace(Parameter, "T22_HOME", "Currently at home (0 = no, 1 = yes)"),
					 Parameter = stringr::str_replace(Parameter, "AGE", "Age (years)"))

	# Set the formatting options according to the arguments specified - these will apply to all subsequent code
	pander::panderOptions('table.style', table_style)
	pander::panderOptions('table.split.cells', table_split_cells)
	pander::panderOptions('table.split.table', table_split_table)
	pander::panderOptions('table.alignment.default', table_alignment)
	pander::panderOptions('table.alignment.rownames', table_alignment_rownames)

	return (split_lcr_table) # Return the table
}
