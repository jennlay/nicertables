context("Making regression tables")

test_that('invalid data input and output format is detected', {

	b1_1 <- matrix(6, 3, 3)
	b1_2 <- "string"
	g1 <- readr::read_csv("messy_mplus_regression_output_1.txt")

	# Just a sampling of tests that could be made along these lines...
	expect_that(make_reg_tables(g1), is.data.frame)
	expect_error(make_reg_tables(b1_1))
	expect_error(make_reg_tables(b1_2))
	expect_error(make_reg_tables(g1, table_style = "buh"))
	expect_error(make_reg_tables(g1, table_style = "simplemultiline"))
})

