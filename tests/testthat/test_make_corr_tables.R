context("Making correlation tables")

test_that('invalid data input and output format is detected', {

	b1_0 <- c(1, 2, 3)
	b1_1 <- matrix(6, 3, 3)
	b1_2 <- "string"
	g1 <- data.frame(1:5, 6:10, 11:15)

	b2_0 <- 1
	b2_1 <- matrix(2,2,2)
	g2 <- "some_string"

	expect_error(make_corr_tables(b1_0, g2, g2))
	expect_error(make_corr_tables(b1_1, g2, g2))
	expect_error(make_corr_tables(b1_2, g2, g2))
	expect_error(make_corr_tables(g1, b2_0, g2))
	expect_error(make_corr_tables(g1, b2_1, g2))
	expect_that(make_corr_tables(g1, g2, g2)$r, is.matrix)
})

