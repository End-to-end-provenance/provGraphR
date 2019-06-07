library(provGraphR)
library(provParseR)
library(testthat)

# no procedure nodes
test_that( "no procedure nodes", {
	test.data.file <- system.file("testdata", "empty.json", package = "provGraphR")
	expect_warning(create.graph(test.data.file))
	expect_null(create.graph(test.data.file))
})

## Loading test data
test.data.file <- system.file("testdata", "prov.json", package = "provGraphR")
#prov.parse(test.data.file)

adj.graph <- create.graph(test.data.file)
expect_setequal (get.lineage (adj.graph, "d33"), 
    c("d27", "p37", "d28", "p38", "d29", "p40", "d31", "p41", "d32", "p42", "d33"))
expect_setequal (get.lineage (adj.graph, "d11", forward=TRUE), 
    c("d11", "p14", "p17", "d14", "p20", "d17", "p22"))
