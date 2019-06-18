library(provGraphR)
library(provParseR)
library(testthat)

# no procedure nodes
test_that( "no procedure nodes", {
	test.data.file <- system.file("testdata", "empty.json", package = "provGraphR")
	expect_null(create.graph(test.data.file))
})

# no proc-data or data-proc edges
test_that( "no data edges", {
  test.data.file <- system.file("testdata", "no-data-edges.json", package = "provGraphR")
  expect_null(create.graph(test.data.file))
})

## Loading test data
test.data.file <- system.file("testdata", "basic.json", package = "provGraphR")
#prov.parse(test.data.file)

adj.graph <- create.graph(test.data.file)
expect_setequal (get.lineage (adj.graph, "d24"), 
    c("d18", "p19", "d19", "p21", "d20", "p23", "d22", "p24", "d23", "p25", "d24"))
expect_setequal (get.lineage (adj.graph, "d3", forward=TRUE), 
    c("d3", "p6", "d5", "p7", "d6", "p14", "d13"))
