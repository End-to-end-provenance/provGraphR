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

test_that ("simple", {
    test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
    graph <- create.graph(test.data.file)
    expect_equal (get.creator (graph, "d1"), "p2")
    expect_null (get.creator (graph, "d10"))
    expect_null (get.creator (graph, "p1"))
    expect_null (get.creator (graph, "d6"))
})

## Loading test data
test_that ("larger test case", {
  test.data.file <- system.file("testdata", "basic.json", package = "provGraphR")
  adj.graph <- create.graph(test.data.file)
  expect_setequal (get.lineage (adj.graph, "d24"), 
    c("d18", "p19", "d19", "p21", "d20", "p23", "d22", "p24", "d23", "p25", "d24"))
  expect_setequal (get.lineage (adj.graph, "d3", forward=TRUE), 
    c("d3", "p6", "d5", "p7", "d6", "p14", "d13"))
})

