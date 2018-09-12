library(provGraphR)
library(provParseR)
library(testthat)

## Loading test data
test.data.file <- system.file("testdata", "prov.json", package = "provGraphR")
prov.parse(test.data.file)

create.graph()
expect_setequal (get.spine ("d33"), 
    c("d27", "p37", "d28", "p38", "d29", "p40", "d31", "p41", "d32", "p42", "d33"))
expect_setequal (get.spine ("d11", forward=TRUE), 
    c("d11", "p14", "p17", "d14", "p20", "d17", "p22"))
