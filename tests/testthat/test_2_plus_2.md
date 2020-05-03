test\_2\_plus\_2.R
================
jek
2020-05-04

``` r
library(testthat)
context("Test 2 + 2.")
```

``` r
test_that("2 + 2 = 4", {
  expect_equivalent(
    2 + 2,
    4
  )
})
```
