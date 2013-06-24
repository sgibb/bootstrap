context("hclust-functions")

test_that("as.binary.matrix.hclust", {
  hc <- hclust(dist(c(1:3, 7:10)))
  m <- matrix(c(1L, 1L, 0L, 0L, 0L, 0L, 0L,
                0L, 0L, 0L, 1L, 1L, 0L, 0L,
                0L, 0L, 0L, 0L, 0L, 1L, 1L,
                1L, 1L, 1L, 0L, 0L, 0L, 0L,
                0L, 0L, 0L, 1L, 1L, 1L, 1L,
                1L, 1L, 1L, 1L, 1L, 1L, 1L), nrow=6, byrow=TRUE)
  expect_identical(bootstrap:::as.binary.matrix.hclust(hc), m)
})

