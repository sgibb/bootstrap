context("helper-functions")

test_that(".clust", {
  m <- matrix(c(1L, 1L, 0L, 0L, 0L, 0L, 0L,
                0L, 0L, 0L, 1L, 1L, 0L, 0L,
                0L, 0L, 0L, 0L, 0L, 1L, 1L,
                1L, 1L, 1L, 0L, 0L, 0L, 0L,
                0L, 0L, 0L, 1L, 1L, 1L, 1L,
                1L, 1L, 1L, 1L, 1L, 1L, 1L), nrow=6, byrow=TRUE)
  expect_identical(bootstrap:::.clust(c(1:3, 7:10),
                     fun=function(x)hclust(dist(x))), m)
})

test_that(".calculateMatches", {
  a <- matrix(c(1L, 1L, 1L, 1L, 1L, 1L,
                1L, 1L, 1L, 1L, 0L, 0L,
                0L, 1L, 1L, 1L, 1L, 0L,
                0L, 0L, 1L, 1L, 1L, 1L), nrow=4, byrow=TRUE)

  b <- matrix(c(1L, 1L, 1L, 1L, 1L, 1L,
                0L, 1L, 1L, 1L, 1L, 0L,
                1L, 1L, 1L, 1L, 0L, 0L,
                1L, 1L, 1L, 1L, 0L, 0L), nrow=4, byrow=TRUE)
  expect_identical(bootstrap:::.calculateMatches(a, b), c(1, 2, 1, 0))
})

