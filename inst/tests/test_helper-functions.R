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

