## Copyright 2013 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

as.binary.matrix.hclust <- function(x) {
  nr <- as.integer(nrow(x$merge))

  m <- matrix(0L, nrow=nr, ncol=nr+1L)

  for (i in seq_len(nr)) {
    left <- x$merge[i, 1L]

    if (left < 0L) {
      ## negative values correspond to observations
      m[i, -left] <- 1L
    } else {
      ## positive values correspond to childcluster
      m[i, ] <- m[left, ]
    }

    right <- x$merge[i, 2L]

    if (right < 0L) {
      ## negative values correspond to observations
      m[i, -right] <- 1L
    } else {
      ## positive values correspond to childcluster
      m[i, ] <- m[i,] | m[right, ]
    }
  }

  return(m)
}

.text.coord.hclust <- function(x) {
  nr <- as.integer(nrow(x$merge))

  p <- matrix(c(rep(0L, nr), x$height), nrow=nr, ncol=2, byrow=FALSE,
              dimnames=list(c(), c("x", "y")))
  o <- order(x$order)
  tmp <- double(2)

  for (i in seq_len(nr)) {
    left <- x$merge[i, 1L]

    if (left < 0L) {
      ## negative values correspond to observations
      tmp[1L] <- o[-left]
    } else {
      ## positive values correspond to childcluster
      tmp[1L] <- p[left, 1L]
    }

    right <- x$merge[i, 2L]

    if (right < 0L) {
      ## negative values correspond to observations
      tmp[2L] <- o[-right]
    } else {
      ## positive values correspond to childcluster
      tmp[2L] <- p[right, 1L]
    }

    p[i, 1L] <- mean(tmp)
  }

  return(p)
}

bootlabels.hclust <- function(x, bootstrapValues, horiz=FALSE, ...) {
  p <- .text.coord.hclust(x)
  if (horiz) {
    p[, c(2,1)] <- p
  }
  labels <- sprintf("%.2f", bootstrapValues)
  text(p, labels=labels, ...)
}

