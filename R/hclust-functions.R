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

  for (i in seq.int(from=1L, to=nr, by=1L)) {
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

