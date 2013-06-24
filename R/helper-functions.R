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

.clust <- function(x, fun) {
  hc <- fun(x)
  return(as.binary.matrix.hclust(hc))
}

.calculateMatches <- function(origin, current) {
  ## both 1
  one <- tcrossprod(origin, current)
  ## both 0
  zero <- tcrossprod(1-origin, 1-current)

  ## calc matches
  return(rowSums(one + zero == ncol(origin)))
}

