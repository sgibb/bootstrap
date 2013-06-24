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

#' @export
bootstrap <- function(x, fun, n=100L, mc.cores=getOption("mc.cores", 2L)) {
  fun <- match.fun(fun)

  origin <- .clust(x, fun=fun)

  v <- mclapply(seq_len(n), function(y, origin, size, fun, nco) {
    current <- .clust(.resample(x, size=size), fun=fun)
    return(.calculateMatches(origin, current, nco))
  }, mc.cores=mc.cores, origin=origin, size=ncol(x), fun=fun, nco=ncol(origin))

  return(colSums(do.call(rbind, v))/n)
}

