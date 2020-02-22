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

#' bootstrap
#'
#' This function provides bootstrapping for hierarchical clustering
#' (\code{\link{hclust}} objects).
#'
#' @param x \code{matrix}, rows: individuals, columns: observations
#' @param fun function which creates the individual hclust object
#' @param n \code{integer}, number of bootstrap replicates
#' @param mc.cores \code{integer}, number of processes to run in parallel
#'
#' @seealso \code{\link{dist}}, \code{link{hclust}}
#'
#' @return \code{numeric} vector with frequencies of each node
#'
#' @references
#' Felsenstein, Joseph.
#' \emph{Confidence limits on phylogenies: an approach using the bootstrap.}
#' Evolution (1985): 783-791.
#'
#' Efron, Bradley, Elizabeth Halloran, and Susan Holmes.
#' \emph{Bootstrap confidence levels for phylogenetic trees.}
#' Proceedings of the National Academy of Sciences 93.23 (1996): 13429-13429.
#'
#' @examples
#'
#' ## hclust example
#' createHclustObject <- function(x)hclust(dist(x), "ave")
#'
#' ## bootstrap
#' b <- bootstrap(USArrests, fun=createHclustObject, n=100L)
#'
#' ## plot
#' hc <- createHclustObject(USArrests)
#' plot(hc)
#'
#' ## draw bootstrap values to corresponding node
#' bootlabels.hclust(hc, b, col="blue")
#'
#' @rdname bootstrap
#' @importFrom parallel mclapply
#' @export
bootstrap <- function(x, fun, n=1000L, mc.cores=1) {
  fun <- match.fun(fun)

  origin <- .clust(x, fun=fun)

  v <- mclapply(seq_len(n), function(y, origin, size, fun, nco) {
    current <- .clust(.resample(x, size=size), fun=fun)
    return(.calculateMatches(origin, current, nco))
  }, mc.cores=mc.cores, origin=origin, size=ncol(x), fun=fun, nco=ncol(origin))

  return(colSums(do.call(rbind, v))/n)
}

