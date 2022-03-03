#' @title seepr_dhr
#' @description The central class of the dhr model.
#'
#' @param x The result of a call to \code{\link{dhr()}}
#'
#' @details The class is a subclass of 'dlmFiltered' from the package "dlm"
#' with special methods implemented for this particular usecase.
#'
#' @export

new_seepr_dhr <- function(x,
                          s){

  stopifnot(inherits(x,"dlmFiltered"))

  x <-
  structure(x,
            class = c("seepr_dhr",class(x)),
            s = s)
  x
}

