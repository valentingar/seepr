#' @title seepr_fit
#' @description The central class for fitted \code{\link{seepr_dat}} objects.
#'
#' @inheritParams fit_dhr
#'
#' @param TREND The trend component of the fitted parameters.
#' @param AMPLITUDE The amplitude of the fitted parameters.
#' @param PHASE The phase of the fitted parameters.
#'
#' @details The order in \code{TREND}, \code{AMPLITUDE} and \code{PHASE}
#' follows that of x.
#'
#' @export

new_seepr_fit <- function(x,
                          s,
                          TREND,
                          AMPLITUDE,
                          PHASE){

  stopifnot(inherits(x, "seepr_dat"))

  x <- structure(x,
                 class = c("seepr_fit", class(x)),
                 s = s,
                 TREND = TREND,
                 AMPLITUDE = AMPLITUDE,
                 PHASE = PHASE)
  x
}


