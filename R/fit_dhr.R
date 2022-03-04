#' @title fit_dhr
#'
#' @description Fits a dynamic harmonic regression model to the data
#' in a valid \code{seepr_dat} object.
#'
#' @param x A valid \code{seepr_dat} object.
#'
#' @inheritParams dhr
#'
#' @export

fit_dhr <- function(x,
                    s){
  UseMethod("fit_dhr")
}

#' @exportS3Method
fit_dhr.seepr_dat <- function(x,
                              s){

  mods <- lapply(2:length(x),
                 function(i, s){
                   dhr(x[,i],s = s)
                 },
                 s = s)

  TREND <- do.call(cbind,
                   lapply(mods,trend))
  AMPLITUDE <- do.call(cbind,
                       lapply(mods,amplitude))
  PHASE <- do.call(cbind,
                   lapply(mods,phase))

  x <- new_seepr_fit(x,
                     s,
                     TREND,
                     AMPLITUDE,
                     PHASE)
  x
}

