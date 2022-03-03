#' @title amplitude
#'
#' @description Calculates the common amplitude based
#' on vectors of cosine and sine amplitudes.
#'
#' @inheritParams phase
#' @export



amplitude <- function(x){
  UseMethod("amplitude")
}

#' @exportS3Method
amplitude.seepr_dhr <- function(x){

  a_cos <- dhr_amp_cos(x)
  b_sin <- dhr_amp_sin(x)

  amplitude_out <-amplitude_from_amp(a_cos,
                             b_sin)
  amplitude_out

}



####### HELPERS ######## -------------------------
amplitude_from_amps <- function(a_cos,
                                b_sin){
  sqrt(a_cos^2 + b_sin^2)
}
