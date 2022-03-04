#' @title phase
#'
#' @description Calculates the phase of an seepr_dhr regression result. A phase
#' of 0 would correspond to \eqn{sin(t+0)}.
#'
#' @param x a seepr_dhr regression result.
#'
#' @return phi The value phi that fits the form \eqn{sin(t+phi)}.
#'
#' @export



phase <- function(x){
  UseMethod("phase")
}

#' @exportS3Method
phase.seepr_dhr <- function(x){

  a_cos <- dhr_amp_cos(x)
  b_sin <- dhr_amp_sin(x)

  phase_out <-phase_from_amp(a_cos,
                             b_sin)
  phase_out[-1]

}


#' @exportS3Method
phase.seepr_fit <- function(x){
  attr(x, "PHASE")
}



####### HELPERS ####### -------------

phase_from_amp <- function(a_cos,
                           b_sin){
  s_b <- sign(b_sin)
  s_a <- sign(a_cos)

  #gamma <- rep(-pi / 2, length(a_cos))
  #gamma[s_b == -1] <- pi/2

  #phi <- (atan(b_sin / a_cos) + gamma) %% (2*pi)
  phi <- (atan2(b_sin, a_cos) - pi/2) %% (2*pi)
  phi
}
