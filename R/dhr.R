#' @title dhr
#' @description Performs a 'dynamic harmonic regression' with a fixed
#' main period and a trend component.
#'
#' @param x A time series or vector
#' @param s The period length in entries in x
#'
#' @export


dhr <- function(x,
                s){

  mod <- dhr_mod(x,s)
  #x <- dlm::dlmFilter(x, mod)
  x <- dlm::dlmSmooth(x, mod)
  x$m <- x$s
  x <- new_seepr_dhr(x,s)
  x
}


####### HELPERS ######## -----------

dhr_mod<- function(x,
                   s){
  phi <- 2 * pi / s * (0:(length(x)-1))

  V <- 0.2
  W_1 <- sd(diff(zoo::rollmean(x,s))) / 4
  m0_1 <- mean(x[1:s], na.rm = TRUE)

  # trend / sin / d(sin) / cos / d(cos)
  mod <- dlm::dlm(FF = matrix(c(1,1,0,1,0),1,5),
                  GG = matrix(c(1,0,0,0,0,
                                0,1,0,0,0,
                                0,1,1,0,0,
                                0,0,0,1,0,
                                0,0,0,1,1),5,5),
                  V = matrix(V),
                  W = diag(c(W_1,0,1e-8,0,1e-8),5),
                  JFF = matrix(c(0,1,0,2,0),1,5),
                  X = matrix(c(sin(phi), cos(phi)), nrow = length(x), ncol = 2),
                  m0 = c(m0_1,1,0,1,0),
                  C0 = diag(1e3,nrow = 5)
  )


}
