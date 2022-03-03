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
  x <- dlm::dlmFilter(x, mod)

}


####### HELPERS ######## -----------

dhr_mod<- function(x,
                   s){
  phi <- 2 * pi / s * (0:(length(x)-1))
  mod <- dlm::dlm(FF = matrix(c(1,1,0,1,0),1,5),
                  GG = matrix(c(1,0,0,0,0,
                                0,1,0,0,0,
                                0,1,1,0,0,
                                0,0,0,1,0,
                                0,0,0,1,1),5,5),
                  V = matrix(1),
                  W = matrix(c(1000, rep(0,24)),5,5),
                  JFF = matrix(c(0,1,0,2,0),1,5),
                  X = matrix(c(sin(phi), cos(phi)), nrow = length(x), ncol = 2),
                  m0 = c(0,0,0,0,0),
                  C0 = diag(1e7,nrow = 5)
  )


}
