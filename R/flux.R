#' @title flux
#'
#' @description Calculates vertical seepage flux from
#' temperature time series.
#'
#' @param t_up,t_low temperature time series of the upper and lower sensor in °C.
#'
#' @param z depth difference between in m
#'
#' @param n total porosity vol/vol
#'
#' @param Cscal volumetric heat capacity of sediments in cal/cm^3-C
#'
#' @param Cwcal volumetric heat capacity of water in cal/cm^3-C
#'
#' @param Kcal baseline thermal conductivity in cal/s-cm-C
#'
#' @param method The method of flux calculation to be used. Must be one of
#' \itemize{
#' \item{keery_amplitude}
#' \item{keery_phase}
#'}
#'
#' @inheritParams dhr
#'
#'
#' @export

flux <- function(x,
                 n,
                 Cscal,
                 Cwcal,
                 Kcal,
                 method){
  UseMethod("flux")
}

#' @exportS3Method
flux.seepr_fit <- function(x,
                           n,
                           Cscal,
                           Cwcal,
                           Kcal,
                           method){

PHASE <- phase(x)
AMPLITUDE <- amplitude(x)
s <- attr(x, "s")
depths <- attr(x, "depths")

FLUX <-lapply(1:(length(depths)-1),
              function(i){
                flux_pair(PHASE[,c(i,i+1)],
                          AMPLITUDE[,c(i,i+1)],
                          s,
                          depths[c(i,i+1)],
                          n,
                          Cscal,
                          Cwcal,
                          Kcal,
                          method)

              }) %>%
  dplyr::bind_cols()


}







flux_pair  <- function(PHASE,
                      AMPLITUDE,
                 s,
                 depths,
                 n,
                 Cscal,
                 Cwcal,
                 Kcal,
                 method){

  method <- match.arg(method, c("keery_amplitude", "keery_phase"))

  # unit conversion
  K <- Kcal * 4.184*100*60*60
  C <- (n*Cwcal + (1-n) * Cscal) * 4.18400 *100^3
  Cw <- Cwcal*4.18400*100^3

  he_ca_ra <- C / Cw
  Ke <- K/C

  H <- Cw / K
  P <- 24 # period in hours

  z <- diff(depths)

  # extract amp and phase
  amps_1 <- AMPLITUDE[,1]
  amps_2 <- AMPLITUDE[,2]
  phases_1 <- PHASE[,1]
  phases_2 <- PHASE[,2]

  # time lag in samples
  t_lag <-((phases_2 - phases_1 ) %% (2*pi)  / 2 / pi * s ) %% s
  t_lag_hrs <- ((phases_2 - phases_1) / 2 / pi * 24)

  # time lag as indices
  t_up_lagged <-round(1:length(phases_1) - t_lag)
  t_up_lagged[t_up_lagged < 1] <- NA

  # relative amplitude
  amps_rel <-(amps_2 / amps_1[t_up_lagged])
  D <- log(amps_rel)

  if (method == "keery_amplitude"){
    flux_out <- flux_keery_amplitude(D,
                                     H,
                                     z,
                                     C,
                                     K,
                                     P)
  } else if (method == "keery_phase"){
    flux_out <- flux_keery_phase(C,
                                 z,
                                 t_lag_hrs,
                                 K,
                                 Cw,
                                 P)
  }
  flux_out
}



###### HELPERS ####### --------------------
flux_keery_amplitude <- function(D,
                                 H,
                                 z,
                                 C,
                                 K,
                                 P){


  flux_out <-
    sapply(as.numeric(D), function(D_i){
      rts <- NA

      rts <-
        tryCatch(
          polyroot(rev(c(H^3*D_i/4/z, -5*H^2*D_i^2/4/z^2, 2*H*D_i^3/z^3, (pi*C/K/P)^2 -(D_i^4/z^4)))),
          error = function(i) NA
        )

      out <- Re(rts[abs(Im(rts)) < 1e-8]) / 3600

      if(length(out) == 1 & is.infinite(out) == FALSE){
        return(out)
      } else{
        return(NA)
      }
    })

  flux_out
}


flux_keery_phase <- function(C,
                             z,
                             t_lag_hrs,
                             K,
                             Cw,
                             P){


  flux_out <-(sqrt((C*z/Cw/t_lag_hrs)^2-(K*4*pi*t_lag_hrs/Cw/P/z)^2)) / 3600
}
