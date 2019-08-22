stationary.matrix <- function(x, y, critical = FALSE){
  library(urca)
  library(dplyr)
  if(missing(y)){
    df <- ur.df(x) %>% summary()
    df_stat <- df@teststat
    df_cval <- df@cval[1,2]
    pp <-  ur.df(x) %>% summary()
    pp_stat <- pp@teststat
    pp_cval <- pp@cval[1,2]
    kpss <- ur.kpss(x) %>% summary()
    kpss_stat <- kpss@teststat
    kpss_cval <- kpss@cval[1,2]
    series <- (deparse(substitute(x)))
    cval <- cbind("Critical Values", df_cval, pp_cval, kpss_cval)
    matrixx <- cbind(series, df_stat, pp_stat, kpss_stat)
    colnames(matrixx) <- c("serie", "ADF", "PP", "KPSS")
    matrixx_cval <- rbind(matrixx, cval)
    ifelse(critical == FALSE, return(matrixx), return(matrixx_cval))
  }
  else{
    dfx <- ur.df(x) %>% summary()
    dfx_stat <- dfx@teststat
    dfx_cval <- dfx@cval[1,2]
    ppx <-  ur.df(x) %>% summary()
    ppx_stat <- ppx@teststat
    ppx_cval <- ppx@cval[1,2]
    kpssx <- ur.kpss(x) %>% summary()
    kpssx_stat <- kpssx@teststat
    kpssx_cval <- kpssx@cval[1,2]
    seriesx <- (deparse(substitute(x)))
    seriesy <- (deparse(substitute(y)))
    dfy <- ur.df(y) %>% summary()
    dfy_stat <- dfy@teststat
    dfy_cval <- dfy@cval[1,2]
    ppy <-  ur.df(y) %>% summary()
    ppy_stat <- ppy@teststat
    ppy_cval <- ppy@cval[1,2]
    kpssy <- ur.kpss(y) %>% summary()
    kpssy_stat <- kpssy@teststat
    kpssy_cval <- kpssy@cval[1,2]
    cvalx <- cbind("Critical Values", dfx_cval, ppx_cval, kpssx_cval)
    matrixxx <- cbind(seriesx, dfx_stat, ppx_stat, kpssx_stat)
    colnames(matrixxx) <- c("serie", "ADF", "PP", "KPSS")
    matrixxy <- cbind(seriesy, dfy_stat, ppy_stat, kpssy_stat)
    matrixx <- rbind(matrixxx, matrixxy)
    matrixx_cval <- rbind(matrixx, cvalx)
    ifelse(critical == FALSE, return(matrixx), return(matrixx_cval))
  }
}
