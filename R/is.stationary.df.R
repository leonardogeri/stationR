is.stationary.df <- function(x){
  library(urca)
  library(dplyr)
  x <- ur.df(x) %>% summary()
  if_df <- if(x@teststat < x@cval[1,2]){
    print("YES")
  } else{
    print("NO")
  }
}
