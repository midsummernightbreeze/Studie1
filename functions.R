library(MASS)
library(logitr)
library(mlogit)
library(mded)
library(showtext)
library(htmltools)


logitrData <- function(filtered_survey_data) {

  incremented_scale <-c()
  dce_count = nrow(filtered_survey_data) / 4

  for (i in 1:dce_count){
    incremented_scale <- append(incremented_scale,c(i,i,i,i))
  }

  filtered_survey_data$obsId <- incremented_scale

  return (filtered_survey_data)
}




wtp_space_results <- function(each_user_results){
  multi_starts = 100
  pars = c("Location","Contacts","Financial","Fingerprint","Text","BrowsingHistory")
  
  mnl_pref_each <- logitr(
    data    = logitrData(each_user_results),
    outcome = "is_selected_int",
    obsID    = "obsId",
    pars     = c("price","Location","Contacts","Financial","Fingerprint","Text","BrowsingHistory")
  )
 
  # wtp_mnl_pref_each <- wtp(mnl_pref_each,scalePar = "price")
  wtp_pref <- coef(mnl_pref_each)
  for(i in 1:7){
    if(i!=1){
      wtp_pref[i]=-(wtp_pref[i]/wtp_pref[1])
    }
  }

  mnl_wtp_each <- logitr(
    data = logitrData(each_user_results),
    outcome = "is_selected_int",
    obsID    = "obsId",
    pars     = pars,
    scalePar = "price",
    numCores = 1,
    numMultiStarts = multi_starts,
    #modelSpace = "wtp"
    startVals = wtp_pref
  )
  # print(wtpCompare(mnl_pref_each,mnl_wtp_each,scalePar = 'price'))
   print(wtp_pref)
   print(coef(mnl_wtp_each))
  return(
   coef(mnl_wtp_each)
  )
}

