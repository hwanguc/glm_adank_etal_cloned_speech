#' Calculate D-Prime Labels
#'
#' Labels trials (i.e., rows in a data frame) as a hit, miss, false alarm, or correct rejection.
#' Outputs a data frame with added columns for labels for hits, misses, false alarms, or correct rejections.
#'
#' @param stim1_col column that has the filename of target stimulus
#' @param stim2_col column that has the filename of probe stimulus
#' @param correct_col column that has correct/incorrect responses, where 1 is correct and 0 is incorrect
#' @param data is a data frame
#'
#' @return
#' @export
#'
#' @examples
#'
#' #Make a data frame
#' df<- data.frame(participant = c(rep("Participant 1",24), rep("Participant 2",24,),
#' rep("Participant 3",24), rep("Participant 4",24)),
#'                stim1 = rep(c(rep(c("Apple", "Orange", "Orange", "Apple"), 6),
#'                              rep(c("Orange", "Apple", "Apple", "Orange"), 6)),2),
#'                stim2 = rep(c(rep(c("Apple", "Orange", "Apple", "Orange"), 6),
#'                              rep(c("Orange", "Apple", "Apple", "Orange"), 6)),2),
#'                correct = c(rep(c(1,1,0,0),1), rep(c(1,0,1,0),1),
#'                            rep(c(0,0,1,1),2), rep(c(0,1,0,1),2),
#'                            rep(c(0,1,0,1),2), rep(c(0,0,1,1),2),
#'                            rep(c(1,0,1,0),1), c(rep(c(1,1,0,0),1))))
#'
#' # Label trials (i.e., rows in the data frame) as hits, misses, false alarms, or correct rejections.
#' dprime_lab("stim1", "stim2", "correct", data = df)
#'
dprime_lab <- function(stim1_col, stim2_col, correct_col, data){

  # Parker Tichko, 2020
  
  # Create columns
  data$Hit <- NA
  data$Miss <- NA
  data$FalseAlarm <- NA
  data$CorrectRej <- NA
  

  # Compute sub-categories for d-prime stats
  for(i in 1:length(data[[stim1_col]])){

    if(data[i,stim1_col] != data[i, stim2_col] & data[i, correct_col] == 1){

      # HIT: different stim, correct response
    data[i,"Hit"] <- 1

    } else if(data[i,stim1_col] != data[i,stim2_col] & data[i,correct_col] == 0){

    # MISS: different stim, incorrect response
    data[i,"Miss"] <- 1

    } else if(data[i,stim1_col] == data[i,stim2_col] & data[i,correct_col] == 0){

      # FALSE ALARM: same stim, incorrect resonse
      data[i, "FalseAlarm"] <- 1

    } else if(data[i,stim1_col] == data[i,stim2_col] & data[i,correct_col] == 1){

      # CORRECT REJECTION: same stim, correct response
      data[i, "CorrectRej"] <- 1


    }

  }

  return(data)
}
