
#' Calculate D-Prime Sub-Scores from D-Prime Labels
#'
#' Note: Must run dprime_lab.r first.
#' Calculates d-prime category scores from d-prime labels: total number of hits, misses, false alarms, and correct rejections for each participant.
#' These columns must already be in the data frame that is passed in as an argument.
#'
#'
#' @param df is the data frame with columns corresponding to d-prime labels
#' @param ... names of cols corresponding to IVs to group by (supports n > 1 cols). IVs must be factors.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # make data frame
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
#'
#' # reorder by participant
#' df <- df[order(df[,"participant"]),]
#'
#' # calculate dprime labels
#' df.dprime <- dprime_lab("stim1", "stim2", "correct", data = df)
#'
#' # calculate dprime sub-scores
#' dprime_cat(df.dprime, participant)
dprime_cat <- function(df, ...){

  # Parker Tichko, 2020

  require(dplyr)
  require(tibble)

  # drops NAs
  na.rm = TRUE

  # quote input args to use with dplyr functions
  group_vars <- dplyr::quos(...)

  # convert to tibble, if not already tibble
  if(tibble::is_tibble(df) == FALSE){
    df <- tibble::as_tibble(df)}

  # group by df variables
  df <- dplyr::group_by(df,!!! group_vars)

  dplyr::summarise(df,
                   Hits = sum(Hit, na.rm = na.rm),
                   Misses = sum(Miss, na.rm = na.rm),
                   FalseAlarms = sum(FalseAlarm, na.rm = na.rm),
                   CorrectRejs = sum(CorrectRej, na.rm = na.rm),
                   TotalTarg = Hits + Misses,
                   TotalDis= FalseAlarms + CorrectRejs,
                   NumRes = TotalTarg + TotalDis)
                   #HitRate = if(Hits/TotalTarg == 1 | Hits/TotalTarg ==  0){Hits/TotalTarg + 0.00001} else{Hits/TotalTarg}) # qnorm(1) & qnorm(0) are Inf. Increase HitRate and Missrate by 0.00001.
                   #MissRate = Misses/TotalTarg #if(Misses/TotalTarg == 1 |Misses/TotalTarg == 0){Misses/TotalTarg + 0.00001} else{Misses/TotalTarg}, # qnorm(1) & qnorm(0) are Inf. Increase HitRate and Missrate by 0.00001.
                   #FalseAlarmRate = FalseAlarms/TotalDis #if(FalseAlarms/TotalDis == 1 | FalseAlarms/TotalDis ==  0){FalseAlarms/TotalDis + 0.00001} else{FalseAlarms/TotalDis}, # qnorm(1) & qnorm(0) are Inf. Increase FA and CirrectRej by 0.00001.
                   #CorrectRejRate = CorrectRejRate/TotalDis #if(CorrectRejRate/TotalDis == 1 | CorrectRejRate/TotalDis ==  0){CorrectRejRate/TotalDis + 0.00001} else{CorrectRejRate/TotalDis},
                   #Dprime = qnorm(HitRate) - qnorm(FalseAlarmRate))

}
