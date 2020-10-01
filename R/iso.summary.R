iso.summary <- function(x, y, n, skip.inquiry = FALSE){

  #Check if the length of the values is a multiplicate of the number of itterations
  length_check <- length(y)/n
  if(length_check%%1!=0){
    print("Some itterations in your measurements are missing.")
  }

  #If no values are missing, check for NAs
  else{
    NAs <- which(is.na(y) == TRUE)
    names <- trimws(x, which = "both")
    NA_names <- unique(names[NAs])

    answer <- "Y"
    if(length(NA_names) > 0){
      print(paste("Following samples contained NAs which will introduce errors:", NA_names))
      if(skip.inquiry == FALSE){
        answer <- readline("Do you wish to continue? <Y/N> ")
      }
    }

    #If no NAs are found or you wish to continue with NAs, fill NAs by measured sample value
    if(answer == "Y"){
      #Fill NAs
      for(i in 2:length(y)){
        if(is.na(y[i]) == TRUE & x[i] == x[i-1]){
          y[i] <- y[i-1]
        }
      }

      #Calculate summary
      means <- aggregate.data.frame(x = y, by = list(x), FUN = "mean")
      SE <- aggregate(x = y, by = list(x), FUN = IsoCorr::st.err)

      summary2 <- cbind(means, SE[,2])
      colnames(summary2) <- c('Sample','mean', 'SE')

      return(summary2)

    }
  }
}
