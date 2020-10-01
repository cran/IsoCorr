

carry.over <- function(x, y, n, ref.names, ref.vals, graph = FALSE, skip.inquiry = FALSE){
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
    NA_names2 <- unique(names[NAs+n])

    answer <- "Y"
    if(length(NA_names) > 0){
      print(paste("Following samples contained NAs which will introduce errors:", NA_names))
      print(paste("Following samples follow samples with NAs which might introduce errors:", NA_names2))
      if(skip.inquiry == FALSE){
        answer <- readline("Do you wish to continue? <Y/N> ")
      }
    }

    #If no NAs are found or you wish to continue with NAs, perform the correction
    if(answer == "Y"){
      #Fill NAs
      for(i in 2:length(y)){
        if(is.na(y[i]) == TRUE & x[i] == x[i-1]){
          y[i] <- y[i-1]
        }
      }

      #Find positions of ref measurements in data
      refs <- vector(length = 0)
      for(i in 2:length(y)){
        if(names[i-1] == ref.names[1] & names[i] == ref.names[2]){
          refs <- c(refs,i)
        }
      }

      #Put all ref measurements in matrix
      ref_matrix <- matrix(ncol = length(refs), nrow = n)
      for(i in 1:length(refs)){
        for(ii in 1:n){
          ref_matrix[ii,i] <- refs[i] + ii - 1
        }
      }

      for(i in 1:nrow(ref_matrix)){
        for(ii in 1:ncol(ref_matrix)){
          ref_matrix[i,ii] <- y[ref_matrix[i,ii]]
        }
      }

      #Calculate carry-over % in ref measurements
      for(i in 1:nrow(ref_matrix)){
        for(ii in 1:ncol(ref_matrix)){
          ref_matrix[i,ii] <- (ref_matrix[i,ii]-ref.vals[2])/(ref.vals[1]-ref.vals[2])
        }
      }

      means <- rowMeans(ref_matrix)
      VAR1 <- apply(ref_matrix, MARGIN = 1, FUN = var)

      #Correct for carry-over
      seq1 <- 1/(seq(1,n))
      fit <- lm(means ~ seq1, weights = VAR1)
      for(i in seq(n, length(y), by = n)){
        for(ii in 1:n){
          if(i+ii <= length(y)){
            values[i+ii] <- (y[i+ii]-(fit$coefficients[1]+fit$coefficients[2]*seq1[ii])*y[i-1])/(1-(fit$coefficients[1]+fit$coefficients[2]*seq1[ii]))
          }
        }
      }

      #Plot an optional graph which illustrates the performed correction
      if(graph == TRUE){
        plot(means, pch = 19, cex = 2, type = 'o', lty = 2, xlab = 'Itterations', ylab = 'Carry-over (%)')
        points(fit$coefficients[1]+fit$coefficients[2]*seq1, col = 'green', pch = 17, cex = 2, type = 'o', lty = 2)

        legend(8, max(means), xjust = 1,
               legend = c("Measured carry-over", "Fitted carry-over"),
               col = c("black", "green"), lty = c(2,2), bty = "n", pch = c(19,17))
      }

      return(cbind(names, values))

    }
  }
}
