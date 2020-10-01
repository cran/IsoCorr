drift <- function(x, y, n, ref.name, ref.val, p.val = 0.05, graph = FALSE, skip.inquiry = FALSE){
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

    #If no NAs are found or you wish to continue with NAs, perform the correction
    if(answer == "Y"){
      #Fill NAs
      for(i in 2:length(y)){
        if(is.na(y[i]) == TRUE & x[i] == x[i-1]){
          y[i] <- y[i-1]
        }
      }

      refs <- which(names == ref.name)
      ref_vals <- matrix(nrow = length(refs)/n, ncol = 3)
      for(i in 1:length(ref_vals[,1])){
        ref_vals[i,1] <- mean(refs[(i*n-(n-1)):(i*n)])
        ref_vals[i,2] <- mean(y[refs[(i*n-(n-1)):(i*n)]])
        ref_vals[i,3] <- var(y[refs[(i*n-(n-1)):(i*n)]])
      }

      fit <- lm(ref_vals[,2] ~ ref_vals[,1], weights = 1/ref_vals[,3])
      lin_regr <- summary(fit)$coefficients

      #Check if the correction is necesarry
      answer2 <- "Y"
      answer3 <- "N"
      cor_dat <- y[refs]
      if(lin_regr[2,4] > p.val & skip.inquiry == FALSE){
        print(paste0("There is no significant slope (p = ", round(lin_regr[2,4], digits = 3), ")."))
        answer2 <- readline("Perform slope correction annyway? <Y/N> ")
      }
      if(answer2 == "Y"){
        values <- y - (seq(1:length(y))*lin_regr[2,1] + lin_regr[1,1]) + ref.val

        cor_dat <- y[refs] - (refs*lin_regr[2,1] + lin_regr[1,1]) + ref.val
      }
      else if(answer2 == "N"){
        answer3 <- readline("Correct for offset between measurements and reference value? <Y/N> ")

      }
      if(answer3 == "Y"){
        values <- y - (mean(refs)*lin_regr[2,1] + lin_regr[1,1]) + ref.val

        cor_dat <- y[refs] - (mean(refs)*lin_regr[2,1] + lin_regr[1,1]) + ref.val
      }

      #Plot an optional graph which illustrates the performed correction
      if(graph == TRUE){
        ref_vals2 <- matrix(nrow = length(refs)/n, ncol = 3)
        for(i in 1:length(ref_vals2[,1])){
          ref_vals2[i,1] <- mean(refs[(i*n-(n-1)):(i*n)])
          ref_vals2[i,2] <- mean(cor_dat[(i*n-(n-1)):(i*n)])
          ref_vals2[i,3] <- var(cor_dat[(i*n-(n-1)):(i*n)])
        }

        plot(ref_vals[,1], ref_vals[,2],
             xlab = paste("Measurement (Out of ", length(x), " measurements)"),
             xlim = c(1, length(y)),
             ylab = paste("Delta value", "\u0028","\u2030","\u0029"), pch = '',
             ylim = c(min(c(ref.val, ref_vals[,2] - sqrt(ref_vals[,3]), ref_vals2[,2] - sqrt(ref_vals2[,3]))),
                      max(c(ref.val, ref_vals[,2] + sqrt(ref_vals[,3]), ref_vals2[,2] + sqrt(ref_vals2[,3])))))
        abline(lin_regr[1,1], lin_regr[2,1], col = "red", lty = 2, lwd = 2)
        points(ref_vals[,1], ref_vals[,2], pch = 19, cex = 2, col = "red")
        arrows(ref_vals[,1], ref_vals[,2], ref_vals[,1], ref_vals[,2] + sqrt(ref_vals[,3]), length = 0.05, angle = 90, code = 3, lwd = 1.5, col = "red")
        arrows(ref_vals[,1], ref_vals[,2], ref_vals[,1], ref_vals[,2] - sqrt(ref_vals[,3]), length = 0.05, angle = 90, code = 3, lwd = 1.5, col = "red")

        fit2 <- lm(ref_vals2[,2] ~ ref_vals2[,1], weights = 1/ref_vals2[,3])
        lin_regr2 <- summary(fit2)$coefficients
        abline(lin_regr2[1,1], lin_regr2[2,1], col = "green", lty = 2, lwd = 2)
        points(ref_vals2[,1], ref_vals2[,2], pch = 17, cex = 2, col = "green")
        arrows(ref_vals2[,1], ref_vals2[,2], ref_vals2[,1], ref_vals2[,2] + sqrt(ref_vals2[,3]), length = 0.05, angle = 90, code = 3, lwd = 1.5, col = "green")
        arrows(ref_vals2[,1], ref_vals2[,2], ref_vals2[,1], ref_vals2[,2] - sqrt(ref_vals2[,3]), length = 0.05, angle = 90, code = 3, lwd = 1.5, col = "green")

        abline(h = ref.val, lty = 3, lwd = 3)

        legend(1, max(c(ref.val, ref_vals[,2] + sqrt(ref_vals[,3]), ref_vals2[,2] + sqrt(ref_vals2[,3]))),
               legend = c("Raw data", "Corrected data", "Reference value"),
               col = c("red", "green", "black"), lty = c(2,2,3), bty = "n", pch = c(19,17,19), pt.cex = c(1,1,0))

      }

      return(cbind(names, values))

    }
  }
}
