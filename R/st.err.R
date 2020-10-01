st.err <- function(x, na.rm = FALSE) {
  if(na.rm == TRUE){
    x <- na.omit(x)
  }
  sd(x)/sqrt(length(x))
}
