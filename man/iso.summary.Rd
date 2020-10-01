\name{iso.summary}
\alias{iso.summary}
\title{Data Summary}

\description{Provides the mean and standard error of continuous isotopic measurements.
}

\usage{
iso.summary(x, y, n, skip.inquiry = FALSE)
}

\arguments{
  \item{x}{Vector with sample names (character). length(x) should equal length(y).
  }
  \item{y}{Vector with sample values (numeric). length(y) should equal length(x).
  }
  \item{n}{Number of itterations per sample. length(x) should be a multiplicate of n.
  }
  \item{skip.inquiry}{Surpres inquiries regarding possible errors, irrespective of possible NAs (No Nas should be present as 'iso.summary' should always be used after 'drift' and 'carry.over', both of which fix NAs). Default setting is FALSE when undifined.
  }
}

%\details{
%}

\value{Returns a three-column data frame. First column contains sample names (factor), second column contains sample means (numeric), third column contains standard errors of the samples (numeric).}

%\references{
%}

\author{J.D.M.S.}

%\note{
%}

%\seealso{
%}

\examples{
data("Iso_data")

## Example
names <- Iso_data$Identifier[1:312]
values <- Iso_data$d.D_H.Mean[1:312]

cor_data <- cbind.data.frame(names,values)
# 5 itterations is enough for this dataset, 10 are used to illustrate convergence
for(i in 1:10){
  cor_data <- drift(cor_data[,1], as.numeric(cor_data[,2]), 8,
              "LAB2", -48.7, graph = TRUE, skip.inquiry = TRUE)
  cor_data <- carry.over(cor_data[,1], as.numeric(cor_data[,2]), 8,
              c("LAB1","LAB3"), c(7.7, -147), graph = TRUE, skip.inquiry = TRUE)
}

sum_dat <- iso.summary(cor_data[,1], as.numeric(cor_data[,2]), 8, skip.inquiry = TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
