\name{carry.over}
\alias{carry.over}

\title{Carry-over correction}

\description{This function corrects for carry-over in continuous isotopic measurements.}

\usage{
carry.over(x, y, n, ref.names, ref.vals, graph = FALSE, skip.inquiry = FALSE)
}

\arguments{
  \item{x}{Vector with sample names (character). length(x) should equal length(y).
  }
  \item{y}{Vector with sample values (numeric). length(y) should equal length(x).
  }
  \item{n}{Number of itterations per sample. length(x) should be a multiplicate of n.
  }
  \item{ref.names}{Character indicating the names of two reference values in x, of which the first reference is directly followed by the second in the isotopic measurements. Leading and trailing whitespace is automaticallye removed from x.
  }
  \item{ref.vals}{Two known reference values (numeric) used for drift correction; refered to in ref.name.
  }
  \item{graph}{Optional graph which illustrates the performed correction per itteration of every sample, based on two reference values in y, by plotting the measured carry-over and the carry-over fitted to these measurements. Default setting is FALSE when undefined.
  }
  \item{skip.inquiry}{Surpres inquiries regarding possible errors, allowing a full carry-over correction, irrespective of possible NAs. This option allows use of the carry.over function in a loop. Default setting is FALSE when undefined.
  }
}

\details{Possible inquiry when skip.inquiry is FALSE: (i) "Some itterations in your measurements are missing." Indicates that length(x) is not a multiplicate of n. (ii) "Following samples contained NAs which will introduce errors:" combined with "Following samples follow samples with NAs which might introduce errors:" and "Do you wish to continue? <Y/N>" If you continue, the correction will be performed. If you do not wish to continue, the original data will be returned.
}

\value{Returns a two-column matrix. First column contains sample names, second column contains corrected measurements.}

%\references{
%}

\author{J.D.M.S.}

%\note{
%}

%\seealso{
%}

\examples{
data("Iso_data")

## Example 1
names <- Iso_data$Identifier
values <- Iso_data$d.D_H.Mean

cor_data <- carry.over(names, values, 8, c("LAB1","LAB3"), c(7.7, -147))

# -> Error: "Some itterations in your measurements are missing"



## Example 2
names <- Iso_data$Identifier[1:312]
values <- Iso_data$d.D_H.Mean[1:312]

cor_data <- carry.over(names, values, 8, c("LAB1","LAB3"), c(7.7, -147), graph = TRUE)

# -> Inquiry: "Following samples contained NAs which will introduce errors: 377"
#             "Following samples follow samples with NAs which might introduce errors: 25"
#             "Do you wish to continue? <Y/N>"

# Y
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
