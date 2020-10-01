\name{drift}
\alias{drift}

\title{Drift Correction}

\description{This function corrects for drift in continuous isotopic measurements.
}

\usage{
drift(x, y, n, ref.name, ref.val, p.val = 0.05, graph = FALSE, skip.inquiry = FALSE)
}

\arguments{
  \item{x}{Vector with sample names (character). length(x) should equal length(y).
}
  \item{y}{Vector with sample values (numeric). length(y) should equal length(x).
}
  \item{n}{Number of itterations per sample. length(x) should be a multiplicate of n.
}
  \item{ref.name}{Character indicating the name of the reference value in x. Leading and trailing whitespace is automaticallye removed from x.
}
  \item{ref.val}{Known reference value (numeric) used for drift correction; refered to in ref.name.
}
  \item{p.val}{p-value used to check whether there is a significant linear drift. Default value of 0.05 is used when undefined.
}
  \item{graph}{Optional graph which illustrates the performed correction for reference values in y by plotting the raw data, corrected data and ref.value. Default setting is FALSE when undefined.
}
  \item{skip.inquiry}{Surpres inquiries regarding possible errors, allowing a full drift correction, irrespective of possible NAs or insignificant p-values. This option allows use of the drift function in a loop. Default setting is FALSE when undefined.
}
}

\details{Possible inquiries when skip.inquiry is FALSE: (i) "Some itterations in your measurements are missing." Indicates that length(x) is not a multiplicate of n. (ii) "Following samples contained NAs which will introduce errors: " combined with "Do you wish to continue? <Y/N>" If you continue, NAs will be filled with the last measured value of the same sample which will affect the mean and standard error of the samples containing NAs. (iii) "There is no significant slope." combined with "Perform slope correction annyway? <Y/N>" If you don't perform a full correction, the following inquiry is returned: (iv) "Correct for offset between measurements and reference value? <Y/N>" If no correction for offset is performed, the original data will be returned.
}

\value{Returns a two-column matrix. First column contains sample names, second column contains corrected measurements.
}

%\references{
%}

\author{J.D.M.S.}

\note{Correction is based on a weighted (= 1/var(x)) linear regression.
}

%\seealso{
%}

\examples{
data("Iso_data")

## Example 1
names <- Iso_data$Identifier
values <- Iso_data$d.D_H.Mean

cor_data <- drift(names, values, 8, "LAB2", -48.7)

# -> Error: "Some itterations in your measurements are missing"



## Example 2
names <- Iso_data$Identifier[1:312]
values <- Iso_data$d.D_H.Mean[1:312]

cor_data <- drift(names, values, 8, "LAB2", -48.7, graph = TRUE)

# -> Inquiry: "Following samples contained NAs which will introduce errors: 377"
#             "Do you wish to continue? <Y/N>"

# Y

# -> Inquiry: "There is no significant slope (p = 0.256)"
            "Perform slope correction annyway? <Y/N>"

# Y



## Example 3
names <- Iso_data$Identifier[1:312]
values <- Iso_data$d.D_H.Mean[1:312]

cor_data <- drift(names, values, 8, "LAB2", -48.7, graph = TRUE)

# -> Inquiry: "Following samples contained NAs which will introduce errors: 377"
            "Do you wish to continue? <Y/N>"

# Y

# -> Inquiry: "There is no significant slope (p = 0.256)"
#             "Perform slope correction annyway? <Y/N>"

# N

# -> Inquiry: "Correct for offset between measurements and reference value? <Y/N>"

# Y
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
