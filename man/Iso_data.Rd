\name{Iso_data}
\alias{Iso_data}
\docType{data}
\title{Stable water isotope dataset
}

\description{Continuous measurements of stable water isotopes.
}

\usage{data("Iso_data")}

\format{
  A data frame with 315 observations on the following 3 variables
  \describe{
    \item{\code{Identifier}}{Sample names.
    }
    \item{\code{d.18_16.Mean}}{Isotopic measurements of delta values for oxygen-18 (numeric vector).
    }
    \item{\code{d.D_H.Mean}}{Isotopic measurements of delta values for deuterium (numeric vector).
    }
  }
  There are 8 itterations per sample. The last sample ("833") is incomplete to illustrate errors due to missing itterations. Sample "377" contains NAs to illustrate errors due to NAs.
}

%\details{
%}

%\source{
%}

%\references{
%}

\author{J.D.M.S.}

\examples{
data("Iso_data")

# See examples in functions 'drift', 'carry.over' and 'iso.summary'.
}


%\keyword{datasets}
