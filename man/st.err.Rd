\name{st.err}
\alias{st.err}

\title{Standard Error}

\description{
This function computes the standard error of the values in x. If na.rm is TRUE then missing values are removed before computation proceeds.}

\usage{
st.err(x, na.rm = FALSE)
}

\arguments{
  \item{x}{A numeric vector or an R object which is coercible to one.}
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computation proceeds.}
}

%\details{
%}

%\value{
%}

%\references{
%}

%\author{
%}

%\note{
%}

%\seealso{
%}

\examples{
## Example 1
x <- c(5.2,6,5.5,4.1,8)
st.err(x)

## Example 2
x <- c(5.2,6,NA,4.1,8,NA)
st.err(x, na.rm = TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
