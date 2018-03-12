\name{daylength}
\alias{daylength}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Daylength
}

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute daylength (photoperiod) for a latitude and date.
}

\usage{
daylength(lat, doy)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
  latitude, in degrees. I.e. between -90.0 and 90.0
  }
  \item{doy}{
  Interger, day of the year (1..365) for leap years; or an object of class Date; or a character that can be coerced into a date, using 'yyyy-mm-dd' format, e.g. '1982-11-23'
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Forsythe, William C., Edward J. Rykiel Jr., Randal S. Stahl, Hsin-i Wu and Robert M. Schoolfield, 1995. A model comparison for daylength as a function of latitude and day of the year. Ecological Modeling 80:87-95.}

\author{
Robert J. Hijmans}

\note{
Function source code and documentation duplicated from `geosphere` package to avoid dependencies in source code for `pnrShad`}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
daylength(-25, '2010-10-10')
daylength(45, 1:365)

# average monthly daylength
dl <- daylength(45, 1:365)
tapply(dl, rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}% use one of  RShowDoc("KEYWORDS")
