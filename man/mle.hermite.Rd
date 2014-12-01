\encoding{utf8} 
\name{mle.hermite}
\alias{mle.hermite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maximum likelihood estimation for the generalized Hermite distribution
}
\description{
Maximum likelihood estimation for the generalized Hermite distribution and likelihood ratio test for Poisson assumption.
}
\usage{
mle.hermite(x, a=NA, b=NA, m=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector containing the sample observations.
}
  \item{a}{
initial value for the first parameter for the Hermite distribution. It's default value is \code{NA}.
}
  \item{b}{
initial value for the second parameter for the Hermite distribution. It's default value is \code{NA}.
}
  \item{m}{
Order of the generalized Hermite distribution. Its default value is \code{NA}.
}
}

\details{
If initial values are supplied they are used to obtain the maximum likelihood estimates for \code{a} and \code{b}. If no initial values are supplied, the naive estimates detailed in Puig (2003) are taken as initial values.
}

\value{
A list with five elements, the first containing the 
estimates of parameters \code{a} and \code{b}. The second element contains the 
maximum likelihood value and the third element contains the Hessian matrix. The 
fourth element of the returned value contains the likelihood ratio test 
statistic \eqn{W} and the last element is the p-value of the likelihood ratio test. 
}
\references{
Kemp C D, Kemp A W. Some Properties of the Hermite Distribution. Biometrika 1965;\bold{52} (3-4):381–394.

McKendrick A G Applications of Mathematics to Medical Problems. Proceedings of the Edinburgh Mathematical Society 1926;\bold{44}:98–130.

Kemp A W, Kemp C D. An alternative derivation of the Hermite distribution. Biometrika 1966;\bold{53} (3-4):627–628.

Patel Y C. Even Point Estimation and Moment Estimation in Hermite Distribution. Biometrics 1976;\bold{32} (4):865–873.

Gupta R P, Jain G C. A Generalized Hermite distribution and Its Properties. SIAM Journal on Applied Mathematics 1974;\bold{27}:359–363.

Bekelis, D. Convolutions of the Poisson laws in number theory. In Analytic & Probabilistic Methods in Number Theory: Proceedings of the 2nd International Conference in Honour of J. Kubilius, Lithuania 1996;\bold{4}:283–296.

Zhang J, Huang H. On Nonnegative Integer-Valued Lévy Processes and Applications in Probabilistic Number Theory and Inventory Policies. American Journal of Theoretical and Applied Statistics 2013;\bold{2}:110–121.

Kotz S. Encyclopedia of statistical sciences. John Wiley 1982-1989.

Kotz S. Univariate discrete distributions. Norman L. Johnson 2005.

Puig P. (2003). Characterizing Additively Closed Discrete Models by a Property of Their Maximum Likelihood Estimators, with an Application to Generalized Hermite Distributions. Journal of the American Statistical Association 2003;\bold{98}:687–692.

}

\author{
David Moriña (Centre for Research in Environmental Epidemiology, CREAL), Manuel Higueras (Universitat Autònoma de Barcelona and Public Health England) and Pedro Puig (Universitat Autònoma de Barcelona)
}

\seealso{
\code{\link{Distributions}} for some other distributions, \code{\link{dhermite}}, \code{\link{phermite}}, \code{\link{qhermite}}, \code{\link{hermite-package}}
}
\examples{
rnd <- rhermite(1000, 0.8, 0.3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hermite }
\keyword{ random }