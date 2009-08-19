\name{DEFFbinary}
\Rdversion{1.1}
\alias{DEFFbinary}
\title{
Compute the design effect for correlated binary data
}
\description{
This package computes the intraclass correlation, the design effect, and standard errors for the intercept for a binary-valued model with a group-level random effect, when the number of groups and number of observations per group are specified.   
}
\usage{
DEFFbinary(logitMean, sigma, NperGroup = NULL, Ngroups = NULL, Nsim = 1000)
}
\arguments{
  \item{logitMean}{
  The mean response, on the logit scale
}
  \item{sigma}{
  The standard deviation of the group-level random effect
}
  \item{NperGroup}{
  Number of observations per group, if unspecified only the ICC is computed
}
  \item{Ngroups}{
 Number of groups, if unspecified standard errors are not computed.
}
  \item{Nsim}{
  Number of simulations for computing the moments of the logistic-normal distribution.
}
}
\details{
Consider the following model, where \eqn{Y_{ij}}{Y_ij} is the jth observation from the ith group:
\deqn{Y_{ij}|P_i \sim  Bernoulli(P_i)}{Y_ij|P_i ~ Bernoulli(P_i)}
\deqn{logit(P_i) = \mu + U_i}{logit(P_i) = mu+ U_i}
\deqn{U_i \sim N(0, \sigma^2)}{U_i ~ N(0, sigma^2)}

\code{logitMean} is  \eqn{\mu}{mu}, the conditional mean on the logit scale.  The \code{sigma} argument is \eqn{\sigma}{sigma} above.  

}
\value{
A vector with the following elements
 \item{ICC}{The correlation \eqn{cor(Y_{ij},Y_{ik})}{cor(Y_ij,Y_ik))}}
\item{DEFF}{The design effect, (the total sample size divided by the effective sample size)}
 \item{SE}{The standard error of the estimate \eqn{\hat\mu}{of mu}  }

The first and second moments of  \eqn{P_i}{P_i} are returned as an attribute
}
\references{
Brown and Jiang (2009), "Intraclass Correlation and the Design Effect for Binary Random Effects Models", unpublished.
}
\author{
Patrick Brown
}
\examples{
# Design effect with conditional mean 0.5, standard deviation 1,
# 10 groups and 10 observations per group
DEFFbinary(0, 1, NperGroup=10, Ngroups = 10, Nsim=10000)

# the same with conditional mean 0.1
DEFFbinary(log(0.1/0.9), 1, NperGroup=10, Ngroups = 10, Nsim=10000)

}
