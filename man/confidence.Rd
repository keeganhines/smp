\name{confidence}
\alias{confidence}
\title{Estimate parameter confidence from binomial data.}
\usage{
  confidence(input.data, Bayes = TRUE, Iterations = 1e+05)
}
\arguments{
  \item{input.data}{input data drawn from binomial
  distribution}

  \item{Bayes}{flag to specify whether Bayesian or
  frequentist methodology is to be used}

  \item{Iterations}{number of samples to use for Monte
  Carlo integration for Bayesian method}
}
\description{
  Use methods described in the accompanying paper in order
  to calculate parameter confidence when analyzing
  binomially distributed data. Can be used for frequentist
  esimate (eqn [4] in main paper) or Bayesian estimate (eqn
  [9] in main paper).
}

