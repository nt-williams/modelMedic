% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{process}
\alias{process}
\title{Pre-process a dataframe for modeling with AIPW or TMLE}
\usage{
process(df)
}
\arguments{
\item{df}{a dataframe or tibble}
}
\value{
a dataframe with missing values imputed at the median, an indicator for missingness, and factors dummy coded.
}
\description{
Pre-process a dataframe for modeling with AIPW or TMLE
}
