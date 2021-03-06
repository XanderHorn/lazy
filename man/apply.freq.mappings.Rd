% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_freq_mappings.R
\name{apply.freq.mappings}
\alias{apply.freq.mappings}
\title{Applies frequency mappings}
\usage{
apply.freq.mappings(data, freq.mappings, progress = TRUE)
}
\arguments{
\item{data}{[required | data.frame] Dataset containing features to apply mapping to}

\item{freq.mappings}{[required | list] Output from function map.freq.encode}

\item{progress}{[optional | logical | default=TRUE] Display a progress bar}
}
\value{
Data frame with frequency features with all other features
}
\description{
Uses output from map.freq.encoding to count how many times a value of a feature occurred in the training set and applies it to another dataset
}
\examples{
fm <- map.freq.encoding(data = iris)
new_iris <- apply.freq.mappings(data = iris, freq.mappings = fm)
}
\author{
Xander Horn
}
