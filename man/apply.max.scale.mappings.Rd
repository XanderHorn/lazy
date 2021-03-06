% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_max_scale_mappings.R
\name{apply.max.scale.mappings}
\alias{apply.max.scale.mappings}
\title{Apply max scaler mappings}
\usage{
apply.max.scale.mappings(data, max.scale.mappings, progress = TRUE)
}
\arguments{
\item{data}{[required | data.frame] Dataset containing features to apply mappings to}

\item{max.scale.mappings}{[required | list] Output from function max.scale.mappings}

\item{progress}{[optional | logical | default=TRUE] Display a progress bar}
}
\value{
Data frame with scaled features
}
\description{
Applies max scaler mappings to new data. Uses the output from the function max.scale.mappings to apply these mappings.
}
\examples{
scaled <- map.max.scaler(data = iris, x = names(iris)[1:4])
new_iris <- apply.max.scale.mappings(data = iris, max.scale.mappings = scaled)
}
\author{
Xander Horn
}
