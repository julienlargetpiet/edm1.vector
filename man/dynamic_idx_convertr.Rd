% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{dynamic_idx_convertr}
\alias{dynamic_idx_convertr}
\title{dynamic_idx_convertr}
\usage{
dynamic_idx_convertr(from_v_ids, from_v_val)
}
\arguments{
\item{from_v_ids}{is the input vector of indices}

\item{from_v_val}{is the input vector of elements, or just the total number of characters of the elementsq in the vector}
}
\description{
Allow to convert the indices of vector ('from_v_ids') which are related to the each characters of a vector (from_v_val), to fit the newly established characters of the vector from_v_val, see examples.
}
\examples{

print(dynamic_idx_convertr(from_v_ids = c(1, 5), from_v_val = c("oui", "no", "ouI")))

[1] 1 2 

print(dynamic_idx_convertr(from_v_ids = c(1, 6), from_v_val = c("oui", "no", "ouI")))

[1] 1 3

}
