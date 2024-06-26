% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{pre_to_post_idx}
\alias{pre_to_post_idx}
\title{pre_to_post_idx}
\usage{
pre_to_post_idx(inpt_v = c(), inpt_idx = c(1:length(inppt_v)))
}
\arguments{
\item{inpt_v}{is the new vector}

\item{inpt_idx}{is the vector containing the pre-indexes}
}
\description{
Allow to convert indexes from a pre-vector to post-indexes based on a current vector, see examples
}
\examples{

print(pre_to_post_idx(inpt_v = c("oui", "no", "eee"), inpt_idx = c(1:8)))

[1] 1 1 1 2 2 3 3 3

As if the first vector was c("o", "u", "i", "n", "o", "e", "e", "e")

}
