## ---------------------------
##
## Script name: Main Script to anonymise patient data
##
##
## Author: Mr. Ashleigh C. Myall
##
## Date Created: 26-06-2020
##
## Copyright (c) Ashleigh C. Myall 2020
## Email: a.myall19@imperial.ac.uk
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

################################################################################

### Load Libs

################################################################################

library(digest)
library(readr)

################################################################################

###
###                               Functions
###

################################################################################


# ------------------------------------------------------------------------------

#' Anonymize a vector.
#'
#' \code{anonymize} anonymizes a vector \code{.x} by first salting it with
#'  \code{\link{salt}} and then hashing it with \code{\link{hash}}. See
#'  both functions for additional documentation.
#'
#' The user is advised to check out \href{https://en.wikipedia.org/wiki/Data_anonymization}{Wikipedia} for more information.
#'
#' @param .x a vector.
#' @param .algo the name of the algorithm.
#' @param .seed an integer to seed the random number generator.
#' @param .chars set of characters to salt with.
#' @param .n_chars an integer; number of characters to salt with.
#' @param ... additional arguments to be passed to \code{hash}.
#' @return An anonymized version of the vector.
#' @export

anonymize <- function(.x, .algo = "crc32", .seed = 123,
                      .chars = letters, .n_chars = 5L, ...){
  hash(paste(.x,salt(.x, .seed = .seed, .chars = .chars, .n_chars = .n_chars),sep = ":"),
       .algo = .algo, .seed = .seed, ...)
}


# ------------------------------------------------------------------------------

#' Hash a vector.
#'
#' The user is advised to check out \href{https://en.wikipedia.org/wiki/Hash_function}{Wikipedia} for more information.
#'
#' @param .x a vector.
#' @param .algo the name of the algorithm.
#' @param .seed an integer to seed the random number generator.
#' @param ... additional arguments to be passed to \code{\link[digest]{digest}}.
#' @return A hashed version of the vector.
#' @export
#' 


hash <- function(.x, .algo = "sha256", .seed = 0, ...){
  if (!is.atomic(.x)) stop("Vector must be an atomic vector.")
  return(unname(vapply(.x, digest::digest, algo = .algo, seed = .seed, ...,
                       character(1))))
}

# ------------------------------------------------------------------------------

#' Salt a vector.
#'
#' \code{salt} takes a vector \code{.x} and returns a salted version of it.
#' The algorithm for salting a vector is:
#' \enumerate{
#'   \item Select a random sample of characters of length \code{.n_chars} from \code{.chars}. Call this random sample \code{.y}.
#'   \item Concatenate \code{.y}, the vector \code{.x}, and \code{.y} again in a vectorized fashion.
#' }
#'
#' The user is advised to check out \href{https://en.wikipedia.org/wiki/Salt_\%28cryptography\%29}{Wikipedia} for more information.
#'
#' @param .x a vector.
#' @param .seed an integer to seed the random number generator.
#' @param .chars set of characters to salt with.
#' @param .n_chars an integer; number of characters to salt with.
#' @return A salted version of the vector.
#' @export

salt <- function(.x, .seed = NULL, .chars = letters, .n_chars = 5L) {
  if (!is.atomic(.x)) stop("Vector must be an atomic vector.")
  if (!is.null(.seed)) set.seed(.seed)
  .y <- paste0(sample(.chars, .n_chars, replace = TRUE), collapse = "")
  return(paste0(.y, .x, .y, sep = ""))
}


################################################################################

##########
##########                       Main Work Flow 
##########

################################################################################

# ------------------------------------------------------------------------------


#
patient_data <- read_csv("patient_data.csv")

# choose columns to mask
cols_to_mask <- c("full_name","gender","ethnicity")
col_index = which(colnames(patient_data) %in% cols_to_mask)
# backup original data
patient_data_org <- patient_data
# anonymize
patient_data[col_index] <- lapply(patient_data[col_index], anonymize)
