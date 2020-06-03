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
## Notes: Load in data. Select columns to anoynomize and run code in the main section
##
## ---------------------------

################################################################################

### Load Libs

################################################################################

library(digest)
library(RODBC)
#library(readr)

################################################################################

###
###                               Functions
###

################################################################################


# ------------------------------------------------------------------------------

#' Anonymize a vector.
#'
#' anonymizes a vector by first salting it with and then hashing it with hash. 
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
#' takes a vector and returns a salted version of it. The algorithm for salting a vector is:
#' Select a random sample of characters of length .n_chars from \.chars. Call this random sample .y.
#' Concatenate .y, the vector .x, and .y again in a vectorized fashion.
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


# ------------------------------------------------------------------------------

#' Anon Wrapper
#' 

wrap_anon = function(x,prefix = ""){
  return(paste(prefix,salt(anonymize(x), .n_chars = 5L),sep = "_"))
}

################################################################################

##########
##########                       Work Flow 
##########

################################################################################

# ------------------------------------------------------------------------------

# Load in data in desried format. Long / wide should not matter as the anoynmize
# functions will always map the same input to the same output.



# Load in data
#patient_data <- read.csv("patient_data.csv")
con <- odbcDriverConnect(connection="driver={SQL Server};server=SQL028-INFCT-L\\INST1;database=IC_STG;trusted_connection=yes;")
patient_data <- sqlQuery(con, "SELECT * FROM [IC_STG].[dbo].[CPE_pathways] ")





# Drop columns 
drop = c("PtName","NHSNo" ,"PtDoB","Tests Performed","Site" ,"AdmDT","DisDT","AdmitDateTime","CurrentLastFacility","EncounterType","Sex","DateOfBirth","DateOfDeath","CollectMinus7DT")
p2_data = patient_data[,!(names(patient_data) %in% drop)]


# Choose columns to anoynomize
#cols_to_mask <- c("PtNumber","Accnumber") # <-------- Replace columns to match your data
#col_index = which(colnames(p2_data) %in% cols_to_mask)

# backup original data
p2_data_org <- p2_data

# Gererate Mapping
df_mapping = data.frame("PtNumber" = p2_data$PtNumber, "Accnumber" = p2_data$Accnumber)


# Anonymize each specified column
#patient_data[col_index] <- lapply(patient_data[col_index], anonymize)
p2_data$PtNumber = wrap_anon(p2_data$PtNumber,"PtNum")
p2_data$Accnumber = wrap_anon(p2_data$Accnumber,"Acnum")

df_mapping$PtNumber_anon = p2_data$PtNumber
df_mapping$Accnumber_anon  = p2_data$Accnumber




# Save Data
write.csv(df_mapping,"data_mapping.csv")
saveRDS(p2_data, "anon_patient_data.rds") # <--- save as RDS
