#' Identify numeric values from a character vector
#'
#' Identify all numbers from a character vector (e.g. ID variables). Can return
#' indices of these numbers, their values, or boolean indicators for whether
#' each value is numeric.
#' @param x A character vector containing possible numeric values that need
#'   identified.
#' @param response The type of values to be returned. One of "index" (default),
#'   "value", or "boolean".
#' @return If \code{response == "index"}, the indices of all numeric values. If
#'   \code{response == "value"}, the values of these numeric values, with
#'   non-numeric values replaced by \code{NA}. If \code{response == "boolean"},
#'   boolean indicators for whether each value in \code{x} is numeric or string.
find.numeric <- function(x, response = c("index", "value", "boolean")) {
    if(missing(response)) {
        response <- "index"
    } else if(!response %in% c("index", "value", "boolean")) {
        stop('response must be one of "index", "value", or "boolean"')
    }

    # convert character vector to string, suppress warnings, keep all non-numeric cases as NA
    x <- suppressWarnings(as.numeric(as.character(x)))

    # if response is "index" (default) returns index of all numeric values
    if(response == "index") {
        which(!is.na(x))
        # if not, either return boolean indicators of all numeric values
    } else if(response == "boolean") {
        is.na(x)
        # or actual values converted to numeric
    } else if(response == "value") {
        x
    }
}

#' Identify \code{NaN} in a dataframe.
#'
#' Identify cells with \code{NaN} in a data frame. Improve on the defeault
#' \code{is.nan()} function, which only works on vectors, by allowing
#' data frames as input.
#'
#' @param x A data frame to be tested.
#' @return A matrix of the same dimension as \code{x}, with TRUE/FALSE values
#' for whether each cell in the original data frame is a number or not.
#' \code{NaN} means 'Not a Number'.
#' @export
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#' "Fill down the blank"
#'
#' For a pre-sorted vector, fill each missing value with that of the preceding
#' element.
#'
#' \code{fill.blanks()} is a wrapper around the \code{zoo::na.locf()} function
#' in the package \code{zoo}. It can handle both missing values from all data
#' types including character vectors where missing values are often encoded as
#' empty strings.
#'
#' @param x A pre-sorted vector that may contain \code{NA} values or empty strings.
#' @return A vector of the same length and type as \code{x}, with all \code{NA}
#' values or empty strings replaced by the value of the preceding element in the
#' vector.
fill.blanks <- function(x) {
    x[x==""] <- NA
    x <- zoo::na.locf(x)
}

#' Negative-log transformation
#'
#' An analogue to the log transformation for negative values.
#'
#' \code{neglog(x)} is calculated such that
#' \eqn{neglog(x) = sign(x) \times \log |x + offset|}{neglog(x) = sign(x) * \log(|x + offset|)}.
#' The offset ensures that no \code{-Inf} value is returned.
#' The default offset is 0.5.
#'
#' @param x a numeric or complex vector
#' @param base a positive or complex number: the base with respect
#' to which logarithms are computed. Defaults to \eqn{e=}\code{exp(1)}
#' @param offset a number specifying an offset to avoid \code{neglog(x)}
#' returning \code{-Inf}. Defaults to 0.5
#' @return A vector of the same length as \code{x} containing the transformed
#' values.
neglog <- function(x, base = exp(1), offset = 0.5) sign(x) * (log(abs(x + offset), base))

#' Best string match using string distance
#'
#' Find the best match for one character string in a vector of character strings
#' using Levenshtein distance.
#'
#' The function \code{bestmatch} is a quick wrapper around the \code{stringdist::amatch()}
#' function using \code{method = "lv"} and \code{maxDist = 1}
#'
#' @param string elements to be approximately matched: will be coerced to
#' \code{character} unless it is a list consisting of \code{integer} vectors.
#' Identical to the \code{x} parameter in \code{stringdist::amatch()}
#' @param stringVector a vector of strings to be used as lookup table for
#' matching. Will be coerced to \code{character} unless it is a list consisting
#'  of \code{integer} vectors. Identical to the \code{table} parameter in
#'  \code{stringdist::amatch()}
#' @return a vector of the same length as \code{string} containing the position
#' of the closest match of each element of \code{string} in \code{stringVector}.
#' Identical to the result returned by \code{stringdist::amatch()}.
bestmatch <- function(string, stringVector){
    stringdist::amatch(string, stringVector, method="lv", maxDist=1)
}

#' Extract year from date
#'
#' Extract the year from a date variable in multiple formats
#'
#' \code{year} can handle dates in the following formats: date in
#' d/m/Y, year without date, and date as number of days since 01/01/1900.
#'
#' @param date a \code{character} object or vector indicating dates in
#' one of three formats: exact date in d/m/Y, exact year without date,
#' and date as number of days since 01/01/1900.
#' @return The year of the date in \code{date}
year <- function(date) {
    unname(sapply(date, function(d) {
        if(grepl("/", d)==TRUE) {
            date <- as.Date(stringr::str_trim(d), "%d/%m/%Y")
        } else if(nchar(stringr::str_trim(d))==4){
            date <- as.Date(stringr::str_trim(d), "%Y")
        } else {
            date <- as.Date(as.numeric(d), origin="1900-01-01")
        }
        year <- as.numeric(substr(date,1,4))
        return(year)
    }))
}

# Clean strings using a lookup table
# lookup.clean <- function(x, dictionary){
#     sapply(1:nrow(dictionary), function(i){
#         org <- dictionary$original[i] #table must contain original and fixed column
#         fix <- dictionary$fixed[i]
#
#         x[as.character(parse(text=paste0("'", x, "'"))) == org] <- fix
#         return(x)
#     })
# }
