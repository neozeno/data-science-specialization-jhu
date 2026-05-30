library(dplyr)

# FindBestHospital: Find the hospital with the lowest 30-day mortality rate
# for a given outcome in a given U.S. state.
#
# Arguments:
#   state   - 2-character abbreviated state name (e.g. "TX", "MD")
#   outcome - one of "heart attack", "heart failure", or "pneumonia"
#
# Returns:
#   A character string with the hospital name. Ties are broken alphabetically.
#
# Errors:
#   Calls stop("invalid state")   if state is not found in the data.
#   Calls stop("invalid outcome") if outcome is not one of the three valid values.
FindBestHospital <- function(state, outcome) {
    # Read outcome data
    # colClass = "character" prevents R from silently coercing columns like
    # "Not Available" into NAs before we've had a chance to filter on them.
    hospitals_data <- read.csv("outcome-of-care-measures.csv", colClass = "character")

    # Derive valid states from the data itself rather than hardcoding them —
    # more robust if the dataset is updated.
    STATES <- unique(hospitals_data$State)

    # Use a character vector (not a list) for a set of scalar string values.
    OUTCOMES <- c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    # %in% (aliased as is.element) checks membership in a set.
    # stop() with the exact message is required by the assignment spec.
    if (!is.element(state, STATES)) stop("invalid state")
    if (!is.element(outcome, OUTCOMES)) stop("invalid outcome")

    # Map outcome names to their corresponding column names in the dataset.
    # A named list acts like a dictionary: COL_MAP[["heart attack"]] returns
    # the column name string, which we later pass to .data[[]] inside dplyr.
    COL_MAP <- list(
        "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )

    # Return hospital name in that state with lowest 30-day death rate.
    # Pipeline steps:
    #   1. filter()  — restrict to the requested state
    #   2. mutate()  — convert the outcome column to numeric; "Not Available"
    #                  strings become NA automatically via as.numeric()
    #   3. filter()  — drop rows with no data for this outcome
    #   4. arrange() — sort by rate ascending; Hospital.Name breaks ties
    #                  alphabetically as required by the spec
    #   5. slice(1)  — keep the top row
    #   6. pull()    — extract the hospital name as a plain character vector
    best_hospital <- hospitals_data %>%
        filter(State == state) %>%
        mutate(rate = as.numeric(.data[[COL_MAP[[outcome]]]])) %>%
        filter(!is.na(rate)) %>%
        arrange(rate, Hospital.Name) %>%
        slice(1) %>%
        pull(Hospital.Name)

    best_hospital
}

# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------
# R doesn't require a testing framework for simple scripts — stopifnot() is
# a lightweight built-in that throws an error if any condition is FALSE,
# making it easy to catch regressions.

# Helper to test that a call throws an error with a specific message.
# tryCatch() is R's try/catch: it lets you intercept errors and inspect them
# instead of letting them crash the script.
expect_error <- function(expr, expected_message) {
    result <- tryCatch(expr, error = function(e) conditionMessage(e))
    stopifnot(result == expected_message)
}

# Valid cases
stopifnot(FindBestHospital("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER")
stopifnot(FindBestHospital("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER")
stopifnot(FindBestHospital("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE")
stopifnot(FindBestHospital("MD", "pneumonia")    == "GREATER BALTIMORE MEDICAL CENTER")

# Invalid argument cases
expect_error(FindBestHospital("BB", "heart attack"), "invalid state")
expect_error(FindBestHospital("NY", "hert attack"), "invalid outcome")

message("All tests passed!")
