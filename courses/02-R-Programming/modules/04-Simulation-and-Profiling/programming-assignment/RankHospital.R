library(dplyr)

# RankHospital: Find the hospital with the nth lowest 30-day mortality rate
# for a given outcome in a given U.S. state.
#
# Arguments:
#   state   - 2-character abbreviated state name (e.g. "TX", "MD")
#   outcome - one of "heart attack", "heart failure", or "pneumonia"
#   num     - ranking to return: a positive integer, "best" (1st), or "worst" (last).
#             If num exceeds the number of hospitals with data, returns NA.
#
# Returns:
#   A character string with the hospital name at the requested rank.
#   Ties in mortality rate are broken alphabetically by hospital name.
#
# Errors:
#   Calls stop("invalid state")   if state is not found in the data.
#   Calls stop("invalid outcome") if outcome is not one of the three valid values.
RankHospital <- function(state, outcome, num) {
    # colClass = "character" prevents R from silently coercing columns like
    # "Not Available" into NAs before we've had a chance to filter on them.
    hospitals_data <- read.csv("outcome-of-care-measures.csv", colClass = "character")

    # Derive valid states from the data itself rather than hardcoding them —
    # more robust if the dataset is updated.
    STATES <- unique(hospitals_data$State)

    # Use a character vector (not a list) for a set of scalar string values.
    OUTCOMES <- c("heart attack", "heart failure", "pneumonia")

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

    # Build the full ranked list for the given state and outcome:
    #   1. filter()  — restrict to the requested state
    #   2. mutate()  — convert the outcome column to numeric; "Not Available"
    #                  becomes NA automatically via as.numeric()
    #   3. filter()  — drop hospitals with no data for this outcome
    #   4. arrange() — sort by rate ascending; Hospital.Name breaks ties
    #                  alphabetically as required by the spec
    ranked <- hospitals_data %>%
        filter(State == state) %>%
        mutate(Rate = as.numeric(.data[[COL_MAP[[outcome]]]])) %>%
        filter(!is.na(Rate)) %>%
        arrange(Rate, Hospital.Name)

    # Resolve num to a row index.
    # "best"  → rank 1 (lowest mortality rate)
    # "worst" → last row after sorting (highest mortality rate)
    # integer → used as-is
    idx <- if (num == "best") 1
           else if (num == "worst") nrow(ranked)
           else num

    # If the requested rank exceeds the number of hospitals with data, return NA.
    if (idx > nrow(ranked)) return(NA)

    # slice() picks the row at position idx; pull() extracts the column as a
    # plain character vector rather than a single-column data frame.
    ranked %>% slice(idx) %>% pull(Hospital.Name)
}

# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------

expect_error <- function(expr, expected_message) {
    result <- tryCatch(expr, error = function(e) conditionMessage(e))
    stopifnot(result == expected_message)
}

stopifnot(RankHospital("TX", "heart failure", 4)      == "DETAR HOSPITAL NAVARRO")
stopifnot(RankHospital("MD", "heart attack", "worst") == "HARFORD MEMORIAL HOSPITAL")
stopifnot(is.na(RankHospital("MN", "heart attack", 5000)))

expect_error(RankHospital("BB", "heart attack", 1), "invalid state")
expect_error(RankHospital("NY", "hert attack", 1),  "invalid outcome")

message("All tests passed!")
