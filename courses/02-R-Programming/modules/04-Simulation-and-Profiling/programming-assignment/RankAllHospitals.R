library(dplyr)

# RankAllHospitals: Find the hospital with the nth lowest 30-day mortality rate
# for a given outcome across all U.S. states.
#
# Arguments:
#   outcome - one of "heart attack", "heart failure", or "pneumonia"
#   num     - ranking to return: a positive integer, "best" (1st), or "worst" (last).
#             If num exceeds the number of hospitals with data in a state, that
#             state's entry will be NA.
#
# Returns:
#   A data frame with one row per state and two columns:
#     - hospital: name of the hospital at the requested rank (or NA)
#     - state:    2-character state abbreviation
#   Ties in mortality rate are broken alphabetically by hospital name.
#
# Errors:
#   Calls stop("invalid outcome") if outcome is not one of the three valid values.
#
# NOTE: This function does NOT call RankHospital internally — it processes all
# states in a single pipeline for efficiency via group_by().
RankAllHospitals <- function(outcome, num) {
    # colClass = "character" prevents R from silently coercing columns like
    # "Not Available" into NAs before we've had a chance to filter on them.
    hospitals_data <- read.csv("outcome-of-care-measures.csv", colClass = "character")

    # Use a character vector (not a list) for a set of scalar string values.
    OUTCOMES <- c("heart attack", "heart failure", "pneumonia")

    # %in% (aliased as is.element) checks membership in a set.
    # stop() with the exact message is required by the assignment spec.
    if (!is.element(outcome, OUTCOMES)) stop("invalid outcome")

    # Map outcome names to their corresponding column names in the dataset.
    # A named list acts like a dictionary: COL_MAP[["heart attack"]] returns
    # the column name string, which we later pass to .data[[]] inside dplyr.
    COL_MAP <- list(
        "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )

    hospitals_data %>%
        # Convert the outcome column to numeric; "Not Available" becomes NA.
        mutate(Rate = as.numeric(.data[[COL_MAP[[outcome]]]])) %>%
        # Exclude hospitals with no data for this outcome.
        filter(!is.na(Rate)) %>%
        # Sort globally first so each group inherits the correct order.
        # Hospital.Name breaks ties alphabetically as required by the spec.
        arrange(Rate, Hospital.Name) %>%
        # group_by() makes subsequent verbs operate per state independently,
        # so slice/summarise see each state's hospitals as a separate dataset.
        group_by(State) %>%
        # summarise() collapses each group to a single row.
        # A {} block allows multi-line logic per group:
        #   1. Resolve num to a row index within this group.
        #      n() gives the current group size — critical for "worst" since
        #      each state has a different number of hospitals.
        #   2. If idx exceeds the group size return NA, otherwise index into
        #      Hospital.Name directly. We check BEFORE indexing to avoid an
        #      out-of-bounds error; slice() would silently drop the group
        #      instead of returning NA, which is why we use summarise here.
        summarise(hospital = {
            idx <- if (num == "best") 1 else if (num == "worst") n() else as.integer(num)
            if (idx > n()) NA_character_ else Hospital.Name[idx]
        }) %>%
        # Rename State to state to match the required output spec, and reorder
        # columns so hospital comes first.
        select(hospital, state = State)
}

# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------

expect_error <- function(expr, expected_message) {
    result <- tryCatch(expr, error = function(e) conditionMessage(e))
    stopifnot(result == expected_message)
}

# Spot-check known values
stopifnot(RankAllHospitals("heart attack", 20)[
    RankAllHospitals("heart attack", 20)$state == "TX", "hospital"
] == "BEAUMONT MEDICAL CENTER-PORT ARTHUR")

# "worst" should return a result for every state (no dropped rows)
result_worst <- RankAllHospitals("pneumonia", "worst")
stopifnot(nrow(result_worst) > 0)
stopifnot(all(c("hospital", "state") %in% colnames(result_worst)))

# num larger than hospital count should produce NA
result_na <- RankAllHospitals("heart attack", 5000)
stopifnot(any(is.na(result_na$hospital)))

expect_error(RankAllHospitals("hert attack", 1), "invalid outcome")

message("All tests passed!")
