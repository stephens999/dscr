#' Tabulate a \code{dscr} scenario.
#'
#' Given a scenario, return a data frame tabulating a scenario's name
#' in one column and its set of seeds in another column.
#'
#' @param scenario Scenario to be expanded, with \code{scenario$name}
#'   giving a string for the scenario's name and \code{scenario$seed}
#'   giving an integer vector of random seeds.
#'
#' @return Data frame with columns \code{scenarioname} and
#'   \code{seed}. The column \code{scenarioname} is coerced to a
#'   character vector.
expand_scenario <- function(scenario) {
    return(data.frame(scenarioname = scenario$name,
                      seed = scenario$seed,
                      stringsAsFactors = FALSE))
}

# @title Comprehensively tabulate a phase of \code{dscr} execution.
#
# @description Depending on the phase of execution requested, starting
# from
# \enumerate{
#   \item the set of scenario-seed combinations (as specified
#     in \code{dsc$scenarios}),
#   \item the set of methods (as specified in
#     \code{dsc$methods}), and
#   \item the set of scores (as specified in
#     \code{dsc$scores}),
# }
# compute (1), the Cartesian product of (1) and (2), or the Cartesian
# product of (1), (2), and (3).
#
# @param dsc The \code{dscr} data structure to expand.
# 
# @param phase The phase of execution to tabulate, either
#   'scenarios', 'scenarios_methods', or 'scenarios_methods_scores'.
#
# @return Data frame with columns \code{scenarioname}, \code{seed},
#   \code{methodname} (if one of the latter two phases is requested),
#   and \code{scorename} (if the last phase is selected).
#
#' @importFrom assertthat assert_that
#' @importFrom plyr ldply
expand_dsc <- function(dsc, phase) {
    assert_that(phase %in% c('scenarios',
                             'scenarios_methods',
                             'scenarios_methods_scores'))

    ## scenarios stage
    result <- plyr::ldply(dsc$scenarios,
                          expand_scenario,
                          .id = 'scenarioname')

    ## methods stage
    if (grepl('methods', phase)) {
        result <- merge(result, data.frame(methodname = names(dsc$methods),
                                           stringsAsFactors = FALSE))
    }

    ## scores stage
    if (grepl('scores', phase)) {
        result <- merge(result, data.frame(scorename = names(dsc$scores),
                                           stringsAsFactors = FALSE))
    }

    return(result)
}

# Apply multiple filters to a data frame.
#
# Build and execute a \code{dplyr::filter} call against a data frame
# with an arbitrary number of filtering clauses, some of which can be
# inactive by being set to \code{NULL}. For example,
# \code{multiple_filter(df, some_col = c(1, 2), other_col = NULL)}
# returns the value of \code{dplyr::filter(df, some_col \%in\% c(1,
# 2))}. Return the data frame unchanged if there are no active
# filters.
#
# @param df Data frame to filter.
# 
# @param ... Set of filters to apply to the data frame. The names of
#     the list elements correspond to column names in \code{df},
#     while the values of the list elements correspond to the values
#     that the column will be restricted to.
#
#' @importFrom assertthat assert_that
multiple_filter <- function(df, ...) {
    
    # Basic sanity check.
    filter_list <- list(...)
    assert_that(all(names(filter_list) %in% names(df)))

    active_filter_list <- Filter(Negate(is.null), filter_list)

    if (length(active_filter_list) == 0) {
        return(df)
    } else {
        
        # We execute the filter call against a custom environment to
        # avoid scoping issues.
        filter_env <- new.env()
        filter_env$df <- df
        filter_env$arg_list <- active_filter_list

        clauses <- lapply(names(active_filter_list),
                          function(name) paste0(name, ' %in% arg_list$', name))
        clauses_text <- paste(clauses, collapse = ', ')
        filter_call <- paste0('dplyr::filter(df, ', clauses_text, ')')

        return(eval(parse(text = filter_call), filter_env))
    }
}
