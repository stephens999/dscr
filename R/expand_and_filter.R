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

#' Comprehensively tabulate a phase of \code{dscr} execution.
#'
#' Depending on the phase of execution requested, starting from
#' \enumerate{
#'   \item the set of scenario-seed combinations (as specified
#'     in \code{dsc$scenarios}),
#'   \item the set of methods (as specified in
#'     \code{dsc$methods}), and
#'   \item the set of scores (as specified in
#'     \code{dsc$scores}),
#' }
#' compute (1), the Cartesian product of (1) and (2), or the Cartesian
#' product of (1), (2), and (3).
#'
#' @param dsc The \code{dscr} data structure to expand.
#' @param phase The phase of execution to tabulate, either
#'   'scenarios', 'scenarios_methods', or 'scenarios_methods_scores'.
#'
#' @return Data frame with columns \code{scenarioname}, \code{seed},
#'   \code{methodname} (if one of the latter two phases is requested),
#'   and \code{scorename} (if the last phase is selected).
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
