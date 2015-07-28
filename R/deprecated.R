# Non-snake-case-named legacy functions that are now deprecated.

# NB: These are not generated via a straightforward closure so that
# roxygen2 can have the hooks it needs to generate the documentation
# and namespace. Feel free to refactor if you can figure out how to do
# so and still get roxygen2 to do its work.

#' Deprecated alias to \code{\link{add_method}}.
#'
#' @export
addMethod <- function(...) {
    warning('The function addMethod is deprecated. Use add_method instead.')
    return(add_method(...))
}

#' Deprecated alias to \code{\link{add_output_parser}}.
#'
#' @export
addOutputParser <- function(...) {
    warning('The function addOutputParser is deprecated. Use add_output_parser instead.')
    return(add_output_parser(...))
}

#' Deprecated alias to \code{\link{add_scenario}}.
#'
#' @export
addScenario <- function(...) {
    warning('The function addScenario is deprecated. Use add_scenario instead.')
    return(add_scenario(...))
}

#' Deprecated alias to \code{\link{add_score}}.
#'
#' @export
addScore <- function(...) {
    warning('The function addScore is deprecated. Use add_score instead.')
    return(add_score(...))
}

#' Deprecated alias to \code{\link{dsc_to_be}}.
#'
#' @export
dsc2BE <- function(...) {
    warning('The function dsc2BE is deprecated. Use dsc_to_be instead.')
    return(dsc_to_be(...))
}

#' Deprecated alias to \code{\link{get_output_types}}.
#'
#' @export
getOutputtypes <- function(...) {
    warning('The function getOutputtypes is deprecated. Use get_output_types instead.')
    return(get_output_types(...))
}

#' Deprecated alias to \code{\link{list_methods}}.
#'
#' @export
listMethods <- function(...) {
    warning('The function listMethods is deprecated. Use list_methods instead.')
    return(list_methods(...))
}

#' Deprecated alias to \code{\link{list_scenarios}}.
#'
#' @export
listScenarios <- function(...) {
    warning('The function listScenarios is deprecated. Use list_scenarios instead.')
    return(list_scenarios(...))
}

#' Deprecated alias to \code{\link{list_scores}}.
#'
#' @export
listScores <- function(...) {
    warning('The function listScores is deprecated. Use list_scores instead.')
    return(list_scores(...))
}

#' Deprecated alias to \code{\link{list_output_parsers}}.
#'
#' @export
listoutputParsers <- function(...) {
    warning('The function listoutputParsers is deprecated. Use list_output_parsers instead.')
    return(list_output_parsers(...))
}

#' Deprecated alias to \code{\link{load_example}}.
#'
#' @export
loadExample <- function(...) {
    warning('The function loadExample is deprecated. Use load_example instead.')
    return(load_example(...))
}

#' Deprecated alias to \code{\link{new_dsc}}.
#'
#' @export
new.dsc <- function(...) {
    warning('The function new.dsc is deprecated. Use new_dsc instead.')
    return(new_dsc(...))
}

#' Deprecated alias to \code{\link{source_dir}}.
#'
#' @export
sourceDir <- function(...) {
    warning('The function sourceDir is deprecated. Use source_dir instead.')
    return(source_dir(...))
}
