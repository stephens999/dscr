# Perform a seeded function call.
#
# Set a random seed and call a given function with a named parameter
# list, part of which can be reconstituted from serialized RDS
# objects.
#
# This is meant to be the abstract unit of execution that can be
# passed to a \code{batchMap} call to the \code{BatchJobs} package
# but that is flexible enough to handle the heterogeneous needs of a
# phase of \code{dscr}.
#
# @param fxn_to_call Function to execute.
# @param seed Integer random seed, passed transparently to
#   \code{set.seed}.
# @param mem_params Named parameter list of objects loaded at
#   invocation time to pass to function.
# @param disk_params Optional data frame of input objects to read
#   from disk. \code{disk_params$file_names} should give the paths to
#   the input files. \code{disk_params$variable_names} should give the
#   (character) names of the variable names expected by
#   \code{fxn_to_call}, which will be parsed to symbols, bound to the
#   loaded objects, and merged with \code{mem_params}.
#
# @return Return value of \code{fxn_to_call}.
# 
seeded_function_call <- function(fxn_to_call, seed, mem_params,
                                 disk_params = NULL) {
    if (!is.null(disk_params)) {
        
        ## Coerce factors to characters.
        disk_params[] <- lapply(disk_params, as.character)

        loaded_params <- lapply(disk_params$file_names, readRDS)
        Map(assign, disk_params$variable_names, loaded_params)
    } else {
        loaded_params <- NULL
    }

    set.seed(seed)

    return(try(do.call(fxn_to_call, c(mem_params, loaded_params))))
}
