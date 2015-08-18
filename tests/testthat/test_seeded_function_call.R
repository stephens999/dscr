context("Seeded function call")

test_that("seeded_function_call sets the random seed and calls a function", {
    set.seed(42)
    result <- rnorm(1)
    expect_equal(seeded_function_call(rnorm, 42, list(1)), result)
})

test_that("seeded_function_call merges disk_params into the argument list", {
    my_subtract <- function(whole, part) whole - part
    whole_file_name <- tempfile(fileext = '.RDS')
    saveRDS(5, whole_file_name)
    my_disk_params <- list(variable_names = 'whole', file_names = whole_file_name)
    expect_equal(seeded_function_call(my_subtract, 0, list(part = 3),
                                      my_disk_params), 2)
})

## TODO: Add test coverage for fault tolerance under BatchJobs control
## once the desired behavior is defined.
