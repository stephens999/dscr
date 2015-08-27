context("Expand and filter")

## Test fixture
fake_dsc <- list(
    scenarios = list(
        scen1 = list(
            name = 'scen1',
            seed = c(1, 2)),
        scen2 = list(
            name = 'scen2',
            seed = c(2))),
    methods = list(
        meth1 = NULL,
        meth2 = NULL),
    scores = list(
        score1 = NULL,
        score2 = NULL))

## Tests
test_that("expand_scenario creates a data frame with the right structure", {
    output <- expand_scenario(fake_dsc$scenarios$scen1)

    expect_is(output, "data.frame")
    expect_equal(length(output), 2)
    expect_equal(nrow(output), 2)
    expect_true('scenarioname' %in% names(output))
    expect_true('seed' %in% names(output))
})

test_that("expand_dsc checks the value of phase", {
    ## sic
    expect_error(expand_dsc(fake_dsc, 'scnearios'))
})

test_that("expand_dsc expands scenarios", {
    output <- expand_dsc(fake_dsc, 'scenarios')

    expect_is(output, "data.frame")
    expect_equal(length(output), 2)
    expect_equal(nrow(output), 3)
    expect_true('scenarioname' %in% names(output))
    expect_true('seed' %in% names(output))
})

test_that("expand_dsc expands scenarios/methods", {
    output <- expand_dsc(fake_dsc, 'scenarios_methods')

    expect_is(output, "data.frame")
    expect_equal(length(output), 3)
    expect_equal(nrow(output), 6)
    expect_true('scenarioname' %in% names(output))
    expect_true('seed' %in% names(output))
    expect_true('methodname' %in% names(output))
})

test_that("expand_dsc expands scenarios/methods/scores", {
    output <- expand_dsc(fake_dsc, 'scenarios_methods_scores')

    expect_is(output, "data.frame")
    expect_equal(length(output), 4)
    expect_equal(nrow(output), 12)
    expect_true('scenarioname' %in% names(output))
    expect_true('seed' %in% names(output))
    expect_true('methodname' %in% names(output))
    expect_true('scorename' %in% names(output))
})
