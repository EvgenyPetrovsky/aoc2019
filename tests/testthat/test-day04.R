test_that("makesequence returns sequence of numbers by low and high bound", {
    s <- day04_makesequence(1000000, 2000000)
    expect_equal(
        length(day04_makesequence(1000000, 2000000)),
        2000000 - 1000000 + 1)
    expect_equal(
        typeof(day04_makesequence(100, 200)),
        "character")
})

test_that("check that digits go in ascending order", {
    expect_equal(day04_filterasc("01234"), TRUE)
    expect_equal(day04_filterasc("00000"), TRUE)
    expect_equal(day04_filterasc("01210"), FALSE)
    expect_equal(day04_filterasc("01122"), TRUE)
})

test_that("check that number has adjacent digits", {
    expect_equal(day04_filteradj("01234"), FALSE)
    expect_equal(day04_filteradj("00000"), TRUE)
    expect_equal(day04_filteradj("01210"), FALSE)
    expect_equal(day04_filteradj("01122"), TRUE)
})