test_that("makesequence returns sequence of numbers by low and high bound", {
    s <- day04_makesequence(1000000, 2000000)
    expect_equal(
        length(day04_makesequence(1000000, 2000000)),
        2000000 - 1000000 + 1)
    expect_equal(
        typeof(day04_makesequence(100, 200)),
        "character")
})

test_that("digits go in ascending order", {
    expect_equal(day04_filterasc("01234"), TRUE)
    expect_equal(day04_filterasc("00000"), TRUE)
    expect_equal(day04_filterasc("01210"), FALSE)
    expect_equal(day04_filterasc("01122"), TRUE)
})

test_that("number has adjacent digits", {
    expect_equal(day04_filteradj("01234"), FALSE)
    expect_equal(day04_filteradj("00000"), TRUE)
    expect_equal(day04_filteradj("01210"), FALSE)
    expect_equal(day04_filteradj("01122"), TRUE)
})

test_that("the two adjacent matching digits are not part of a larger group of matching digits", {
    expect_equal(day04_filteradj2("000000"), FALSE)
    expect_equal(day04_filteradj2("000123"), FALSE)
    expect_equal(day04_filteradj2("000001"), FALSE)
    expect_equal(day04_filteradj2("001000"), TRUE)
    expect_equal(day04_filteradj2("001113"), TRUE)
    expect_equal(day04_filteradj2("001234"), TRUE)
    expect_equal(day04_filteradj2("001233"), TRUE)
    expect_equal(day04_filteradj2("666679"), FALSE)

    expect_equal(day04_filteradj2("112233"), TRUE)
    expect_equal(day04_filteradj2("123444"), FALSE)
    expect_equal(day04_filteradj2("111122"), TRUE)

})
