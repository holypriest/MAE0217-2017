#!/usr/bin/Rscript

GRADE_A = c(5, 7, 3, 5, 4, 6)
GRADE_B = c(2, 5, 4, 3, 6)
GRADE_C = c(2, 2, 2, 7, 8, 3, 4)
GRADE_TOTAL = c(GRADE_A, GRADE_B, GRADE_C)

# Returns the populational variance of the given data array
varpop = function(data) {
    n = length(data)
    return ((n-1)/n * var(data))
}

# ---------------------------------------------------------------------
# Analysis of quantitative (grade) x qualitative (class) variables

var_total = varpop(GRADE_TOTAL)

var_avg = length(GRADE_A) * varpop(GRADE_A) +
          length(GRADE_B) * varpop(GRADE_B) +
          length(GRADE_C) * varpop(GRADE_C)
var_avg = var_avg / length(GRADE_TOTAL)

r_squared = 1 - var_avg/var_total

paste("var_total =", round(var_total, 3))
paste("var_avg =", round(var_avg, 3))
paste("R² =", round(r_squared, 3))
