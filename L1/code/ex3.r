#!/usr/bin/Rscript

CUMUL_A = "ex3-class-a.pdf"
CUMUL_B = "ex3-class-b.pdf"
CUMUL_EG = "ex3-example.pdf"
QQPLOT = "ex3-qqplot.pdf"

# ---------------------------------------------------------------------
# Calculates and returns an array representing the empiric cumulative
# function for the given data.
#
# If data is sorted and contains unique values, then the returned
# array (say, "emp_cumul") is such that emp_cumul[i] holds the empiric
# cumulative function value for data[i].
#
# Arguments:
#   data: Data array to be used for determining the array.
#
# Returns:
#   An array representing the empiric cumulative function.
emp_cumulative = function(data) {
    data_sorted = sort(data)
    unique_data = unique(data_sorted)

    # Calculate cumulative function values
    step = 1.0/length(data_sorted)
    cumul_func = c(0.0)
    cumul_prob = 0.0
    for (x in unique_data) {
        repeats = sum(data_sorted == x)
        cumul_prob = cumul_prob + step*repeats
        cumul_func = c(cumul_func, cumul_prob)
    }

    # Calculate middle point in each step of the cumulative function,
    # resulting in the empiric cumulative values
    emp_cumul = c()
    for (i in 1:length(cumul_func) - 1) {
        emp_cumul[i] = (cumul_func[i] + cumul_func[i+1])/2.0
    }

    return (emp_cumul)
}

# ---------------------------------------------------------------------
# Plots the cumulative and empiric cumulative function in a single graph.
#
# Arguments:
#   data:      A data array containing values to be used.
#   emp_cumul: An array of values representing the empiric cumulative
#              function for the given data.
#   filename:  Filename to save the created graph.
#   x_label:   Label of the graph on the x axis.
#
# Returns:
#   An array representing the empiric cumulative function for unique
#   values of the specified data.
plot_cumul = function(data, emp_cumul, filename, x_label) {
    unique_data = unique(sort(data))
    pdf(filename)

    # Cumulative
    plot(ecdf(data),
         main="",
         xlab=x_label,
         ylab="F(x) (acumulada)")

    # Empiric cumulative
    lines(unique_data, emp_cumul, col="red")
    points(unique_data, emp_cumul, pch=21, cex=0.7, col="red", bg="red")

    # Change axis interval and step values
    axis(1, at=seq(0, 8, by=1))
    axis(2, at=seq(0.0, 1.0, by=0.1))

    invisible(dev.off())
    paste("Cumulative function graph saved as", filename)
}

# ---------------------------------------------------------------------
# Calculates a percentile for a given order using the empiric cumulative
# function.
#
# Arguments:
#   p:         Order to be used, determines the numeric value q such that
#              p% of the data is <= q.
#   data:      Data previously used for obtaining the empiric cumulative
#              function.
#   emp_cumul: An array of values representing the empiric cumulative
#              function for the given data.
#
# Returns:
#   The percentile q such that p% of the data is <= q.
percentile = function(p, data, emp_cumul) {
    unique_data = unique(sort(data))

    # Supress +inf return value, which means that p is greater
    # than all values in emp_cumul
    i_next = suppressWarnings(min(which(emp_cumul > p)))
    i = i_next - 1

    # Case p < p1
    if (i == 0)
        return (unique_data[1])
    # Case p > pn
    if (is.infinite(i))
        return (unique_data[length(unique_data)])

    # Case p == Fe(x(i))
    if (p == emp_cumul[i])
        return (unique_data[i])

    # Case pi < p < pi+1
    fi = (p - emp_cumul[i])/(emp_cumul[i+1] - emp_cumul[i])
    result = (1 - fi)*unique_data[i] + fi*unique_data[i+1]
    return (result)
}

# ---------------------------------------------------------------------
# (A)
cat("[Item a]\n")

dfa = read.csv("../data/ex3-class-a.csv", sep=";", fileEncoding="UTF-8")
dfb = read.csv("../data/ex3-class-b.csv", sep=";", fileEncoding="UTF-8")

emp_cumul_a = emp_cumulative(dfa$Notas)
emp_cumul_b = emp_cumulative(dfb$Notas)

plot_cumul(dfa$Notas, emp_cumul_a, CUMUL_A, "Notas dos alunos da turma A")
plot_cumul(dfb$Notas, emp_cumul_b, CUMUL_B, "Notas dos alunos da turma B")

# Test a few values
paste("Percentile for p = 0.0 in class A:", percentile(0.0, dfa$Notas, emp_cumul_a))
paste("Percentile for p = 0.3 in class A:", percentile(0.3, dfa$Notas, emp_cumul_a))
paste("Percentile for p = 0.5 in class A:", percentile(0.5, dfa$Notas, emp_cumul_a))
paste("Percentile for p = 1.0 in class A:", percentile(1.0, dfa$Notas, emp_cumul_a))

# ---------------------------------------------------------------------
# (B)
cat("\n[Item b]\n")

pdf(QQPLOT)
qqplot(dfa$Notas, dfb$Notas,
       main="",
       xlab="Notas da turma A",
       ylab="Notas da turma B",
       xlim=c(2, 8),
       ylim=c(2, 8))
lines(c(0, 10), c(0, 10), lty=2, col="dimgray")  # Line y = x
invisible(dev.off())
paste("Quantiles x quantiles graph saved as", QQPLOT)

# ---------------------------------------------------------------------
# (C)
cat("[Item c]\n")

df_eg = read.csv("../data/ex3-example.csv", sep=";", fileEncoding="UTF-8")
emp_cumul_eg = emp_cumulative(df_eg$Notas)
plot_cumul(df_eg$Notas, emp_cumul_b, CUMUL_EG, "Notas dos alunos da turma C")

# Test a few values
paste("Percentile for p = 0.25 in class C:",
      percentile(0.25, df_eg$Notas, emp_cumul_eg))
paste("Percentile for p = 0.8 in class C:",
      percentile(0.8, df_eg$Notas, emp_cumul_eg))
