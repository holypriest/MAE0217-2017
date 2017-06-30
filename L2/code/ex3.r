#!/usr/bin/Rscript

# Minimum wage value (in R$)
MW = 788.00

# Alpha value for Chi-Squared test
CHISQ_ALPHA = 0.05

df = read.csv("../data/ex3.csv", sep=";", fileEncoding="UTF-8")

# ---------------------------------------------------------------------
# Item a

cat("[Item a]\n")

filter = df[,c("consumo_per_capita", "TIPODEDOMICILIO")]
house = filter$consumo_per_capita[filter$TIPODEDOMICILIO == 1]
apt = filter$consumo_per_capita[filter$TIPODEDOMICILIO == 2]
room = filter$consumo_per_capita[filter$TIPODEDOMICILIO == 3]

# Create a table to check values
summary = rbind(as.numeric(c(length(house), mean(house), sd(house))),
                as.numeric(c(length(apt), mean(apt), sd(apt))),
                as.numeric(c(length(room), mean(room), sd(room))))

rownames(summary) = c("Casa", "Apartamento", "Cômodo")
colnames(summary) = c("N", "Média", "Desvio Padrão")
print(round(summary, 2), print.gap=3, quote=F)

# Calculate R² association between consumption and residence
var_total = var(filter$consumo_per_capita)

var_avg = length(house) * var(house) +
          length(apt) * var(apt) +
          length(room) * var(room)
var_avg = var_avg / length(filter$consumo_per_capita)

r_squared = 1 - var_avg/var_total

paste("var_total =", round(var_total, 3))
paste("var_avg =", round(var_avg, 3))
paste("R² =", round(r_squared, 4))

# ---------------------------------------------------------------------
# Item b

cat("\n[Item b]\n")

filter = df[,c("RENDA_PER_CAPITA_MENSAL", "TIPODEDOMICILIO")]
filter["renda_cat"] = 0  # Add new column with all values being 0

# Sort income based on minimum wage intervals
class_1 = filter$RENDA_PER_CAPITA_MENSAL <= MW
class_2 = filter$RENDA_PER_CAPITA_MENSAL > MW &
          filter$RENDA_PER_CAPITA_MENSAL <= 2 * MW
class_3 = filter$RENDA_PER_CAPITA_MENSAL > 2 * MW &
          filter$RENDA_PER_CAPITA_MENSAL <= 4 * MW
class_4 = filter$RENDA_PER_CAPITA_MENSAL > 4 * MW

filter$renda_cat[class_1] = 1
filter$renda_cat[class_2] = 2
filter$renda_cat[class_3] = 3
filter$renda_cat[class_4] = 4

# Create empty table
summary = data.frame()

# Calculate table cell values
for (i in 1:4) {
    for (j in 1:3) {
        class = (filter$renda_cat == i)
        residence = (filter$TIPODEDOMICILIO == j)
        summary[i,j] = sum(class & residence)
    }
}

# Calculate table total values
for (i in 1:4)
    summary[i, 4] = sum(summary[i, 1:3])
for (j in 1:4)
    summary[5, j] = sum(summary[1:4, j])

# Add info to rows and columns
rownames(summary) = c("Até 1 S.M.",
                      "De 1 a 2 S.M.",
                      "De 2 a 4 S.M.",
                      "Mais que 4 S.M.",
                      "Total")
colnames(summary) = c("Casa", "Apartamento", "Cômodo", "Total")
print(summary, print.gap=2)

# Calculate association measures
n = summary[5,4]
chi_squared = as.numeric(chisq.test(summary[1:4, 1:3])[1])
degrees = as.numeric(chisq.test(summary[1:4, 1:3])[2])
c_coef = sqrt(chi_squared/(chi_squared + n))
t_coef = sqrt(chi_squared/(n * 3 * 2))

paste("Chi-squared = ", round(chi_squared, 2), " (degrees = ", degrees, ")", sep="")
paste("C =", round(c_coef, 2))
paste("T =", round(t_coef, 2))

# ---------------------------------------------------------------------
# Item c

cat("\n[item c]\n")

chi_squared_ref = qchisq(1 - CHISQ_ALPHA, degrees)
paste("Chi-squared of reference =", round(chi_squared_ref, 2))
