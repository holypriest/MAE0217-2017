#!/usr/bin/Rscript
# L1 - Ex. 1

# Filenames for the generated boxplots
BOXPLOT_A = "ex1a.pdf"
BOXPLOT_B = "ex1b.pdf"

source("util.r")  # basic_measures()

# ---------------------------------------------------------------------
# (A)

cat("[Exercise 1a]\n")

df = read.csv("../data/ex1.csv", sep=";", fileEncoding="UTF-8")
basic_measures(df$Velocidade)

# Save boxplot as a PDF
pdf(BOXPLOT_B)
boxplot(df$Velocidade, ylab="Velocidade do vento (km/h)")
invisible(dev.off())  # Turn off plotting and hide output of command
paste("Boxplot saved as", BOXPLOT_A)

# ---------------------------------------------------------------------
# (B)

cat("\n[Exercise 1b]\n")

# Remove outlier, which is the highest value
max_spd = max(df$Velocidade)
max_spd_day = df$Dia[df$Velocidade == max_spd]
df = df[-c(max_spd_day, max_spd), ]

basic_measures(df$Velocidade)

# Save boxplot as a PDF
pdf(BOXPLOT_B)
boxplot(df$Velocidade, ylab="Velocidade do vento (km/h)")
invisible(dev.off())
paste("Boxplot saved as", BOXPLOT_B)
