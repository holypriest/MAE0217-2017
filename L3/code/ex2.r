#!/usr/bin/Rscript

DISPERSION_A1 = "ex2-a1.pdf"
DISPERSION_A2 = "ex2-a2.pdf"
DISPERSION_C  = "ex2-c.pdf"
RESIDUALS_D   = "ex2-d.pdf"

WEIGHT_B = 3000

df = read.csv("../data/ex2.csv", sep=";", fileEncoding="UTF-8")

# ---------------------------------------------------------------------
# Item a

cat("[Item a]\n")

print("Valid observations:")
paste("- Weight: ",             sum(!is.na(df$peso)), "/", length(df$peso), sep="")
paste("- Height: ",             sum(!is.na(df$est)),  "/", length(df$est),  sep="")
paste("- Cephalic perimeter: ", sum(!is.na(df$pc)),   "/", length(df$pc),   sep="")
cat("\n")

# Cephalic perimeter x weight
r = cor(df$pc, df$peso, use="pairwise.complete.obs")
paste("Cephalic perimeter x weight: r² =", round(r, 3))

pdf(DISPERSION_A1)
plot(df$pc ~ df$peso,
     xlab="Peso (g)",
     ylab="Perímetro cefálico (cm)")
invisible(dev.off())
paste("Dispersion diagram saved as", DISPERSION_A1)
cat("\n")

# Cephalic perimeter x height
r = cor(df$pc, df$est, use="pairwise.complete.obs")
paste("Cephalic perimeter x height: r² =", round(r, 3))

# Count number of times that (est=48.0, pc=33.0) appeared in the dataframe
rep = (df$est == 48.0 & df$pc == 33.0)
paste("(48.0, 33.0) appeared", length(which(rep == T)), "times")

pdf(DISPERSION_A2)
plot(df$pc ~ df$est,
     xlab="Estatura (cm)",
     ylab="Perímetro cefálico (cm)")
invisible(dev.off())
paste("Dispersion diagram saved as", DISPERSION_A2)

# ---------------------------------------------------------------------
# Item b

cat("\n[Item b]\n")

model = lm(df$pc ~ df$peso)
summary(model)

coefs = round(coefficients(model), 3)
paste("Regression line: Y = ", coefs[1], " + ", coefs[2], "X", sep="")

pred = coefs[1] + coefs[2] * WEIGHT_B
paste("Predicted cephalic perimeter for 3000g:", pred)

# ---------------------------------------------------------------------
# Item c

cat("\n[Item c]\n")

# Split newborns into 2 groups, according to malaria presence
control = df[df$grupo == 0,]
malaria = df[df$grupo %in% seq(1, 3),]

# Quick check if the split was done right
stopifnot(nrow(control) + nrow(malaria) == nrow(df))

pdf(DISPERSION_C)

plot(malaria$pc ~ malaria$peso,
     col="red", pch=20,
     xlab="Peso (g)",
     ylab="Perímetro cefálico (cm)")
points(control$pc ~ control$peso, pch=18)
legend("topleft",
        legend=c("Controle", "Malária"),
        col=c("black", "red"),
        pch=c(18, 20))

invisible(dev.off())

paste("Dispersion diagram saved as", DISPERSION_C)

# ---------------------------------------------------------------------
# Item d

cat("\n[Item d]\n")

# Create new categories
df$tem_malaria = 0
df$tem_malaria[df$grupo %in% seq(1, 3)] = 1

df$mais_de_35_anos = 0
df$mais_de_35_anos[df$idade > 35] = 1

model = lm(df$pc ~ as.factor(df$tem_malaria) + as.factor(df$mais_de_35_anos))
s = summary(model)$sigma
print(summary(model))

# Plot residuals x fitted values
pdf(RESIDUALS_D)
plot(residuals(model) ~ fitted(model),
     xlab="Valores de perímetro cefálico ajustados pelo modelo (cm)",
     ylab="Resíduos para perímetro cefálico (cm)")


# Plot residual standard error lines
abline(s, 0, lty=3, col="blue")
abline(-s, 0, lty=3, col="blue")
abline(2*s, 0, col="blue")
abline(-2*s, 0, col="blue")

# Add legend to explain error lines
legend("topright",
        # \u00B1 is the plus/minus symbol
        legend=c(expression("\u00B1 S"["r"]), expression("\u00B1 2S"["r"])),
        col=c("blue", "blue"),
        lty=c(3, 1))

invisible(dev.off())
paste("Residuals graph saved as", RESIDUALS_D)
