#!/usr/bin/Rscript

library(car)  # qqPlot()

# Graph filenames
RESIDUALS  = "ex3-residuals.pdf"
QQNORM     = "ex3-qqnorm.pdf"
QQNORM2    = "ex3-qqnorm2.pdf"

df = read.csv("../data/ex3.csv", sep=";", fileEncoding="UTF-8")

# ---------------------------------------------------------------------

s = sd(df$r)  # sd already uses denominator n-1
paste("Sr =", round(s, 3))

# Plot residuals graph
pdf(RESIDUALS)
plot(df$r ~ df$x, xlab="x", ylab="Resíduos")

# Add residual standard error lines
abline(s, 0, lty=3, col="blue")
abline(-s, 0, lty=3, col="blue")
abline(2*s, 0, col="blue")
abline(-2*s, 0, col="blue")

# Add legend to explain error lines
legend("topright",
        legend=c(expression("\u00B1 S"["r"]), expression("\u00B1 2S"["r"])),
        col=c("blue", "blue"),
        lty=c(3, 1))
invisible(dev.off())
paste("Residuals graph saved as", RESIDUALS)

# Plot qqnorm graph
pdf(QQNORM)
qqPlot(df$r, col.lines="blue", lwd=1, grid=FALSE,
       xlab="Quantis teóricos", ylab="Resíduos")
invisible(dev.off())
paste("qqnorm graph saved as", QQNORM)

pdf(QQNORM2)
df2 <- read.csv("../data/ex3-2.csv", sep=";")
qqPlot(df2$r, xlab="Quantis teóricos", ylab="Resíduos",
       col.lines="blue", lwd=1)
invisible(dev.off())
paste("qqnorm2 graph saved as", QQNORM2)
