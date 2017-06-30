#!/usr/bin/Rscript

library(car)  # qqPlot()

# Graph filenames
RESIDUALS = "ex2-residuals.pdf"
QQNORM    = "ex2-qqnorm.pdf"

df = read.csv("../data/ex2.csv", sep=";", fileEncoding="UTF-8")

# ---------------------------------------------------------------------

# Apply linear model
model = lm(df$capt ~ df$dist)
summary(model)
s = summary(model)$sigma

# Plot residuals x explanatory variable
pdf(RESIDUALS)
plot(residuals(model) ~ df$dist,
     xlab="Distância (cm)",
     ylab="Resíduos para média do número de moscas capturadas")

# Plot residual standard error lines
abline(s, 0, lty=3, col="blue")
abline(-s, 0, lty=3, col="blue")
abline(2*s, 0, col="blue")
abline(-2*s, 0, col="blue")

# Add legend to explain error lines
legend("top", inset=c(0, -0.1), horiz=T, xpd=T,
        # \u00B1 is the plus/minus symbol
        legend=c(expression("\u00B1 S"["r"]), expression("\u00B1 2S"["r"])),
        col=c("blue", "blue"),
        lty=c(3, 1))

invisible(dev.off())
paste("Residuals graph saved as", RESIDUALS)

# Plot qqnorm graph
pdf(QQNORM)
qqPlot(residuals(model), col.lines="blue", lwd=1, grid=FALSE,
       xlab="Quantis teóricos",
       ylab="Quantis dos resíduos")
invisible(dev.off())
paste("qqnorm graph saved as", QQNORM)
