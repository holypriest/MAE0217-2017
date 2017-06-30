library(car)

# Ex 1 - a

df <- read.csv('ex1.csv', sep=';')
linear <- lm(df$y~df$x)

# Ex 1 - b

plot(df$x, df$y, xlab='x (UPM)', ylab='y (clientes)', cex.lab=1.2)
abline(linear, col="blue")

# Ex 1 - c
x <- (df$x)
x2 <- x^2
quad <- lm(df$y ~ x2 + x)

resquad <- resid(quad)
s = sd(resquad)
paste("Sr =", round(s, 3))
plot(resquad ~ x, xlab="x (UPM)", ylab="Resíduos de y (clientes)", ylim=range(-35,35))
# Add legend to explain error lines
legend("top", inset=c(0, -0.1), horiz=T, xpd=T,
       # \u00B1 is the plus/minus symbol
       legend=c(expression("\u00B1 S"["r"]), expression("\u00B1 2S"["r"])),
       col=c("blue", "blue"),
       lty=c(3, 1))
abline(s, 0, lty=3, col="blue")
abline(-s, 0, lty=3, col="blue")
abline(2*s, 0, col="blue")
abline(-2*s, 0, col="blue")

qqPlot(resquad, ylab="Quantis dos resíduos", xlab="Quantis teóricos", col.lines="blue", lwd=1, grid=F)

predicted_y <- predict(quad, list(x, x2))
lines(x, predicted_y, col = "blue")
