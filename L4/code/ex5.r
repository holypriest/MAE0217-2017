library(car)

# Ex 5 - b
summary(Duncan)
Duncan$type = factor(Duncan$type, c("bc", "wc", "prof"))
boxplot(Duncan$income ~ Duncan$type, xlab="type", ylab="income")
boxplot(Duncan$education ~ Duncan$type, xlab="type", ylab="education")
boxplot(Duncan$prestige ~ Duncan$type, xlab="type", ylab="prestige")

# Ex 5 - c
lmr <- lm(Duncan$prestige ~ Duncan$education + Duncan$income + Duncan$type)
summary(lmr)

# Ex 5 - d

s = sd(resid(lmr))
paste("Sr =", round(s, 3))

plot(resid(lmr) ~ fitted(lmr), xlab="prestige estimado", ylab="Resíduos para prestige", ylim=range(-30,30))

abline(s, 0, lty=3, col="blue")
abline(-s, 0, lty=3, col="blue")
abline(2*s, 0, col="blue")
abline(-2*s, 0, col="blue")

legend("topright",
              legend=c(expression("\u00B1 S"["r"]), expression("\u00B1 2S"["r"])),
	             col=c("blue", "blue"),
	             lty=c(3, 1))

qqPlot(resid(lmr), grid=FALSE, col.lines="blue", lwd=1, xlab="Quantis teóricos", ylab="Quantis dos resíduos")
