# ex1-a
df = read.csv('../data/ex1.csv', sep=';')
plot(df$preco ~ df$colheita, xlab="Colheita (milhares de hectolitros)", ylab="Preço (escudos/litro)")

# ex1-b
lin = lm(df$preco ~ df$colheita)
coefs = round(coefficients(lin), 5)
print(coefs)
abline(lin, col="blue")

# ex1-c
res = residuals(lin)
plot(res ~ df$colheita, xlab="Colheita (milhares de hectolitros)", ylab="Resíduos (escudos/litro)")

# ex1-d
par(mar=c(4, 4, 5, 5))

# Plot first graph
plot(df$colheita ~ df$ano, xlab="Ano", ylab="Colheita (milhares de hectolitros)", pch=20, axes=F)
lines(df$colheita ~ df$ano)

# Add bottom and left axis
axis(side=1, at=seq(1942, 1961, 2), las=2)
axis(side=2)

# Plot other graph on same image
par(new=T)
plot(df$preco ~ df$ano, col="blue", pch=20, axes=F, xlab=NA, ylab=NA)
lines(df$preco ~ df$ano, col="blue", lty=3)

# Add right axis and its label
axis(side=4)
mtext(side=4, line=3, "Preço (escudos/litro)")

# Plot box around graph
box(which="plot", bty="o")

# ex1-e
library(car)
qqPlot(res, col.lines="blue", xlab="Quantis teóricos", ylab="Resíduos (escudos/litro)", lwd=1, grid=FALSE)
