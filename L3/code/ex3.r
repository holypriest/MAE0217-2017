library(sigmoid)

# Ex 3 - a
curve(dnorm(x,-8), from= -13, to = 5, ylab="", xlab="Resultado do teste", xaxt="n", col="blue")
curve(dnorm(x), from= -13, to = 5, xaxt="n", col="red", add=T)

# Ex 3 - b
curve(sigmoid(800*x -4), xlab="1 - Et", ylab="St", ylim=c(0,1))

# Ex 3 - c
curve(dnorm(x, 0.05), from= -5, to = 5, ylab="", xlab="Resultado do teste", xaxt="n", col="blue")
curve(dnorm(x), from= -5, to = 5, xaxt="n", col="red", add=T)

# Ex 3 - d
curve(x * 1, from=0, to=1, xlab="1 - Et", ylab="St")

# Ex 3 - e
curve(dnorm(x,-2), from= -7, to = 5, ylab="", xlab="Resultado do teste", xaxt="n", col="blue")
curve(dnorm(x), from= -7, to = 5, xaxt="n", col="red", add=T)

# Ex 3 - f
curve(x^0.08 , xlab="1 - Et", ylab="St", ylim=c(0,1))
