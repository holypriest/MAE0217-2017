#!/usr/bin/Rscript

# Conditional density plot filename
COND_DENSITY = "ex4.pdf"

df = read.csv("../data/ex4.csv", sep=";", fileEncoding="UTF-8")
df$resp = as.factor(df$resp)

# ---------------------------------------------------------------------

args = commandArgs(TRUE)       # Only read arguments that come after '--args'
values = as.numeric(args[-1])  # Remove '--args' from arguments array

model = glm(resp ~ score, data=df, family=binomial(link="logit"))
summary(model)

# Conditional density plot
pdf(COND_DENSITY)
par(mar=c(5, 5, 3, 5))
cdplot(resp ~ score, data=df, xlab="Escore", ylab="Resposta")
mtext(side=4, line=3, "P(resposta | escore)")
invisible(dev.off())
paste("Conditional density plot saved as", COND_DENSITY)

# For the binomial family, predict() can be used to return values for the equation:
#
# log(p_i/(1 - p_i)) = B0 + B1 * x_i
#
# - p_i: Probability of the binary variable (resp) being a success
# - B0, B1: Coefficients; determined by glm()
# - x_i: ith observation for the continuous variable x (score, in this case)
if (length(args) > 0) {
    print("P(resp = 1 | score) for the given score values:")

    # Column name of the provided dataframe must be the same as the variable used in glm()
    pred = predict(model, data.frame(score=values))

    # Isolate p_i from the equation shown above
    pred = exp(pred)
    pred = pred / (1 + pred)

    names(pred) = values
    print(round(pred, 3))
}
