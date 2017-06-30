#!/usr/bin/Rscript

BOXPLOT_A = "ex2-a.pdf"
BOXPLOT_D = "ex2-d.pdf"
HIST_F    = "ex2-f.pdf"
PIE_G     = "ex2-g.pdf"
BOXPLOT_H = "ex2-h.pdf"
QQNORM_I  = "ex2-i.png"

df <- read.csv("../data/ex2.csv", sep=";", fileEncoding="UTF-8")

# Explicitly change order of factor
df$renda_cat = factor(df$renda_cat,
                      c("_Ate 0,5",
                        "> 0,5 ate 0,75",
                        "> 0,75 ate 1,5",
                        "> 1,5 ate 2,5",
                        "> 2,5 ate 3,75",
                        "> 3,75"))

# ---------------------------------------------------------------------

### item a
cat("[Item a]\n")

pdf(BOXPLOT_A)
boxplot(df$consumo_per_capita ~ df$TIPODEDOMICILIO,
        names=c("Casa", "Apartamento", "Cômodo"),
        xlab="Tipo de domicílio",
        ylab="Consumo de gás anual domiciliar per capita (Kg)")
invisible(dev.off())
paste("Boxplot saved as", BOXPLOT_A)

### item b
cat("\n[Item b]\n")

tapply(df$consumo_per_capita, df$TIPODEDOMICILIO, summary)
cat("Standard Deviation:\n")
tapply(df$consumo_per_capita, df$TIPODEDOMICILIO, sd)

### item d
cat("\n[Item d]\n")

pdf(BOXPLOT_D)
boxplot(df$consumo_per_capita ~ df$REDEGERALDEENERGIAELETRICA,
        names=c("Sim", "Não"),
        xlab="Presença de rede geral de energia elétrica",
        ylab="Consumo de gás anual domiciliar per capita (Kg)")
invisible(dev.off())
paste("Boxplot saved as", BOXPLOT_D)

library(moments)
tapply(df$consumo_per_capita, df$REDEGERALDEENERGIAELETRICA, skewness)

### item e
x_sim = df$consumo_per_capita[df$REDEGERALDEENERGIAELETRICA == 1]
x_nao = df$consumo_per_capita[df$REDEGERALDEENERGIAELETRICA == 2]

b1_sim = 1 / length(x_sim) * sum(((x_sim - mean(x_sim))/sd(x_sim))^3)
b1_nao = 1 / length(x_nao) * sum(((x_nao - mean(x_nao))/sd(x_nao))^3)

b2_sim = 1 / length(x_sim) * sum(((x_sim - mean(x_sim))/sd(x_sim))^4)
b2_nao = 1 / length(x_nao) * sum(((x_nao - mean(x_nao))/sd(x_nao))^4)

# Alternativamente:
library(moments)

skewness(x_sim)
skewness(x_nao)
kurtosis(x_sim)
kurtosis(x_nao)

### item f
cat("\n[Item f]\n")

pdf(HIST_F)
hist(df$RENDA_PER_CAPITA_MENSAL,
     breaks=88,
     xlim=c(0,88000),
     main = "",
     xlab="Renda per capita mensal (R$)",
     ylab="Frequência",
     axes=F,
     col="green")
axis(2)
axis(1, at=seq(0,90000,5000), labels=NULL)
invisible(dev.off())
paste("Histogram saved as", HIST_F)

### item g
cat("\n[Item g]\n")

sm = 788.00

class_1 = length(df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL <= sm])
inter_2 = intersect(df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL > sm],
                    df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL <= 2 * sm])
class_2 = length(inter_2)
inter_3 = intersect(df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL > 2 * sm],
                    df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL <= 4 * sm])
class_3 = length(inter_3)
class_4 = length(df$RENDA_PER_CAPITA_MENSAL[df$RENDA_PER_CAPITA_MENSAL > 4 * sm])

slices = c(class_1, class_2, class_3, class_4)
print(slices)
percentages = slices/length(df$RENDA_PER_CAPITA_MENSAL) * 100

# DEBUG
paste("Sum of percentages = ", round(sum(percentages), 2), "%", sep="")
paste("Counted", sum(slices), "of", length(df$RENDA_PER_CAPITA_MENSAL))

colors = c("red", "yellow", "green", "blue")
lab = c("Abaixo de 1 s.m.", "De 1 a 2 s.m.", "De 2 a 4 s.m.", "Acima de 4 s.m.")
lab = paste(lab, " (", round(percentages, 2), "%)", sep="")

pdf(PIE_G, width=9.5)
par(lwd=1.5)  # Modify width of outline
pie(slices, labels=lab, main="", cex=1.0, font=2, radius=1.0, col=colors)
invisible(dev.off())
paste("Pie chart saved as", PIE_G)

### item h
cat("\n[Item h]\n")

tapply(df$consumo_per_capita, df$renda_cat, summary)
cat("Standard Deviation:\n")
tapply(df$consumo_per_capita, df$renda_cat, sd)

pdf(BOXPLOT_H)
par(cex.axis=0.8)  # Change size of x axis (works until next dev.off())
boxplot(df$consumo_per_capita ~ df$renda_cat,
        names=c("< 0,5", "(0,5; 0,75]", "(0,75; 1,5]",
                "(1,5; 2,5]", "(2,5; 3,75]", "> 3,75"),
        xlab="Renda per capita (R$)",
        ylab="Consumo de gás anual domiciliar per capita (Kg)")
invisible(dev.off())
paste("Boxplot saved as", BOXPLOT_H)

### item i
cat("\n[Item i]\n")

png(QQNORM_I)
qqnorm(df$RENDA_PER_CAPITA_MENSAL,
       main="",
       xlab="Quantis teóricos",
       ylab="Renda domiciliar per capita mensal (R$)")
# Draw line that passes through Q1 and Q3
qqline(df$RENDA_PER_CAPITA_MENSAL, col="red")
invisible(dev.off())
paste("Normal probabilites graphic (qqnorm) saved as", QQNORM_I)
