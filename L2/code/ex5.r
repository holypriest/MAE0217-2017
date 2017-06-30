#!/usr/bin/Rscript

df = read.csv("../data/ex5.csv", sep=";", fileEncoding="UTF-8")

# --------------------------------------------------------------------
# item a

tapply(df$peso, df$espmal1, summary)
tapply(df$peso, df$sexorn, summary)
tapply(df$peso, df$prematrn, summary)

# -------------------------------------------------------------------
# item b

filter = df[,c("peso", "espmal1")]

vivax = filter$peso[filter$espmal1 == 1]
vivax = vivax[!is.na(vivax)]

falciparum = filter$peso[filter$espmal1 == 2]
falciparum = falciparum[!is.na(falciparum)]

mixed = filter$peso[filter$espmal1 == 3]
mixed = mixed[!is.na(mixed)]

var_total = var(filter$peso, na.rm=TRUE)
var_avg = length(vivax) * var(vivax) +
	  length(falciparum) * var(falciparum) +
	  length(mixed) * var(mixed)
var_avg = var_avg / length(filter$peso)

r_squared = 1 - var_avg/var_total

paste("espmal1 var_total =", round(var_total, 3))
paste("espmal1 var_avg =", round(var_avg, 3))
paste("espmal1 r_sq =", round(r_squared, 4))

filter = df[,c("peso", "sexorn")]

masc = filter$peso[filter$sexorn == 1]
masc = masc[!is.na(masc)]

fem = filter$peso[filter$sexorn == 2]
fem = fem[!is.na(fem)]

var_total = var(filter$peso, na.rm=TRUE)
var_avg = length(masc) * var(masc) +
	  length(fem) * var(fem)
var_avg = var_avg / length(filter$peso)

r_squared = 1 - var_avg/var_total

paste("sexorn var_total =", round(var_total, 3))
paste("sexorn var_avg =", round(var_avg, 3))
paste("sexorn r_sq =", round(r_squared, 4))

filter = df[,c("peso", "prematrn")]

no = filter$peso[filter$prematrn == 0]
no = no[!is.na(no)]

yes = filter$peso[filter$prematrn == 1]
yes = yes[!is.na(yes)]

var_total = var(filter$peso, na.rm=TRUE)
var_avg = length(no) * var(no) +
	  length(yes) * var(yes)
var_avg = var_avg / length(filter$peso)

r_squared = 1 - var_avg/var_total

paste("prematrn var_total =", round(var_total, 3))
paste("prematrn var_avg =", round(var_avg, 3))
paste("prematrn r_sq =", round(r_squared, 4))

# -----------------------------------------------------------
# item c

filter = df[,c("peso", "sexorn", "prematrn")]
filter = filter[!is.na(filter$peso),]

masc_norm = filter$peso[filter$sexorn == 1 & filter$prematrn == 0]
masc_norm = masc_norm[!is.na(masc_norm)]

fem_norm = filter$peso[filter$sexorn == 2 & filter$prematrn == 0]
fem_norm = fem_norm[!is.na(fem_norm)]

masc_prem = filter$peso[filter$sexorn == 1 & filter$prematrn == 1]
masc_prem = masc_prem[!is.na(masc_prem)]

fem_prem = filter$peso[filter$sexorn == 2 & filter$prematrn == 1]
fem_prem = fem_prem[!is.na(fem_prem)]

sex_prem_avg = c(mean(masc_norm), mean(fem_norm), mean(masc_prem), mean(fem_prem))
sex_prem_stdev = c(sd(masc_norm), sd(fem_norm), sd(masc_prem), sd(fem_prem))

sex_prem_x = c(1,2,3,4)

plot(sex_prem_x, sex_prem_avg,
     ylim=range(c(sex_prem_avg - sex_prem_stdev, sex_prem_avg + sex_prem_stdev)),
     pch=19, xlab="Indicador de prematuridade (Sexo do recém-nascido)", xlim=range(0,5), xaxt="n", ylab="Peso médio (g)", main=NULL
)

axis(1, at=c(1,2,3,4), labels=c("Não prematuro (M)", "Não prematuro (F)", "Prematuro (M)", "Prematuro (F)"))

arrows(sex_prem_x, sex_prem_avg - sex_prem_stdev, sex_prem_x, sex_prem_avg + sex_prem_stdev, length=0.05, angle=90, code=3)

lines(sex_prem_x, sex_prem_avg, lty=3, col="blue")

# -----------------------------------------------------------
# item d

filter = df[,c("peso", "sexorn", "espmal1")]
filter = filter[!is.na(filter$peso),]

masc_vivax = filter$peso[filter$sexorn == 1 & filter$espmal1 == 1]
masc_vivax = masc_vivax[!is.na(masc_vivax)]

fem_vivax = filter$peso[filter$sexorn == 2 & filter$espmal1 == 1]
fem_vivax = fem_vivax[!is.na(fem_vivax)]

masc_falc = filter$peso[filter$sexorn == 1 & filter$espmal1 == 2]
masc_falc = masc_falc[!is.na(masc_falc)]

fem_falc = filter$peso[filter$sexorn == 2 & filter$espmal1 == 2]
fem_falc = fem_falc[!is.na(fem_falc)]

sex_inftype_avg = c(mean(masc_vivax), mean(fem_vivax), mean(masc_falc), mean(fem_falc))
sex_inftype_stdev = c(sd(masc_vivax), sd(fem_vivax), sd(masc_falc), sd(fem_falc))

sex_inftype_x = c(1,2,3,4)

plot(sex_inftype_x, sex_inftype_avg,
     ylim=range(c(sex_inftype_avg - sex_inftype_stdev, sex_inftype_avg + sex_inftype_stdev)),
     pch=19, xlab="Espécie de malária (Sexo do recém-nascido)", xlim=range(0,5), xaxt="n", ylab="Peso médio (g)", main=NULL
)

axis(1, at=c(1,2,3,4), labels=c("Vivax (M)", "Vivax (F)", "Falciparum (M)", "Falciparum (F)"))

arrows(sex_inftype_x, sex_inftype_avg - sex_inftype_stdev, sex_inftype_x, sex_inftype_avg + sex_inftype_stdev, length=0.05, angle=90, code=3)

lines(sex_inftype_x, sex_inftype_avg, lty=3, col="blue")

