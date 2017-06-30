#!/usr/bin/Rscript

# Filenames for generated images
GRAPH_A = "ex6-a.pdf"
GRAPH_B = "ex6-b.pdf"
GRAPH_C = "ex6-c.pdf"

SAMPLE_SIZE = 200

# ---------------------------------------------------------------------
# Item a

sample = rnorm(SAMPLE_SIZE, 0, 1)
pdf(GRAPH_A)
qqnorm(sample,
       main="",
       xlab="Quantis teóricos",
       ylab="Quantis amostrados")
qqline(sample, col="red")
invisible(dev.off())

# ---------------------------------------------------------------------
# Item b

sample = rchisq(SAMPLE_SIZE, 3)
pdf(GRAPH_B)
qqnorm(sample,
       main="",
       xlab="Quantis teóricos",
       ylab="Quantis amostrados")
qqline(sample, col="red")
invisible(dev.off())

# ---------------------------------------------------------------------
# Item c

sample = rt(SAMPLE_SIZE, 2)
pdf(GRAPH_C)
qqnorm(sample,
       main="",
       xlab="Quantis teóricos",
       ylab="Quantis amostrados")
qqline(sample, col="red")
invisible(dev.off())
