# This link can help us:
# http://stackoverflow.com/questions/9563368/create-stacked-percent-barplot-in-r

df_docentes = read.csv('../data/ex5-docentes.csv', sep=";", fileEncoding="UTF-8")
df_discentes = read.csv('../data/ex5-discentes.csv', sep=";", fileEncoding="UTF-8")

myvars = c("ID_DOCENTE", "NM_PROGRAMA_IES", "NM_ENTIDADE_ENSINO", "TP_SEXO_DOCENTE")
df_doc_filtered = df_docentes[myvars]

myvars = c("X", "NM_PROGRAMA_IES", "NM_ENTIDADE_ENSINO", "TP_SEXO_DISCENTE")
df_dis_filtered = df_discentes[myvars]

df_doc_filtered = df_doc_filtered[df_doc_filtered["NM_ENTIDADE_ENSINO"] == "UNIVERSIDADE DE SÃO PAULO",]
df_dis_filtered = df_dis_filtered[df_dis_filtered["NM_ENTIDADE_ENSINO"] == "UNIVERSIDADE DE SÃO PAULO",]

library(reshape)

df_doc_area_sexo = cast(df_doc_filtered, NM_PROGRAMA_IES~TP_SEXO_DOCENTE, length, value="ID_DOCENTE")
df_dis_area_sexo = cast(df_dis_filtered, NM_PROGRAMA_IES~TP_SEXO_DISCENTE, length, value="X")

# Using NM_PROGRAMA_IES as index
tmp <- df_doc_area_sexo[,-1]
rownames(tmp) <- df_doc_area_sexo[,1]
df_doc_area_sexo <- tmp

tmp <- df_dis_area_sexo[,-1]
rownames(tmp) <- df_dis_area_sexo[,1]
df_dis_area_sexo <- tmp

myvars <- c("CIÊNCIA DA COMPUTAÇÃO", "ESTATÍSTICA", "MATEMÁTICA", "MATEMÁTICA APLICADA")

# Info that we need to build the barplots
df_doc_final <- df_doc_area_sexo[myvars,]
df_dis_final <- df_dis_area_sexo[myvars,]

rownames(df_doc_final) <- paste(rownames(df_doc_final), "_DOCENTE", sep="")
rownames(df_dis_final) <- paste(rownames(df_dis_final), "_DISCENTE", sep="")

df_final <- rbind(df_doc_final, df_dis_final)

labels = c("discente", "docente", "discente", "docente", "discente", "docente", "discente", "docente")
sp = c(0, .1, .6, .1, .6, .1, .6, .1)
barplot(t(prop.table(as.matrix(df_final), 1)), col=c("coral2", "darkseagreen3"), space=sp, names.arg=labels, border=FALSE, cex.names=0.7, legend.text=c("Feminino", "Masculino"), args.legend=list(x="topleft", border="white", bty="n", horiz=TRUE), ylim=c(0,1.1))
axis(1, at=c(1, 3.85, 6.5, 9.2), labels = c("Ciência da Computação", "Estatística", "Matemática Aplicada", "Matemática Pura"), pos=-.05, tick=FALSE, cex.names=.7)
