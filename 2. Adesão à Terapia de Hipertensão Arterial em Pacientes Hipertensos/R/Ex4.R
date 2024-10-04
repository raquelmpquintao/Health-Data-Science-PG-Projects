# 4. Obtenha uma representação gráfica que permita comparar a compra de medicamentos genéricos ou 
# marca (marca_generico) por sexo e interprete-o.

# sexo - Masculino 1, Feminino 2
# marca_generico: Comprou a marca ou o genérico (0-genérico; 1-marca) 
# TEM VALORES OMISSOS


marca_generico_sexo<- table(amostra$marca_generico, amostra$sexo)
marca_generico_sexo_summary<- summary(marca_generico_sexo, na.rm=TRUE)
marca_generico_sexo_freqrel<-prop.table(marca_generico_sexo)*100


# Gráfico de barras empilhadas
marca_generico_sexo_barplot<-barplot(marca_generico_sexo, 
        beside = TRUE,
        col = c("gainsboro", "tomato"),
        main = "Compra de Medicamentos Genéricos ou de Marca por Sexo",
        xlab = "Sexo",
        ylab = "Quantidade de Medicamentos",
        legend = c("Genérico", "Marca"),
        ylim=c(0,40),
        names.arg=sexos)

print(marca_generico_sexo_summary)
print(marca_generico_sexo)
print(marca_generico_sexo_freqrel)
print(marca_generico_sexo_barplot)

      