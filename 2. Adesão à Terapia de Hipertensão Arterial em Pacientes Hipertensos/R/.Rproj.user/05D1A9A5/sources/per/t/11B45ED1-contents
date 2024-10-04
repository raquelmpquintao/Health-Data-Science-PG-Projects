# Selecione aleatoriamente 20% dos indivíduos da BD BD_Epoca_Recurso_DIOS_2024.xls e guarde a nova DB num ficheiro *.xls.


# Calcular o correspondente a 20% da população
tamanho_amostra <- nrow(BD_original) * 0.20 

# Selecionar 20% dos indivíduos de forma aleatória
amostra <- BD_original[sample(nrow(BD_original), tamanho_amostra, replace = FALSE), ]

library(openxlsx)

# Salvar a nova base de dados em um novo arquivo Excel
write.xlsx(amostra, "amostra_bd.xlsx")

amostra<- amostra_bd
