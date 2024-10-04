# 6. Verifique se existe associação entre a variável “inicio” e o código ICPC.  


# ICPC: código da hipertensão: K86 – hipertensão sem complicações (1) / 
# K87 – hipertensão com complicações (2) #Tem valores omissos
# inicio: Doente inicia tratamento (sim (1) ou não (0))

#  2 variáveis qualitativas nominais dicotómicas -  Phi

# library(vcd)   # biblioteca necessária para o phi

inicio_icpc_table<-prop.table(table(amostra$inicio, amostra$ICPC))*100  # tabela de contingência - freq. relativa

print(inicio_icpc_table)

# assocstats(table(amostra$inicio, amostra$ICPC))




