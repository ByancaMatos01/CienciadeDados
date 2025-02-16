# 02. Qual a probabilidade de um acidente ocorrer em condições climáticas claras? 


library(readr)
library(dplyr) 

dados <- read_csv2("D:/donwloads/datatran2024.csv", show_col_types = FALSE)
View(dados)

dados_selecionados <- dados %>% select(condicao_metereologica, uf)
View(dados_selecionados)

total_registros <- nrow(dados_selecionados)
print(total_registros)


fl_esta_claro <- dados_selecionados %>% 
  group_by(condicao_metereologica) %>% 
  summarise(qtde = n()) %>% 
  mutate(proporcao = (qtde / total_registros) * 100)  # Converte para %

# Exibir resultado
View(fl_esta_claro)
