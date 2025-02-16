# 01. Estado com mais acidente no pais 
library(readr)
library(dplyr) 

dados <- read_csv2("D:/donwloads/datatran2024.csv", show_col_types = FALSE)
View(dados)

dados_agrupados <- dados %>%
  group_by(uf) %>%
  summarise(qtde = n())

dados_agrupados_ordenados <- dados_agrupados %>%
  arrange(desc(qtde))

estado_mais_acidentes <- dados_agrupados_ordenados %>% slice(1)

print(estado_mais_acidentes)
View(dados_agrupados_ordenados)











# 02. Qual a probabilidade de ocorrer um acidente no claro ? 


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




# 03. Como a fase do dia afeta a ocorrencia de acidentes


install.packages("scales")  # Instalar (se ainda não tiver)
library(scales)  # Carregar o pacote

# Criar categorias agrupadas
fl_esta_claro2 <- dados_selecionados %>%
  mutate(clima_agrupado = case_when(
    condicao_metereologica %in% c("Chuva", "Garoa/Chuvisco", "Granizo") ~ "Chuvoso",
    condicao_metereologica %in% c("Céu Claro", "Sol") ~ "Céu Claro",
    condicao_metereologica %in% c("Nublado", "Nevoeiro/Neblina") ~ "Nebuloso",
    condicao_metereologica %in% c("Neve", "Vento") ~ "Extremos",
    condicao_metereologica == "Ignorado" ~ "Ignorado",
    TRUE ~ condicao_metereologica  # Mantém o original caso não se encaixe em nenhuma categoria
  )) %>%
  group_by(clima_agrupado) %>%
  summarise(qtde = n()) %>%
  mutate(proporcao = percent(qtde / sum(qtde), accuracy = 0.1))  # Formata em 
View(fl_esta_claro2)




# 04. Que insigths podem ser gerados 


library(dplyr)
library(scales)

tipo_acidente_agrupado <- dados %>%
  mutate(categoria = case_when(
    causa_acidente %in% c("Reação tardia ou ineficiente do condutor", 
                          "Ausência de reação do condutor",
                          "Condutor deixou de manter distância do veículo da frente",
                          "Manobra de mudança de faixa",
                          "Velocidade Incompatível",
                          "Ultrapassagem Indevida",
                          "Condutor usando celular",
                          "Condutor Dormindo",
                          "Transitar na contramão",
                          "Frear bruscamente") ~ "Erro humano",
    
    causa_acidente %in% c("Chuva", "Neblina", "Fumaça", "Demais Fenômenos da natureza") ~ "Fatores ambientais",
    
    causa_acidente %in% c("Pista Escorregadia", "Pista esburacada", "Ausência de sinalização",
                          "Curva acentuada", "Falta de acostamento", "Declive acentuado") ~ "Condições da via",
    
    causa_acidente %in% c("Avarias e/ou desgaste excessivo no pneu", "Problema com o freio",
                          "Problema na suspensão", "Faróis desregulados", "Sistema de drenagem ineficiente") ~ "Problemas mecânicos",
    
    causa_acidente %in% c("Pedestre cruzava a pista fora da faixa", "Pedestre andava na pista",
                          "Pedestre - Ingestão de álcool/ substâncias psicoativas") ~ "Comportamento de pedestres",
    
    TRUE ~ "Pedestres na Via / Substancias Psicoativas"
  )) %>%
  group_by(categoria) %>%
  summarise(qtde = n()) %>%
  mutate(proporcao = percent(qtde / sum(qtde), accuracy = 0.1))

View(tipo_acidente_agrupado)







total_registros <- nrow(dados_selecionados)
print(total_registros)



total_registros <- nrow(dados)
print(total_registros)

library(dplyr)
library(scales)

dados_agrupados <- dados  %>%
  mutate(regiao = case_when(
    uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",           # Norte
    uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",  # Nordeste
    uf %in% c("GO", "MS", "MT") ~ "Centro-Oeste",                            # Centro-Oeste
    uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",                            # Sudeste
    uf %in% c("PR", "RS", "SC") ~ "Sul"                                      # Sul
  )) %>%
  group_by(regiao) %>%
  summarise(qtde = n()) %>%   
  mutate(probabilidade = percent(qtde / total_registros, accuracy = 0.1)) 
View(dados_agrupados)









