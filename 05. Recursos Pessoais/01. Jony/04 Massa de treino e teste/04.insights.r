# Principais Insights 

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


# Comentário 90% dos acidentes envolvem pedestres e Substancias Psicoativas




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

# cerca de 60% dos acidentes estão focados no sul e sudeste