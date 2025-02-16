# 03. Como a fase do dia afeta a ocorrência de acidentes? 

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