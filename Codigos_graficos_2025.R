install.packages("readr")

library(readr)

ls()

  )
############################################################
names(dados.2025.região)
head(dados.2025.região)
regiao2025 <- dados.2025.região[-1, ]
names(regiao2025) <- c("Regiao", "A", "B", "C", "D", "E", "Total")
regiao2025 <- regiao2025[, c("Regiao", "Total")]
regiao2025$Total <- as.numeric(regiao2025$Total)
regiao2025$Total <- as.numeric(regiao2025$Total)


regiao2025 <- dados.2025.região[-1, ]

names(regiao2025) <- c("Local", "A", "B", "C", "D", "E", "Total")

regiao2025 <- regiao2025[, c("Local", "Total")]

regiao2025 <- regiao2025[!grepl("^\\.\\.", regiao2025$Local), ]

regiao2025 <- regiao2025[grepl("^Região|^Total", regiao2025$Local), ]


regiao2025$Total <- as.numeric(regiao2025$Total)

library(ggplot2)
library(scales)

max_row <- regiao2025[which.max(regiao2025$Total), ]

format_num <- label_number(scale_cut = cut_si(""), accuracy = 0.1)

ggplot(regiao2025, aes(x = Local, y = Total, fill = Local)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = format_num(Total)), vjust = -0.5, fontface = "bold") +
  geom_label(
    data = max_row,
    aes(label = paste0("Maior região:\n", format_num(Total))),
    vjust = -1.5,
    fill = "#fdebd0",
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Total de Casos por Região — 2025",
    x = "Região",
    y = "Número de Casos"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
################################################################

names(dados.2025.faixa.etaria)
head(dados.2025.faixa.etaria)

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

##############################################
#   LIMPEZA DOS DADOS — REGIÕES 2025
##############################################

regiao2025 <- dados.2025.região

# 1) Remover a linha que contém o cabeçalho verdadeiro
regiao2025 <- regiao2025[-1, ]

# 2) Renomear colunas
names(regiao2025) <- c("Local", "A", "B", "C", "D", "E", "Total")

# 3) Manter apenas Local + Total
regiao2025 <- regiao2025[, c("Local", "Total")]

# 4) Remover linhas de estados (começam com "..")
regiao2025 <- regiao2025[!grepl("^\\.\\.", regiao2025$Local), ]

# 5) Manter somente as regiões e o total
regiao2025 <- regiao2025[grepl("^Região|^Total", regiao2025$Local), ]

# 6) Converter Total para número
regiao2025$Total <- as.numeric(regiao2025$Total)


################################################
#   GRÁFICO — TOTAL POR REGIÃO (2025)
################################################

max_row <- regiao2025[which.max(regiao2025$Total), ]
format_num <- label_number(scale_cut = cut_si(""), accuracy = 0.1)

ggplot(regiao2025, aes(x = Local, y = Total, fill = Local)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = format_num(Total)),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  geom_label(
    data = max_row,
    aes(label = paste0("Maior região:\n", format_num(Total))),
    vjust = -1.5,
    fill = "#fdebd0",
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Total de Casos por Região — 2025",
    x = "Região",
    y = "Número de Casos"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold"))


##############################################
#   LIMPEZA DOS DADOS — FAIXA ETÁRIA 2025
##############################################

faixa2025 <- dados.2025.faixa.etaria

# Cabeçalho real
cab <- faixa2025[1, ]

# Remover linha 1
faixa2025 <- faixa2025[-1, ]

# Aplicar nome correto das colunas
names(faixa2025) <- cab

# Remover linhas de UF
faixa2025 <- faixa2025[!grepl("^\\.\\.", faixa2025$`Região/UF de notificação`), ]

# Manter apenas regiões
faixa2025 <- faixa2025[grepl("^Região", faixa2025$`Região/UF de notificação`), ]

# Transformar colunas em numéricas
faixa2025[, -1] <- lapply(faixa2025[, -1], function(x) as.numeric(x))


##############################################
#   TRANSFORMAR EM FORMATO LONGO
##############################################

faixa_long2025 <- faixa2025 %>%
  pivot_longer(cols = -`Região/UF de notificação`,
               names_to = "Faixa_Etaria",
               values_to = "Casos")

# Remover faixa "Total"
faixa_long2025 <- faixa_long2025 %>%
  filter(Faixa_Etaria != "Total")

# Maior valor por faixa etária
max_por_faixa <- faixa_long2025 %>%
  group_by(Faixa_Etaria) %>%
  filter(Casos == max(Casos, na.rm = TRUE))


##############################################
#   GRÁFICO — FAIXAS ETÁRIAS (FACETADO)
##############################################

format_num <- label_number(scale_cut = cut_si(""), accuracy = 0.1)

ggplot(faixa_long2025,
       aes(x = `Região/UF de notificação`,
           y = Casos,
           color = `Região/UF de notificação`,
           group = `Região/UF de notificação`)) +
  
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  
  geom_label(
    data = max_por_faixa,
    aes(label = format_num(Casos)),
    fill = "#fff3b0",
    color = "black",
    fontface = "bold",
    size = 3,
    linewidth = 0.3
  ) +
  
  facet_wrap(~ Faixa_Etaria, scales = "free_y") +
  
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  
  labs(
    title = "Distribuição dos Casos por Faixa Etária — Regiões (2025)",
    x = "Região",
    y = "Número de Casos",
    color = "Região"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 18, face = "bold")
  )

