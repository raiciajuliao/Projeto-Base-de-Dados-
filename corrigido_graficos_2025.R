# Remover linha extra "&" e linhas NA
regiao2025 <- regiao2025[!is.na(regiao2025$Total), ]

# Converter Total novamente para garantir
regiao2025$Total <- as.numeric(regiao2025$Total)

# Identificar valor total real (da linha Total)
valor_total <- regiao2025$Total[regiao2025$Local == "Total"]

# Calcular porcentagem SOMENTE para as regiões
regiao2025$Porcentagem <- ifelse(
  regiao2025$Local == "Total",
  100,   # <- força 100% na linha Total
  (regiao2025$Total / valor_total) * 100
)

# Conferir resultado final
regiao2025

library(ggplot2)
library(dplyr)

ggplot(regiao2025, aes(x = reorder(Local, Porcentagem), 
                       y = Porcentagem, 
                       fill = Local)) +
  
  geom_col(width = 0.6, color = NA) +
  
  geom_text(aes(label = sprintf("%.1f%% (%s casos)",
                                Porcentagem,
                                format(Total, big.mark=".", decimal.mark=","))),
            vjust = -0.5,
            size = 5,
            family = "Times New Roman",
            fontface = "bold",
            color = "#333333") +
  
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  scale_fill_manual(values = cores_soft) +
  
  labs(
    title = "Distribuição Percentual dos Casos por Região — 2025",
    x = "Região",
    y = "Porcentagem (%)"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(

    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
###################################################

#########################################
#AGORA VAI#

library(dplyr)
library(tidyr)

library(stringr)
library(ggplot2)

# Carregar o dataset original
faixa <- dados.2025.faixa.etaria

# Remover linha de cabeçalho interna
faixa <- faixa[-1, ]

# Renomear com nomes seguros (sem hífen)
names(faixa) <- c("Local","IGN",
                  "faixa_0_1","faixa_1_4","faixa_5_9",
                  "faixa_10_14","faixa_15_19","faixa_20_39",
                  "faixa_40_59","faixa_60_64","faixa_65_69",
                  "faixa_70_79","faixa_80_plus","Total")

# Remover linhas de UFs

faixa <- faixa[!grepl("^\\.\\.", faixa$Local), ]

# Remover coluna IGN
faixa <- faixa %>% select(-IGN)

# Converter tudo para número (garantido)
for(i in 2:ncol(faixa)){
  faixa[[i]] <- faixa[[i]] |>
    trimws() |>
    gsub("[^0-9]", "", x = _) |>
    as.numeric()
}

# SOMAR TODAS AS REGIÕES POR FAIXA

faixa_total <- faixa %>%
  filter(Local != "Total") %>%
  summarise(across(-Local, sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Faixa",
               values_to = "TotalFaixa")

ggplot(faixa_total, aes(x = Faixa, y = TotalFaixa, fill = Faixa)) +
  
  geom_col(width = 0.65) +
  
  geom_text(
    aes(label = format(TotalFaixa, big.mark=".", decimal.mark=",")),
    vjust = -0.3,
    size = 4.5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  
  scale_fill_brewer(palette = "Set3") +
  
  labs(
    title = "Casos por Faixa Etária Regiões (2025)",
    x = "Faixa Etária",
    y = "Número de Casos"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )


