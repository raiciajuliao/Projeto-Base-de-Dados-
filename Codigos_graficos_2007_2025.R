dados.ano.faixa.etaria.regiao <- read.csv(
  "C:/projeto FC/dados-ano-faixa-etaria-regiao.txt",
  header = TRUE,
  sep = ";",
  dec = ".",
  quote = "\"",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

View(dados.ano.faixa.etaria.regiao)

#conversão das colunas númericas#

for (i in 2:ncol(dados.ano.faixa.etaria.regiao)) {
  dados.ano.faixa.etaria.regiao[[i]] <- as.numeric(dados.ano.faixa.etaria.regiao[[i]])
}

#filtrar por região#

regioes <- dados.ano.faixa.etaria.regiao[
  grep("^Região", dados.ano.faixa.etaria.regiao[[1]]),
]

#gráfico#

barplot(regioes$Total,
        names.arg = regioes[[1]],
        las = 2,
        cex.names = 0.8,
        main = "Total de casos por Região",
        ylab = "Total de casos",
        xlab = "Região")

#um gráfico mais bonitinho# 

library(ggplot2)
ggplot(regioes, aes(x = `Região/UF de notificação`, y = Total, fill = `Região/UF de notificação`)) +
  geom_col(width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  labs(title = "Total de Casos por Região",
       x = "Região",
       y = "Total de Casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 18))

#por faixa etária#

library(tidyr)
regioes_long <- pivot_longer(
  regioes,
  cols = 3:14,
  names_to = "Faixa_Etaria",
  values_to = "Casos"
)
ggplot(regioes_long, aes(x = Faixa_Etaria, y = Casos,
                         color = `Região/UF de notificação`,
                         group = `Região/UF de notificação`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribuição dos Casos por Faixa Etária — Regiões",
       x = "Faixa Etária",
       y = "Número de Casos",
       color = "Região") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 18))




#mais específico#

library(ggplot2)

ggplot(regioes, aes(x = `Região/UF de notificação`,
                    y = Total,
                    fill = `Região/UF de notificação`)) +
  
  geom_col(width = 0.65, show.legend = FALSE) +
  
  geom_text(aes(label = format(Total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5,
            size = 4.5,
            fontface = "bold") +
  
  scale_fill_brewer(palette = "Set2") +
  
  theme_minimal(base_size = 14) +
  
  labs(title = "Total de Casos por Região",
       x = "Região",
       y = "Número de Casos") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold"))
library(tidyr)

regioes_long <- pivot_longer(
  regioes,
  cols = 3:14,
  names_to = "Faixa_Etaria",
  values_to = "Casos"
)
ggplot(regioes_long,
       aes(x = Faixa_Etaria,
           y = Casos,
           color = `Região/UF de notificação`,
           group = `Região/UF de notificação`)) +
  
  geom_line(size = 1.2) +
  
  geom_point(size = 3) +
  
  geom_text(aes(label = Casos),
            vjust = -0.8,
            size = 3.6,
            fontface = "bold") +
  
  theme_minimal(base_size = 14) +
  
  labs(title = "Distribuição dos Casos por Faixa Etária — Regiões",
       x = "Faixa Etária",
       y = "Número de Casos",
       color = "Região") +
  
  scale_color_brewer(palette = "Dark2") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold"))

library(ggplot2)
library(scales)
max_row <- regioes[which.max(regioes$Total), ]

ggplot(regioes,
       aes(x = `Região/UF de notificação`,
           y = Total,
           fill = `Região/UF de notificação`)) +
  
  geom_col(width = 0.7, show.legend = FALSE) +
  
  geom_text(
    aes(label = scales::label_number_si()(Total)),
    vjust = -0.5,
    size = 4.5,
    fontface = "bold"
  ) +
  
  geom_label(
    data = max_row,
    aes(label = paste0("Maior valor:\n",
                       scales::label_number_si()(Total))),
    vjust = -1.5,
    fill = "#fdebd0",
    color = "black",
    fontface = "bold",
    size = 4,
    linewidth = 0.3
  ) +
  
  scale_fill_brewer(palette = "Dark2") +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Total de Casos por Região",
    x = "Região",
    y = "Número de Casos"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold")
  )
scales::label_number(scale_cut = scales::cut_si(""), accuracy = 0.1)




###############################################################################

library(ggplot2)
library(scales)

# identificar maior região
max_row <- regioes[which.max(regioes$Total), ]

format_num <- label_number(scale_cut = cut_si(""), accuracy = 0.1)

ggplot(regioes,
       aes(x = `Região/UF de notificação`,
           y = Total,
           fill = `Região/UF de notificação`)) +
  
  geom_col(width = 0.7, show.legend = FALSE) +
  
  geom_text(
    aes(label = format_num(Total)),
    vjust = -0.5,
    size = 4.5,
    fontface = "bold"
  ) +
  
  geom_label(
    data = max_row,
    aes(label = paste0("Maior valor:\n", format_num(Total))),
    vjust = -1.5,
    fill = "#fdebd0",
    color = "black",
    fontface = "bold",
    size = 4,
    linewidth = 0.3
  ) +
  
  scale_fill_brewer(palette = "Dark2") +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Total de Casos por Região",
    x = "Região",
    y = "Número de Casos"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold")
  )

#por faixa etária#

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

#tirar o total das faixas#
regioes_long2 <- regioes_long %>% 
  filter(Faixa_Etaria != "Total")
#maior valor 
max_por_faixa <- regioes_long2 %>%
  group_by(Faixa_Etaria) %>%
  filter(Casos == max(Casos))
format_num <- label_number(scale_cut = cut_si(""), accuracy = 0.1)

ggplot(regioes_long2,
       aes(x = `Região/UF de notificação`,
           y = Casos,
           color = `Região/UF de notificação`,
           group = `Região/UF de notificação`)) +
  
  geom_line(size = 1) +
  geom_point(size = 2.5) +

  # adicionar SOMENTE o maior de cada faixa
  geom_label(
    data = max_por_faixa,
    aes(label = format_num(Casos)),
    fill = "#fff3b0",
    color = "black",
    fontface = "bold",
    size = 3,
    linewidth = 0.3,
    nudge_y = 0.05 * max(regioes_long2$Casos)
  ) +
  
  facet_wrap(~ Faixa_Etaria, scales = "free_y") +
  
  scale_color_brewer(palette = "Dark2") +
  
  theme_minimal(base_size = 13) +
  
  labs(
    title = "Distribuição dos Casos por Faixa Etária — Regiões",
    x = "Região",
    y = "Número de Casos",
    color = "Região"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 18, face = "bold")
  )
