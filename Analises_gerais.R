# --- --- --- --- --- --- --- --- --- --- --- ---
# ETAPA 1: LIMPEZA E TRANSFORMAÇÃO (Todos os Peçonhentos)
# --- --- --- --- --- --- --- --- --- --- --- ---

# Filtrar apenas para garantir que TP_ACIDENT é válido (1 a 5)

dados <- dados_brutos %>%
  filter(TP_ACIDENT %in% c("1", "2", "3", "4", "5")) 

# Filtrar apenas para garantir que TP_ACIDENT é válido (1 a 5)

dados1 <- dados_atuais %>%
  filter(TP_ACIDENT %in% c("1", "2", "3", "4", "5")) 

# Filtrar apenas para garantir que TP_ACIDENT é válido (1 a 5)

dados2 <- dados_antigos %>%
  filter(TP_ACIDENT %in% c("1", "2", "3", "4", "5")) 


# --- BLOCO DE SEGURANÇA: GARANTIR COLUNAS ---
# Isso verifica se as colunas de Aranha, Escorpião e Lagarta existem.
# Se não existirem, o R cria elas vazias para não dar erro.

if(!"ANI_SERPEN" %in% names(dados2)) dados2$ANI_SERPEN <- NA
if(!"ANI_ARANHA" %in% names(dados2)) dados2$ANI_ARANHA <- NA
if(!"ANI_ESCORP" %in% names(dados2)) dados2$ANI_ESCORP <- NA
if(!"ANI_LAGART" %in% names(dados2)) dados2$ANI_LAGART <- NA


# Limpar a memória
rm(dados_brutos)

dados2 <- dados2 %>%
  mutate(
    # --- IDADE ---
    IDADE_NUM = suppressWarnings(as.numeric(str_sub(NU_IDADE_N, 2, 4))),
    
    # --- TIPO DE ANIMAL (Grande Grupo) ---
    TIPO_ANIMAL = factor(case_when(
      TP_ACIDENT == "1" ~ "Serpente",
      TP_ACIDENT == "2" ~ "Aranha",
      TP_ACIDENT == "3" ~ "Escorpião",
      TP_ACIDENT == "4" ~ "Lagarta",
      TP_ACIDENT == "5" ~ "Abelha",
      TRUE ~ "Outros/Ignorado"
    )),
    
    # --- CLASSIFICAÇÃO ESPECÍFICA (Gênero/Tipo) ---
    # Aqui consolidamos as colunas ANI_SERPEN, ANI_ARANHA, ANI_ESCORP, etc.
    AGENTE_f = factor(case_when(
      # SERPENTES (ANI_SERPEN)
      TP_ACIDENT == "1" & ANI_SERPEN == "1" ~ "Serpente - Bothrops (Jararaca)",
      TP_ACIDENT == "1" & ANI_SERPEN == "2" ~ "Serpente - Crotalus (Cascavel)",
      TP_ACIDENT == "1" & ANI_SERPEN == "3" ~ "Serpente - Lachesis (Surucucu)",
      TP_ACIDENT == "1" & ANI_SERPEN == "4" ~ "Serpente - Micrurus (Coral)",
      TP_ACIDENT == "1" & ANI_SERPEN == "5" ~ "Serpente - Não Peçonhenta",
      
      # ARANHAS (ANI_ARANHA)
      TP_ACIDENT == "2" & ANI_ARANHA == "1" ~ "Aranha - Phoneutria (Armadeira)",
      TP_ACIDENT == "2" & ANI_ARANHA == "2" ~ "Aranha - Loxosceles (Marrom)",
      TP_ACIDENT == "2" & ANI_ARANHA == "3" ~ "Aranha - Latrodectus (Viúva-Negra)",
      TP_ACIDENT == "2" & ANI_ARANHA == "4" ~ "Aranha - Outras/Viúva-Marrom",
      
      # ESCORPIÕES (ANI_ESCORP)
      TP_ACIDENT == "3" ~ "Escorpião - Tityus (Amarelo/Marrom)",
      
      # LAGARTAS (ANI_LAGART)
      TP_ACIDENT == "4" & ANI_LAGART == "1" ~ "Lagarta - Lonomia",
      TP_ACIDENT == "4" & ANI_LAGART == "2" ~ "Lagarta - Outras",
      
      # ABELHAS (Não tem sub-classificação no SINAN comum)
      TP_ACIDENT == "5" ~ "Abelhas/Vespas",
      
      # CASOS ONDE O GÊNERO FOI IGNORADO
      TRUE ~ "Espécie Ignorada/Não Informada"
    )),
    
    # --- OUTRAS VARIÁVEIS ---
    
    # Gravidade
    GRAVIDADE_f = factor(case_when(
      TRA_CLASSI == "1" ~ "Leve",
      TRA_CLASSI == "2" ~ "Moderada",
      TRA_CLASSI == "3" ~ "Grave",
      TRUE ~ "Ignorado/Em Branco"
    ), levels = c("Leve", "Moderada", "Grave", "Ignorado/Em Branco")),
    
    # Evolução
    EVOLUCAO_f = factor(case_when(
      EVOLUCAO == "1" ~ "Cura",
      EVOLUCAO == "2" ~ "Óbito (pelo Acidente)",
      EVOLUCAO == "3" ~ "Óbito (Outra Causa)",
      TRUE ~ "Ignorado/Em Branco"
    )),
    
    # Sexo
    SEXO_f = factor(case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      TRUE ~ "Ignorado"
    )),
    
    # Tempo Atendimento
    TEMPO_ATEND_f = factor(case_when(
      ANT_TEMPO_ == "1" ~ "0-1 hora",
      ANT_TEMPO_ == "2" ~ "1-3 horas",
      ANT_TEMPO_ == "3" ~ "3-6 horas",
      ANT_TEMPO_ == "4" ~ "6-12 horas",
      ANT_TEMPO_ == "5" ~ "12-24 horas",
      ANT_TEMPO_ == "6" ~ "+24 horas",
      TRUE ~ "Ignorado"
    ),
    levels = c("0-1 hora", "1-3 horas", "3-6 horas", "6-12 horas", "12-24 horas", "+24 horas", "Ignorado")
    ),
    
    # Datas
    DATA_NOT = as_date(DT_NOTIFIC), 
    ANO_NOT = year(DATA_NOT),
    MES_NOT = month(DATA_NOT, label = TRUE)
    
  )


### --- ANÁLISE 1: FREQUÊNCIAS (AGENTE CAUSADOR DETALHADO) --- ###

### Frequência de Acidentes por Agente Específico ###

# Filtrar os ignorados para o gráfico ficar mais limpo (testar)

freq_agente2 <- dados2 %>%
  filter(AGENTE_f != "Espécie Ignorada/Não Informada") %>%
  count(AGENTE_f, TIPO_ANIMAL, sort = TRUE) %>%
  mutate(Percentual = n / sum(n) * 100)

print(head(freq_agente, 20))

library(scales) ### Para melhorar as escalas de números nos gráficos

# Gráfico 1: Frequência Geral

cores_personalizadas <- c(
  "Serpente" = "#2E86C1",  # Azul Aço 
  "Aranha"   = "#D35400",  # Laranja Queimado 
  "Escorpião"= "#C0392B",  # Vermelho Escuro 
  "Lagarta"  = "#27AE60",  # Verde Folha 
  "Abelha"   = "#F1C40F",  # Amarelo Ouro 
  "Outros"   = "#7F8C8D"   # Cinza
)

g1_antigo <- ggplot(freq_agente2, aes(x = reorder(AGENTE_f, n), y = n, fill = TIPO_ANIMAL)) +
  
  # 1. Barras
  geom_col(width = 0.8) + 
  
  # 2. Rótulos (Números na ponta das barras)
  # Aqui usamos uma lógica: Se for maior que 1 milhão, usa "Mi", senão "mil"
  geom_text(
    aes(label = ifelse(n > 1000000, 
                       paste0(format(round(n/1000000, 2), decimal.mark=","), " M"),
                       paste0(format(round(n/1000, 1), decimal.mark=","), " k"))),
    hjust = -0.1, # Afasta um pouco o texto da barra
    size = 3.5,
    fontface = "plain",
    color = "#333333"
  ) +
  
  # 3. Inverter eixos
  coord_flip() + 
  
  # 4. Cores Personalizadas
  scale_fill_manual(values = cores_personalizadas) +
  
  # 5. Eixo Numérico (O eixo de baixo)
  scale_y_continuous(
    # Aplicamos a escala 0.001 para dividir por 1000
    labels = scales::label_number(
      scale = 0.001, 
      suffix = " k", # Define o sufixo como " mil"
      big.mark = ".", 
      decimal.mark = "," # Garante a formatação BR
    ),
    # Expande o limite (mantido)
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  # 6. Títulos e Tema
  labs(title = "Notificações por Agente Específico",
       subtitle = "Total de casos registrados de 2007 a 2019",
       x = NULL, # Remove label "Agente" (redundante)
       y = "Número de Notificações",
       fill = "Grupo Animal") + # Nome da Legenda
  
  theme_minimal() +
  theme(
    legend.position = "bottom", # Legenda embaixo
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.grid.major.y = element_blank() # Remove linhas horizontais
  )

print(g1_antigo)

# Se não necessitar de alterações salvar em boa resolução .png

ggsave(
  filename = "Frequencia_Agentes_2007_2019_HD.png",
  plot = g1_antigo,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)


### --- ANÁLISE 2: PERFIL (IDADE) --- ###

g2_ATUAL <- ggplot(dados1 %>% filter(IDADE_NUM > 0 & IDADE_NUM < 95), aes(x = IDADE_NUM)) +
  geom_histogram(binwidth = 5, fill = "darkorange", color = "white") +
  scale_y_continuous(
    # scale = 0.001 -> Divide o número por 1000 (300000 vira 300)
    # suffix = " k" -> Adiciona o texto no final
    labels = label_number(scale = 0.001, suffix = " k", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_continuous(
    breaks = seq(0, 95, by = 5), # Mostra a idade de 5 em 5 anos
    limits = c(0, 95) # Trava o gráfico no limite escolhido
  ) +
  labs(title = "Distribuição de Casos por Idade (Todos os Acidentes Peçonhentos)",
       subtitle = paste("Casos registrados de 2007 a 2025"),
       x = "Idade (anos)",
       y = "Número de Casos") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10), # Aumenta um pouco a fonte da idade
    axis.text.y = element_text(size = 10)
  )
print(g2)

ggsave(
  filename = "Casos_Idade_HD.png",
  plot = g2,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)

### --- ANÁLISE 3: GRAVIDADE X TIPO DE ANIMAL --- ###

grav_x_animal <- dados2 %>%
  filter(GRAVIDADE_f != "Ignorado/Em Branco") %>%
  count(TIPO_ANIMAL, GRAVIDADE_f) %>%
  group_by(TIPO_ANIMAL) %>%
  mutate(Percentual = n / sum(n) * 100)

g3_antigo <- ggplot(grav_x_animal, aes(x = TIPO_ANIMAL, y = n, fill = GRAVIDADE_f)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Gravidade do Acidente por Grupo Animal",
       x = "Grupo", y = "Proporção (%)", fill = "Gravidade") +
  coord_flip() +
  theme_minimal()
print(g3)

ggsave(
  filename = "Casos_Idade_HD.png",
  plot = g3,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)


# Definindo uma paleta de cores específicas para a GRAVIDADE
# Cores personalizadas para Gravidade (ex: Verde para Leve, Amarelo para Moderado, Vermelho para Grave)
cores_gravidade <- c(
  "Leve"      = "darkgreen", # Verde Limão
  "Moderada"  = "yellow", # Amarelo Ouro
  "Grave"     = "darkred", # Vermelho Laranja
  "Ignorado/Em Branco" = "#CCCCCC" # Cinza para ignorado
)

g3_agrupado_antigo <- ggplot(grav_x_animal, aes(x = TIPO_ANIMAL, y = Percentual, fill = GRAVIDADE_f)) +
  
  # Barras Agrupadas
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Rótulos de Porcentagem nas Barras
  geom_text(
    aes(label = paste0(round(Percentual, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5, # Ajuste vertical para o texto ficar acima da barra
    size = 3,
    color = "black"
  ) +
  
  # Usar as cores personalizadas para Gravidade
  scale_fill_manual(values = cores_gravidade) +
  
  # Eixo Y como porcentagem
  scale_y_continuous(labels = percent_format(scale = 1),
                     limits = c(0, 100), # Garante que o eixo vá de 0 a 100%
                     expand = expansion(mult = c(0, 0.1))) + # Espaço extra acima
  
  # Títulos e rótulos
  labs(
    title = "Gravidade dos Acidentes por Grupo Animal",
    subtitle = "Proporção de casos Leves, Moderados e Graves em cada grupo (2007 - 2019)",
    x = "Grupo Animal",
    y = "Proporção de Casos (%)",
    fill = "Gravidade do Acidente" # Legenda da cor
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"), # Gira o texto e negrito
    legend.position = "bottom", # Legenda na parte inferior
    panel.grid.major.x = element_blank() # Remove linhas de grade verticais
  )

print(g3_agrupado_antigo)

ggsave(
  filename = "Gravidade_por_Grupo_2007_2019_HD.png",
  plot = g3_agrupado_antigo,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)

# Repensar esse gráfico!

### --- ANÁLISE 4: LETALIDADE POR AGENTE ESPECÍFICO --- ###
# Observar quais grupos mais matam proporcionalmente

letalidade2 <- dados2 %>%
  filter(EVOLUCAO_f != "Ignorado/Em Branco" & AGENTE_f != "Espécie Ignorada/Não Informada") %>%
  group_by(AGENTE_f) %>%
  summarise(
    Total_Casos = n(),
    Obitos = sum(EVOLUCAO_f == "Óbito (pelo Acidente)"),
    Letalidade_pct = (Obitos / Total_Casos) * 100
  ) %>%
  filter(Total_Casos > 50) %>% # Filtrar agentes com poucos casos para não distorcer
  arrange(desc(Letalidade_pct))

print(letalidade1)

# Gráfico de Letalidade
g_let2 <- ggplot(letalidade1, aes(x = reorder(AGENTE_f, Letalidade_pct), y = Letalidade_pct)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Letalidade (% de Óbitos) por Agente",
       subtitle = "Considerando apenas agentes com > 50 casos registrados",
       x = "Agente", y = "Letalidade (%)") +
  theme_minimal()
print(g_let1)

ggsave(
  filename = "Letalidade_por_Agente_2020_2025_HD.png",
  plot = g_let1,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)

## Gráfico com escala de cor e % a frente

g_let_cor2 <- ggplot(letalidade2, aes(x = reorder(AGENTE_f, Letalidade_pct), y = Letalidade_pct)) +
  
  # Adiciona as barras com preenchimento (fill) baseado na Letalidade_pct
  # Usamos geom_col (sinônimo de geom_bar(stat="identity"))
  geom_col(aes(fill = Letalidade_pct)) +
  geom_text(
    aes(label = paste0(format(round(Letalidade_pct, 2), decimal.mark = ","), "%")), 
    hjust = -0.1,     
    size = 3.5,       
    fontface = "plain" 
  ) +
  
  # Inverte os eixos para barras horizontais
  coord_flip() +
  
  # Define a escala de cor gradiente
  # scale_fill_gradient ou scale_fill_gradientn
  # low = cor mais clara, high = cor mais escura
  scale_fill_gradient(
    low = "#fbdada",  # Um vermelho bem claro (ou outro tom que preferir)
    high = "darkred", # O vermelho escuro que você já usou
    name = "Letalidade (%)", # Título da legenda da cor
    labels = percent_format(scale = 1) # Formata a legenda da cor como %
  ) +
    scale_y_continuous(
    labels = percent_format(scale = 1), # Formata como porcentagem
    expand = expansion(mult = c(0.01, 0.05)) # Ajusta um pouco o espaço para as barras não ficarem "coladas"
  ) +
  labs(
    title = "Letalidade por Agente Causador de Acidentes Peçonhentos",
    subtitle = "Considerando apenas agentes com mais de 50 casos registrados (2007 - 2019)",
    x = "Agente / Gênero do Animal",
    y = "Taxa de Letalidade"
  ) +
  
  # Tema minimalista para um visual limpo
  theme_minimal() +
  theme(
   # Título do gráfico em negrito
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", color = "black"), # Nomes dos bichos mais escuros
    panel.grid.major.y = element_blank(), # Remove linhas horizontais para limpar o visual
    legend.position = "none" # Remove a legenda de cor, pois o texto já diz o valor
  )

print(g_let_cor2)

ggsave(
  filename = "Letalidade_por_Agente_2020_2025_HD.png",
  plot = g_let_cor1,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)


# --- ANÁLISE DE CASOS TOTAIS POR ANO  --- #

# 1. Contar os casos por ANO_NOT
freq_ano <- dados %>% 
  count(ANO_NOT, name = "Total_Casos")

# 2. Gráfico de Barras
g_ano <- ggplot(freq_ano, aes(x = factor(ANO_NOT), y = Total_Casos)) +
  
  geom_col(fill = "#2E86C1") + 
  
  geom_text(
    aes(label = scales::label_number(scale = 0.001, suffix = " k", big.mark = ".", decimal.mark = ",")(Total_Casos)),
    vjust = -0.5,  # Afasta o texto um pouco da barra
    size = 3,
    color = "#333333",
    fontface = "plain"
  ) +
  
  # Formatar o eixo Y (mantido igual, está correto)
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = " k", big.mark = ".", decimal.mark = ","),
    # Expandir o eixo para o rótulo da maior barra caber sem cortar
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  labs(title = "Acidentes por Ano",
       subtitle = "Período de 2007 a 2025",
       x = "Ano de Notificação",
       y = "Número de Casos") +
  
  theme_minimal() +
  theme(
    
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
    
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(g_ano)

ggsave(
  filename = "Acidentes_por_Ano_HD.png",
  plot = g_ano,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)

### Análises relacionadas ao sexo ###

# --- ANÁLISE DE BARRAS AGRUPADAS (IDADE x SEXO) ---

# 1. Preparar os dados (Mantido igual)
dados_agrupados1 <- dados1 %>%
  # Filtrar Idade e Sexo
  filter(IDADE_NUM >= 0 & IDADE_NUM <= 100 & SEXO_f %in% c("Masculino", "Feminino")) %>%
  
  # Criar faixas etárias de 5 em 5 anos e Contar (n)
  group_by(SEXO_f, IDADE_BIN = cut(IDADE_NUM, breaks = seq(0, 100, 5), right = FALSE, include.lowest = TRUE)) %>%
  summarise(n = n(), .groups = 'drop')


# 2. Gráfico de Barras Agrupadas
g2_horizontal1 <- ggplot(dados_agrupados1, aes(x = IDADE_BIN, y = n, fill = SEXO_f)) +
  
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = " k", big.mark = "."),
    expand = expansion(mult = c(0, 0.10)) # Espaço extra acima das barras
  ) +
  
  # Cores para Sexo
  scale_fill_manual(values = c("Masculino" = "#2E86C1", "Feminino" = "#C0392B")) +
  
  labs(title = "Distribuição de Casos por Idade e Sexo",
       subtitle = "Número de notificações em faixas etárias de 5 anos.",
       x = "Faixa Etária",
       y = "Número de Casos",
       fill = "Sexo") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Centraliza título
    plot.subtitle = element_text(hjust = 0.5),           # Centraliza subtítulo
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    
    panel.grid.major.y = element_blank() # Remove linhas de grade horizontais
  )

print(g2_horizontal1)

ggsave(
  filename = "Distribuicao_por_idade_HD.png",
  plot = g2_horizontal,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)

# --- ANÁLISE: SÉRIE TEMPORAL POR SEXO ---

# 1. Preparar os dados: Filtrar apenas Masculino e Feminino e contar por ano
freq_ano_sexo <- dados %>%
  filter(SEXO_f %in% c("Masculino", "Feminino")) %>%
  count(ANO_NOT, SEXO_f, name = "Total_Casos")

# 2. Gráfico de Barras Agrupadas
g_ano_sexo <- ggplot(freq_ano_sexo, aes(x = factor(ANO_NOT), y = Total_Casos, fill = SEXO_f)) +
  
  # Barras agrupadas (lado a lado)
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Adicionar Rótulos (Usamos precisão 1e-3 para mostrar em mil)
  geom_text(
    # Ex: 100 mil. accuracy=1 evita casas decimais longas
    aes(label = scales::number(Total_Casos, big.mark = ".", scale = 1e-3, accuracy = 1)),
    position = position_dodge(width = 0.8), 
    vjust = -0.5,
    size = 2.5,
    color = "gray30"
  ) +
  
  # Formatar o eixo Y para milhares (mil)
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = " k", big.mark = "."),
    expand = expansion(mult = c(0, 0.10)) # Espaço extra acima
  ) +
  scale_fill_manual(values = c("Masculino" = "#2E86C1", "Feminino" = "#C0392B")) +
  
  labs(title = "Evolução Anual de Acidentes por Sexo",
       subtitle = "Comparativo do volume de notificações entre Masculino e Feminino ao longo dos anos.",
       x = "Ano de Notificação",
       y = "Número de Casos",
       fill = "Sexo") +
  
  theme_minimal() +
  theme(
    # Centralização
    plot.title = element_text(face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5),
    # Rotação para anos
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.position = "bottom"
  )

print(g_ano_sexo)

ggsave(
  filename = "Evolucao_anual_sexo_HD.png",
  plot = g_ano_sexo,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300 # Define a qualidade (300 DPI é o mínimo para impressão)
)
