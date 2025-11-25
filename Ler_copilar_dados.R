# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# PROJETO: Análise Epidemiológica de Acidentes Ofídicos no Brasil (SINAN/DATASUS)
# Autor: Raícia Caroline de Souza Julião; Camila Shyu Fiorind]
# Fonte dos Dados: Microdados .dbc (2007-2024)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- ---
# ETAPA 1: INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# --- --- --- --- --- --- --- --- --- --- --- ---

# Instalar pacotes necessários (só precisa rodar uma vez)
install.packages("remotes")
remotes::install_github("danicat/read.dbc") # Pacote essencial (Link corrigido)
install.packages("dplyr")    # Para manipulação de dados
install.packages("ggplot2")  # Para criar os gráficos
install.packages("stringr")  # Para limpar a coluna de Idade
install.packages("lubridate") # Para trabalhar com datas

# Carregar os pacotes que usaremos
library(read.dbc) # Para ler arquivos .dbc
library(dplyr)    # Para manipulação de dados
library(stringr)  # Para manipulação de texto (Idade)
library(ggplot2)  # Para gráficos
library(lubridate) # Para extrair o ano e mês da notificação

# --- --- --- --- --- --- --- --- --- --- --- ---
# ETAPA 2: EXPLORANDO UM ÚNICO ARQUIVO (Ex: o mais recente)
# --- --- --- --- --- --- --- --- --- --- --- ---

# Coloque TODOS os seus 24 arquivos .dbc na mesma pasta deste script.

# Vamos carregar apenas UM arquivo primeiro para ver como ele é.
# (Use o nome de um arquivo que você baixou, ex: "ANIMBR07.dbc")
tryCatch({
  # Corrigido para o nome do arquivo que você usou
  dados_exemplo <- read.dbc("ANIMBR07.dbc") 
  
  print("Arquivo de exemplo (ANIMBR07.dbc) carregado. Vamos inspecionar:")
  
  # Ver os nomes das colunas
  print("--- Nomes das Colunas (Variáveis) ---")
  print(names(dados_exemplo))
  
  # Ver a estrutura (tipos de dados)
  print("--- Estrutura dos Dados (str) ---")
  str(dados_exemplo, max.level = 1)
  
  # Ver as primeiras linhas
  print("--- Primeiras Linhas (head) ---")
  print(head(dados_exemplo))
  
  # Variáveis-chave que vamos procurar (com base na saída de names()):
  # DT_NOTIFIC (Data da Notificação)
  # NU_IDADE_N (Idade, no formato "4025")
  # CS_SEXO (Sexo)
  # TP_ACIDENT (Tipo de Acidente / Animal)
  # ANI_SERPEN (Gênero da Serpente)
  # ANT_TEMPO_ (Tempo até Atendimento)
  # TRA_CLASSI (Classificação Final / Gravidade)
  # EVOLUCAO (Evolução do Caso)
  
}, error = function(e) {
  print("Erro ao carregar o arquivo de exemplo.")
  print("Verifique se o arquivo (ex: 'ANIMBR07.dbc') está na mesma pasta do script.")
})


# --- --- --- --- --- --- --- --- --- --- --- ---
# ETAPA 3: CARREGAR E COMPILAR TODOS OS 24 ARQUIVOS
# --- --- --- --- --- --- --- --- --- --- --- ---

print("Iniciando carregamento de TODOS os arquivos .dbc...")

# 1. Listar todos os arquivos .dbc na pasta
# CORREÇÃO: Alterado de "ANIMPECO" para "ANIMBR" com base no seu arquivo.
arquivos_dbc <- list.files(pattern = "^ANIMBR.*\\.dbc$", ignore.case = TRUE)

if (length(arquivos_dbc) == 0) {
  print("Nenhum arquivo .dbc (ex: ANIMBRXX.dbc) encontrado na pasta de trabalho.")
  print(paste("Verifique se os arquivos estão neste diretório:", getwd()))} 

else {print(paste(length(arquivos_dbc), "arquivos .dbc encontrados:", toString(arquivos_dbc)))}
  
  # 2. Ler todos os arquivos em uma lista de data.frames
  
  lista_de_dados <- lapply(arquivos_dbc, function(f) {
    print(paste("Carregando:", f))
    tryCatch({
      # CORREÇÃO VITAL (A): Ler o arquivo
      df <- read.dbc(f)
      # FORÇAR TODAS AS COLUNAS PARA CHARACTER ANTES DE JUNTAR
      # Isso evita erros de "bind" entre colunas Date, Factor e Numeric.
      df <- df %>% mutate_all(as.character)
      return(df)
      
    }, error = function(e) {
      print(paste("Erro ao ler o arquivo", f, ":", e$message))
      NULL # Retorna NULL se houver erro
    })
  })
  
  # Remove arquivos que falharam (NULLs)
  
  lista_de_dados <- Filter(Negate(is.null), lista_de_dados)
  
  # 3. Combinar (empilhar) todos os data.frames em um único
  
  dados_brutos <- dplyr::bind_rows(lista_de_dados)
  
  # --- FILTRO PARA DADOS ATUAIS (2020-2025) ---
  
  dados_atuais <- dados_brutos %>%
    filter(DT_NOTIFIC >= 2020 & DT_NOTIFIC <= 2025)
  
  # Limpar a memória (opcional)
  
  rm(lista_de_dados)

  
  # --- FILTRO PARA DADOS ANTIGOS (2007-2019) ---
  
  dados_antigos <- dados_brutos %>%
    filter(DT_NOTIFIC >= 2007 & DT_NOTIFIC <= 2019)
  
  # Limpar a memória (opcional)
  
  rm(lista_de_dados)
  
  