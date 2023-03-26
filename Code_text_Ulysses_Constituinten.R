
rm(list = ls(all=T)) # para limpar tudo no ambiente
# Packages
library(tidyverse)   # pacotes de organizaçõa de dados
library(tm)          # text mining package
library(pdftools)    # para extrair textos, anexos e metadata de arquivos pdf
library(tidytext)    # tidy text for plots
library(rvest)       # usado para ler páginas html
library(knitr)       # used to make kable tables
library(SnowballC)   # applies Porter's stemmming algorithm (discussed later)
library(magrittr)    # allows pipe operator
library(wordcloud2)

#Carregando o stopwords do pacote tm
sw_pt_tm <- stopwords('portuguese')

#Convertendo para dataframe
sw_pt_tm <- data.frame(sw_pt_tm)

#Renomeando a variável
sw_pt_tm <- rename(sw_pt_tm, palavra = sw_pt_tm)

#Possível complementar a biblioteca de stopwords:
stopwords_personal <- tibble(palavra = c("é", "ainda", "alguns", "pra"))
# *****************************************************************************************
# Quais são as fontes objetos de análise:
getSources()
# As mais utilizadas são:
## VectorSource a vector of characters (treats each component as a document)
## DataframeSource a data frame containing text (like CSV files)
## DirSource for use with file directories

# recursos de leitura do pacote:
getReaders()
# *****************************************************************************************
# Lendo em pdf:
# Análise discurso Ulysses Guimarães:
# https://www.bbc.com/portuguese/brasil-45750071
# Íntegra do discurso de 05 de outubro de 1988:
# https://www.camara.leg.br/radio/programas/277285-integra-do-discurso-presidente-da-assembleia-nacional-constituinte-dr-ulysses-guimaraes-10-23/
# *****************************************************************************************
pdf_info("Ulysses_Constituinte.pdf")
ulysses <- pdf_text("Ulysses_Constituinte.pdf")
View(ulysses)
ulysses
# O conteúdo do texto está nas linhas 1 e 2
ulysses <- ulysses[c(1,2)]
View(ulysses)
ulysses <- paste(ulysses, collapse = " ")
View(ulysses)
ulysses

# grava o arquivo em formato txt:
write_lines(ulysses, "ulysses.txt")
# # Importante verificar como ficou o arquivo txt e eliminar as linhas não informativas
ulysses <- str_remove_all(ulysses, "\nhttps://www.camara.leg.br/radio/programas/277285-integra-do-discurs…nte-da-assembleia-nacional-constituinte-dr-ulysses-guimaraes-10-23/        Página 1 de 3
#  Portal da Câmara dos Deputados                                                                                                               02/10/2020 23'15\n")
# ulysses

texto <- read_csv("ulysses.txt", col_names = FALSE) 
novo_texto <- texto[11,1] 
novo_texto

novo_texto <- as.matrix(novo_texto)
novo_texto <- as.vector(novo_texto)
novo_texto

novo_texto <- str_replace(novo_texto, "https://www.camara.leg.br/radio/programas/277285-integra-do-discurs…nte-da-assembleia-nacional-constituinte-dr-ulysses-guimaraes-10-23/        Página 1 de 3
 Portal da Câmara dos Deputados                                                                                                               02/10/2020 23'15", "")
novo_texto

# *****************************************************************************************
# *****************************************************************************************
novo_origem <- read_lines("ulysses.txt") 
str(novo_origem)

dez <- rep(1:ceiling(length(novo_origem)/10), each = 10)
str(dez)

dez <- dez[1:length(novo_origem)]
str(dez)

novo_texto <- cbind(dez, novo_origem) %>% data.frame()

novo_texto <- aggregate(formula = novo_origem ~ dez,
                        data = novo_texto,
                        FUN = paste,
                        collapse = " ")

novo_texto <- novo_texto %>% select(novo_origem) %>% as.matrix
dim(novo_texto)

# *****************************************************************************************
# *****************************************************************************************
novo_texto <- gsub("[[:cntrl:]]", " ", novo_texto) # elimina caracteres especiais, saltos de linha,..
novo_texto <- tolower(novo_texto) # tudo em minusculas
novo_texto <- removePunctuation(novo_texto) # remove pontuação
novo_texto <- removeWords(novo_texto, stopwords("pt"))
novo_texto <- stripWhitespace(novo_texto) # remove espaços em branco
novo_texto # texto limpo

tibble(novo_texto)
novo_texto <- tibble(texto = novo_texto) #renomear a variável
novo_texto

texto_palavras <- novo_texto %>% 
  unnest_tokens(palavra, texto, strip_numeric = TRUE) %>% 
  count(palavra, sort = TRUE)

palavras_freq <- texto_palavras %>% 
  anti_join(stopwords_personal) %>%
  anti_join(sw_pt_tm)

# Trabalhar os radicais das palavras
stemDocument(c("politics", "political", "politically"), language = "english")

# **************************************************************************
# **************************************************************************
# **************************************************************************
# WORDCLOUD ----

wordcloud2(data = palavras_freq, size = 0.5)

wordcloud2(data = palavras_freq, size = 0.5, shape = "star", ellipticity = 0.8, gridSize =  0)

# **************************************************************************
# **************************************************************************
grafico_palavras <- palavras_freq %>% 
  top_n(10) %>% 
  ggplot(aes(y = reorder(palavra, n), x = n)) + 
  geom_col(fill = "#EEBBAA") +labs(y = NULL, x = "frequencia",
                                   title = "As 10 palavras mais frequentes") + theme_minimal()

grafico_palavras
# ******
# OBS: tem uma função para plotar o wordcloud no shiny:
# No help do R: Plot wordcloud2 in shiny
# *****
# SHAPES:
##  circle.cardioid.diamond.triangle-forward.triangle.pentagon.star
# **************************************************************************
# **************************************************************************
#unlist transforma a lista em um vetor único:
novo_texto <- as.matrix(novo_texto)
novo_texto <- as.vector(novo_texto)

corpus_source <- VectorSource(novo_texto)
novo_corpus <- Corpus(corpus_source)

#Mapeando o Corpus indicando que é uma matriz de termos para fazer associação entre palavras
novo_tdm <- TermDocumentMatrix(novo_corpus)

novo_tdm #terms == número de palavras distintas no corpus. documents: quantos textos tomados

findAssocs(novo_tdm, "constituição", 0.7) # associaçã mínima de 0.7 com a palavra país

findAssocs(novo_tdm, terms = c("sociedade", "nação", "estado"), corlimit = .70)

# Remover termos com baixa associação para reduzir a matriz de associação
inspect(removeSparseTerms(novo_tdm, 0.4))

# Necessário para fazer as associações selecionar a quantidade de palavras
## A função 'removeEparseTerms'possui um range de 0 a 1.Elimina palavras que aparecem com pouca frequencia
### Mas proximo de 0 mais espaçado, mais próximo de 1 menos espaçado e maior a associação
### Idealmente encontrar um número de palavras que não seja reprodução do vocabulário do texto 
### mas tenha um número de palavras adequado para análise estruturação de gráficos.
novo_new <- removeSparseTerms(novo_tdm, sparse = .75)

novo_tdm # quantos termos originalmente

novo_new # quantos termos ficaram

novo_tdm$nrow # para ver o número de linhas/ palavras do corpus original
novo_new$nrow # para ver o número de linhas palavras do corpus reduzido
##########################################################
# MATRIZ DE DISTANCIAS ----
##########################################################
# Para realizar as operações transformar em matriz
novo_new <- novo_new %>% as.matrix()

# Existe a função 'scale' ela realiza a padronização a partir de cada coluna. 
## Muito util de cada coluna for um texto  como uma unidade completa

# No caso vamos usar a média por linha para ver a associação das palavras por linha
novo_new <- novo_new / rowSums(novo_new)
novo_new

# matriz de distâncias com distâncias euclidianas
novo_dist <- dist(novo_new, method = "euclidian")
novo_dist
##########################################################
# AGRUPAMENTO HIERÁRQUICO
##########################################################
novo_hclust <-  hclust(novo_dist, method = "ward.D")

# para geraar o gráfico

plot(novo_hclust, main = "Dendrograma do discurso - hclust", sub = "", xlab = "")

# para o caso de querer colocar os clusters em destaque:
plot(novo_hclust, main = "Dendrograma do discurso - hclust", sub = "", xlab = "")
rect.hclust(novo_hclust, k = 5, border="blue")
