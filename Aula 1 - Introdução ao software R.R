
####################################################
#   Manipulação e Análise Exploratória de dados    #
####################################################

# Pacotes
library(dplyr)
#install.packages("dplyr") #Caso não tenha o Dplyr instalado, retirar o primeiro "#" e rodar

# Dados do Atlas de Desenvolvimento Humano de 2013, com municípios do ano 2010
# Veja o dicionário de dados "ATLAS_Layout.xls"

### 1º passo: modificar o diretório
### 2º passo: leitura de dados
### 3º passo: manipulação da base de dados
### 4º passo: análise descritiva

####################################
### 1º passo: modificar o diretório

getwd() #verificar o diretório atual

# Escolher diretório de trabalho
# Selecione o diretório que contenha o conjunto de dados a ser analisado
setwd("C:\\Users\\PedroGomes\\Google Drive\\Curso da POF\\Curso POF - Alunos\\Dados") #atenção para as barras duplas na hora de colocar o diretório

getwd() #Ok

# O diretório contém os arquivos desejados?
list.files()

##############################
### 2º passo: leitura de dados

# Define número de dígitos usados para impressão de valores na tela console
options(digits = 4) #comando opcional

# Importar dados
# A primeira linha apresenta o nome das variávies, separador ";" e decimal "."
munics <- read.csv("ATLAS_2013.csv", header=T, sep=";", dec=".")

# Estudando a base lida
View(munics)      #visualizar a base de dados, cuidado em bases muito grandes, usar head(). LEMBRAR DE FECHAR A BASE
fix(munics)       #outra forma. LEMBRAR DE FECHAR A BASE

head(munics,5)    #visualizar as 5 primeiras linhas
tail(munics,5)    #visualizar as 5 últimas linhas

str(munics)       #verificar a escala de mensuração de cada variável
glimpse(munics)   #outra forma
dim(munics)       #consultar a dimesão do conjunto de dados

names(munics)

###########################################
### 3º passo: manipulação da base de dados

# Olhe o dicionário de dados "ATLAS_Layout.xls"
# As variáveis estão com escala de mensuração coerente?

# Recodificar variáveis, de números para caracteres
# A função mutate() do pacote "dplyr" permite criação/atualização de variáveis

str(munics$REGIAO)

munics <- munics %>%
  dplyr::mutate(REGIAO = as.factor(REGIAO),
                UF = as.factor(UF),
                UFN = as.factor(UFN),
                Codmun6 = as.factor(Codmun6),
                Município = as.factor(Município),
                one = 1)
  
str(munics) #ok, tudo bem agora
glimpse(munics) #outra forma

dim(munics) #5565 municípios e 239 variáveis (variáveis modificadas foram sobrescritas)

colSums(is.na(munics)) #contagem de missings por variável 

# Codificação das Grandes Regiões, conforme dicionário de dados
str(munics$REGIAO)
levels(munics$REGIAO)

munics <- munics %>%
  dplyr::mutate(REGIAO = factor(munics$REGIAO,
                        label = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro Oeste"), levels = 1:5))

levels(munics$REGIAO) #ok, agora aparece o nome das Grandes Regiões

#################################
### 4º passo: análise descritiva

# Esquema de 5 números + a média
summary(munics) #não faz sentido para todas as variáveis

summary(munics$ESPVIDA) #fazendo para apenas uma


#################################################################################
# Análises de Expectativa de Vida:                                              #
#################################################################################
# Quantos municípios cada UF possui?                                            #
# Qual o município que apresenta a menor expectativa de vida do país?           #
# Identifique o município com a maior expectativa de vida (função max)          #
# Quais os municípios, apresentam maior expectativa de vida por UF?             #
# Quais os municípios, apresentam maior expectativa de vida por Grande Região?  #
#################################################################################

#####################################
# Quantos municípios cada UF possui?
#install.packages("janitor") #caso não tenha instalado, rodar sem os comentários
require(janitor) #permite usar "adorn_totals"

munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(n = sum(one)) %>%  # ou n()
  dplyr::mutate(percent = (n/sum(n)*100)) %>%
  as.data.frame() %>%
  adorn_totals("row")

#outra forma, n()
munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(n = n()) %>%  # ou n()
  dplyr::mutate(percent = (n/sum(n)*100)) %>%
  as.data.frame() %>%
  adorn_totals("row")

#Outra forma mais simples, tabyl()   (gera proporções)
janitor::tabyl(munics$UFN) %>% #ótima função para variáveis categóricas
  adorn_totals("row")

# Guardando o resultado e diminuíndo as casas decimais
tab1 <- munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(n = sum(one)) %>%  # ou n()
  dplyr::mutate(percent = round((n/sum(n)*100),2)) %>%
  as.data.frame() %>%
  adorn_totals("row")

tab1

write.csv2(tab1, file = "tabela1.csv", row.names = FALSE)

#####################################################################
# Qual o município que apresenta a menor expectativa de vida do país?

munics %>%
dplyr::filter(ESPVIDA == min(ESPVIDA))

# Organizando melhor o resultado
munics %>%
  dplyr::filter(ESPVIDA == min(ESPVIDA)) %>%
  dplyr::select(Município) #Adicione mais variáveis, como UFN e REGIAO

#######################################################################
# Identifique o município com a maior expectativa de vida (função max)
#...

#######################################################################
# Quais os municípios, apresentam maior expectativa de vida por UF?
munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::filter(ESPVIDA == min(ESPVIDA)) %>%
  dplyr::select(Município, UFN, REGIAO, ESPVIDA) %>%
  as.data.frame()

# Quais os municípios, apresentam maior expectativa de vida por Grande Região?
#...

###############################################################################
# Análises do IDH:                                                            #
###############################################################################
# Separe o IDH em classes                                                     #
# Baixo Desenvolvimento Humano: menor que 0,550                               #
# Médio entre 0,550 e 0,699,                                                  #
# Alto entre 0,700 e 0,799,                                                   #
# Muito Alto Desenvolvimento Humano acima de 0,800.                           #
# Qual a Expetativa de vida média em cada classe de IDH?                      #
# Qual a Expetativa de vida média em cada classe de IDH, por Grande Região?   #
# Faça o esquema de 5 números para o IDH, em cada Grande Região.              #
# Faça um gráfico de barras da média de IDH por Grande Região.                #
###############################################################################

# Separe o IDH em classes:
munics <- munics %>%
  dplyr::mutate(IDH_categ = factor(
  1 + findInterval(IDHM, c(0.55, 0.7, 0.8)) ,
  levels = 1:4 , labels = c("Baixo", "Médio", "Alto", "Muito Alto")))

str(munics$IDH_categ)
levels(munics$IDH_categ)

# Qual a Expetativa de vida média em cada classe de IDH?
resultBR <- munics %>%
  dplyr::group_by(IDH_categ) %>%
  dplyr::summarise(IDH_medio = mean(IDHM)) %>%
  cbind(REGIAO = "Brasil",.)

# Qual a Expetativa de vida média em cada classe de IDH, por Grande Região?
resultGR <- munics %>%
  dplyr::group_by(REGIAO,IDH_categ) %>%
  dplyr::summarise(IDH_medio = mean(IDHM))

# Unindo os resultados de Brasil com Grandes Regiões
result <- rbind(resultBR, as.data.frame(resultGR))

#organizando
require(data.table)
result <- result %>%
  data.table::dcast(REGIAO ~ IDH_categ, value.var = "IDH_medio")

result

write.csv2(result, file = "result.csv", row.names = FALSE)

# Faça o esquema de 5 números para o IDH, em cada Grande Região?
tab_desc <- munics %>%
  dplyr::group_by(REGIAO) %>%
  dplyr::summarise(`Mínimo` = min(IDHM),
                   `1º Quartil`= quantile(IDHM, probs=0.25),
                   `Mediana`= quantile(IDHM, probs=0.5),
                   `3º Quartil`= quantile(IDHM, probs=0.75),
                   `Máximo`= max(IDHM)) %>%
  as.data.frame()

# Faça um gráfico de barras da média de IDH por Grande Região.
library(ggplot2)
?ggplot

graf<-munics %>%
  dplyr::group_by(REGIAO) %>%
  dplyr::summarise(IDH_media = mean(IDHM)) %>%
  ggplot(aes(x = REGIAO, y = IDH_media)) +
  geom_bar(stat="identity", fill = "blue") +
  ylab("IDH") +
  xlab("Grandes Regiões") +
  ggtitle("Média de IDH por Grandes Regiões, 2010") +
  coord_cartesian(ylim = c(0,1))

#Para exportar em jpeg
dev.off()
jpeg(filename = "Gráfico de barras.jpeg",
     units = 'cm', width = 16, height = 8,
     res = 300, bg = 'transparent')
graf  #inserir objeto que representa o gráfico
dev.off()

#vamos para os exercícios

### FIM
#######