
####################################################
#   Manipula��o e An�lise Explorat�ria de dados    #
####################################################

# Pacotes
library(dplyr)
#install.packages("dplyr") #Caso n�o tenha o Dplyr instalado, retirar o primeiro "#" e rodar

# Dados do Atlas de Desenvolvimento Humano de 2013, com munic�pios do ano 2010
# Veja o dicion�rio de dados "ATLAS_Layout.xls"

### 1� passo: modificar o diret�rio
### 2� passo: leitura de dados
### 3� passo: manipula��o da base de dados
### 4� passo: an�lise descritiva

####################################
### 1� passo: modificar o diret�rio

getwd() #verificar o diret�rio atual

# Escolher diret�rio de trabalho
# Selecione o diret�rio que contenha o conjunto de dados a ser analisado
setwd("C:\\Users\\PedroGomes\\Google Drive\\Curso da POF\\Curso POF - Alunos\\Dados") #aten��o para as barras duplas na hora de colocar o diret�rio

getwd() #Ok

# O diret�rio cont�m os arquivos desejados?
list.files()

##############################
### 2� passo: leitura de dados

# Define n�mero de d�gitos usados para impress�o de valores na tela console
options(digits = 4) #comando opcional

# Importar dados
# A primeira linha apresenta o nome das vari�vies, separador ";" e decimal "."
munics <- read.csv("ATLAS_2013.csv", header=T, sep=";", dec=".")

# Estudando a base lida
View(munics)      #visualizar a base de dados, cuidado em bases muito grandes, usar head(). LEMBRAR DE FECHAR A BASE
fix(munics)       #outra forma. LEMBRAR DE FECHAR A BASE

head(munics,5)    #visualizar as 5 primeiras linhas
tail(munics,5)    #visualizar as 5 �ltimas linhas

str(munics)       #verificar a escala de mensura��o de cada vari�vel
glimpse(munics)   #outra forma
dim(munics)       #consultar a dimes�o do conjunto de dados

names(munics)

###########################################
### 3� passo: manipula��o da base de dados

# Olhe o dicion�rio de dados "ATLAS_Layout.xls"
# As vari�veis est�o com escala de mensura��o coerente?

# Recodificar vari�veis, de n�meros para caracteres
# A fun��o mutate() do pacote "dplyr" permite cria��o/atualiza��o de vari�veis

str(munics$REGIAO)

munics <- munics %>%
  dplyr::mutate(REGIAO = as.factor(REGIAO),
                UF = as.factor(UF),
                UFN = as.factor(UFN),
                Codmun6 = as.factor(Codmun6),
                Munic�pio = as.factor(Munic�pio),
                one = 1)
  
str(munics) #ok, tudo bem agora
glimpse(munics) #outra forma

dim(munics) #5565 munic�pios e 239 vari�veis (vari�veis modificadas foram sobrescritas)

colSums(is.na(munics)) #contagem de missings por vari�vel 

# Codifica��o das Grandes Regi�es, conforme dicion�rio de dados
str(munics$REGIAO)
levels(munics$REGIAO)

munics <- munics %>%
  dplyr::mutate(REGIAO = factor(munics$REGIAO,
                        label = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro Oeste"), levels = 1:5))

levels(munics$REGIAO) #ok, agora aparece o nome das Grandes Regi�es

#################################
### 4� passo: an�lise descritiva

# Esquema de 5 n�meros + a m�dia
summary(munics) #n�o faz sentido para todas as vari�veis

summary(munics$ESPVIDA) #fazendo para apenas uma


#################################################################################
# An�lises de Expectativa de Vida:                                              #
#################################################################################
# Quantos munic�pios cada UF possui?                                            #
# Qual o munic�pio que apresenta a menor expectativa de vida do pa�s?           #
# Identifique o munic�pio com a maior expectativa de vida (fun��o max)          #
# Quais os munic�pios, apresentam maior expectativa de vida por UF?             #
# Quais os munic�pios, apresentam maior expectativa de vida por Grande Regi�o?  #
#################################################################################

#####################################
# Quantos munic�pios cada UF possui?
#install.packages("janitor") #caso n�o tenha instalado, rodar sem os coment�rios
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

#Outra forma mais simples, tabyl()   (gera propor��es)
janitor::tabyl(munics$UFN) %>% #�tima fun��o para vari�veis categ�ricas
  adorn_totals("row")

# Guardando o resultado e diminu�ndo as casas decimais
tab1 <- munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(n = sum(one)) %>%  # ou n()
  dplyr::mutate(percent = round((n/sum(n)*100),2)) %>%
  as.data.frame() %>%
  adorn_totals("row")

tab1

write.csv2(tab1, file = "tabela1.csv", row.names = FALSE)

#####################################################################
# Qual o munic�pio que apresenta a menor expectativa de vida do pa�s?

munics %>%
dplyr::filter(ESPVIDA == min(ESPVIDA))

# Organizando melhor o resultado
munics %>%
  dplyr::filter(ESPVIDA == min(ESPVIDA)) %>%
  dplyr::select(Munic�pio) #Adicione mais vari�veis, como UFN e REGIAO

#######################################################################
# Identifique o munic�pio com a maior expectativa de vida (fun��o max)
#...

#######################################################################
# Quais os munic�pios, apresentam maior expectativa de vida por UF?
munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::filter(ESPVIDA == min(ESPVIDA)) %>%
  dplyr::select(Munic�pio, UFN, REGIAO, ESPVIDA) %>%
  as.data.frame()

# Quais os munic�pios, apresentam maior expectativa de vida por Grande Regi�o?
#...

###############################################################################
# An�lises do IDH:                                                            #
###############################################################################
# Separe o IDH em classes                                                     #
# Baixo Desenvolvimento Humano: menor que 0,550                               #
# M�dio entre 0,550 e 0,699,                                                  #
# Alto entre 0,700 e 0,799,                                                   #
# Muito Alto Desenvolvimento Humano acima de 0,800.                           #
# Qual a Expetativa de vida m�dia em cada classe de IDH?                      #
# Qual a Expetativa de vida m�dia em cada classe de IDH, por Grande Regi�o?   #
# Fa�a o esquema de 5 n�meros para o IDH, em cada Grande Regi�o.              #
# Fa�a um gr�fico de barras da m�dia de IDH por Grande Regi�o.                #
###############################################################################

# Separe o IDH em classes:
munics <- munics %>%
  dplyr::mutate(IDH_categ = factor(
  1 + findInterval(IDHM, c(0.55, 0.7, 0.8)) ,
  levels = 1:4 , labels = c("Baixo", "M�dio", "Alto", "Muito Alto")))

str(munics$IDH_categ)
levels(munics$IDH_categ)

# Qual a Expetativa de vida m�dia em cada classe de IDH?
resultBR <- munics %>%
  dplyr::group_by(IDH_categ) %>%
  dplyr::summarise(IDH_medio = mean(IDHM)) %>%
  cbind(REGIAO = "Brasil",.)

# Qual a Expetativa de vida m�dia em cada classe de IDH, por Grande Regi�o?
resultGR <- munics %>%
  dplyr::group_by(REGIAO,IDH_categ) %>%
  dplyr::summarise(IDH_medio = mean(IDHM))

# Unindo os resultados de Brasil com Grandes Regi�es
result <- rbind(resultBR, as.data.frame(resultGR))

#organizando
require(data.table)
result <- result %>%
  data.table::dcast(REGIAO ~ IDH_categ, value.var = "IDH_medio")

result

write.csv2(result, file = "result.csv", row.names = FALSE)

# Fa�a o esquema de 5 n�meros para o IDH, em cada Grande Regi�o?
tab_desc <- munics %>%
  dplyr::group_by(REGIAO) %>%
  dplyr::summarise(`M�nimo` = min(IDHM),
                   `1� Quartil`= quantile(IDHM, probs=0.25),
                   `Mediana`= quantile(IDHM, probs=0.5),
                   `3� Quartil`= quantile(IDHM, probs=0.75),
                   `M�ximo`= max(IDHM)) %>%
  as.data.frame()

# Fa�a um gr�fico de barras da m�dia de IDH por Grande Regi�o.
library(ggplot2)
?ggplot

graf<-munics %>%
  dplyr::group_by(REGIAO) %>%
  dplyr::summarise(IDH_media = mean(IDHM)) %>%
  ggplot(aes(x = REGIAO, y = IDH_media)) +
  geom_bar(stat="identity", fill = "blue") +
  ylab("IDH") +
  xlab("Grandes Regi�es") +
  ggtitle("M�dia de IDH por Grandes Regi�es, 2010") +
  coord_cartesian(ylim = c(0,1))

#Para exportar em jpeg
dev.off()
jpeg(filename = "Gr�fico de barras.jpeg",
     units = 'cm', width = 16, height = 8,
     res = 300, bg = 'transparent')
graf  #inserir objeto que representa o gr�fico
dev.off()

#vamos para os exerc�cios

### FIM
#######