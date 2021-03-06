
##################################################################################
#                                                                                #
# Exerc�cio 2: Vamos retornar a base de dados original, com todas as grandes     #
# regi�es. Selecione alguma vari�vel quantitativa do seu interesse,              #          
# monte uma nova base de dados com esta vari�vel e adicione                      #
# GEOCODIGO, REGIAO, UF, UFN                                                     #
#                                                                                # 
# a) Calcule o esquema de 5 n�meros por UF da vari�vel de interesse              #
#    dica: min, quantile, max                                              #
# b) Exporte os resultados para o excel                                          #
# c) Fa�a o histograma e boxplot (fun��es hist e boxplot)                        #
#                                                                                #
##################################################################################

#...
#GABARITO para expectativa de vida

munics <- read.csv("C:\\Users\\PedroGomes\\Google Drive\\Curso da POF\\AULAS\\ATLAS_2013.csv", header=T, sep=";", dec=".")
require(dplyr)

munics <- munics %>%
  dplyr::mutate(REGIAO = as.factor(REGIAO),
                UF = as.factor(UF),
                UFN = as.factor(UFN),
                Codmun6 = as.factor(Codmun6),
                Munic�pio = as.factor(Munic�pio),
                one = 1)
?

# 2-a)
#acertando casas decimais
tab2a <- munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(`M�nimo` = min(ESPVIDA),
                   `1� Quartil`= quantile(ESPVIDA, probs=0.25),
                   `Mediana`= quantile(ESPVIDA, probs=0.5),
                   `3� Quartil`= quantile(ESPVIDA, probs=0.75),
                   `M�ximo`= max(ESPVIDA)) %>%
  as.data.frame()

# 2-b)
write.csv2(tab2a, file = "tab2a.csv", row.names = FALSE)

# 2-c)
?hist
hist(munics$ESPVIDA,
     xlab = "Expectativa de Vida",
     ylab = "Frequ�ncia",
     main = "Histograma da Expectativa de Vida",
     col = "grey")
#histograma para densidade
hist(munics$ESPVIDA,
     xlab = "Expectativa de Vida",
     ylab = "Densidade",
     main = "Histograma da Expectativa de Vida",
     col = "grey",
     freq = FALSE)
#boxplot
boxplot(munics$ESPVIDA,
     ylab = "Expectativa de Vida",
     main = "Boxplot da Expectativa de Vida",
     col = "grey")

#boxplot por Grande Regi�o
boxplot(ESPVIDA ~ REGIAO,
        data = munics,
        ylab = "Expectativa de Vida",
        main = "Boxplot da Expectativa de Vida",
        col = "grey")

