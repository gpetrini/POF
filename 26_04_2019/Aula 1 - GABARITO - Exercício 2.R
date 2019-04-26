
##################################################################################
#                                                                                #
# Exercício 2: Vamos retornar a base de dados original, com todas as grandes     #
# regiões. Selecione alguma variável quantitativa do seu interesse,              #          
# monte uma nova base de dados com esta variável e adicione                      #
# GEOCODIGO, REGIAO, UF, UFN                                                     #
#                                                                                # 
# a) Calcule o esquema de 5 números por UF da variável de interesse              #
#    dica: min, quantile, max                                              #
# b) Exporte os resultados para o excel                                          #
# c) Faça o histograma e boxplot (funções hist e boxplot)                        #
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
                Município = as.factor(Município),
                one = 1)
?

# 2-a)
#acertando casas decimais
tab2a <- munics %>%
  dplyr::group_by(UFN) %>%
  dplyr::summarise(`Mínimo` = min(ESPVIDA),
                   `1º Quartil`= quantile(ESPVIDA, probs=0.25),
                   `Mediana`= quantile(ESPVIDA, probs=0.5),
                   `3º Quartil`= quantile(ESPVIDA, probs=0.75),
                   `Máximo`= max(ESPVIDA)) %>%
  as.data.frame()

# 2-b)
write.csv2(tab2a, file = "tab2a.csv", row.names = FALSE)

# 2-c)
?hist
hist(munics$ESPVIDA,
     xlab = "Expectativa de Vida",
     ylab = "Frequência",
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

#boxplot por Grande Região
boxplot(ESPVIDA ~ REGIAO,
        data = munics,
        ylab = "Expectativa de Vida",
        main = "Boxplot da Expectativa de Vida",
        col = "grey")

