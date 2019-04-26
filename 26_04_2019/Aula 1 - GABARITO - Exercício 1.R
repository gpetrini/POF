
######################################################################################
#                                                                                    #
# Exercício 1: Agora estamos interessados em analisar apenas o Sudeste               #
# e apenas as variáveis: GEOCODIGO, REGIAO, UF, UFN, ESPVIDA, T_ANALF15M,            #
# RDPC e Município.                                                                  #
#                                                                                    # 
# a) Crie uma base de dados reduzida, apenas com o Sudeste                           #
# b) Quantos municípios pertencem a região Sudeste?                                  #
# c) Quais municípios possuem expectativa de vida acima de 78 anos?                  #
# d) Quantos municípios estão abaixo da média da taxa de analf. da região?           #
# e) Crie uma variável indicadora para os municípios que estão abaixo da             #
#    média da renda domiciliar per capita da região Sudeste e calcule a média de     #
#    expectativa  de vida para estes municípios (FAREMOS JUNTOS)                     #
#                                                                                    #
######################################################################################

#...
#GABARITO
# 1-a)
munics.sudeste <- munics %>%
  dplyr::select(GEOCODIGO, REGIAO, UF, UFN, Município, ESPVIDA, T_ANALF15M,RDPC) %>%
  dplyr::filter(REGIAO == "Sudeste")

# 1-b)
glimpse(munics.sudeste) #passamos a ter 1668 municípios e 6 variáveis
nrow(munics.sudeste) #outra forma
n_distinct(munics.sudeste$GEOCODIGO) #especial para casos de dupla contagem

# 1-c)
munics.sudeste %>%
  dplyr::filter(ESPVIDA > 78) %>%
  dplyr::select(Município, UFN)

# 1-d)
munics.sudeste %>%
  dplyr::filter(T_ANALF15M < mean(T_ANALF15M)) %>%
  n_distinct()

# 1-e)
munics.sudeste <- munics.sudeste %>%
  dplyr::mutate(indicadora = case_when(RDPC < mean(RDPC) ~ 1, TRUE ~ 0))

table(munics.sudeste$indicadora)

munics.sudeste %>%
  dplyr::group_by(indicadora) %>%
  dplyr::summarise(E0 = mean(ESPVIDA))

