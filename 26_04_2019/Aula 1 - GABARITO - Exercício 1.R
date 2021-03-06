
######################################################################################
#                                                                                    #
# Exerc�cio 1: Agora estamos interessados em analisar apenas o Sudeste               #
# e apenas as vari�veis: GEOCODIGO, REGIAO, UF, UFN, ESPVIDA, T_ANALF15M,            #
# RDPC e Munic�pio.                                                                  #
#                                                                                    # 
# a) Crie uma base de dados reduzida, apenas com o Sudeste                           #
# b) Quantos munic�pios pertencem a regi�o Sudeste?                                  #
# c) Quais munic�pios possuem expectativa de vida acima de 78 anos?                  #
# d) Quantos munic�pios est�o abaixo da m�dia da taxa de analf. da regi�o?           #
# e) Crie uma vari�vel indicadora para os munic�pios que est�o abaixo da             #
#    m�dia da renda domiciliar per capita da regi�o Sudeste e calcule a m�dia de     #
#    expectativa  de vida para estes munic�pios (FAREMOS JUNTOS)                     #
#                                                                                    #
######################################################################################

#...
#GABARITO
# 1-a)
munics.sudeste <- munics %>%
  dplyr::select(GEOCODIGO, REGIAO, UF, UFN, Munic�pio, ESPVIDA, T_ANALF15M,RDPC) %>%
  dplyr::filter(REGIAO == "Sudeste")

# 1-b)
glimpse(munics.sudeste) #passamos a ter 1668 munic�pios e 6 vari�veis
nrow(munics.sudeste) #outra forma
n_distinct(munics.sudeste$GEOCODIGO) #especial para casos de dupla contagem

# 1-c)
munics.sudeste %>%
  dplyr::filter(ESPVIDA > 78) %>%
  dplyr::select(Munic�pio, UFN)

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

