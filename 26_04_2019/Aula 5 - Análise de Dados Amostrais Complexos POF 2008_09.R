###########################################################
#                                                         #
# Análise de Dados Amostrais Complexos com a POF 2008/09: #
# com base em asdfree.com                                 #
#                                                         #
###########################################################

# O pacote lodown permite baixar arquivos de dados e pós-estratificação, disponibilizados pelo IBGE no blog do ASDFREE

#o pacote não está disponível no Cran, apenas na página do seu criador
#devtools::install_github( "ajdamico/lodown" ) #para instalar um pacote fora do Cran
#library(lodown)

# Pacotes que serão utilizados
library(survey) #install.packages("survey")
library(dplyr)  #install.packages("dplyr")
library(srvyr)  #install.packages("srvyr")

#Comando para ser utilizado na criação do objeto de desenho da pesquisa
options(survey.lonely.psu = "adjust")

#mudança de diretório
setwd("C:\\Users\\PedroGomes\\Google Drive\\Curso da POF\\Curso POF - Alunos\\Dados") #atenção para a barras duplas: \\

#Arquivo de dados contendo informações de pós-estratificação da amostra
poststr <- 
  readRDS( 
    file.path("poststr.rds")
  )

names(poststr)
glimpse(poststr)
head(poststr)

#Arquivo de dados com informações dos moradores
#selecionando apenas as variáveis de interesse
t_morador_s <- 
  readRDS( 
    file.path("t_morador_s.rds")
  ) %>%
  dplyr::select(cod_uf, num_seq, num_dv,fator_expansao1,
                idade_anos,altura_imputado,altura_imputado,num_altura, cod_sexo, cod_gravida, peso_imputado)

names(t_morador_s)
glimpse(t_morador_s)
head(t_morador_s)

#Criação da variável de controle, ou seja, o "id" do conjunto de dados
#A função paste0 pertite concatenar informações de UF, número de sequência e dv do sequencial, para gerar o id
t_morador_s <- t_morador_s %>%
  dplyr::mutate(control = paste0(cod_uf, num_seq, num_dv))

glimpse(t_morador_s$control)

#Criação do conjunto de dados a ser análisado, junção de informações dos moradores e as informações de pós estratificação
#A base de dados de pós-estratificação já contém a variável "control"
pof_df <- t_morador_s %>%
  left_join(poststr, by = c("control")) #control pertence a ambas as bases, mas a mesma escala de mensuração é diferente, por isso o erro.

#Note que elas tem escalas de mensuração diferentes e por isso o merge não deu certo...
class(t_morador_s$control)
class(poststr$control)

#Vamos modificar a escala de "control" na base "poststr"
poststr <- poststr %>%
  dplyr::mutate(control = as.character(control))

#Agora estão na mesma escala
glimpse(poststr$control) #forma alternativa a função "class"
glimpse(t_morador_s$control)

#Nova tentativa de criação da base de dados a ser analisada
pof_df <- t_morador_s %>%
  left_join(poststr, by = c("control")) #control pertence a ambas as bases, mas a mesma escala de mensuração é diferente.

#Note que o merge deu certo, pois manteve o número de linhas de "t_morador_s"
 nrow(pof_df) == nrow(t_morador_s) #ok
 nrow(pof_df)
 nrow(t_morador_s)
 
########################################################################################
# Contrução do objeto de desenho amostral, utilizando informações de pós-estratificação
 
# Objeto de desenho sem ainda informações de pós-estratificação 
 pre_stratified_design <- 
  svydesign(
    id = ~control ,               #id que foi construído
    strata = ~estrato_unico ,     #informação de estrato
    weights = ~fator_expansao1 ,  #peso original
    data = pof_df ,               #base de dados
    nest = TRUE                   #amostragem conglomerada
  )

# População em cada pós-estrato 
population_totals <- 
  data.frame(
    pos_estrato = unique( pof_df$pos_estrato ) , 
    Freq = unique( pof_df$tot_pop ) 
  )

# Agora objeto de desenho com informações de pós-estratificação 
pof_design <-
  postStratify(
    pre_stratified_design , #objeto anterior, para ser atualizado
    ~ pos_estrato , 
    population_totals
  )

# Fim da construção do objeto de desenho, com pós-estratificação
glimpse(pof_design) #note que o próprio objeto de desenho passa a ser nosso conjunto de dados a ser analisado
################################################################

###############################################################################
# Para não ter que rodar todos estes comandos toda vez que for utilizar a POF:

#?save
#save(list = ls(all = TRUE), file= "POF_0809_BD_morador_desenho.rda")
#rm(list = ls()) #removendo todos os objetos da memória, para testar o "load"
#load(file= "POF_0809_BD_morador_desenho.rda") #resgatando tudo que foi feito

###############################################################################



################################
# Manipulação e análise de dados

#Agora nossa base de dados é o objeto de desenho do plano amostral!
#Devemos atualizar o objeto de desenho para criar novas variáveis e filtros.
#Novas variáveis também poderiam ser construídas, antes da criação do objeto de desenho.

#Lembre-se que estamos apenas com informações da pós-estratificação e da base de dados de moradores.
#Caso estivéssemos interessados em outras informações, o objeto de desenho deveria ser reconstruído,
#utilizando as informações de interesse.

########################################################################################################################
# Agora estamos interessados em realizar um estudo sobre medidas antropométricas da população brasileira de 20 anos    #
# e mais, excluíndo-se as mulheres grávidas.                                                                           #
########################################################################################################################

#O pacote "srvyr" permite utilizar funções do "dplyr" e do "survey", facilitando a tabulação
#require(srvyr)

pof_srvyr_design <- as_survey(pof_design) #transformando o objeto de desenho para um objeto de desenho do pacote srvyr

sum(is.na(pof_srvyr_design$variables$idade_anos)) #contagem de missings
sum(is.na(pof_srvyr_design$variables$altura_imputado)) #contagem de missings
summary(pof_srvyr_design$variables$altura_imputado) #pq o valor mínimo é zero?
summary(pof_srvyr_design$variables$num_altura) #note que a variável não imputada é diferente

# Olhar a documentação: "Descrição dos Registros da POF" ...
#"Para moradores ausentes ou moradores com menos de 24 meses, este campo
#está preenchido com ZEROS. Quando não foi possível obter a altura da pessoa
#por algum motivo (doença, recusa, etc.) este campo está preenchido com
#999.9."

# Para maiores informações ver no mesmo documento, na parte:
#Tratamentos das informações antropométricas
#Crítica e imputação das variáveis peso e altura

##################
# Vamos começar? #
##################

# Vamos agora atualizar a base de dados, criando novas variáveis:
#     Altura em metros
#     Grupos etários quinquenais, iniciando aos 20 anos e fechando em 75 anos e mais
#     IMC
#     Indicadora para: déficit de peso, excesso de peso e pessoas obesas

# Vamos também:
#     Adicionar rótulos a variável sexo
#     Restringir o domínio de análise as pessoas com 20 anos e mais
#     Retirar da análise antropométrica as grávidas (por conta do IMC)

# Vamos criar o novo objeto de desenho
pof_srvyr_design_adultos_ngrav <- pof_srvyr_design %>%
  dplyr::mutate(one = 1, #variável útil para fazer contagens
                altura_imputado = altura_imputado/100,                           
                grupo_etario = factor(
                  1 + findInterval(idade_anos, c(20, 25, 30, 35, 45, 55, 65, 75)),
                  levels = 1:9 , labels = c("< 20", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65-74", "75+")),
body_mass_index = ifelse(altura_imputado == 0, 0, peso_imputado/(altura_imputado^2)), # o "0" não tem problema, pois iremos usar apenas p maiores de 20 anos
sexo = ifelse( cod_sexo == '01' , "masculino" , ifelse( cod_sexo == '02' , "feminino" , NA ) ),
underweight = ifelse( body_mass_index < 18.5 , 1 , 0 ) , 
overweight = ifelse( body_mass_index >= 25 , 1 , 0 ) ,  
obese = ifelse( body_mass_index >= 30 , 1 , 0 )) %>%
  dplyr::filter(grupo_etario != "< 20", cod_gravida != "01" )

# Cáculo do IMC, com intervalo de confiança vartype = c("ci")
?survey_mean
pof_srvyr_design_adultos_ngrav %>%
  dplyr::summarize(mean = survey_mean(body_mass_index , na.rm = TRUE, vartype = c("ci"))) %>%
  as.data.frame()

# Cálculo da idade mediana, com erro padrão (lembrando que estamos só com adultos e sem as grávidas)
pof_srvyr_design_adultos_ngrav %>%
  dplyr::summarize(mean = survey_median(idade_anos , na.rm = TRUE, vartype = c("se"))) %>%
  as.data.frame()

# Cálculo da prevalência de obesidade
# Duas formas de fazer, vamos ver qual tem menor tempo de processamento
library(tictoc) #mensuração de tempo de processamento

tic()
pof_srvyr_design_adultos_ngrav %>%
  dplyr::summarize(prev = survey_ratio(numerator = obese,
                                       denominator = one,
                                       na.rm = TRUE, vartype = c("se"))) %>%
  as.data.frame()
toc()

tic()
pof_srvyr_design_adultos_ngrav %>%
  dplyr::summarize(prev = survey_mean(obese,
                                      na.rm = TRUE, vartype = c("se"))) %>%
  as.data.frame()
toc()

# Por uf o tempo gasto já passa a fazer diferença
   # survey_ratio 66.34 seg
   # survey_mean 135 seg

tic()
pof_srvyr_design_adultos_ngrav %>%
  dplyr::group_by(cod_uf) %>%
  dplyr::summarize(prev = survey_ratio(numerator = obese,
                                       denominator = one,
                                       na.rm = TRUE, vartype = c("se"))) %>%
  as.data.frame()
toc()

tic()
pof_srvyr_design_adultos_ngrav %>%
  dplyr::group_by(cod_uf) %>%
  dplyr::summarize(prev = survey_mean(obese,
                                      na.rm = TRUE, vartype = c("se"))) %>%
  as.data.frame()
toc()


# Cálculo das prevalência de déficit de peso, de excesso de peso e de obesidade
# na população de 20 anos e mais
# por sexo e para ambos os sexos

tab_prevalencias_sexo <- pof_srvyr_design_adultos_ngrav %>%
  dplyr::group_by(sexo, grupo_etario) %>%
  dplyr::summarize_at(c("underweight","overweight","obese"),  ~survey_mean(., na.rm = TRUE,vartype = c("ci"))) %>%
  as.data.frame()
  
tab_prevalencias_sexo

tab_prevalencias_ambos_os_sexos <- pof_srvyr_design_adultos_ngrav %>%
  dplyr::group_by(grupo_etario) %>%
  dplyr::summarize_at(c("underweight","overweight","obese"),  ~survey_mean(., na.rm = TRUE,vartype = c("ci"))) %>%
  as.data.frame() %>%
  cbind(sexo = "Ambos os Sexos", .)

tab_prevalencias_ambos_os_sexos

tab_prevalencias <- rbind(tab_prevalencias_sexo, tab_prevalencias_ambos_os_sexos)

#Exportando os resultados
write.csv2(tab_prevalencias, file="tab_prevalencias.csv", row.names = FALSE)

#Fazendo um Gráfico de barras
library(ggplot2)
?ggplot

ggplot(data = tab_prevalencias, aes(x = grupo_etario, y = obese, fill = sexo)) +
  geom_bar(stat="identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = obese_low, ymax = obese_upp),
                width = 0.2,
                position = position_dodge(0.9)) +
  ylab("Prevalência de Obesos") +
  xlab("Grupos Etários") +
  ggtitle("Prevalência de Obesidade no Brasil, segundo sexo, POF 2008-2009") +
  scale_fill_grey(start = 0.4, end = 0.8, name = "Sexo", labels = c("Ambos", "Feminino", "Masculino")) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(0,0.3))

#############################################################################################################
# Exercício: Calcule a prevalência de sobrepeso e seu IC, por UF, da população adulta                       # 
# (sem considerar as grávidas).                                                                             #
#         a) Qual UF tem a maior prevalência de sobrepeso?                                                  #
#         b) Qual UF tem a menor prevalência de sobrepeso?                                                  #
#         c) Quais UFs estão abaixo da mediana de prevalência de sobrepeso?                                 #
#         d) Exporte os resultados para o Excel.                                                            #
#         e) Faça um gráfico de barras, com os IC da prevalência de sobrepeso, por UF                       #
#############################################################################################################

#...

levels(as.factor(pof_srvyr_design_adultos_ngrav$variables$cod_uf)) # Olhar na documentão o significado dos códigos, note que já foi imputado

tab_uf <- pof_srvyr_design_adultos_ngrav %>%
  dplyr::group_by(cod_uf) %>%
  dplyr::summarize(prev = survey_mean(overweight,
                                      na.rm = TRUE, vartype = c("ci"))) %>%
  as.data.frame() %>%
  dplyr::mutate( UF = case_when(cod_uf == "11" ~ "Rondônia",
                                cod_uf == "12" ~ "Acre",
                                cod_uf == "13" ~ "Amazonas",
                                cod_uf == "14" ~ "Roraima",
                                cod_uf == "15" ~ "Pará",
                                cod_uf == "16" ~ "Amapá",
                                cod_uf == "17" ~ "Tocantins",
                                cod_uf == "21" ~ "Maranhão",
                                cod_uf == "22" ~ "Piauí",
                                cod_uf == "23" ~ "Ceará",
                                cod_uf == "24" ~ "Rio Grande do Norte",
                                cod_uf == "25" ~ "Paraíba",
                                cod_uf == "26" ~ "Pernambuco",
                                cod_uf == "27" ~ "Alagoas",
                                cod_uf == "28" ~ "Sergipe",
                                cod_uf == "29" ~ "Bahia",
                                cod_uf == "31" ~ "Minas Gerais",
                                cod_uf == "32" ~ "Espírito Santo",
                                cod_uf == "33" ~ "Rio de Janeiro",
                                cod_uf == "35" ~ "São Paulo",
                                cod_uf == "41" ~ "Paraná",
                                cod_uf == "42" ~ "Santa Catarina",
                                cod_uf == "43" ~ "Rio Grande do Sul",
                                cod_uf == "50" ~ "Mato Grosso do Sul",
                                cod_uf == "51" ~ "Mato Grosso",
                                cod_uf == "52" ~ "Goiás",
                                cod_uf == "53" ~ "Distrito Federal",
                                cod_uf == "-1" ~ "Não Aplicável",
                                cod_uf == "60" ~ "Fora do País",
                                cod_uf == "99" ~ "Não Sabe",
                                TRUE ~ "VERIFICAR"))
                 


tab_uf %>%
  dplyr::filter(prev == max(prev))

tab_uf %>%
  dplyr::filter(prev == min(prev))

tab_uf %>%
  dplyr::filter(prev < median(prev))


##################################################################################

