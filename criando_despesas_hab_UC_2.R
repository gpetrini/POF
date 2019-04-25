
############################################################################################################################
################################ Leitura da base de Despesas da POF 2008-2009 #################################################################
###########################################################################################################################

# Responsáveis: Fernanda De Lena e Pedro Andrade Gomes

# Mudança de diretório
setwd("/Dados20082009")
getwd()

load(file="Leitura_POF_2008_habitacao_subgrupos.rda")
# Pacotes utilizados e instalados
library(dplyr)
library(survey)
library (readr)
library(foreign)
library(data.table)
library(gtools)
library(srvyr)

options("scipen" = 15)
options(survey.lonely.psu="adjust")

###############################################################################################################################
##########################  DESPESAS DE HABITAÇÃO POR UNIDADE DE CONSUMO ################################################################################
##############################################################################################################################
################# TRANSFORMANDO AS LINHAS EM COLUNAS E FAZENDO A SOMA DE CADA GRUPO ###################################
######################################################################################################################

despesas_habitacao_UC <- base_despesas_habitacao %>%
  dplyr::group_by(UniConsumoID,sub_groups) %>%
  dplyr::summarize(valor = sum(valor_defl_anu)) %>%
  data.table::dcast(.,UniConsumoID ~ sub_groups ,fun=sum, value.var = "valor")


## remover base de despesas totais
#rm(base_despesas_habitacao)

#################################################################################################################################
############## BASE DE MORADORES - POF1 - QUADRO 3 E 4 - REGISTRO 2 #################################################################
#################################################################################################################################
#dicionario  
pof1.reg2.pes <- read.csv2("pof1.reg2.pes.csv", sep=",")

#base txt
pof1.reg2.pes$VAR <- as.character(pof1.reg2.pes$VAR)
col.pos1 <- fwf_widths(pof1.reg2.pes$tamanho, col_names = pof1.reg2.pes$VAR)
pof1.reg2.pes <- read_fwf(file = "T_MORADOR_S.txt", col_positions=col.pos1 )

#Mudando as variáveis de carácter para númericas
pof1.reg2.pes <- pof1.reg2.pes %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2))
                

#IdentificaÁ„o da Unidade de Consumo;
pof1.reg2.pes  <- pof1.reg2.pes  %>%
  dplyr::mutate (UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc)) 

# verificar qual a classe da variável de controle 
str(pof1.reg2.pes$UniConsumoID)

dim(pof1.reg2.pes) #número de linhas e colunas da base - ANTES

# Variáveis de interesse:
pof1.reg2.pes <- pof1.reg2.pes %>%
  dplyr::select(UniConsumoID, fator_exp, fator_exp_2,sexo,uf,cond_uc)

dim(pof1.reg2.pes)#número de linhas e colunas da base - DEPOIS

######################################################################################################################
####### criando base de pessoas de referencia da Unidade de consumo####################################################
######################################################################################################################
base_pessoa_ref <- pof1.reg2.pes %>%
  dplyr::filter(cond_uc == "01")

dim(base_pessoa_ref)

rm(pof1.reg2.pes,col.pos1)

######################################################################################################################
########### Juntar base de Despesas com Habitação por UC e PESSOA DE REFERENCIA #################################################################
######################################################################################################################

habitacao_pes_ref <- despesas_habitacao_UC   %>%
  dplyr::right_join(base_pessoa_ref, by=c("UniConsumoID")) 
 

habitacao_pes_ref[is.na(habitacao_pes_ref)] <- 0

                
dim(habitacao_pes_ref)

#rm(base_pessoa_ref,despesas_habitacao_UC)


### CRIACAO DO DESENHO BASICO ####

desenho_basico <- svydesign(id=~UniConsumoID, weights = ~fator_exp_2, data = habitacao_pes_ref)
desenho_basico <- as_survey(desenho_basico)

### criar medias por grandes grupos

medias<-desenho_basico %>%
  dplyr::summarize_at(c(" telefone_ tv_internet",
                        "Agua_esgoto",
                        "Aluguel_Monetario",
                        "Aluguel_N_Monetario",
                        "Artigo_limpeza",
                        "Condominio",
                        "Consertos",
                        "Eletrodomesticos",
                        "Energia_eletrica",
                        "Gas_domestico",
                        "Manutencao_lar",
                        "Mobiliario",
                        "Outros",
                        "Telefone_celular",
                        "Telefone_fixo"
                        ), ~survey_mean(., na.rm=TRUE,vartype=c("se")))




t(medias)

write.csv(t(medias),file="medias_1.csv",row.names = FALSE,dec=".",sep=",")

medias_uf<-desenho_basico %>%
  dplyr::group_by(uf) %>%
  dplyr::summarize_at(c(" telefone_ tv_internet",
                        "Agua_esgoto",
                        "Aluguel_Monetario",
                        "Aluguel_N_Monetario",
                        "Artigo_limpeza",
                        "Condominio",
                        "Consertos",
                        "Eletrodomesticos",
                        "Energia_eletrica",
                        "Gas_domestico",
                        "Manutencao_lar",
                        "Mobiliario",
                        "Outros",
                        "Telefone_celular",
                        "Telefone_fixo"), ~survey_mean(., na.rm=TRUE,vartype=c("se")))

(medias_uf)

write.csv(medias_uf,file="medias_uf.csv",row.names = FALSE,dec=".",sep=",")
