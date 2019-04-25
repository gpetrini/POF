
############################################################################################################################
################################ Leitura da POF 2008-2009 #################################################################
###########################################################################################################################
# CURSO POF - IE - UNICAMP - 25/04/2019
# Responsáveis: Fernanda De Lena e Pedro Gomes Andrade

# Mudança de diretório
setwd("/Users/fortesdelena/Downloads/Dados20082009")
getwd()

# Pacotes utilizados e instalados
library(dplyr)
library(survey)
library (readr)
library(foreign)
library(tidyr)
library(stringr)
library(data.table)
library(gtools)
library(srvyr)

#################################################################################################################################
############## BASE DESPESAS DE 90 DIAS - POF2 - QUADRO 6 a 9 - REGISTRO 6 #################################################################
#################################################################################################################################

#################### 1. LEITURA DO DICIONARIO EM CSV ###################################################
pof2.reg6.desp.90.dias <- read.csv2("pof2.reg6.desp.90.dias.csv", sep=",")

################### 2. LEITURA DA BASE EM TXT USANDO O DICIONARIO ######################################
pof2.reg6.desp.90.dias$VAR <- as.character(pof2.reg6.desp.90.dias$VAR)
col.pos <- fwf_widths(pof2.reg6.desp.90.dias$tamanho, col_names = pof2.reg6.desp.90.dias$VAR)
pof2.reg6.desp.90.dias<- read_fwf(file = "T_DESPESA_90DIAS_S.txt", col_positions=col.pos )

################# 3. MODIFICANDO ALGUMAS VARIAVEIS DE CARACTER PARA NUMERICA ###########################
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp),
                valor_despe = as.numeric(valor_despe))

################# 4. RETIRANDO A EXPANSAO DA VARIAVEL DE VALOR DEFLACIONADO ANULA ######################

# Criação da variável de valor deflacionado anualizado Sem fator de expansão 
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
dplyr::mutate (valor_defl_anu= (desp_anu_exp/fator_exp_2))

################ 5. CRIANDO IDENTIFICADOR A UNIDADE DE CONSUMO ##########################################
#IdentificaÁ„o da Unidade de Consumo;
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

############### 6. CRIANDO O CODIGO IBGE DE IDENTIFICACAO NA TABULACAO DO IBGE ########################
#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
  dplyr::mutate(code=substr(cod_item,1,3))
# Juntar o número do quadro com o codigo do item
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))
    
############### 6. 1 Verificar qual a classe da variável de controle  ##################################
str(pof2.reg6.desp.90.dias$UniConsumoID)
str(pof2.reg6.desp.90.dias$codeIBGE)
dim(pof2.reg6.desp.90.dias) #número de linhas e colunas da base - ANTES

#De acordo com as varáveis de interesse é preciso verificar se necessário transformar alguma em variável numérica. 

############## 7. SELECIONADO AS VARIAVEIS PARA DEPOIS JUNTAR AS BASES DE DESPESAS #####################
# Variáveis de interesse:
pof2.reg6.desp.90.dias <- pof2.reg6.desp.90.dias %>%
  dplyr::select(UniConsumoID,codeIBGE, fator_exp, fator_exp_2,valor_defl_anu)

dim(pof2.reg6.desp.90.dias)#número de linhas e colunas da base - DEPOIS
  
###################################################################################################################################
############################## BASE DESPESAS DE 12 MESES - POF2 - QUADRO 10 E 13 - REGISTRO 7 #################################################
##################################################################################################################################
#dicionario  
pof2.reg7.desp.12.meses <- read.csv2("pof2.reg7.desp.12.meses.csv", sep=",")

#base txt
pof2.reg7.desp.12.meses$VAR <- as.character(pof2.reg7.desp.12.meses$VAR)
col.pos1 <- fwf_widths(pof2.reg7.desp.12.meses$tamanho, col_names = pof2.reg7.desp.12.meses$VAR)
pof2.reg7.desp.12.meses<- read_fwf(file = "T_DESPESA_12MESES_S.txt", col_positions=col.pos1 )

#variáveis numericas
pof2.reg7.desp.12.meses <- pof2.reg7.desp.12.meses %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp))

# Criação da variável de valor deflacionado anualizado em fator de expansão #comentário o valor deflacionado já expandido
pof2.reg7.desp.12.meses <- pof2.reg7.desp.12.meses %>%
  dplyr::mutate(valor_defl_anu= (desp_anu_exp/fator_exp_2))

#IdentificaÁ„o da Unidade de Consumo;
pof2.reg7.desp.12.meses <- pof2.reg7.desp.12.meses %>%
  dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))  

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof2.reg7.desp.12.meses <- pof2.reg7.desp.12.meses %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof2.reg7.desp.12.meses <- pof2.reg7.desp.12.meses %>%
  dplyr::select(UniConsumoID,codeIBGE,fator_exp,fator_exp_2,valor_defl_anu)


dim(pof2.reg7.desp.12.meses)#número de linhas e colunas da base

################################################################################################################################################
###################### OUTRAS DESPESAS - POF2 - QUADRO 15 E 18 - REGISTRO 8 ######################################################################
#######################################################################################################################################
#dicionario  
pof2.reg8.outras.desp <- read.csv2("pof2.reg8.outras.desp.csv", sep=",")
#base txt
pof2.reg8.outras.desp$VAR <- as.character(pof2.reg8.outras.desp$VAR)
col.pos2 <- fwf_widths(pof2.reg8.outras.desp$tamanho, col_names = pof2.reg8.outras.desp$VAR)
pof2.reg8.outras.desp<- read_fwf(file = "T_OUTRAS_DESPESAS_S.txt", col_positions=col.pos2 )

#variáveis numericas
pof2.reg8.outras.desp <- pof2.reg8.outras.desp %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp))  

# Criação da variável de valor deflacionado anualizado em fator de expansão 
pof2.reg8.outras.desp <- pof2.reg8.outras.desp %>%
  dplyr::mutate (valor_defl_anu= (desp_anu_exp/fator_exp_2))

#IdentificaÁ„o da Unidade de Consumo;
pof2.reg8.outras.desp <- pof2.reg8.outras.desp %>%
  dplyr::mutate (UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof2.reg8.outras.desp <- pof2.reg8.outras.desp %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof2.reg8.outras.desp <- pof2.reg8.outras.desp %>%
  dplyr::select(UniConsumoID,codeIBGE,fator_exp,fator_exp_2,valor_defl_anu)

dim(pof2.reg8.outras.desp)

##################################################################################################################################
####################### DESPESA SERVIÇOS DOMESTICOS - POF2 - QUADRO 19 - REGISTRO 9 ##############################################
##################################################################################################################################
#dicionario  
pof2.reg9.serv.dom<- read.csv2("pof2.reg9.serv.dom.csv", sep=",")
#base txt
pof2.reg9.serv.dom$VAR <- as.character(pof2.reg9.serv.dom$VAR)
col.pos3 <- fwf_widths(pof2.reg9.serv.dom$tamanho, col_names = pof2.reg9.serv.dom$VAR)
pof2.reg9.serv.dom <- read_fwf(file = "T_SERVICO_DOMS_S.txt", col_positions=col.pos3 )

#variáveis numericas
pof2.reg9.serv.dom<- pof2.reg9.serv.dom %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp))
                
# Criação da variável de valor deflacionado anualizado em fator de expansão

pof2.reg9.serv.dom <-pof2.reg9.serv.dom %>%
  dplyr::mutate (valor_defl_anu= (desp_anu_exp/fator_exp_2))

#IdentificaÁ„o da Unidade de Consumo;
pof2.reg9.serv.dom <-pof2.reg9.serv.dom %>%
  dplyr::mutate (UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof2.reg9.serv.dom <- pof2.reg9.serv.dom %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate (codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof2.reg9.serv.dom <- pof2.reg9.serv.dom %>%
  dplyr::select(UniConsumoID,codeIBGE,fator_exp,fator_exp_2,valor_defl_anu)

dim(pof2.reg9.serv.dom)#número de linhas e colunas da base 

##################################################################################################################################
####################### DESPESA SERVIÇOS DOMESTICOS - INSS - POF2 - QUADRO 19 - REGISTRO 9 ##############################################
##################################################################################################################################
#dicionario  
pof2.reg9.serv.dom.inss<- read.csv2("pof2.reg9.serv.dom.csv", sep=",")
#base txt
pof2.reg9.serv.dom.inss$VAR <- as.character(pof2.reg9.serv.dom.inss$VAR)
col.pos3a <- fwf_widths(pof2.reg9.serv.dom.inss$tamanho, col_names = pof2.reg9.serv.dom.inss$VAR)
pof2.reg9.serv.dom.inss <- read_fwf(file = "T_SERVICO_DOMS_S.txt", col_positions=col.pos3a )

#variáveis numericas
pof2.reg9.serv.dom.inss <- pof2.reg9.serv.dom.inss %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                val_inss_anu_exp = as.numeric (val_inss_anu_exp)) # valor gasto com INSS

pof2.reg9.serv.dom.inss <-pof2.reg9.serv.dom.inss %>%
  dplyr::mutate( valor_defl_anu= (val_inss_anu_exp/fator_exp_2))

#IdentificaÁ„o da Unidade de Consumo;
pof2.reg9.serv.dom.inss <-pof2.reg9.serv.dom.inss %>%
  dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do inss  
pof2.reg9.serv.dom.inss <- pof2.reg9.serv.dom.inss %>%
  dplyr::mutate(code=substr(cod_inss,1,3)) %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof2.reg9.serv.dom.inss <- pof2.reg9.serv.dom.inss %>%
  dplyr::select(UniConsumoID,codeIBGE,fator_exp,fator_exp_2,valor_defl_anu)

dim(pof2.reg9.serv.dom.inss)#número de linhas e colunas da base 

##################################################################################################################################
###################### ALUGUEL ESTIMADO - POF1 - QUADRO 2 - REGISTRO 10 ########################################################
##################################################################################################################################
#dicionario  
pof1.reg10.aluguel<- read.csv2("pof1.reg10.aluguel.csv", sep=",")
#base txt
pof1.reg10.aluguel$VAR <- as.character(pof1.reg10.aluguel$VAR)
col.pos4<- fwf_widths(pof1.reg10.aluguel$tamanho, col_names = pof1.reg10.aluguel$VAR)
pof1.reg10.aluguel<- read_fwf(file = "T_ALUGUEL_ESTIMADO_S.txt", col_positions=col.pos4 )

#variáveis numericas
pof1.reg10.aluguel <- pof1.reg10.aluguel %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp),
                num_meses = as.numeric(num_meses))

# Criação da variável de valor deflacionado anualizado em fator de expansão 
pof1.reg10.aluguel<-pof1.reg10.aluguel %>%
  dplyr::mutate(valor_defl_anu= (desp_anu_exp/fator_exp_2))


#IdentificaÁ„o da Unidade de Consumo;
pof1.reg10.aluguel<-pof1.reg10.aluguel %>%
  dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof1.reg10.aluguel <- pof1.reg10.aluguel %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof1.reg10.aluguel <- pof1.reg10.aluguel %>%
  dplyr::select(UniConsumoID,codeIBGE, fator_exp, fator_exp_2,valor_defl_anu)

dim(pof1.reg10.aluguel)#número de linhas e colunas da base - DEPOIS

##################################################################################################################################
################ CADERNETA DE DESPESAS - POF3 - QUADRO 63 A 69 - REGISTRO 11 #####################################################
##################################################################################################################################
#dicionario  
pof3.reg11.caderneta.desp<- read.csv2("pof3.reg11.caderneta.desp.csv", sep=",")
#base txt
pof3.reg11.caderneta.desp$VAR <- as.character(pof3.reg11.caderneta.desp$VAR)
col.pos5 <- fwf_widths(pof3.reg11.caderneta.desp$tamanho, col_names = pof3.reg11.caderneta.desp$VAR)
pof3.reg11.caderneta.desp<- read_fwf(file = "T_CADERNETA_DESPESA_S.txt", col_positions=col.pos5 )

#variáveis numericas
pof3.reg11.caderneta.desp<- pof3.reg11.caderneta.desp %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp)) 

# Criação da variável de valor deflacionado anualizado em fator de expansão 
pof3.reg11.caderneta.desp<-pof3.reg11.caderneta.desp %>%
  dplyr::mutate(valor_defl_anu= (desp_anu_exp/fator_exp_2))

#IdentificaÁ„o da Unidade de Consumo;
pof3.reg11.caderneta.desp<-pof3.reg11.caderneta.desp %>%
  dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof3.reg11.caderneta.desp <- pof3.reg11.caderneta.desp %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate (codeIBGE = paste0(n_gru_despe,code))

# Variáveis de interesse:
pof3.reg11.caderneta.desp <- pof3.reg11.caderneta.desp %>%
  dplyr::select(UniConsumoID,codeIBGE, fator_exp, fator_exp_2,valor_defl_anu)

dim(pof3.reg11.caderneta.desp)#número de linhas e colunas da base 

##################################################################################################################################
##################### CADERNETA DE DESPESAS INDIVIDUAIS - POF4 - QUADRO 22 A 50 - REGISTRO 12 ####################################
#################################################################################################################################

#dicionario  
pof4.reg12.desp.indiv<- read.csv2("pof4.reg12.desp.indiv.csv", sep=",")
#base txt
pof4.reg12.desp.indiv$VAR <- as.character(pof4.reg12.desp.indiv$VAR)
col.pos6 <- fwf_widths(pof4.reg12.desp.indiv$tamanho, col_names = pof4.reg12.desp.indiv$VAR)
pof4.reg12.desp.indiv<- read_fwf(file = "T_DESPESA_INDIVIDUAL_S.txt", col_positions=col.pos6 )

#variáveis numericas
pof4.reg12.desp.indiv<- pof4.reg12.desp.indiv %>%
  dplyr::mutate(fator_exp = as.numeric(fator_exp),fator_exp_2 = as.numeric(fator_exp_2),
                desp_anu_exp= as.numeric(desp_anu_exp))  

# Criação da variável de valor deflacionado anualizado em fator de expansão 
pof4.reg12.desp.indiv<- pof4.reg12.desp.indiv %>%
  dplyr::mutate(valor_defl_anu= (desp_anu_exp/fator_exp_2)) 
  
#IdentificaÁ„o da Unidade de Consumo;
pof4.reg12.desp.indiv<-pof4.reg12.desp.indiv%>%
  dplyr::mutate(UniConsumoID = paste0(uf, n_seq, dv_seq, n_dom,n_uc))

#Identificador dos itens de despesas (codeIBGE)
#Pegando os 3 primeiros números do código do item 
pof4.reg12.desp.indiv <- pof4.reg12.desp.indiv %>%
  dplyr::mutate(code=substr(cod_item,1,3)) %>%
  dplyr::mutate(codeIBGE = paste0(n_quadro,code))

# Variáveis de interesse:
pof4.reg12.desp.indiv <- pof4.reg12.desp.indiv %>%
  dplyr::select(UniConsumoID,codeIBGE,fator_exp,fator_exp_2,valor_defl_anu)

dim(pof4.reg12.desp.indiv)#número de linhas e colunas da base - DEPOIS


########################################################################################################################
########## UNINDO AS BASES DE DESPESAS #################################################################################
########################################################################################################################

base_despesas_habitacao <- rbind(pof2.reg6.desp.90.dias,
                              pof2.reg7.desp.12.meses,
                              pof2.reg8.outras.desp,
                              pof2.reg9.serv.dom,
                              pof2.reg9.serv.dom.inss,
                              pof1.reg10.aluguel,
                              pof3.reg11.caderneta.desp,
                              pof4.reg12.desp.indiv)
                            
dim(base_despesas_habitacao)

rm(col.pos,col.pos1,col.pos2,col.pos3, col.pos3a,col.pos4,col.pos5,col.pos6,
   pof2.reg6.desp.90.dias,pof2.reg7.desp.12.meses,pof2.reg8.outras.desp,
   pof2.reg9.serv.dom,pof2.reg9.serv.dom.inss,
   pof3.reg11.caderneta.desp,pof1.reg10.aluguel,pof4.reg12.desp.indiv)



###############################################################################################################################
################ GRANDES GRUPOS DE DESPESAS TOTAIS ################################################################################
##############################################################################################################################
codigosIBGE <- read.csv2("/Users/fortesdelena/Downloads/pofcvs/codes_IBGE.csv", sep=",")

str(codigosIBGE$codeIBGE)
levels (codigosIBGE$groups)

#transformar em caracter
codigosIBGE <- codigosIBGE %>%
  dplyr::mutate(codeIBGE = str_pad(codeIBGE, width = 5, side = "left", pad = "0")) %>%
  dplyr::mutate(codeIBGE = as.character(codeIBGE), groups = as.character(groups))

# verificar que sao caracters
str(codigosIBGE$codeIBGE)
str(codigosIBGE$groups)

# unindo a base de despesas com a base de codigos do IBGE
base_despesas_habitacao <- base_despesas_habitacao %>%
  dplyr::left_join(codigosIBGE, by=c("codeIBGE")) %>%
  dplyr::filter(groups=="Habitacao")



##############################################################################################################################
subgroupsIBGE <- read.csv2("/Users/fortesdelena/Downloads/subgroupsIBGE.csv", sep=",")

#transformar em caracter
subgroupsIBGE <- subgroupsIBGE %>%
  dplyr::mutate(codeIBGE = str_pad(codeIBGE, width = 5, side = "left", pad = "0")) %>%
  dplyr::mutate(codeIBGE = as.character(codeIBGE), sub_groups = as.character(sub_groups))

# unindo a base de habitação com subgrupos de codigos do IBGE
base_despesas_habitacao <- base_despesas_habitacao %>%
  dplyr::left_join(subgroupsIBGE, by=c("codeIBGE")) 
 
dim(base_despesas_habitacao)


?save
save(list = ls(all = TRUE), file= "Leitura_POF_2008_habitacao_subgrupos.rda")
rm(list = ls()) #removendo todos os objetos da memória, para testar o "load"

#######################################################################################################
########################################### FIM  #####################################################
#######################################################################################################