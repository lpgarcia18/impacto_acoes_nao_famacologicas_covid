# Trabalhar com áreas de saúde
# Dividir as ações em 2 tipos: Fechadas e Abertas com restrição
# Calcular o contrafactual para cada área de saúde como não tratado
# Ver a questão do tempo de tratamento (dose por dia) (número de infectados por dia)
# Problema pode ser com a adesão ao tratamento
# A simples prescrição de um remédio não vai produzir efeito. Ele só poderá fazer efeito se o paciente tomar o remédio
# Não avaliaremos no sentido da abertura. Apenas dos fechamentos
# guido imbens the book of why. https://arxiv.org/pdf/1907.07271.pdf


# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx16g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(mlr)
library(forecast)
library(doParallel)
library(parallelMap)
library(tigerstats)
library(matrixStats)
library(httr)
library(RCurl)

# Importanto bases ---------------------------------------------------------------
## Dados de suspeitos 

#URL do github
git_url <- getURL("https://raw.githubusercontent.com/geinfosms/cenarios_covid_florianopolis/master/nowcasting/dados/covid_ajustado.csv")
#Baisando do github
covid_floripa <-read.csv(text=git_url, skip=0, header=T,encoding = "UTF-8")
# Transformando base ------------------------------------------------------
covid_floripa$ID <- as.factor(covid_floripa$ID)
covid_floripa$RESULTADO <- as.factor(covid_floripa$RESULTADO)
covid_floripa$TERRITORIO <-as.factor(covid_floripa$TERRITORIO)
covid_floripa$SEXO <-as.factor(covid_floripa$SEXO)
covid_floripa$RACA_COR <-as.factor(covid_floripa$RACA_COR)
covid_floripa$FAIXA_ETARIA <-as.factor(covid_floripa$FAIXA_ETARIA)
covid_floripa$INICIO_SINTOMAS <-  as.numeric(covid_floripa$INICIO_SINTOMAS)#Transformando em número, pois o learner do mlr não trabalha com data
covid_floripa$DATA_NOTIFICACAO <- as.numeric(covid_floripa$DATA_NOTIFICACAO)#Transformando em número, pois o learner do mlr não trabalha com data
covid_floripa[,-c(1:7)] <- sapply(covid_floripa[,-c(1:7)], as.numeric) %>% as.data.frame()


# Pré-settings e paralelização --------------------------------------------

#Iniciando paralelização
parallelStartSocket(4)

#Chamando a função de classificação
source("func_classificacao.R")

#Boostrap para calcular IC
n_boot <- 2


# Classificação dos casos faltantes ---------------------------------------
cum_base_ter <- list()

for(i in seq_along(covid$TERRITORIO)) {
	#Realizando subset por área de saúde
	covid <- subset(covid_floripa, covid_floripa$TERRITORIO == levels(covid_floripa$TERRITORIO)[i])
	covid$TERRITORIO <- NULL
	
	# Formação das bases de treino, teste e predição --------------------------
	train_test_base <- subset(covid, covid$RESULTADO == "descartado" |
				  	covid$RESULTADO == "confirmado")
	train_test_base$RESULTADO <- factor(train_test_base$RESULTADO, levels = c("confirmado", "descartado"))
	summary(train_test_base)
	
	#Base para predição
	predic_base <- subset(covid, !(covid$RESULTADO %in% c("confirmado", "descartado")))
	summary(predic_base)
	
	
	boot_base <- list()
	for(i in 1:n_boot){
		boot_base[[i]] <- estimativa(train_test_base, predic_base, 1)
	}
	
	cum_base <- list()
	for(i in 1:n_boot){
		cum_base[[i]] <- boot_base[[i]][[1]][c(1,2,4)] %>% as.data.frame()	
	}
	
	cum_base <- Reduce(function(x,y) merge(x , y, by = c("INICIO_SINTOMAS", "DADOS"), all = T), cum_base) %>% as.data.frame()
	cum_base_mat <- as.matrix(cum_base[,c(3:ncol(cum_base))])
	cum_base$MEDIANA_CASOS <- rowMedians(cum_base_mat, na.rm = T)
	cum_base$II_025 <- rowQuantiles(cum_base_mat, probs = 0.025, na.rm = T) 
	cum_base$II_975 <- rowQuantiles(cum_base_mat, probs = 0.975, na.rm = T) 
	cum_base_ter[[i]] <- cum_base

}

#Parando paralelização
parallelStop()
