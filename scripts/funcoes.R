library(lubridate)
library(purrr)
library(dplyr)
library(zoo)
library(magrittr)
library(glue)


## string no dplyr ##
source("./srcs_2/string_dplyr.R")

## Funcao que retorna ultimo dia do mes de uma determinada data ##
dia_final <- function(data){
  meses31 <- c(1,3,5,7,8,10,12)
  meses30 <- c(4,6,9,11)
  if (month(data) %in% meses31) {
    d_final <- date(paste0(as.character(year(data)),
                           "-",
                           as.character(month(data)),
                           "-",
                           "31"))
  } else if (month(data) %in% meses30) {
    d_final <- date(paste0(as.character(year(data)),
                           "-",
                           as.character(month(data)),
                           "-",
                           "30"))
  } else {
    if (year(data) %% 4 == 0) {
      d_final <- date(paste0(as.character(year(data)),
                             "-",
                             as.character(month(data)),
                             "-",
                             "29"))
    } else {  d_final <- date(paste0(as.character(year(data)),
                                     "-",
                                     as.character(month(data)),
                                     "-",
                                     "28"))
    }
  }      
  return(d_final)
}

## Função que informa o número de meses entre duas datas
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));
lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

## Função que replica linhas de um data.frame de acordo com o valor de uma determinada coluna naquela linha
replicate_rows <- function(df,coluna){
  dt1 <- setDT(df[1,])
  dt1 %<>% slice(rep(row_number(), eval(parse(text = paste0("dt1$",glue("{coluna}"))))+1))
  for (i in 2:nrow(df)) {
    dt <- setDT(df[i,])
    dt %<>% slice(rep(row_number(), eval(parse(text = paste0("dt$",glue("{coluna}"))))+1))
    dt1 <- rbind(dt1,dt)
  }
  return(dt1)
}


## Função que calcula receita mensal por fatura ###
Rec_mensal_por_id <-function(dataframe,data.fim.p){
  tt <- dataframe
  if (tt$diff_meses[1] < 1) {
    tt$data_inicio2 <- tt$data_pagamento
    tt$data_fim2 <- tt$data_fim
  } else {
    tt$data_pagamento[2:nrow(tt)] <- floor_date(tt$data_pagamento[1], "month")
    tt$index <- seq(0,nrow(tt)-1)
    
    tt$data_inicio2 <- tt$data_pagamento 
    tt$data_inicio2 <- tt$data_pagamento %m+% months(tt$index) 
    
    tt$data_fim2 <- tt$data_fim
    if (data.fim.p == "Sim") {
      tt$data_fim2[1:nrow(tt)-1] <- do.call("c",map(tt$data_inicio2[1:nrow(tt)-1],dia_final))  
    } else { 
      tt$data_fim2[1:nrow(tt)] <- do.call("c",map(tt$data_inicio2[1:nrow(tt)],dia_final))  
      }
  }
  tt$n_dias <- tt$data_fim2-tt$data_inicio2
  if (tt$dias[1] == 30) {
    tt$Valor <- as.numeric(tt$valor_dia*(tt$n_dias+1))
  } else {
    tt$Valor <- as.numeric(tt$valor_dia*(tt$n_dias+1))
    tt$Valor[nrow(tt)] <- tt$Valor[nrow(tt)]-tt$valor_dia[1] 
  }
  tt$Mes_ano <- as.yearmon(tt$data_fim2)
  tt <- select(tt,
               id_fatura,
               Mes_ano,
               valor_fatura,
               dias,
               recorrente,
               valor_dia,
               diff_meses,
               data_inicio2,data_fim2,
               n_dias,
               Valor)
  return(tt)
}


### Função que cria datas de inicio e final de mes
create_data <- function(x,data.fim.p){
  teste <- x
  if (teste$diff_meses[1] < 1) {
    teste$data_inicio2 <- teste$data_pagamento
    teste$data_fim2 <- teste$data_fim
  } else {
    teste$data_pagamento[2:nrow(teste)] <- floor_date(teste$data_pagamento[1], "month")
    teste$index <- seq(0,nrow(teste)-1)
    
    teste$data_inicio2 <- teste$data_pagamento 
    teste$data_inicio2 <- teste$data_pagamento %m+% months(teste$index) 
    
    teste$data_fim2 <- teste$data_fim
    if (data.fim.p == "Sim") {
      teste$data_fim2[1:nrow(teste)-1] <- do.call("c",map(teste$data_inicio2[1:nrow(teste)-1],dia_final))  
    } else { 
      teste$data_fim2[1:nrow(teste)] <- do.call("c",map(teste$data_inicio2[1:nrow(teste)],dia_final))  
    }
  }
  return(teste)
}


### Função que calcula quais faturas foram canceladas na empresa 2 (dada a existência da informação data_fim, a metodologia difera da aplicada na empresa 1)
cancelados <- function(data,metodologia){
  data$n_dias <- data$data_fim - data$data_pagamento 
  if (metodologia == 2) {
    data$cancelado <- ifelse(as.numeric((data$n_dias+1))%%as.numeric(data$dias) > 0 | data$recorrente == 1 & data$data_fim < data_base,
                             1,
                             0)
  } else if (metodologia == 1) {
    data$cancelado <- ifelse(data$n_dias+1 < data$dias | data$recorrente == 1 & data$data_fim < data_base,
                             1,
                             0)
  }
  
  
  colnames(data)[which(colnames(data)=="cancelado")] <- glue("cancelados_m{metodologia}")
  return(data)
  
}












