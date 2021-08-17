source("./srcs_2/funcoes.R")

library(data.table)
library(lubridate)
library(purrr)
library(dplyr)
library(zoo)
library(magrittr)
library(glue)

###### Arrumando Base empresa 1 ####

base <- fread("./Dados/faturas_company_1_gf.csv")
dic <- fread("./Dados/periodicidade_pagamento_company_1_gf.csv")
colnames(dic)[1] <- "periodicidade_pagamento"

base <- merge(base,dic,by="periodicidade_pagamento")
base$data_cancelamento <- NA

base <- select(base,id_fatura,id_empresa,valor_fatura,dias,recorrente,cancelado,data_cancelamento,data_pagamento)

base$data_pagamento <- as.Date(base$data_pagamento, format = "%d/%m/%Y")


base$data_fim <- base$data_pagamento+base$dias
base$valor_dia <- base$valor_fatura/base$dias

data_base = as.Date("2021-02-28")
base$diff_meses <- ifelse(base$recorrente == 1 & base$cancelado == "Nao" & data_base > base$data_fim,
                          mondf(base$data_pagamento,data_base),
                          mondf(base$data_pagamento,base$data_fim))

## objeto ativos é usado em Clientes_Churn.R 
ativos <- base

###### Calculando Receita E1 ######

st <- Sys.time()
print(st)
dt <- replicate_rows(base,"diff_meses")
print(Sys.time()-st)


list_ids <- unique(dt$id_fatura)

dt2 <- dt[which(dt$id_fatura == list_ids[1])] 
dt2 <- Rec_mensal_por_id(dt2,data.fim.p = "Não")

Sys.time()
st <- Sys.time()
for (i in 2:length(list_ids)) {
  print(i)
  dtx <- dt[which(dt$id_fatura == list_ids[i])]
  dtx <- Rec_mensal_por_id(dtx,data.fim.p = "Não") 
  dt2 <- rbind(dt2,dtx)
}
print(Sys.time()-st)


Receita_E1 <- dt2
Receita_E1$Valor <- as.numeric(Receita_E1$Valor)

Receita_E1 <- Receita_E1 %>% 
  group_by(Mes_ano,dias) %>% 
  summarise(Valor = sum(Valor))

saveRDS(Receita_E1,"./Resultados/Receita_E1_tipo_plano.rds")

###### Arrumando Base empresa 2 ######
orders_e2 <- fread("./Dados/orders_company_2_gf.csv")
orders_profile_e2 <- fread("./Dados/order_profiles_company_2_gf.csv")
payment_forms_e2 <- fread("./Dados/payment_forms_company_2_gf.csv")


orders_profile_e2$date_start <- as.Date(orders_profile_e2$date_start)
orders_profile_e2$date_end <- as.Date(orders_profile_e2$date_end)

colnames(orders_e2)[1] <- "order_id"

orders <- merge(orders_profile_e2,orders_e2,by="order_id")

orders <- select(orders,
                 account_id,
                 profile_id,
                 order_id,
                 id,
                 value,
                 payment_form_id,
                 recurring,
                 date_start,
                 date_end)


orders_T <- select(orders,account_id,
                   id,value,payment_form_id,recurring,date_start,date_end)

colnames(orders_T) <- c("id_empresa","id_fatura","valor_fatura","dias","recorrente","data_pagamento","data_fim")

colnames(payment_forms_e2)[1] <- "dias"
orders_T <- merge(orders_T,
                  select(payment_forms_e2,
                         dias,
                         months),
                  by = "dias")

#### Aqui pode usar o dicio da empresa 1 pra automatizar 
orders_T$months[which(orders_T$months == 1)] <- 30
orders_T$months[which(orders_T$months == 3)] <- 90
orders_T$months[which(orders_T$months == 6)] <- 180
orders_T$months[which(orders_T$months == 12)] <- 365


orders_T <- select(orders_T,
                   id_empresa,
                   id_fatura,
                   valor_fatura,
                   months,
                   recorrente,
                   data_pagamento,
                   data_fim)
colnames(orders_T)[which(colnames(orders_T)=="months")] <- "dias"

orders_T$valor_dia <- orders_T$valor_fatura/orders_T$dias

# data_base = Sys.Date() ## em uma automatização a data base provavalmente será o dia atual
data_base = as.Date("2021-02-28")



orders_T$diff_meses <- mondf(orders_T$data_pagamento,orders_T$data_fim)

## objeto artivos2 é usadao no Clientes_Churn.R
ativos2 <- orders_T


###### Calculando Receita Empresa 2 #####


st <- Sys.time()
print(st)
dt_e2 <- replicate_rows(orders_T,"diff_meses")
print(Sys.time()-st)


list_ids_e2 <- unique(dt_e2$id_fatura)

dt2_e2 <- dt_e2[which(dt_e2$id_fatura == list_ids_e2[1])] 
dt2_e2 <- Rec_mensal_por_id(dt2_e2,data.fim.p = "Sim")

Sys.time()
st <- Sys.time()
for (i in 2:length(list_ids_e2)) {
  print(i)
  dtx <- dt_e2[which(dt_e2$id_fatura == list_ids_e2[i])]
  dtx <- Rec_mensal_por_id(dtx,data.fim.p = "Sim") 
  dt2_e2 <- rbind(dt2_e2,dtx)
}
print(Sys.time()-st)


Receita_E2 <- dt2_e2
Receita_E2$Valor <- as.numeric(Receita_E2$Valor)

Receita_E2 <- Receita_E2 %>% 
  group_by(Mes_ano,dias) %>% 
  summarise(Valor = sum(Valor))

saveRDS(Receita_E2,"./Resultados/Receita_E2_tipo_plano.rds")







