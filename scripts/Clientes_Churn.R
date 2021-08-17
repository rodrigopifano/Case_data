source("./srcs_2/Receita.R")



##### Clientes Ativos Empresa 1  (aqui já se calcula clientes que saem da base e calcula-se churn de cliente) #####
ativos$meses <- mondf(base$data_pagamento[order(base$data_pagamento)[1]],data_base)

st <- Sys.time()
print(st)
ativos.dt <- replicate_rows(ativos,"meses")
print(Sys.time()-st)



list_ids2 <- unique(ativos.dt$id_empresa)
ativos.dt2 <- ativos.dt[which(ativos.dt$id_empresa == list_ids2[1])][order(ativos.dt[which(ativos.dt$id_empresa==list_ids2[1])]$data_pagamento)]
list_ids_fatura <- unique(ativos.dt2$id_fatura)
ttt <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[1])]
if (ttt$recorrente[1] == 1) {
  ttt  <- create_data(ttt,data.fim.p = "Não")  
}else{
  ttt  <- create_data(ttt,data.fim.p = "Sim")
}


ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
ttt$mes_fim <- as.yearmon(ttt$data_fim2)

entr_said1 <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
entr_said1$Entrada <- 0
entr_said1$Entrada[1] <- 1

entr_said1$Saida <- 0
if (all(ttt$recorrente!=1)) {
  entr_said1$Saida[which(entr_said1$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1
}

  
for (i in 2:length(list_ids2)) {
  ativos.dt2 <- ativos.dt[which(ativos.dt$id_empresa == list_ids2[i])][order(ativos.dt[which(ativos.dt$id_empresa==list_ids2[i])]$data_pagamento)]
  list_ids_fatura <- unique(ativos.dt2$id_fatura)
  ttt <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[1])]
  if (ttt$recorrente[1] == 1) {
    ttt  <- create_data(ttt,data.fim.p = "Não")  
  }else{
    ttt  <- create_data(ttt,data.fim.p = "Sim")
  }
  if (length(list_ids_fatura) == 1) {
    ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
    ttt$mes_fim <- as.yearmon(ttt$data_fim2)
    
    entr_said <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
    entr_said$Entrada <- 0
    entr_said$Entrada[1] <- 1
    
    entr_said$Saida <- 0
    if (all(ttt$recorrente!=1)) {
      entr_said$Saida[which(entr_said$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1
    }
    entr_said1 <- rbind(entr_said1,entr_said)
  } else {
    for (n in 2:length(list_ids_fatura)) {
      ttt2 <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[n])]
      if (ttt2$recorrente[1] == 1) {
        ttt2  <- create_data(ttt2,data.fim.p = "Não")  
      }else{
        ttt2  <- create_data(ttt2,data.fim.p = "Sim")
      }
      ttt <- rbind(ttt,ttt2)
    }
    
    ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
    ttt$mes_fim <- as.yearmon(ttt$data_fim2)
    
    entr_said <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
    entr_said$Entrada <- 0
    entr_said$Entrada[1] <- 1

    entr_said$Saida <- 0
    if (all(ttt$recorrente!=1)) {
      entr_said$Saida[which(entr_said$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1
    }
    entr_said1 <- rbind(entr_said1,entr_said) 
  }
}

clientes_e1 <- entr_said1

clientes_e1 <- clientes_e1 %>% 
  group_by(Mes_inicio) %>% 
  s_summarise(paste(names(clientes_e1[,-1]),paste0("= sum(",names(clientes_e1[,-1]),")"),collapse = ","))

clientes_e1$Ativos <- cumsum(clientes_e1$Entrada)
clientes_e1$Ativos2 <- clientes_e1$Ativos-c(0,clientes_e1$Saida[-nrow(clientes_e1)])
clientes_e1$churn_e1 <- round(clientes_e1$Saida/clientes_e1$Ativos2*100,2) 

saveRDS(clientes_e1,"./Resultados/clientes_e1_v2.rds")


##### Churn de Receita E1  (percentual está calulado na hora da renderizacao do resultado) #####
st <- Sys.time()
print(st)
dt <- replicate_rows(base,"diff_meses")
print(Sys.time()-st)

mrr_churn_e1 <- dt[which(dt$dias!=30)]
lista_faturas_mmr <- unique(mrr_churn_e1$id_fatura)
aux1 <- mrr_churn_e1[which(mrr_churn_e1$id_fatura == lista_faturas_mmr[1])]
aux1 <- create_data(aux1,"Sim")
aux1 <- aux1[nrow(aux1),]
for (k in 2:length(lista_faturas_mmr)) {
  aux2 <- mrr_churn_e1[which(mrr_churn_e1$id_fatura == lista_faturas_mmr[k])]
  aux2 <- create_data(aux2,"Sim")
  aux2 <- aux2[nrow(aux2),]
  aux1 <- rbind(aux1,aux2)
}
aux1$dia_final_mes_fim <- do.call("c",map(aux1$data_fim2,dia_final))  
aux1$diff_dias <- aux1$dia_final_mes_fim-aux1$data_fim2
aux1$mrr_churn <- as.numeric(aux1$valor_dia*(aux1$diff_dias+1))
mrr_churn_e1 <- select(aux1,
                       dia_final_mes_fim,mrr_churn)
mrr_churn_e1$dia_final_mes_fim <- as.yearmon(mrr_churn_e1$dia_final_mes_fim)

mrr_churn_e1 <- mrr_churn_e1 %>%
  group_by(dia_final_mes_fim) %>%
  summarise(mrr_churn = sum(mrr_churn))

colnames(mrr_churn_e1)[1] <- "Mês"
saveRDS(mrr_churn_e1,"./Resultados/mrr_churn_e1.rds")





##### Clientes Ativos Empresa 2 (aqui já se calcula clientes que saem da base e calcula-se churn de cliente) #####


ativos2$meses <- mondf(orders_T$data_pagamento[order(orders_T$data_pagamento)[1]],data_base)

st <- Sys.time()
print(st)
ativos.dt <- replicate_rows(ativos2,"meses")
print(Sys.time()-st)



list_ids2 <- unique(ativos.dt$id_empresa)
ativos.dt2 <- ativos.dt[which(ativos.dt$id_empresa == list_ids2[1])][order(ativos.dt[which(ativos.dt$id_empresa==list_ids2[1])]$data_pagamento)]

list_ids_fatura <- unique(ativos.dt2$id_fatura)
ttt <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[1])]
ttt  <- create_data(ttt,data.fim.p = "Sim")
ttt <- select(ttt,
              id_empresa,
              id_fatura,
              dias,
              data_pagamento,
              data_fim,
              data_inicio2,data_fim2)

for (i in 2:length(list_ids_fatura)) {
  ttt2 <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[i])]
  ttt2 <- create_data(ttt2,data.fim.p = "Sim")
  ttt2 <- select(ttt2,
                id_empresa,
                id_fatura,
                dias,
                data_pagamento,
                data_fim,
                data_inicio2,data_fim2)
  ttt <- rbind(ttt,ttt2)
}


ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
ttt$mes_fim <- as.yearmon(ttt$data_fim2)

entr_said2 <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
entr_said2$Entrada <- 0
entr_said2$Entrada[1] <- 1

entr_said2$Saida <- 0

entr_said2$Saida[which(entr_said2$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1  


st <- Sys.time()
print(st)
for (i in 2:length(list_ids2)) {
  ativos.dt2 <- ativos.dt[which(ativos.dt$id_empresa == list_ids2[i])][order(ativos.dt[which(ativos.dt$id_empresa==list_ids2[i])]$data_pagamento)]
  list_ids_fatura <- unique(ativos.dt2$id_fatura)
  ttt <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[1])]
  ttt  <- create_data(ttt,data.fim.p = "Sim")
  ttt <- select(ttt,
                id_empresa,
                id_fatura,
                dias,
                data_pagamento,
                data_fim,
                data_inicio2,data_fim2)
  if (length(list_ids_fatura) == 1) {
    ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
    ttt$mes_fim <- as.yearmon(ttt$data_fim2)
    
    entr_said <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
    entr_said$Entrada <- 0
    entr_said$Entrada[1] <- 1
    
    entr_said$Saida <- 0
    entr_said$Saida[which(entr_said$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1  
    entr_said2 <- rbind(entr_said2,entr_said)
  } else {
    for (n in 2:length(list_ids_fatura)) {
      ttt2 <- ativos.dt2[which(ativos.dt2$id_fatura == list_ids_fatura[n])]
      ttt2 <- create_data(ttt2,data.fim.p = "Sim")
      ttt2 <- select(ttt2,
                     id_empresa,
                     id_fatura,
                     dias,
                     data_pagamento,
                     data_fim,
                     data_inicio2,data_fim2)
      ttt <- rbind(ttt,ttt2)
    }
    
    ttt$mes_inicio <- as.yearmon(ttt$data_inicio2)
    ttt$mes_fim <- as.yearmon(ttt$data_fim2)
    
    entr_said <- data.frame("Mes_inicio" = unique(ttt$mes_inicio))
    entr_said$Entrada <- 0
    entr_said$Entrada[1] <- 1
    
    entr_said$Saida <- 0
    entr_said$Saida[which(entr_said$Mes_inicio == as.yearmon(ttt$data_fim[nrow(ttt)]))] <- 1  
    entr_said2 <- rbind(entr_said2,entr_said) 
  }
}



clientes_e2 <- entr_said2

clientes_e2 <- clientes_e2 %>% 
  group_by(Mes_inicio) %>% 
  s_summarise(paste(names(clientes_e2[,-1]),paste0("= sum(",names(clientes_e2[,-1]),")"),collapse = ","))

clientes_e2$Ativos <- cumsum(clientes_e2$Entrada)
clientes_e2$Ativos2 <- clientes_e2$Ativos-c(0,clientes_e2$Saida[-nrow(clientes_e2)])

clientes_e2$churn_e2 <- round(clientes_e2$Saida/clientes_e2$Ativos2*100,2) 

saveRDS(clientes_e2,"./Resultados/clientes_e2.rds")




##### Faturas canceladas empresa 2  #######


canc <- cancelados(orders_T,1) %>%
  cancelados(2)

cancelados_mes_mes <- select(canc[which(canc$cancelados_m2 == 1 |canc$cancelados_m1 == 1 ),],data_fim,cancelados_m1,cancelados_m2)
cancelados_mes_mes$data_fim <- as.yearmon(cancelados_mes_mes$data_fim)

cancelados_mes_mes <- cancelados_mes_mes %>% 
  group_by(data_fim) %>% 
  s_summarise(paste(names(cancelados_mes_mes[,-1]),paste0("= sum(",names(cancelados_mes_mes[,-1]),")"),collapse = ","))
colnames(cancelados_mes_mes)[1] <- "Mes_ano"  


##### Churn de receita E2  #####

canc$dia_final_mes_fim <- do.call("c",map(canc$data_fim,dia_final))

canc$dif_dias_canc <- canc$dia_final_mes_fim-canc$data_fim 

canc$receita_perdida_m1 <- 0
canc$receita_perdida_m1[which(canc$cancelados_m1==1)] <- canc$valor_dia[which(canc$cancelados_m1==1)]*as.numeric(canc$dif_dias_canc[which(canc$cancelados_m1==1)])

canc$receita_perdida_m2 <- 0
canc$receita_perdida_m2[which(canc$cancelados_m2==1)] <- canc$valor_dia[which(canc$cancelados_m2==1)]*as.numeric(canc$dif_dias_canc[which(canc$cancelados_m2==1)])


mrr_churn <- select(canc,data_fim,cancelados_m1,cancelados_m2,receita_perdida_m1,receita_perdida_m2)
mrr_churn$data_fim <- as.yearmon(mrr_churn$data_fim)

mrr_churn_m1 <- mrr_churn

mrr_churn_m1 <- mrr_churn_m1 %>% 
  group_by(data_fim) %>% 
  summarise(receita_perdida_m1 = sum(receita_perdida_m1))


mrr_churn_m2 <- mrr_churn

mrr_churn_m2 <- mrr_churn_m2 %>% 
  group_by(data_fim) %>% 
  summarise(receita_perdida_m2 = sum(receita_perdida_m2))

mrr_churn <- merge(mrr_churn_m1,mrr_churn_m2)
colnames(mrr_churn)[1] <- "Mês"

saveRDS(mrr_churn,"./Resultados/MRR_churn_e2.rds")


