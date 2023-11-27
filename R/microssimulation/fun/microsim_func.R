microsim_func<- function(x){
  pb$tick(0)
  # x <- puma_list[4]
  #Set
  message(paste("Rodadndo APONDE", x))
  census_cons = census_cons 
  census_micro = census_micro
  id_puma<-x
  n_micro<- census_micro %>% filter (APONDE==id_puma) %>% drop_na()
  n_micro<- n_micro %>% mutate(
    across(age_sex:renda_class_pc,~droplevels(as.factor(.x))
    )
  )
  #Adding clone census micro with n_col individuals
  perc_col<- 2*sum(as.numeric(as.character(census_cons$n_col[census_cons$CD_APONDE==id_puma])))/sum(as.numeric(as.character(census_cons$inc_pc_2[census_cons$CD_APONDE==id_puma]))+as.numeric(as.character(census_cons$inc_pc_2_plus[census_cons$CD_APONDE==id_puma]))+as.numeric(as.character(census_cons$n_col[census_cons$CD_APONDE==id_puma])))
  
  # n_micro_col<- n_micro %>% mutate(renda_class_pc = as.factor("n_col")) %>% sample_n(round(nrow(n_micro)*perc_col))
  # n_micro_ind<- n_micro %>% sample_n(round(nrow(n_micro)*(1-perc_col)))
  n_micro_ind<- n_micro
  n_micro_col<- n_micro %>% mutate(renda_class_pc = as.factor("n_col"))
  n_micro<- rbind(n_micro_col,n_micro_ind)
  
  
  #Set the matrices
  census_micro_matrix<- create_mat(data = n_micro,
                                   first_col = 2,
                                   last_col = 6)

  set_columns<-colnames(census_cons)[colnames (census_cons) %in% colnames(census_micro_matrix)]
  census_micro_matrix<- census_micro_matrix[,set_columns]
  census_cons<- census_cons[,c("code_tract","CD_APONDE",set_columns)]

  #Test if the columns are the same
  colnames(census_cons)[3:ncol(census_cons)]==colnames(census_micro_matrix)

  #RMA Setores para remover:
  # 280030805000155 - Asilo
  
  # n_census_tract<-census_cons %>% filter(CD_APONDE==id_puma) %>%
  #   filter(
  #     
  #     #Contagem
  #     code_tract != 311860105120027 , code_tract != 311860105270052,
  #          code_tract != 311860105130034,
  #          
  #          #RMA
  #          code_tract != 280030805000155,
  #     code_tract != 280480505000164,
  #     code_tract != 280480505000150,
  #     code_tract != 280480505000153
  #     
  #          ) %>%
  #   select(-c("code_tract","CD_APONDE"))
  
  
  
  n_census_tract<-census_cons %>% filter(CD_APONDE==id_puma) #%>%
  # select(-c("code_tract","CD_APONDE"))
  # n_census_tract <- n_census_tract[1:89,] %>%
    n_census_tract <- n_census_tract %>%
  
    filter(
      
      #Contagem
      code_tract != 311860105120027 , code_tract != 311860105270052,
      code_tract != 311860105130034,
      
      #RMA
      code_tract != 280030805000155,
      code_tract != 280480505000106,
      code_tract != 280480505000134,
      code_tract != 280480505000194,
      
      code_tract != 280480505000137,
      code_tract != 130260305120576
    ) %>%
    select(-c("code_tract","CD_APONDE"))

  ind_agg <- colSums(census_micro_matrix) # save the result
  
  #Set the weights
  weights <- matrix(data = NA, nrow = nrow(n_micro), ncol = nrow(n_census_tract))

  # convert matrix census to numeric data type
  n_census_tract <- apply(n_census_tract, 2, as.numeric)
  n_micro_t <- t(census_micro_matrix) # save transposed version
  x0 <- rep(1, nrow(n_micro)) # save the initial vector
  
  #Weighting individuals
  weights <- apply(n_census_tract, 1, function(x) ipfp(x,
                                                       n_micro_t,
                                                       x0,
                                                       maxit = 10000))
  
  ints <- apply(weights, 2, int_trs) # generate integerised result
  ints_2<-ints

  indices <- NULL
  ints <- for(i in 1:ncol(ints)){
    indices <- c(indices, int_expand_vector(ints[,i]))
  }

  ints_df <- data.frame(id = indices,
                        zone = rep(census_cons$code_tract[census_cons$CD_APONDE==id_puma &
                                                            census_cons$code_tract != 311860105120027 &
                                                            census_cons$code_tract != 311860105270052 &
                                                            census_cons$code_tract != 311860105130034 &
                                                            #RMA
                                                            census_cons$code_tract != 280030805000155 &
                                                            census_cons$code_tract != 280480505000106 &
                                                            census_cons$code_tract != 280480505000134 &
                                                            census_cons$code_tract != 280480505000194 &
                                                            census_cons$code_tract != 280480505000137 &
                                                            census_cons$code_tract != 130260305120576
                                                            ],
                                   colSums(ints_2)))
  n_micro$id<-seq(1:nrow(n_micro))
  microsim_data<- left_join(ints_df,n_micro,by="id")
  names(microsim_data)[2]<-"code_tract"

  microsim_data_cr<- microsim_data %>% filter(Rend_pes>0)
  # 6. Validation -----------------------------------------------------------
  ind_agg <- t(apply(weights, 2, function(x) colSums(x * census_micro_matrix)))
  colnames(ind_agg) <- colnames(n_census_tract)
  
  # 6.1 Internal validation -------------------------------------------------
  
  #correlation constraint vs simulated
  ivr.cor<-cor(as.numeric(n_census_tract), as.numeric(ind_agg))

  #correlation constraint vs simulated per zone
  # initialize the vector of correlations
  CorVec <- rep (0, dim(n_census_tract)[1])
  # calculate the correlation for each zone
  for (i in 1:dim(n_census_tract)[1]){
    num_cons <- as.numeric(n_census_tract[i,])
    num_ind_agg <- as.numeric(ind_agg[i,])
    CorVec[i] <- cor (num_cons, num_ind_agg)
  }
  head(order(CorVec), n = 3)
  summary(CorVec)
  
  #Total absolute error (TAE)
  tae <- function(observed, simulated){
    obs_vec <- as.numeric(observed)
    sim_vec <- as.numeric(simulated)
    sum(abs(obs_vec - sim_vec))
  }
  
  #TAE TOTAL
  ivr.TAE<-tae(n_census_tract, ind_agg)
  #RE TOTAL
  ivr.RE<-tae(n_census_tract, ind_agg)/sum(n_census_tract)
  
  
  
  TAEVec <- rep(0, nrow(n_census_tract))
  REVec <- rep(0, nrow(n_census_tract))
  
  for (i in 1:nrow(n_census_tract)){
    TAEVec[i] <- tae (n_census_tract[i,], ind_agg[i,])
    REVec[i] <- TAEVec[i] / sum(n_census_tract[i,])
  }
  
  # Summary of the RE per zone
  summary(TAEVec)
  
  ivr.TAE.max<-max(TAEVec)
  
  # Summary of the RE per zone
  #summary(REVec)
  ivr.RE.max<-max(REVec)
  
  
  # 6.2 External validation -------------------------------------------------
  
  # 6.2.1 Compare the means GERAL -------------------------------------------

  microsim_resume<- microsim_data %>% filter(renda_class!="Total_u10" &
                                               Rend_pes>0)%>%
    group_by(code_tract) %>% summarise(n=n(),renda_media_m=mean(Rend_pes)) %>%
    left_join(data_basico,by=c("code_tract"="code_tract")) %>% 
    filter(!is.na(renda_media11))

  microsim_resume$DIF<-microsim_resume$renda_media_m-microsim_resume$renda_media11
  vr.g<-t.test(microsim_resume$DIF,mu=0)
  vr.g<-vr.g$p.value

   # 6.2.1 Compare the means por setor - Pessoas com rendimento --------------
  
  p_value_cr<-function(code_sector){
    data.renda<-microsim_data_cr$Rend_pes[microsim_data_cr$code_tract==code_sector & microsim_data_cr$Rend_pes>0]
    mean<-as.numeric(microsim_resume[microsim_resume$code_tract==code_sector,'renda_media11'])
    if(is.na(mean))(mean=0)
    xx<-t.test(data.renda,mu=mean)
    return(xx$p.value)
  }
  
  data.p_value_cr<-sapply(as.character(unique(microsim_data_cr$code_tract)),p_value_cr)
  vr.pv.cr<-sum(data.p_value_cr>=0.05)/length(data.p_value_cr)
  
  
  
  
  p_value_z_cr<-function(code_sector){
    data.renda<-microsim_data$Rend_pes[microsim_data$code_tract==code_sector & microsim_data$Rend_pes>0]
    if(is.na(mean))(mean=0)
    mean<-as.numeric(microsim_resume[microsim_resume$code_tract==code_sector,'renda_media11'])
    sigma<-sqrt(as.numeric(microsim_resume[microsim_resume$code_tract==code_sector,'variancia11']))
    xx<-z.test(data.renda,mu=mean,sigma.x=sigma)
    return(xx$p.value)
  }
  
  data.p_value_z_cr<-sapply(as.character(unique(microsim_data_cr$code_tract)),p_value_z_cr)
  vr.pvz.cr<-sum(data.p_value_z_cr>=0.05)/length(data.p_value_z_cr)
  
  
  # 7. Save the data --------------------------------------------------------
  
  
  validation.resume.internal<-data.frame(c(id_puma),
                                         c(ivr.cor),
                                         c(ivr.TAE),
                                         c(ivr.RE),
                                         c(ivr.TAE.max),
                                         c(ivr.RE.max))
  
  
  validation.resume<-data.frame(c(id_puma),
                                c(vr.g),
                                c(vr.pv.cr),
                                c(vr.pvz.cr))
  names(validation.resume)<-c("APONDE","Test_dif_p.value","ttest_perc_com_renda","ztest_perc_com_renda")
  
  if (!file.exists("data/03_microsimulated")) dir.create("data/03_microsimulated")
  saveRDS(microsim_data,paste0("data/03_microsimulated/01_microsim_puma_",id_puma,".RDS"))
  saveRDS(validation.resume.internal,paste0("data/03_microsimulated/02_intv_puma_",id_puma,".RDS"))
  saveRDS(validation.resume,paste0("data/03_microsimulated/03_extv_puma_",id_puma,".RDS"))
  pb$tick()
  Sys.sleep(0.1)
}


  
  

