shiny_test<- function(temp_sel,temp_sel,experiment_plate){
temp_sel$Cq<-replace(temp_sel$Cq,is.na(temp_sel$Cq),"40")
temp_sel$Cq<-replace(temp_sel$Cq,is.na(temp_sel$Cq),"40")

temp_sel$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_sel$`Cq Mean`,perl = TRUE)
temp_sel$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_sel$`Cq Mean`,perl = TRUE)

temp_sel$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_sel$Target)
temp_sel$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_sel$Target)

temp_sel$Target<-gsub(replacement = "ELF", pattern = "elf",temp_sel$Target)
temp_sel$Target<-gsub(replacement = "ELF",pattern = "elf",temp_sel$Target)

temp_sel$Cq <- as.numeric(temp_sel$Cq)
temp_sel$Cq <- as.numeric(temp_sel$Cq)


temp_sel$`Cq Mean` <- as.numeric(temp_sel$`Cq Mean`)
temp_sel$`Cq Mean` <- as.numeric(temp_sel$`Cq Mean`)



temp_sel$Target <- gsub(pattern = "sel",replacement = "bFLO", temp_sel$Target)
temp_sel$Target <- gsub(pattern = "Bflo",replacement = "bFLO", temp_sel$Target)

temp2_sel<-temp_sel %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))

temp2_sel <- temp2_sel %>% mutate("ΔCq" = Cq_bFLO - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_bFLO` - `Cq Mean_ELF`)

temp2_sel<-temp_sel  %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))

temp2_sel <- temp2_sel %>% mutate("ΔCq" = Cq_Spec - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_Spec` - `Cq Mean_ELF`)


temp2_sel_e46 <- temp2_sel %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
temp2_sel_e46 <- distinct(.data = temp2_sel_e46, .keep_all = TRUE)
temp2_sel_e46 <- temp2_sel %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
temp2_sel_e46 <- distinct(.data = temp2_sel_e46,Plate,.keep_all = TRUE)

temp2_sel <- temp2_sel %>% left_join(temp2_sel_e46, by = "Plate")
temp2_sel <- temp2_sel %>% left_join(temp2_sel_e46, by = "Plate")

colnames(temp2_sel)[10] <- "Calibrator_ΔCq"
colnames(temp2_sel)[10] <- "Calibrator_ΔCq"

colnames(temp2_sel)[9] <- "ΔCq_Mean"
colnames(temp2_sel)[9] <- "ΔCq_Mean"
#Working
# temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
# temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
# 
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
# }
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
# }
# 
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
# }
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
# }

temp2_sel<- temp2_sel %>% mutate(ΔΔCq_sel =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_sel =  ΔCq_Mean - Calibrator_ΔCq)
temp2_sel<- temp2_sel %>% mutate(ΔΔCq_sel =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_sel =  ΔCq_Mean - Calibrator_ΔCq)

temp2_sel   <- temp2_sel %>% mutate(Copy_Number_sel = 2^(-ΔΔCq_sel)) %>% mutate(Copy_Number_Mean_sel = 2^(-ΔΔCq_Mean_sel))
temp2_sel <- temp2_sel %>% mutate(Copy_Number_sel = 2^(-ΔΔCq_sel)) %>% mutate(Copy_Number_Mean_sel = 2^(-ΔΔCq_Mean_sel))

assign(paste(experiment_plate,"_sel", sep = ""),temp2_sel, envir = .GlobalEnv)
assign(paste(experiment_plate,"_sel", sep = ""),temp2_sel, envir = .GlobalEnv)

temp3 <- if(length(temp2_sel$Sample) <= length(temp2_sel$Sample)) {
  temp2_sel %>% left_join(select(temp2_sel,Well,Sample,ΔΔCq_sel,ΔΔCq_Mean_sel,Copy_Number_sel,Copy_Number_Mean_sel), by = c("Well","Sample"))
} else{temp2_sel %>% left_join(select(temp2_sel,Well,Sample,ΔΔCq_sel,ΔΔCq_Mean_sel,Copy_Number_sel,Copy_Number_Mean_sel), by = c("Well","Sample"))
}
temp3 <- temp3 %>% left_join(select(py$df_plant, Barcode, Generation ,`Parent Plant Line`), by = c("Sample" = "Barcode"))
temp3 <- temp3 %>% filter(Sample != "Thorne" & Sample != "thorne" & Sample != "E46" & Sample != "e46" & Sample != "E46-10" & Sample != "No template" & Sample != "NTC" & Sample != "No Template" & Sample != "no template")
temp3 <- temp3 %>% select(-Plate,-Calibrator_ΔCq)
temp3 <- temp3 %>% mutate("Experiment" = experiment_plate)
temp3 <- temp3 %>% mutate_at(3:16,round,2)
assign(experiment_plate,temp3,envir = .GlobalEnv)
#write.csv(temp3,paste(experiment_plate,".csv",sep=""))
temp3
}
bFlo_only<- function(temp_sel,experiment_plate){
  temp_sel$Cq<-replace(temp_sel$Cq,is.na(temp_sel$Cq),"40")
  
  temp_sel$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_sel$`Cq Mean`,perl = TRUE)
  
  
  temp_sel$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_sel$Target)
  
  
  temp_sel$Target<-gsub(replacement = "ELF", pattern = "elf",temp_sel$Target)
  
  
  temp_sel$Cq <- as.numeric(temp_sel$Cq)
  
  
  
  temp_sel$`Cq Mean` <- as.numeric(temp_sel$`Cq Mean`)
  
  
  temp_sel$Target <- gsub(pattern = "sel",replacement = "bFLO", temp_sel$Target)
  temp_sel$Target <- gsub(pattern = "Bflo",replacement = "bFLO", temp_sel$Target)
  
  temp2_sel<-temp_sel %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))
  
  temp2_sel <- temp2_sel %>% mutate("ΔCq" = Cq_bFLO - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_bFLO` - `Cq Mean_ELF`)
  
  
  
  temp2_sel_e46 <- temp2_sel %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
  temp2_sel_e46 <- distinct(.data = temp2_sel_e46, .keep_all = TRUE)
  
  
  temp2_sel <- temp2_sel %>% left_join(temp2_sel_e46, by = "Plate")
  
  
  colnames(temp2_sel)[10] <- "Calibrator_ΔCq"
  
  
  colnames(temp2_sel)[9] <- "ΔCq_Mean"
  
  #Working
  # temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
  # temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
  # 
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # 
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  
  temp2_sel<- temp2_sel %>% mutate(ΔΔCq_sel =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_sel =  ΔCq_Mean - Calibrator_ΔCq)
  
  
  temp2_sel   <- temp2_sel %>% mutate(Copy_Number_sel = 2^(-ΔΔCq_sel)) %>% mutate(Copy_Number_Mean_sel = 2^(-ΔΔCq_Mean_sel))
  
  
  assign(x = paste(experiment_plate,"_sel", sep = ""),value = temp2_sel, envir = .GlobalEnv)
  
  temp3 <- temp2_sel
  
  temp3 <- temp3 %>% left_join(select(py$df_plant, Barcode, Generation ,`Parent Plant Line`), by = c("Sample" = "Barcode"))
  temp3 <- temp3 %>% filter(Sample != "Thorne" & Sample != "thorne" & Sample != "E46" & Sample != "e46" & Sample != "E46-10" & Sample != "No template" & Sample != "NTC" & Sample != "No Template" & Sample != "no template")
  temp3 <- temp3 %>% select(-Plate,-Calibrator_ΔCq)
  temp3 <- temp3 %>% mutate("Experiment" = experiment_plate)
  temp3 <- temp3 %>% mutate_at(3:10,round,2)
  assign(experiment_plate,temp3,envir = .GlobalEnv)
  #write.csv(temp3,paste(experiment_plate,".csv",sep=""))
  temp3
}

Sel_only<- function(temp_sel,experiment_plate){
  temp_sel$Cq<-replace(temp_sel$Cq,is.na(temp_sel$Cq),"40")
  
  temp_sel$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_sel$`Cq Mean`,perl = TRUE)
  
  
  temp_sel$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_sel$Target)
  
  
  temp_sel$Target<-gsub(replacement = "ELF", pattern = "elf",temp_sel$Target)
  
  
  temp_sel$Cq <- as.numeric(temp_sel$Cq)
  
  
  
  temp_sel$`Cq Mean` <- as.numeric(temp_sel$`Cq Mean`)
  
  
  temp_sel$Target <- gsub(pattern = "sel",replacement = "bFLO", temp_sel$Target)
  temp_sel$Target <- gsub(pattern = "Bflo",replacement = "bFLO", temp_sel$Target)
  
  temp2_sel<-temp_sel %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))
  
  temp2_sel <- temp2_sel %>% mutate("ΔCq" = Cq_bFLO - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_bFLO` - `Cq Mean_ELF`)
  
  
  
  temp2_sel_e46 <- temp2_sel %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
  temp2_sel_e46 <- distinct(.data = temp2_sel_e46, .keep_all = TRUE)
  
  
  temp2_sel <- temp2_sel %>% left_join(temp2_sel_e46, by = "Plate")
  
  
  colnames(temp2_sel)[10] <- "Calibrator_ΔCq"
  
  
  colnames(temp2_sel)[9] <- "ΔCq_Mean"
  
  #Working
  # temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
  # temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
  # 
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # 
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  # if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
  #   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
  # }
  
  temp2_sel<- temp2_sel %>% mutate(ΔΔCq_sel =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_sel =  ΔCq_Mean - Calibrator_ΔCq)
  
  
  temp2_sel   <- temp2_sel %>% mutate(Copy_Number_sel = 2^(-ΔΔCq_sel)) %>% mutate(Copy_Number_Mean_sel = 2^(-ΔΔCq_Mean_sel))
  
  
  assign(x = paste(experiment_plate,"_sel", sep = ""),value = temp2_sel, envir = .GlobalEnv)
  
  temp3 <- temp2_sel
  
  temp3 <- temp3 %>% left_join(select(py$df_plant, Barcode, Generation ,`Parent Plant Line`), by = c("Sample" = "Barcode"))
  temp3 <- temp3 %>% filter(Sample != "Thorne" & Sample != "thorne" & Sample != "E46" & Sample != "e46" & Sample != "E46-10" & Sample != "No template" & Sample != "NTC" & Sample != "No Template" & Sample != "no template")
  temp3 <- temp3 %>% select(-Plate,-Calibrator_ΔCq)
  temp3 <- temp3 %>% mutate("Experiment" = experiment_plate)
  temp3 <- temp3 %>% mutate_at(3:10,round,2)
  assign(experiment_plate,temp3,envir = .GlobalEnv)
  #write.csv(temp3,paste(experiment_plate,".csv",sep=""))
  temp3
}


