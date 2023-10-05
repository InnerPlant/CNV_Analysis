shiny_test<- function(temp_bflo,temp_sel,experiment_plate){
temp_bflo$Cq<-replace(temp_bflo$Cq,is.na(temp_bflo$Cq),"40")
temp_sel$Cq<-replace(temp_sel$Cq,is.na(temp_sel$Cq),"40")

temp_bflo$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_bflo$`Cq Mean`,perl = TRUE)
temp_sel$`Cq Mean` <- gsub(replacement = "40",pattern="^0$",temp_sel$`Cq Mean`,perl = TRUE)

temp_bflo$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_bflo$Target)
temp_sel$Target<-gsub(replacement = "ELF",pattern = "Elf",temp_sel$Target)

temp_bflo$Target<-gsub(replacement = "ELF", pattern = "elf",temp_bflo$Target)
temp_sel$Target<-gsub(replacement = "ELF",pattern = "elf",temp_sel$Target)

temp_bflo$Cq <- as.numeric(temp_bflo$Cq)
temp_sel$Cq <- as.numeric(temp_sel$Cq)


temp_bflo$`Cq Mean` <- as.numeric(temp_bflo$`Cq Mean`)
temp_sel$`Cq Mean` <- as.numeric(temp_sel$`Cq Mean`)



temp_bflo$Target <- gsub(pattern = "bflo",replacement = "bFLO", temp_bflo$Target)
temp_bflo$Target <- gsub(pattern = "Bflo",replacement = "bFLO", temp_bflo$Target)

temp2_bflo<-temp_bflo %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))

temp2_bflo <- temp2_bflo %>% mutate("ΔCq" = Cq_bFLO - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_bFLO` - `Cq Mean_ELF`)

temp2_sel<-temp_sel  %>% pivot_wider(id_cols = c("Well", "Sample","Plate"),names_from = Target,values_from = c("Cq","Cq Mean"))

temp2_sel <- temp2_sel %>% mutate("ΔCq" = Cq_Spec - Cq_ELF) %>% mutate("ΔCq_Mean" = `Cq Mean_Spec` - `Cq Mean_ELF`)


temp2_bflo_e46 <- temp2_bflo %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
temp2_bflo_e46 <- distinct(.data = temp2_bflo_e46, .keep_all = TRUE)
temp2_sel_e46 <- temp2_sel %>% filter(Sample=="E46-10"|Sample == "E46"| Sample == "e46")%>%select(Plate,ΔCq_Mean)
temp2_sel_e46 <- distinct(.data = temp2_sel_e46,Plate,.keep_all = TRUE)

temp2_bflo <- temp2_bflo %>% left_join(temp2_bflo_e46, by = "Plate")
temp2_sel <- temp2_sel %>% left_join(temp2_sel_e46, by = "Plate")

colnames(temp2_bflo)[10] <- "Calibrator_ΔCq"
colnames(temp2_sel)[10] <- "Calibrator_ΔCq"

colnames(temp2_bflo)[9] <- "ΔCq_Mean"
colnames(temp2_sel)[9] <- "ΔCq_Mean"
#Working
# temp2_bflo <- temp2_bflo %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_bflo,"Sample", "ΔCq_Mean"))
# temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46-10",temp2_sel,"Sample", "ΔCq_Mean"))
# 
# if(is.na(temp2_bflo$Calibrator_ΔCq[2])) {
#   temp2_bflo <- temp2_bflo %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_bflo,"Sample", "ΔCq_Mean"))
# }
# if(is.na(temp2_bflo$Calibrator_ΔCq[2])) {
#   temp2_bflo <- temp2_bflo %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_bflo,"Sample", "ΔCq_Mean"))
# }
# 
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("e46",temp2_sel,"Sample", "ΔCq_Mean"))
# }
# if(is.na(temp2_sel$Calibrator_ΔCq[2])) {
#   temp2_sel <- temp2_sel %>% mutate("Calibrator_ΔCq"=vlookup("E46",temp2_sel,"Sample", "ΔCq_Mean"))
# }

temp2_bflo<- temp2_bflo %>% mutate(ΔΔCq_bflo =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_bflo =  ΔCq_Mean - Calibrator_ΔCq)
temp2_sel<- temp2_sel %>% mutate(ΔΔCq_sel =  ΔCq - Calibrator_ΔCq) %>% mutate(ΔΔCq_Mean_sel =  ΔCq_Mean - Calibrator_ΔCq)

temp2_bflo   <- temp2_bflo %>% mutate(Copy_Number_bflo = 2^(-ΔΔCq_bflo)) %>% mutate(Copy_Number_Mean_bflo = 2^(-ΔΔCq_Mean_bflo))
temp2_sel <- temp2_sel %>% mutate(Copy_Number_sel = 2^(-ΔΔCq_sel)) %>% mutate(Copy_Number_Mean_sel = 2^(-ΔΔCq_Mean_sel))

assign(paste(experiment_plate,"_bflo", sep = ""),temp2_bflo, envir = .GlobalEnv)
assign(paste(experiment_plate,"_sel", sep = ""),temp2_sel, envir = .GlobalEnv)

temp3 <- if(length(temp2_bflo$Sample) <= length(temp2_sel$Sample)) {
  temp2_bflo %>% left_join(select(temp2_sel,Well,Sample,ΔΔCq_sel,ΔΔCq_Mean_sel,Copy_Number_sel,Copy_Number_Mean_sel), by = c("Well","Sample"))
} else{temp2_sel %>% left_join(select(temp2_bflo,Well,Sample,ΔΔCq_bflo,ΔΔCq_Mean_bflo,Copy_Number_bflo,Copy_Number_Mean_bflo), by = c("Well","Sample"))
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
