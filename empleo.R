#library ####
library(redatam)
library(openxlsx)
library(Rcpp)
library(dplyr)
library(tidyverse)
library(RcppProgress)
library(pivottabler)

library(MAGRITTR)
library(pivottabler) # for pivot tables

# Data ####

dic_org12 <-redatam.open("d:/eechoque/Documents/Edison_INE/Redatam/Datita/2012/BASEORG/BaseMunicipio_V3/cpv2012municipio.dicX")
dic_geo12 <-redatam.open("d:/eechoque/Documents/Edison_INE/Redatam/Datita/2012/BASEGEO24/baser/cen2012-g24.dicx")

dic_geo01 <-redatam.open("d:/eechoque/Documents/Edison_INE/Redatam/Datita/2001/BASEGEO24/baser/cen2001.dicx")
dic_org01 <-redatam.open("d:/eechoque/Documents/Edison_INE/Redatam/Datita/2001/BASEORG/BaseOriginal/cpv2001.dicx")

# read variable and entities
redatam.entities(dic_org12)
redatam.variables(dic_org12,"persona")
redatam.variables(dic_org12,"munic")

redatam.entities(dic_geo12)
redatam.variables(dic_geo12,"persona")
redatam.variables(dic_geo12,"depto") 

redatam.entities(dic_org01)
redatam.variables(dic_org01, "persona")
redatam.variables(dic_org01, "depto") # sin munic 

redatam.entities(dic_geo01)
redatam.variables(dic_geo01, "persona")
redatam.variables(dic_geo01, "munic")

# 1. GRUPO OCU. ####
# INE geo 2012
redatam.query(dic_geo12, "t_go12 <- freq persona.P42 by munic.redcoden") # sin label P42B_OCUPCOD
t_go12 <- bind_rows(t_go12, data.frame(value = sum(t_go12$value)))

# INE geo 2001
redatam.query(dic_geo01, "t_go01 <- freq persona.p45cod by munic.nmunic") # sin label p45cod, no hay alternativa como geo 2012
t_go01 <- bind_rows(t_go01, data.frame(value = sum(t_go01$value)))

# 2. SIT. EMPLEO ####
# INE geo 2012
redatam.query(dic_geo12, "t_se12 <- freq persona.p43 by munic.redcoden") # tambien hay p43_categor
t_se12 <- bind_rows(t_se12, data.frame(value = sum(t_se12$value)))

# INE geo 2001
redatam.query(dic_geo01, "t_se01 <- freq persona.p46 by munic.nmunic") # recoden value para munic no existe
t_se01 <- bind_rows(t_se01, data.frame(value = sum(t_se01$value))) 

# 3. ACT. ECO. ####
# INE geo 2012
redatam.query(dic_geo12, "t_ae12 <- freq persona.p44 by munic.redcoden") # p44b_rama sin label, tiene mas filas que p44 porque este ultimo es a1 dig.
                                                                        # P44B1_LETRACOD sin label 
t_ae12 <- bind_rows(t_ae12, data.frame(value = sum(t_ae12$value)))

# INE geo 2001
redatam.query(dic_geo01, "t_ae01 <- freq persona.p47cod by munic.redcoden") # recoden value para munic no existe
                                                                        # sin label p47cod      
t_ae01 <- bind_rows(t_ae01, data.frame(value = sum(t_ae01$value)))


#4. PET PEA PEI ####
# INE geo 2012
redatam.query(dic_geo12, "t_pet12 <- freq persona.pet2 by depto.ndepto")
t_pet12 <- bind_rows(t_pet12, data.frame(value = sum(t_pet12$value)))

redatam.query(dic_geo12, "t_pea12 <- freq persona.pea2 by munic.redcoden")
t_pea12 <- bind_rows(t_pea12, data.frame(value = sum(t_pea12$value)))

redatam.query(dic_geo12, "t_pei12 <- freq persona.pei2 by munic.redcoden")
t_pei12 <- bind_rows(t_pei12, data.frame(value = sum(t_pei12$value)))


# INE geo 2001

redatam.query(dic_geo01, "t_pet01 <- freq persona.pet by munic.nmunic")
t_pet01 <- bind_rows(t_pet01, data.frame(value = sum(t_pet01$value)))

redatam.query(dic_geo01, "t_pea01 <- freq persona.pea by munic.nmunic")
t_pea01 <- bind_rows(t_pea01, data.frame(value = sum(t_pea01$value)))

redatam.query(dic_geo01, "t_pei01 <- freq persona.pei by munic.nmunic")
t_pei01 <- bind_rows(t_pei01, data.frame(value = sum(t_pei01$value)))

# Uniendo tablas 
t_pea12 <- t_pea12 %>% arrange(REDCODEN2_label)
t_pea01 <- t_pea01 %>% arrange(NMUNIC2_label)

all_pea <- cbind(t_pea01, t_pea12[5])
new_colnames <- c( value = "2001", value.1="2012")
colnames(all_pea)[names(all_pea) %in% names(new_colnames)] <- new_colnames

# 5. OCU POR GO ####

redatam.query(dic_geo12, "t_ocu_go <- freq persona.p42 by munic.redcoden")

redatam.query(dic_geo12, "t_ocu_go <- freq persona.p42")
total_value <- sum(t_ocu_go$value)
t_ocu_go <- t_ocu_go %>%
  mutate(percentage = (value / total_value) * 100)

# 6. OCU por SE ####

redatam.query(dic_geo12, "t_ocu_se <- freq persona.p43 by munic.redcoden")
arrange(t_ocu_se)
t_ocu_se <- t_ocu_se %>%
  group_by(REDCODEN2_value) %>%
  mutate(subtotal = sum(value)) %>%
  ungroup()

t_ocu_se <- t_ocu_se %>%
  mutate(percentage = round((value / subtotal) * 100, 2))

# 7. OCU por AE ####

redatam.query(dic_geo12, "t_ocu_ae <- freq persona.p44 by munic.redcoden")
arrange(t_ocu_ae)
t_ocu_ae <- t_ocu_ae %>%
  group_by(REDCODEN2_value) %>%
  mutate(subtotal = sum(value)) %>%
  ungroup()

t_ocu_ae <- t_ocu_ae %>%
  mutate(percentage = round((value / subtotal) * 100, 2))

# 8. OCU por ML ####

# geo 2024 
redatam.query(dic_geo12, "t_ocu_ml <- freq persona.p52 by munic.redcoden")
arrange(t_ocu_ml)
t_ocu_ml <- t_ocu_ml %>%
  group_by(REDCODEN2_value) %>%
  mutate(subtotal = sum(value)) %>%
  ungroup()

t_ocu_ml <- t_ocu_ml %>%
  mutate(percentage = round((value / subtotal) * 100, 2))


# 9. ICE ####

redatam.query(dic_geo12, "t_auxpea <- freq persona.pea3 
              t_auxpei <- freq persona.pei3
              t_area <- arealist of munic munic.nmunic, t_auxpea, t_auxpei")
t_area <- t_area %>% 
    mutate(ICE = round((pei3_2_1/pea3_1_1)*100,2))

# 10. TBP ####

redatam.query(dic_geo12, "t_auxpea <- freq persona.pea3 
              t_auxpet <- freq persona.pet2
              t_area <- arealist of munic munic.nmunic, t_auxpea, t_auxpet")
t_area <- t_area %>% 
  mutate(totpob = pet2_2_1 + pet2_3_2)

t_area <- t_area %>% 
  mutate(TPB = round((pea3_1_1/totpob)*100,2))

t_area <- bind_rows(t_area, data.frame(totpob = sum(t_area$totpob))) # Para check de totpob, si cumple :)


# 11. TGP ####

redatam.query(dic_geo12, "t_auxpea <- freq persona.pea3 
              t_auxpet <- freq persona.pet2
              t_area <- arealist of munic munic.nmunic, t_auxpea, t_auxpet")

t_area <- t_area %>% 
  mutate(TGP = round((pea3_1_1/pet2_2_1)*100,2))


# 12. TO ####

redatam.query(dic_geo12, "t_auxocu <- freq persona.ocu 
              t_auxpet <- freq persona.pet2
              t_area <- arealist of munic munic.nmunic, t_auxocu, t_auxpet")

t_area <- t_area %>% 
  mutate(TO = round((ocu_1_1/pet2_2_1)*100,2))

# 13. TOP ####

redatam.query(dic_geo12, "t_auxpet <- freq persona.pet2
              t_area <- arealist of munic munic.nmunic, t_auxpet")

t_area <- t_area %>% 
  mutate(totpob = pet2_1_1 + pet2_2_2)

t_area <- t_area %>% 
  mutate(TOP = round((pet2_1_1/totpob)*100,2))


# 14. ID ####

redatam.query(dic_geo12, "t_auxpet <- freq persona.pet2
              t_auxocu <- freq persona.ocu
              t_area <- arealist of munic munic.nmunic, t_auxpet, t_auxocu")

t_area <- t_area %>% 
  mutate(totpob = pet2_1_1 + pet2_2_2)

t_area <- t_area %>% 
  mutate(ID = round(((totpob - ocu_3_1)/ocu_3_1)*100,2))

# Org vs. geo ####
#2012
redatam.query(dic_org12, "t_org12 <- freq persona.p44 by munic.redcoden") # CAEB
redatam.query(dic_geo12, "t_geo12 <- freq persona.p44 by munic.redcoden") 

redatam.query(dic_org12, "t_org12 <- freq persona.p42 by munic.redcoden") # COB
redatam.query(dic_geo12, "t_geo12 <- freq persona.p42 by munic.redcoden")

#2001
redatam.query(dic_org01, "t_org01 <- freq persona.ocup1dig by seccion.idseccio") #caeb
redatam.query(dic_geo01, "t_geo01 <- freq persona.p45cod by munic.nmunic") # sin label ni nombre de p45cod

redatam.query(dic_org01, "t_org01 <- freq persona.activ2dig by seccion.idseccio") #cob
redatam.query(dic_geo01, "t_geo01 <- freq persona.p47cod by munic.nmunic")  # sin label ni nombre de p47cod

# Auxiliares ####

redatam.query(dic_geo12, "t1 <-frequency munic.redcoden")
redatam.query(dic_geo01, "t1 <-frequency munic.nmunic")
qpvt(t1, rows = NULL, columns = NULL, "sum(value)")


names(t1)
unique(t1$P433_label)

# Exploration codes ####

# labels en las bases 
redatam.query(dic_geo01, "t_go01 <- freq persona.p45cod") # sin label p45cod, no hay alternativa como geo 2012
redatam.query(dic_org01, "t_go01 <- freq persona.ACTIV2DIG") # sin label p45cod, no hay alternativa como geo 2012

redatam.query(dic_org01, "t_go01 <- freq depto.I02_DEPTO")

redatam.query(dic_geo12, "t_aux <- freq persona.p36_asiste by munic.redcoden")

# Area list
redatam.query(dic_geo01, "t_pet12 <- freq persona.p42 
              t_pea12 <- freq persona.pea2
              t_pei12 <- freq persona.pei2
              t_area <- arealist of munic munic.redcoden, t_pet12, t_pea12, t_pei12")

redatam.query(dic_geo01, "t_aux <- freq persona.p48 
              t_area <- arealist of munic munic.nmunic, t_aux ")

t_area <- t_area %>%
  rowwise() %>%
  mutate(Total = sum(c_across(pet2_1_1:pei2_9_4))) %>%
  ungroup()

t_col<- t_area %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
t_area <- bind_rows(t_area, t_col)

redatam.query(dic_geo12, "t_pet12 <- freq persona.pet2 by sexo")


arrange(t_ocu_ml)
t_ocu_ml <- t_ocu_ml %>%
  group_by(REDCODEN2_value) %>%
  mutate(subtotal = sum(value)) %>%
  ungroup()

t_ocu_ml <- t_ocu_ml %>%
  mutate(percentage = round((value / subtotal) * 100, 2))

# Totals
t2_cross<-as.data.frame.matrix(xtabs(t2$value~t2$elabel+t2$slabel))
t2_cross<-cbind(t2_cross, Total=rowSums(t2_cross))

# generate the pivot table
qpvt( t3, rows = c("slabel","elabel"), columns = "alabel", "sum(value)")

# If you want to render the pivot table as a html-widgets, use
# qhpvt( t3, rows = c("slabel","elabel"), columns = "alabel", "sum(value)" )


redatam.query(dic, "t <-freq person.sexo
  t2<-arealist comuna comuna.ncomuna, t")
colnames(t2)<-c("Codigo","Nombre","Hombres","Mujeres")

redatam.query(dic, "
              t2<- freq depto.redcoden
              by persona.p24
              by persona.p43
              TOT.OMIT = FALSE) %>%
              FILTER(PROVNOMB1_LABEL !=  "__TOT__",
             EDADQUIN2_LABEL !=  "__TOT__",
              RP17TRAB3_LABEL !=  "__TOT__")                                                                                  
              "
)

t_pared_piso_techo <- cbind(t_pared, t_piso[3:5], t_techo[3:5])
new_colnames <- c(value = "value_pared", value.1 = "value_piso", value.2="value_techo")
colnames(t_pared_piso_techo)[names(t_pared_piso_techo) %in% names(new_colnames)] <- new_colnames

# Comparacion censos ####
 redatam.query(dic1, "t01 <- freq depto.redcoden by persona.p28")
 redatam.query(dic, "t12 <- freq depto.redcoden by persona.p24")
 
t_01_12 <- cbind(t01, t12[3:5])
new_colnames <- c(value = "value_2001", value.1 = "value_2012")
colnames(t_01_12)[names(t_01_12) %in% names(new_colnames)] <- new_colnames

t_01_12$con %>% transmute(con=value_2001-value_2012) # promete 

#check
qpvt(t_pared_piso_techo, rows = NULL, columns = NULL, "sum(value_pared)")
qpvt(t_pared_piso_techo, rows = NULL, columns = NULL, "sum(value_piso)")
qpvt(t_pared_piso_techo, rows = NULL, columns = NULL, "sum(value_techo)")

t_irrec <- t_pared_piso_techo  %>% 
  transmute(depto=REDCODEN1_value,
            irrec=ifelse(
              DEPARED2_value %in% 3 & DEPISO2_value %in% 3 ,1,0),
            value
  ) %>% 
  group_by(depto, irrec) %>% summarise(value=sum(value)) %>% as.data.frame()


# Codes Redatam####
// Tabla de verificación de resultados
AREALIST DEPTO, DEPTO.NDEPTO,
DEPTO.PERCSA, DEPTO.PERCSB, DEPTO.PERCSC, DEPTO.PERCSD, DEPTO.PERCSE,
DEPTO.PERCSF, DEPTO.PERCSG
DECIMALS 1

TABLE VERIF AS AREALIST DEPTO, DEPTO.NDEPTO, DEPTO.PERCSA, DEPTO.PERCSB  TOTAL
TABLE VERIF AS AREALIST OF DEPTO, DEPTO.NDEPTO, DEPTO.D1,
DEPTO.D2, DEPTO.D3, DEPTO.D4, DEPTO.D5,DEPTO.DM, DEPTO.D 
TOTAL

TABLE VERIF AS AREALIST DEPTO, DEPTO.NDEPTO, DEPTO.A, DEPTO.B, DEPTO.C, 
DEPTO.D TOTAL 

# Export ####
write.xlsx(t_area, file = "d:/eechoque/Documents/Edison_INE/Datita_R/Out/area1.xlsx", rowNames = FALSE)

wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data",
                         topRowNumber=1, leftMostColumnNumber=1,
                         applyStyles=TRUE)
saveWorkbook(wb, file="C:/BASES/test.xlsx", overwrite = TRUE)

#close dic ####
redatam.close(dic)

