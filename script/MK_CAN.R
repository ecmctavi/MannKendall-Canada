### Canada Streamflow and Baseflow MannKendall Trends ###
# Written by: Ethan McTavish

library(weathercan)
library(tidyhydat)
library(EcoHydRology)
library(Kendall)
library(tidyverse)
library(here)
library(rgdal)
library(leaflet)

# read in climate and hydrometric stations -----

stationsCANhy <- hy_stations(prov_terr_state_loc = c("AB","BC", "MB", "NB","NL", "NT", "NS","NU", "PE","QC", "SK", "YT", "ON"))

# Import monthly hydrometric data for Ontario -----

flow_CAN <- hy_monthly_flows(prov_terr_state_loc = c("AB","BC", "MB", "NB","NL", "NT", "NS","NU", "PE","QC", "SK", "YT", "ON"))

# add baseflow and quickflow -----

CAN_hy_clean <- flow_CAN %>%
  filter(Sum_stat == "MEAN") %>%
  select(-Date_occurred) %>%
  drop_na(c(Value))

CAN_bf_sep <- BaseflowSeparation(CAN_hy_clean$Value, filter_parameter = 0.925, passes = 3)

CAN_bf_bind <- cbind(CAN_hy_clean, CAN_bf_sep)

CAN_hy_flow <- rename(CAN_bf_bind, c(Streamflow = Value, Baseflow = bt, Quickflow = qft)) # a file with monthly streamflow, quickflow and baseflow

# adding MannKendall -----

prov_hy_flow <- cbind(CAN_hy_flow) # assign df to a universal var to not have to change anything from lines "MK streamflow month 1" to "MK streamflow month 12"

# function
# removes df with less then 12 years of data, can be set to desired value
remMK <- function(x) {
  if(nrow(x) < 12) {
    x <- NULL
  }
  return(x)
}

# MK streamflow month 1 -----
MK_SF_month_1 <- prov_hy_flow %>%
  filter(Month == "1") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_1 <- group_split(MK_SF_month_1, STATION_NUMBER)

MK_SF_split_1_apply <-lapply(MK_SF_split_1, remMK)

MK_SF_split_1_clean <- MK_SF_split_1_apply[lengths(MK_SF_split_1_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_1 <- list()

for (i in 1:length(MK_SF_split_1_clean)) {
  
  
  MK_SF_list_1[[i]] <- MannKendall(MK_SF_split_1_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_1 <- as.data.frame(do.call(rbind,  MK_SF_split_1_clean))

df_MK_SF_names_1 <- df_MK_SF_split_names_1[!duplicated(df_MK_SF_split_names_1$STATION_NUMBER),]

df_MK_SF_values_1 <- data.frame(STATION_NUMBER = df_MK_SF_names_1$STATION_NUMBER, Month = df_MK_SF_names_1$Month, matrix(unlist(MK_SF_list_1), nrow=length(MK_SF_list_1), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)

# MK streamflow month 2 -----
MK_SF_month_2 <- prov_hy_flow %>%
  filter(Month == "2") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_2 <- group_split(MK_SF_month_2, STATION_NUMBER)

MK_SF_split_2_apply <-lapply(MK_SF_split_2, remMK)

MK_SF_split_2_clean <- MK_SF_split_2_apply[lengths(MK_SF_split_2_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_2 <- list()

for (i in 1:length(MK_SF_split_2_clean)) {
  
  
  MK_SF_list_2[[i]] <- MannKendall(MK_SF_split_2_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_2 <- as.data.frame(do.call(rbind,  MK_SF_split_2_clean))

df_MK_SF_names_2 <- df_MK_SF_split_names_2[!duplicated(df_MK_SF_split_names_2$STATION_NUMBER),]

df_MK_SF_values_2 <- data.frame(STATION_NUMBER = df_MK_SF_names_2$STATION_NUMBER, Month = df_MK_SF_names_2$Month, 
                                matrix(unlist(MK_SF_list_2), nrow=length(MK_SF_list_2), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 3 -----
MK_SF_month_3 <- prov_hy_flow %>%
  filter(Month == "3") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_3 <- group_split(MK_SF_month_3, STATION_NUMBER)

MK_SF_split_3_apply <-lapply(MK_SF_split_3, remMK)

MK_SF_split_3_clean <- MK_SF_split_3_apply[lengths(MK_SF_split_3_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_3 <- list()

for (i in 1:length(MK_SF_split_3_clean)) {
  
  
  MK_SF_list_3[[i]] <- MannKendall(MK_SF_split_3_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_3 <- as.data.frame(do.call(rbind,  MK_SF_split_3_clean))

df_MK_SF_names_3 <- df_MK_SF_split_names_3[!duplicated(df_MK_SF_split_names_3$STATION_NUMBER),]

df_MK_SF_values_3 <- data.frame(STATION_NUMBER = df_MK_SF_names_3$STATION_NUMBER, Month = df_MK_SF_names_3$Month, 
                                matrix(unlist(MK_SF_list_3), nrow=length(MK_SF_list_3), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 4 -----
MK_SF_month_4 <- prov_hy_flow %>%
  filter(Month == "4") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_4 <- group_split(MK_SF_month_4, STATION_NUMBER)

MK_SF_split_4_apply <-lapply(MK_SF_split_4, remMK)

MK_SF_split_4_clean <- MK_SF_split_4_apply[lengths(MK_SF_split_4_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_4 <- list()

for (i in 1:length(MK_SF_split_4_clean)) {
  
  
  MK_SF_list_4[[i]] <- MannKendall(MK_SF_split_4_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_4 <- as.data.frame(do.call(rbind, MK_SF_split_4_clean))

df_MK_SF_names_4 <- df_MK_SF_split_names_4[!duplicated(df_MK_SF_split_names_4$STATION_NUMBER),]

df_MK_SF_values_4 <- data.frame(STATION_NUMBER = df_MK_SF_names_4$STATION_NUMBER, Month = df_MK_SF_names_4$Month, 
                                matrix(unlist(MK_SF_list_4), nrow=length(MK_SF_list_4), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 5 -----
MK_SF_month_5 <- prov_hy_flow %>%
  filter(Month == "5") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_5 <- group_split(MK_SF_month_5, STATION_NUMBER)

MK_SF_split_5_apply <-lapply(MK_SF_split_5, remMK)

MK_SF_split_5_clean <- MK_SF_split_5_apply[lengths(MK_SF_split_5_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_5 <- list()

for (i in 1:length(MK_SF_split_5_clean)) {
  
  
  MK_SF_list_5[[i]] <- MannKendall(MK_SF_split_5_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_5 <- as.data.frame(do.call(rbind,  MK_SF_split_5_clean))

df_MK_SF_names_5 <- df_MK_SF_split_names_5[!duplicated(df_MK_SF_split_names_5$STATION_NUMBER),]

df_MK_SF_values_5 <- data.frame(STATION_NUMBER = df_MK_SF_names_5$STATION_NUMBER, Month = df_MK_SF_names_5$Month, 
                                matrix(unlist(MK_SF_list_5), nrow=length(MK_SF_list_5), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 6 -----
MK_SF_month_6 <- prov_hy_flow %>%
  filter(Month == "6") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_6 <- group_split(MK_SF_month_6, STATION_NUMBER)

MK_SF_split_6_apply <-lapply(MK_SF_split_6, remMK)

MK_SF_split_6_clean <- MK_SF_split_6_apply[lengths(MK_SF_split_6_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_6 <- list()

for (i in 1:length(MK_SF_split_6_clean)) {
  
  
  MK_SF_list_6[[i]] <- MannKendall(MK_SF_split_6_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_6 <- as.data.frame(do.call(rbind,  MK_SF_split_6_clean))

df_MK_SF_names_6 <- df_MK_SF_split_names_6[!duplicated(df_MK_SF_split_names_6$STATION_NUMBER),]

df_MK_SF_values_6 <- data.frame(STATION_NUMBER = df_MK_SF_names_6$STATION_NUMBER, Month = df_MK_SF_names_6$Month, 
                                matrix(unlist(MK_SF_list_6), nrow=length(MK_SF_list_6), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 7 -----
MK_SF_month_7 <- prov_hy_flow %>%
  filter(Month == "7") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_7 <- group_split(MK_SF_month_7, STATION_NUMBER)

MK_SF_split_7_apply <-lapply(MK_SF_split_7, remMK)

MK_SF_split_7_clean <- MK_SF_split_7_apply[lengths(MK_SF_split_7_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_7 <- list()

for (i in 1:length(MK_SF_split_7_clean)) {
  
  
  MK_SF_list_7[[i]] <- MannKendall(MK_SF_split_7_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_7 <- as.data.frame(do.call(rbind,  MK_SF_split_7_clean))

df_MK_SF_names_7 <- df_MK_SF_split_names_7[!duplicated(df_MK_SF_split_names_7$STATION_NUMBER),]

df_MK_SF_values_7 <- data.frame(STATION_NUMBER = df_MK_SF_names_7$STATION_NUMBER, Month = df_MK_SF_names_7$Month, 
                                matrix(unlist(MK_SF_list_7), nrow=length(MK_SF_list_7), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 8 -----
MK_SF_month_8 <- prov_hy_flow %>%
  filter(Month == "8") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_8 <- group_split(MK_SF_month_8, STATION_NUMBER)

MK_SF_split_8_apply <-lapply(MK_SF_split_8, remMK)

MK_SF_split_8_clean <- MK_SF_split_8_apply[lengths(MK_SF_split_8_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_8 <- list()

for (i in 1:length(MK_SF_split_8_clean)) {
  
  
  MK_SF_list_8[[i]] <- MannKendall(MK_SF_split_8_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_8 <- as.data.frame(do.call(rbind,  MK_SF_split_8_clean))

df_MK_SF_names_8 <- df_MK_SF_split_names_8[!duplicated(df_MK_SF_split_names_8$STATION_NUMBER),]

df_MK_SF_values_8 <- data.frame(STATION_NUMBER = df_MK_SF_names_8$STATION_NUMBER, Month = df_MK_SF_names_8$Month, 
                                matrix(unlist(MK_SF_list_8), nrow=length(MK_SF_list_8), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 9 -----
MK_SF_month_9 <- prov_hy_flow %>%
  filter(Month == "9") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_9 <- group_split(MK_SF_month_9, STATION_NUMBER)

MK_SF_split_9_apply <-lapply(MK_SF_split_9, remMK)

MK_SF_split_9_clean <- MK_SF_split_9_apply[lengths(MK_SF_split_9_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_9 <- list()

for (i in 1:length(MK_SF_split_9_clean)) {
  
  
  MK_SF_list_9[[i]] <- MannKendall(MK_SF_split_9_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_9 <- as.data.frame(do.call(rbind,  MK_SF_split_9_clean))

df_MK_SF_names_9 <- df_MK_SF_split_names_9[!duplicated(df_MK_SF_split_names_9$STATION_NUMBER),]

df_MK_SF_values_9 <- data.frame(STATION_NUMBER = df_MK_SF_names_9$STATION_NUMBER, Month = df_MK_SF_names_9$Month, 
                                matrix(unlist(MK_SF_list_9), nrow=length(MK_SF_list_9), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 10 -----
MK_SF_month_10 <- prov_hy_flow %>%
  filter(Month == "10") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_10 <- group_split(MK_SF_month_10, STATION_NUMBER)

MK_SF_split_10_apply <-lapply(MK_SF_split_10, remMK)

MK_SF_split_10_clean <- MK_SF_split_10_apply[lengths(MK_SF_split_10_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_10 <- list()

for (i in 1:length(MK_SF_split_10_clean)) {
  
  
  MK_SF_list_10[[i]] <- MannKendall(MK_SF_split_10_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_10 <- as.data.frame(do.call(rbind,  MK_SF_split_10_clean))

df_MK_SF_names_10 <- df_MK_SF_split_names_10[!duplicated(df_MK_SF_split_names_10$STATION_NUMBER),]

df_MK_SF_values_10 <- data.frame(STATION_NUMBER = df_MK_SF_names_10$STATION_NUMBER, Month = df_MK_SF_names_10$Month, 
                                 matrix(unlist(MK_SF_list_10), nrow=length(MK_SF_list_10), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 11 -----
MK_SF_month_11 <- prov_hy_flow %>%
  filter(Month == "11") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_11 <- group_split(MK_SF_month_11, STATION_NUMBER)

MK_SF_split_11_apply <-lapply(MK_SF_split_11, remMK)

MK_SF_split_11_clean <- MK_SF_split_11_apply[lengths(MK_SF_split_11_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_11 <- list()

for (i in 1:length(MK_SF_split_11_clean)) {
  
  
  MK_SF_list_11[[i]] <- MannKendall(MK_SF_split_11_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_11 <- as.data.frame(do.call(rbind,  MK_SF_split_11_clean))

df_MK_SF_names_11<- df_MK_SF_split_names_11[!duplicated(df_MK_SF_split_names_11$STATION_NUMBER),]

df_MK_SF_values_11 <- data.frame(STATION_NUMBER = df_MK_SF_names_11$STATION_NUMBER, Month = df_MK_SF_names_11$Month, 
                                 matrix(unlist(MK_SF_list_11), nrow=length(MK_SF_list_11), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK streamflow month 12 -----
MK_SF_month_12 <- prov_hy_flow %>%
  filter(Month == "12") %>%
  select(STATION_NUMBER, Month, Streamflow)

MK_SF_split_12 <- group_split(MK_SF_month_12, STATION_NUMBER)

MK_SF_split_12_apply <-lapply(MK_SF_split_12, remMK)

MK_SF_split_12_clean <- MK_SF_split_12_apply[lengths(MK_SF_split_12_apply) != 0 ]


# MK test !!can make into a lapply
MK_SF_list_12 <- list()

for (i in 1:length(MK_SF_split_12_clean)) {
  
  
  MK_SF_list_12[[i]] <- MannKendall(MK_SF_split_12_clean[[i]]$Streamflow)
  
}

df_MK_SF_split_names_12<- as.data.frame(do.call(rbind,  MK_SF_split_12_clean))

df_MK_SF_names_12 <- df_MK_SF_split_names_12[!duplicated(df_MK_SF_split_names_12$STATION_NUMBER),]

df_MK_SF_values_12 <- data.frame(STATION_NUMBER = df_MK_SF_names_12$STATION_NUMBER, Month = df_MK_SF_names_12$Month, 
                                 matrix(unlist(MK_SF_list_12), nrow=length(MK_SF_list_12), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)

# MK streamflow month 1-12 combined -----

CAN_MK_SF_values_all <- rbind(df_MK_SF_values_1, df_MK_SF_values_2, df_MK_SF_values_3, df_MK_SF_values_4, df_MK_SF_values_5, df_MK_SF_values_6,
                              df_MK_SF_values_7, df_MK_SF_values_8, df_MK_SF_values_9, df_MK_SF_values_10, df_MK_SF_values_11, df_MK_SF_values_12)
CAN_MK_SF_coord <- merge(CAN_MK_SF_values_all, stationsCANhy, by = "STATION_NUMBER")

# shinyR prep Streamflow-----

# determines if there are trends present based on p-values and tau
CAN_MK_SF_coord$Trend <- ifelse(CAN_MK_SF_coord$sl < 0.05 & CAN_MK_SF_coord$tau > 0, "Increasing",
                                ifelse(CAN_MK_SF_coord$sl < 0.05 & CAN_MK_SF_coord$tau < 0, "Decreasing",
                                       ifelse(CAN_MK_SF_coord$sl > 0,"No_trend",
                                              ifelse("NA")))) # adds trend to df

CAN_MK_SF_coord <- CAN_MK_SF_coord %>% arrange(Month)


saveRDS(CAN_MK_SF_coord, "CAN_Trends_Map/data/CAN_MK_Trends_SF.RDS")





# MK baseflow month 1 -----
MK_BF_month_1 <- prov_hy_flow %>%
  filter(Month == "1") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_1 <- group_split(MK_BF_month_1, STATION_NUMBER)

MK_BF_split_1_apply <-lapply(MK_BF_split_1, remMK)

MK_BF_split_1_clean <- MK_BF_split_1_apply[lengths(MK_BF_split_1_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_1 <- list()

for (i in 1:length(MK_BF_split_1_clean)) {
  
  
  MK_BF_list_1[[i]] <- MannKendall(MK_BF_split_1_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_1 <- as.data.frame(do.call(rbind,  MK_BF_split_1_clean))

df_MK_BF_names_1 <- df_MK_BF_split_names_1[!duplicated(df_MK_BF_split_names_1$STATION_NUMBER),]

df_MK_BF_values_1 <- data.frame(STATION_NUMBER = df_MK_BF_names_1$STATION_NUMBER, Month = df_MK_BF_names_1$Month, matrix(unlist(MK_BF_list_1), nrow=length(MK_BF_list_1), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)

# MK baseflow month 2 -----
MK_BF_month_2 <- prov_hy_flow %>%
  filter(Month == "2") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_2 <- group_split(MK_BF_month_2, STATION_NUMBER)

MK_BF_split_2_apply <-lapply(MK_BF_split_2, remMK)

MK_BF_split_2_clean <- MK_BF_split_2_apply[lengths(MK_BF_split_2_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_2 <- list()

for (i in 1:length(MK_BF_split_2_clean)) {
  
  
  MK_BF_list_2[[i]] <- MannKendall(MK_BF_split_2_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_2 <- as.data.frame(do.call(rbind,  MK_BF_split_2_clean))

df_MK_BF_names_2 <- df_MK_BF_split_names_2[!duplicated(df_MK_BF_split_names_2$STATION_NUMBER),]

df_MK_BF_values_2 <- data.frame(STATION_NUMBER = df_MK_BF_names_2$STATION_NUMBER, Month = df_MK_BF_names_2$Month, 
                                matrix(unlist(MK_BF_list_2), nrow=length(MK_BF_list_2), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 3 -----
MK_BF_month_3 <- prov_hy_flow %>%
  filter(Month == "3") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_3 <- group_split(MK_BF_month_3, STATION_NUMBER)

MK_BF_split_3_apply <-lapply(MK_BF_split_3, remMK)

MK_BF_split_3_clean <- MK_BF_split_3_apply[lengths(MK_BF_split_3_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_3 <- list()

for (i in 1:length(MK_BF_split_3_clean)) {
  
  
  MK_BF_list_3[[i]] <- MannKendall(MK_BF_split_3_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_3 <- as.data.frame(do.call(rbind,  MK_BF_split_3_clean))

df_MK_BF_names_3 <- df_MK_BF_split_names_3[!duplicated(df_MK_BF_split_names_3$STATION_NUMBER),]

df_MK_BF_values_3 <- data.frame(STATION_NUMBER = df_MK_BF_names_3$STATION_NUMBER, Month = df_MK_BF_names_3$Month, 
                                matrix(unlist(MK_BF_list_3), nrow=length(MK_BF_list_3), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 4 -----
MK_BF_month_4 <- prov_hy_flow %>%
  filter(Month == "4") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_4 <- group_split(MK_BF_month_4, STATION_NUMBER)

MK_BF_split_4_apply <-lapply(MK_BF_split_4, remMK)

MK_BF_split_4_clean <- MK_BF_split_4_apply[lengths(MK_BF_split_4_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_4 <- list()

for (i in 1:length(MK_BF_split_4_clean)) {
  
  
  MK_BF_list_4[[i]] <- MannKendall(MK_BF_split_4_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_4 <- as.data.frame(do.call(rbind, MK_BF_split_4_clean))

df_MK_BF_names_4 <- df_MK_BF_split_names_4[!duplicated(df_MK_BF_split_names_4$STATION_NUMBER),]

df_MK_BF_values_4 <- data.frame(STATION_NUMBER = df_MK_BF_names_4$STATION_NUMBER, Month = df_MK_BF_names_4$Month, 
                                matrix(unlist(MK_SF_list_4), nrow=length(MK_BF_list_4), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 5 -----
MK_BF_month_5 <- prov_hy_flow %>%
  filter(Month == "5") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_5 <- group_split(MK_BF_month_5, STATION_NUMBER)

MK_BF_split_5_apply <-lapply(MK_BF_split_5, remMK)

MK_BF_split_5_clean <- MK_BF_split_5_apply[lengths(MK_BF_split_5_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_5 <- list()

for (i in 1:length(MK_BF_split_5_clean)) {
  
  
  MK_BF_list_5[[i]] <- MannKendall(MK_BF_split_5_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_5 <- as.data.frame(do.call(rbind,  MK_BF_split_5_clean))

df_MK_BF_names_5 <- df_MK_BF_split_names_5[!duplicated(df_MK_BF_split_names_5$STATION_NUMBER),]

df_MK_BF_values_5 <- data.frame(STATION_NUMBER = df_MK_BF_names_5$STATION_NUMBER, Month = df_MK_BF_names_5$Month, 
                                matrix(unlist(MK_BF_list_5), nrow=length(MK_BF_list_5), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 6 -----
MK_BF_month_6 <- prov_hy_flow %>%
  filter(Month == "6") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_6 <- group_split(MK_BF_month_6, STATION_NUMBER)

MK_BF_split_6_apply <-lapply(MK_BF_split_6, remMK)

MK_BF_split_6_clean <- MK_BF_split_6_apply[lengths(MK_BF_split_6_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_6 <- list()

for (i in 1:length(MK_BF_split_6_clean)) {
  
  
  MK_BF_list_6[[i]] <- MannKendall(MK_BF_split_6_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_6 <- as.data.frame(do.call(rbind,  MK_BF_split_6_clean))

df_MK_BF_names_6 <- df_MK_BF_split_names_6[!duplicated(df_MK_BF_split_names_6$STATION_NUMBER),]

df_MK_BF_values_6 <- data.frame(STATION_NUMBER = df_MK_BF_names_6$STATION_NUMBER, Month = df_MK_BF_names_6$Month, 
                                matrix(unlist(MK_BF_list_6), nrow=length(MK_BF_list_6), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 7 -----
MK_BF_month_7 <- prov_hy_flow %>%
  filter(Month == "7") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_7 <- group_split(MK_BF_month_7, STATION_NUMBER)

MK_BF_split_7_apply <-lapply(MK_BF_split_7, remMK)

MK_BF_split_7_clean <- MK_BF_split_7_apply[lengths(MK_BF_split_7_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_7 <- list()

for (i in 1:length(MK_BF_split_7_clean)) {
  
  
  MK_BF_list_7[[i]] <- MannKendall(MK_BF_split_7_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_7 <- as.data.frame(do.call(rbind,  MK_BF_split_7_clean))

df_MK_BF_names_7 <- df_MK_BF_split_names_7[!duplicated(df_MK_BF_split_names_7$STATION_NUMBER),]

df_MK_BF_values_7 <- data.frame(STATION_NUMBER = df_MK_BF_names_7$STATION_NUMBER, Month = df_MK_BF_names_7$Month, 
                                matrix(unlist(MK_BF_list_7), nrow=length(MK_BF_list_7), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 8 -----
MK_BF_month_8 <- prov_hy_flow %>%
  filter(Month == "8") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_8 <- group_split(MK_BF_month_8, STATION_NUMBER)

MK_BF_split_8_apply <-lapply(MK_BF_split_8, remMK)

MK_BF_split_8_clean <- MK_BF_split_8_apply[lengths(MK_BF_split_8_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_8 <- list()

for (i in 1:length(MK_BF_split_8_clean)) {
  
  
  MK_BF_list_8[[i]] <- MannKendall(MK_BF_split_8_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_8 <- as.data.frame(do.call(rbind,  MK_BF_split_8_clean))

df_MK_BF_names_8 <- df_MK_BF_split_names_8[!duplicated(df_MK_BF_split_names_8$STATION_NUMBER),]

df_MK_BF_values_8 <- data.frame(STATION_NUMBER = df_MK_BF_names_8$STATION_NUMBER, Month = df_MK_BF_names_8$Month, 
                                matrix(unlist(MK_BF_list_8), nrow=length(MK_BF_list_8), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 9 -----
MK_BF_month_9 <- prov_hy_flow %>%
  filter(Month == "9") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_9 <- group_split(MK_BF_month_9, STATION_NUMBER)

MK_BF_split_9_apply <-lapply(MK_BF_split_9, remMK)

MK_BF_split_9_clean <- MK_BF_split_9_apply[lengths(MK_BF_split_9_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_9 <- list()

for (i in 1:length(MK_BF_split_9_clean)) {
  
  
  MK_BF_list_9[[i]] <- MannKendall(MK_BF_split_9_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_9 <- as.data.frame(do.call(rbind,  MK_BF_split_9_clean))

df_MK_BF_names_9 <- df_MK_BF_split_names_9[!duplicated(df_MK_BF_split_names_9$STATION_NUMBER),]

df_MK_BF_values_9 <- data.frame(STATION_NUMBER = df_MK_BF_names_9$STATION_NUMBER, Month = df_MK_BF_names_9$Month, 
                                matrix(unlist(MK_BF_list_9), nrow=length(MK_BF_list_9), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 10 -----
MK_BF_month_10 <- prov_hy_flow %>%
  filter(Month == "10") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_10 <- group_split(MK_BF_month_10, STATION_NUMBER)

MK_BF_split_10_apply <-lapply(MK_BF_split_10, remMK)

MK_BF_split_10_clean <- MK_BF_split_10_apply[lengths(MK_BF_split_10_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_10 <- list()

for (i in 1:length(MK_BF_split_10_clean)) {
  
  
  MK_BF_list_10[[i]] <- MannKendall(MK_BF_split_10_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_10 <- as.data.frame(do.call(rbind,  MK_BF_split_10_clean))

df_MK_BF_names_10 <- df_MK_BF_split_names_10[!duplicated(df_MK_BF_split_names_10$STATION_NUMBER),]

df_MK_BF_values_10 <- data.frame(STATION_NUMBER = df_MK_BF_names_10$STATION_NUMBER, Month = df_MK_BF_names_10$Month, 
                                 matrix(unlist(MK_BF_list_10), nrow=length(MK_BF_list_10), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 11 -----
MK_BF_month_11 <- prov_hy_flow %>%
  filter(Month == "11") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_11 <- group_split(MK_BF_month_11, STATION_NUMBER)

MK_BF_split_11_apply <-lapply(MK_BF_split_11, remMK)

MK_BF_split_11_clean <- MK_BF_split_11_apply[lengths(MK_BF_split_11_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_11 <- list()

for (i in 1:length(MK_BF_split_11_clean)) {
  
  
  MK_BF_list_11[[i]] <- MannKendall(MK_BF_split_11_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_11 <- as.data.frame(do.call(rbind,  MK_BF_split_11_clean))

df_MK_BF_names_11<- df_MK_BF_split_names_11[!duplicated(df_MK_BF_split_names_11$STATION_NUMBER),]

df_MK_BF_values_11 <- data.frame(STATION_NUMBER = df_MK_BF_names_11$STATION_NUMBER, Month = df_MK_BF_names_11$Month, 
                                 matrix(unlist(MK_BF_list_11), nrow=length(MK_BF_list_11), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)


# MK baseflow month 12 -----
MK_BF_month_12 <- prov_hy_flow %>%
  filter(Month == "12") %>%
  select(STATION_NUMBER, Month, Baseflow)

MK_BF_split_12 <- group_split(MK_BF_month_12, STATION_NUMBER)

MK_BF_split_12_apply <-lapply(MK_BF_split_12, remMK)

MK_BF_split_12_clean <- MK_BF_split_12_apply[lengths(MK_BF_split_12_apply) != 0 ]


# MK test !!can make into a lapply
MK_BF_list_12 <- list()

for (i in 1:length(MK_BF_split_12_clean)) {
  
  
  MK_BF_list_12[[i]] <- MannKendall(MK_BF_split_12_clean[[i]]$Baseflow)
  
}

df_MK_BF_split_names_12<- as.data.frame(do.call(rbind,  MK_BF_split_12_clean))

df_MK_BF_names_12 <- df_MK_BF_split_names_12[!duplicated(df_MK_BF_split_names_12$STATION_NUMBER),]

df_MK_BF_values_12 <- data.frame(STATION_NUMBER = df_MK_BF_names_12$STATION_NUMBER, Month = df_MK_BF_names_12$Month, 
                                 matrix(unlist(MK_BF_list_12), nrow=length(MK_BF_list_12), byrow=TRUE)) %>%
  rename(tau = X1, sl = X2 , S = X3, D = X4, varS = X5)

# MK baseflow month 1-12 combined -----

CAN_MK_BF_values_all <- rbind(df_MK_BF_values_1, df_MK_BF_values_2, df_MK_BF_values_3, df_MK_BF_values_4, df_MK_BF_values_5, df_MK_BF_values_6,
                              df_MK_BF_values_7, df_MK_BF_values_8, df_MK_BF_values_9, df_MK_BF_values_10, df_MK_BF_values_11, df_MK_BF_values_12)

CAN_MK_BF_coord <- merge(CAN_MK_BF_values_all, stationsCANhy, by = "STATION_NUMBER")



# shinyR prep Baseflow -----

# determines if there are trends present based on p-values and tau
CAN_MK_BF_coord$Trend <- ifelse(CAN_MK_BF_coord$sl < 0.05 & CAN_MK_BF_coord$tau > 0, "Increasing",
                                ifelse(CAN_MK_BF_coord$sl < 0.05 & CAN_MK_BF_coord$tau < 0, "Decreasing",
                                       ifelse(CAN_MK_BF_coord$sl > 0,"No_trend",
                                              ifelse("NA")))) # adds trend to df

CAN_MK_BF_coord <- CAN_MK_BF_coord %>% arrange(Month)

saveRDS(CAN_MK_BF_coord, "CAN_Trends_Map/data/CAN_MK_Trends_BF.RDS")



