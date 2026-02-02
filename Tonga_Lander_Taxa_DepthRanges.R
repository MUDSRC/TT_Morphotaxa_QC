# Load packages
library(tidyverse)
library(dplyr)
library(readxl)
library(vegan)
library(ggplot2)

rm(list = ls())
cruise <- "2024_06_Tonga Trench"

data.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_06_Tonga Trench/Video Analysis/4_Data output products/EM Export_Lander") #Denise
metadata.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/#Video Analysis Documents") #Denise
export.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_06_Tonga Trench/Video Analysis/4_Data output products/TT_Morphotaxa_QC")

setwd(data.dir)

dat<-read.delim("ALL Point measurements.txt",header=T,sep = "\t", skip = 4)

#long format - create ID based on the Family, Genus, Species and comment columns
depcounts<-dat%>%
  dplyr::group_by(OpCode,Period,Family,Genus,X.8,Species,Number,Time..mins.,Comment,Comment.1)%>% 
  dplyr::mutate(Number=as.numeric(Number))%>%
  filter(Number != "NA")%>%#only include things with number 
  summarise(
    Count = sum(replace_na(Number, 1)),  # Run this summarise to also include things with number removed (e.g. forams)
    .groups = "drop"
  ) %>%
  #dplyr::summarise(Count=sum(Number))%>% #run this summarise line to only have things with numbers
  #filter(Period != "NA")%>% #only include things in period
  filter(Family !="")%>% #only include biological annotations
  unite("ID", Family:Species, sep= ".", #Created an 'ID' column to be able to join with higher taxonomic level obs 
        na.rm=TRUE,remove = FALSE)%>%
  mutate(
    ID = ID %>% 
      gsub("\\.{2,}", ".", .) %>%  # Replace two or more dots with a single dot
      gsub("\\.$", "", .)           # Remove any dot at the end of the ID
  )
head(depcounts)

# Bring in depth data
setwd(metadata.dir)
metadata <- read_excel("MASTER_Lander&Sub_Logsheet.xlsx",sheet=cruise)%>%
  rename(
    OpCode = opcode
  )

# Link deployment depth to EM data 
depcounts_env<- depcounts %>%
  left_join(metadata, by = c("OpCode"))

# Create min/max depth summary per ID 
depth_summary <- depcounts_env %>%
  group_by(ID) %>%
  summarise(
    Min_Depth = min(depth_m, na.rm = TRUE),
    Max_Depth = max(depth_m, na.rm = TRUE),
    frequency = n_distinct(OpCode)
  )


# Check unique IDs and in which OpCode they arew
depcounts_summary <- depcounts_env %>%
  select(-Period) %>%
  group_by(ID, OpCode) %>%
  summarise(Total_Count = sum(Count), .groups = "drop") %>%
  group_by(ID) %>%
  summarise(
    Total_Count = sum(Total_Count),
    OpCodes_List = paste(unique(OpCode), collapse = ", "),
    .groups = "drop"
  )

# Combine with depth ranges
op_summary <- depcounts_summary  %>%
  left_join(depth_summary, by = "ID")

####
setwd(export.dir)
write.csv(op_summary,"LanderTaxa_Depth_QC_20260202.csv",row.names = FALSE)
