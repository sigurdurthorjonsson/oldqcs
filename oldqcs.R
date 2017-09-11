## Chunk 1: restore dump of Splus versions upto and including 1999 assessments

library(foreign)

setwd("qcs-dump")
data.restore("dumpdataOldStyle")
## 63 stock lists restored, assume it was into an empty workspace

stock_names <- ls()

## Chunk 2: read in 'cod on the Icelandic grounds' from an xls 
## created from an qcs-doc from ~2003 as it was missing the last time around

library(readxl)

sheetNames<- c("SSB", "Recruitment", "FSB", "Fmor")
codiceg <- lapply(sheetNames, 
  function(x) data <- read_excel("cod-iceg-qcs.xlsx", sheet = x))
names(codiceg) <- sheetNames
codiceg <- lapply(codiceg, function(x) {
  dimNames <- list(unlist(x[,1]), names(x)[-1])
  x <- as.matrix(x[ , -1])
  dimnames(x) <- dimNames
  x
})

# Chunk 3: some extra preamble

# get recruitment age from  a text file since age field in dumped objects 
# was incomplete 
Rage <- read.table("stockRage.txt", header = TRUE, as.is = TRUE)
# omit dash from stock-acronyms, R-objects with dashes in name are problematic
Rage$stock <- paste0(substring(Rage$stock, 1, 3), substring(Rage$stock, 5))

# only collect the stocks that are in both
stock_names <- intersect(stock_names, Rage$stock)
# and also add the Icelandic cod
stock_names <- sort(c(stock_names, "codiceg"))
# and also drop Sole in the Irish Sea since they only run on warning
#id <- which(stock_names == "soliris")
#stock_names <- stock_names[-id]
# after adply-processing keep a data frame with new names 
new_stock_object_names <- paste(stock_names, "df", sep = "_")

# helper / utility to convert factor to numeric
factor2numeric <- function(x) as.numeric(levels(x))[x]

# Chunk 4: the change from  lower diagonal matrices to a dataframe 
# similar to the ICES Stoc Assessment Summaries sheet

library(plyr)

for(i in seq(along = stock_names)) {
  print(stock_names[i])
  tmp <- get(stock_names[i])
  
## quick fix to get rid of double entry in Fmor(hernoss)
  if(stock_names[i] == "hernoss") tmp$Fmor <- tmp$Fmor[-8,]
  
  Fmor <- adply(tmp$Fmor, .margins = c(1,2))
  names(Fmor) <- c("AssYear", "Year", "F")
## as the dimnames are factors in the dataframe-output of 'adply'
  Fmor$AssYear <- factor2numeric(Fmor$AssYear)
  Fmor$Year <- factor2numeric(Fmor$Year)
  
## as we don't have SSB(codiceg):  
  if(stock_names[i] == "codiceg") tmp$SSB <- tmp$FSB
  
  SSB <- adply(tmp$SSB, .margins = c(1,2))
  names(SSB) <- c("AssYear", "Year", "SSB")
  SSB$AssYear <- factor2numeric(SSB$AssYear)
  SSB$Year <- factor2numeric(SSB$Year)
  
## quick fix to get rid of double entry in Recr(soliris)
  if(stock_names[i] == "soliris") tmp$Recr <- tmp$Recr[-7,]
  
  Recruitment <- adply(tmp$Recr, .margins = c(1,2))
  names(Recruitment) <- c("AssYear", "Yearclass", "Recruitment")
  Recruitment$AssYear <- factor2numeric(Recruitment$AssYear)
  Recruitment$Yearclass <- factor2numeric(Recruitment$Yearclass)
  Recruitment$Rage <- rep(Rage$Rage[Rage$stock == stock_names[i]], nrow(Recruitment))
  Recruitment$Year <- Recruitment$Yearclass + Recruitment$Rage
  
  tmp <- join_all(list(a = Fmor, b = SSB, c = Recruitment), 
    by = c("AssYear", "Year"), type = "full")
  tmp$FishStock <- rep(stock_names[i], nrow(tmp))
  assign(new_stock_object_names[i], tmp)
}

## runs with warnings for some stocks, but looks OK

c <- do.call("rbind", lapply(new_stock_object_names, get))
id <- which(!sapply(1:nrow(c), function(x) 
  all(is.na(c$F[x]), is.na(c$Recruitment[x]), is.na(c$SSB[x]))))
c <- c[id, ]

library(tidyverse)

faroeStocks <- c("codfarp", "hadfaro", "saifaro")

c %>%
  filter(FishStock %in% faroeStocks) -> d

d %>% 
  ggplot(aes(Year, SSB, group = AssYear)) +
  geom_line() + xlim(c(1990,2001)) +
  facet_wrap(~ FishStock) +
  labs(x = NULL, y = "SSB",
       title = "Faroe SSB retrospective patterns")
