rm(list = ls())
########
# GOAL #
########

# ~ Take all active stocks in 2016 and plot stock trends over reference points by ecoregion and fishery guild~ #

#####################
### load packages ###
#####################
# install.packages("devtools")
install.packages("icesSAG")
# install.packages("curl")
# library(devtools)

# install_github("ices-tools-prod/icesSLD")
# install_github("ices-tools-prod/icesSAG")

library(icesSLD)
library(icesSAG)
library(dplyr)
library(reshape2)

source("~/stockSummaryTrends.R")


# # Download stock list to find the stocks that are active in 2015
rawsl <- getSLD() %>%
  filter(ActiveYear == 2015)
# Each ecoregion is a column, so we'll need to select the columns we need and make a long data.frame

ecoregions <- colnames(rawsl)[grepl("^.+(Ecoregion)$", colnames(rawsl))]

dots <- lapply(c("StockCode", "AdviceType", "FisheriesGuild", ecoregions),
               as.symbol)

sl <- rawsl %>%
  select_(.dots = dots) %>%
  melt(id.vars = c("StockCode", "AdviceType", "FisheriesGuild"),
       variable.name = "variable",
       value.name = "VALUE") %>%
  filter(!is.na(VALUE)) %>%
  select(STOCK.CODE = StockCode,
         FISHERIES.GUILD = FisheriesGuild,
         ADVICE.TYPE = AdviceType,
         ECOREGION = variable,
         - VALUE) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE),
         FISHERIES.GUILD = tolower(FISHERIES.GUILD),
         ADVICE.TYPE = ifelse(ADVICE.TYPE == "MSY/PA",
                              "MSY", ADVICE.TYPE),
         ECOREGION = as.character(ECOREGION),
         ECOREGION = recode(ECOREGION, "AzoresEcoregion" = "Azores"),
         ECOREGION = recode(ECOREGION, "BayofBiscayandtheIberianCoastEcoregion" = "Bay of Biscay and the Iberian Coast"),
         ECOREGION = recode(ECOREGION, "BalticSeaEcoregion" = "Baltic Sea"),
         ECOREGION = recode(ECOREGION, "CelticSeasEcoregion" = "Celtic Seas"),
         ECOREGION = recode(ECOREGION, "FaroesEcoregion" = "Faroes"),
         ECOREGION = recode(ECOREGION, "GreenlandSeaEcoregion" = "Greenland Sea"),
         ECOREGION = recode(ECOREGION, "IcelandSeaEcoregion" = "Iceland Sea"),
         ECOREGION = recode(ECOREGION, "GreaterNorthSeaEcoregion" = "Greater North Sea"),
         ECOREGION = recode(ECOREGION, "OceanicNortheastAtlanticEcoregion" = "Oceanic north-east Atlantic"),
         ECOREGION = recode(ECOREGION, "NorwegianSeaEcoregion" = "Norwegian Sea"),
         ECOREGION = recode(ECOREGION, "BarentsSeaEcoregion" = "Barents Sea"))


summaryTable <- getSummaryTable(2016)

refPts <- getFishStockReferencePoints(2015)
refPts[refPts == ""] <- NA



refPtsClean <- refPts %>%
  select(-key,
         -AssessmentYear,
         -RecruitmentAge,
         -RecruitmentLength,
         -MSYBescapement,
         STOCK.CODE = FishStockName) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE))

fullSummary <- summaryTblClean %>%
  left_join(refPtsClean, by = c("STOCK.CODE" = "STOCK.CODE")) %>%
  mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
                              0.5,
                              MSYBtrigger),
         MSYBtrigger= ifelse(!stockSizeDescription %in% keeperSSB,
                             NA,
                             MSYBtrigger),
         FMSY = ifelse(fishingPressureDescription %in% relativeF,
                       1,
                       FMSY),
         FMSY = ifelse(!fishingPressureDescription %in% keeperF,
                       NA,
                       FMSY))
charCols <- c("STOCK.CODE", "fishingPressureDescription",
              "stockSizeDescription",
              "FmsyDescription",
              "BmsyDescription")
fullSummary[!colnames(fullSummary) %in% charCols] <- lapply(fullSummary[!colnames(fullSummary) %in% charCols],
                                                            as.numeric)




##############################
### Stock Status over time ###
##############################
colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
             "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
ltyList <- c(1,3:6)


summaryTbl <- getSummaryTable(2016)

keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure")
relativeF <- c("F/FMSY", "Harvest rate/FMSY")

keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
relativeSSB <- c("B/BMSY", "Total biomass/BMSY")

summaryTblClean <- summaryTbl %>%
  # filter(Year >= 2013) %>%
  select(Year,
         STOCK.CODE = fishstock,
         F,
         SSB,
         fishingPressureDescription,
         stockSizeDescription,
         LANDINGS = landings,
         CATCHES = catches) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE),
         fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),

         stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("Stock size: ", "", stockSizeDescription),
         stockSizeDescription = gsub("msy", "MSY", stockSizeDescription),
         stockSizeDescription = ifelse(is.na(stockSizeDescription), "Relative",
                                       stockSizeDescription),
         FmsyDescription = "FMSY",
         FmsyDescription = ifelse(fishingPressureDescription %in% relativeF,
                                  "F/FMSY",
                                  fishingPressureDescription),
         BmsyDescription = "MSYBtrigger",
         BmsyDescription = ifelse(stockSizeDescription %in% relativeSSB,
                                  "SSB/BMSY",
                                  stockSizeDescription)) %>%
  filter(stockSizeDescription %in% keeperSSB |
           fishingPressureDescription %in% keeperF)

refPts <- getFishStockReferencePoints(2016)
refPts[refPts == ""] <- NA

refPtsClean <- refPts %>%
  select(-key,
         -AssessmentYear,
         -RecruitmentAge,
         -RecruitmentLength,
         -MSYBescapement,
         STOCK.CODE = FishStockName) %>%
  mutate(STOCK.CODE = tolower(STOCK.CODE))

fullSummary <- summaryTblClean %>%
  left_join(refPtsClean, by = c("STOCK.CODE" = "STOCK.CODE")) %>%
  mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
                              0.5,
                              MSYBtrigger),
         MSYBtrigger= ifelse(!stockSizeDescription %in% keeperSSB,
                             NA,
                             MSYBtrigger),
         FMSY = ifelse(fishingPressureDescription %in% relativeF,
                       1,
                       FMSY),
         FMSY = ifelse(!fishingPressureDescription %in% keeperF,
                       NA,
                       FMSY))
charCols <- c("STOCK.CODE", "fishingPressureDescription",
              "stockSizeDescription",
              "FmsyDescription",
              "BmsyDescription")
fullSummary[!colnames(fullSummary) %in% charCols] <- lapply(fullSummary[!colnames(fullSummary) %in% charCols],
                                                            as.numeric)




stockTrends <- fullSummary %>%
  left_join(sl, by = "STOCK.CODE") %>%
  distinct(.keep_all = TRUE) %>%
  mutate(F_FMSY = ifelse(!is.na(FMSY),
                         F / FMSY,
                         NA),
         SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                  SSB / MSYBtrigger,
                                  NA)) %>%
  select(Year,
         STOCK.CODE,
         FISHERIES.GUILD,
         ECOREGION,
         F_FMSY,
         SSB_MSYBtrigger) %>%
  melt(id.vars = c("Year",  "STOCK.CODE", "FISHERIES.GUILD","ECOREGION"),
       measure.vars = c("F_FMSY", "SSB_MSYBtrigger"),
       variable.name = "METRIC",
       value.name = "stockValue") %>%
  group_by(ECOREGION, FISHERIES.GUILD, METRIC, Year) %>%
  mutate(ecoGuildMean = mean(stockValue, na.rm = TRUE))


allDat <- stockTrends %>%
  mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
  ungroup() %>%
  select(pageGroup = ECOGUILD,
         lineGroup = STOCK.CODE,
         Year,
         plotGroup = METRIC,
         plotValue = stockValue) %>%
  filter(!is.na(plotValue))
#
oMean <- stockTrends %>%
  mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
  ungroup() %>%
  distinct(ECOGUILD, METRIC, Year, .keep_all = TRUE) %>%
  select(pageGroup = ECOGUILD,
         Year,
         plotGroup = METRIC,
         plotValue = ecoGuildMean) %>%
  mutate(lineGroup = "MEAN") %>%
  filter(!is.na(plotValue))

allDat <- bind_rows(allDat, oMean)

# Set up colors
plotList <- allDat %>%
  group_by(pageGroup) %>%
  select(pageGroup, lineGroup) %>%
  mutate(nLines = n_distinct(lineGroup) - 1,
         COLOR = NA) %>%
  distinct(lineGroup, .keep_all = TRUE) %>%
  arrange(pageGroup)
#
singleList <- plotList %>%
  filter(nLines == 1) %>%
  mutate(COLOR = colList[1:length(nLines)])
#
normList <- plotList %>%
  filter(nLines <= 9 &
           nLines > 1 &
           lineGroup != "MEAN") %>%
  mutate(COLOR = colList[1:length(nLines)])
#
longList <- plotList %>%
  filter(nLines > 9 &
           lineGroup != "MEAN") %>%
  mutate(COLOR = "grey80")
#
meanList <- plotList %>%
  filter(nLines > 1 &
           lineGroup == "MEAN") %>%
  mutate(COLOR = "grey40")

colorList <- bind_rows(singleList, normList, longList, meanList)
allDat <- left_join(colorList, allDat, by = c("pageGroup", "lineGroup"))

allDat <- allDat %>%
  group_by(pageGroup) %>%
  mutate(nLines = n_distinct(lineGroup)) %>%
  filter(nLines > 2 | lineGroup != "MEAN") %>%
  filter(lineGroup != "MEAN" | Year != 2016 | plotGroup != "F_FMSY")


df <- allDat[allDat$plotGroup == "F_FMSY" &
            allDat$pageGroup == "Greater North Sea, benthic",]

stockSummaryTrends(df = df,
                   overallMean = TRUE,
                   plotDir = "~/git/ices-dk/webservicesDemo/")

