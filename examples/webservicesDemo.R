
# -----------------------------------------------------------------
# -----------------------------------------------------------------
#
#
#   R and ICES Web services  demo
#
#   3rd November 2017
#
# -----------------------------------------------------------------
# -----------------------------------------------------------------



# -----------------------------
#  Web services
# -----------------------------


# what is a web service? --------------------------------------

url <- "https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSurveyList"


# how to use a web service? --------------------------------------

url <- "https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSurveyList"

# store the response from the url in 'txt'
txt <- RCurl::getURL(url)

# what is in txt
txt

# what IS txt
mode(txt)
length(txt)

# convert to data.frame
ret <- XML::xmlToDataFrame(txt)

# what is in ret
ret

# what IS ret
str(ret)

# since it is just a vector we could just keep the survey column
ret$Survey

# Enter icesDatras ---------------------------------

icesDatras::getSurveyList()




# -----------------------------
#  ICES Web services
# -----------------------------

# DATRAS --------------------------------

hh <- icesDatras::getHHdata("NS-IBTS", 2015, 1)
View(hh)


# SAG ----------------------------------

sag <- icesSAG::getListStocks(2015)
View(sag)

# SAG is based on keys
# lets look at north sea haddock in 2015
sag <- subset(sag, grepl("Haddock", StockDescription) & EcoRegion == "North Sea")
key <- 6589 # or key <- sag$key

# get stock summary table -----------------------------
summary_table <- icesSAG::getSummaryTable(key)
head(summary_table, 10)

# get a standard graph -------------------------------
landings_graph <- icesSAG:::getLandingsGraph(key)
plot(landings_graph)


# SLD ----------------------------------

sld <- icesSLD::getSLD()
View(sld)

url <- "http://stocklist.ices.dk/services/odata4/StockListDWs4"

sld <- jsonlite::fromJSON(url)$value



#  querying sld VIA the webservice
# ---------------------------------

# ask for herring stocks from WGBFAS
query <- "$filter=contains(StockCode, 'had') and EG eq 'WGNSSK'"
query <- gsub(" ", "%20", query)

sld <- jsonlite::fromJSON(paste0(url, "?", query))$value
View(sld)

# ask for all TSA assessments
query <- "$filter=contains(AssessmentType, 'TSA')"
query <- gsub(" ", "%20", query)

sld <- jsonlite::fromJSON(paste0(url, "?", query))$value

# which EGs and which species?
table(sld$EG, sld$SpeciesCommonName)


# Vocab ----------------------------------

type_list <- icesVocab::getCodeTypeList()
View(type_list)

subset(type_list, grepl("aphia", tolower(Description)))

aphias <- icesVocab::getCodeList("SpecWorms")
View(aphias)

subset(aphias, grepl("haddock", tolower(LongDescription)))
aphiaID <- aphias$Key[grep("haddock", tolower(aphias$LongDescription))]










