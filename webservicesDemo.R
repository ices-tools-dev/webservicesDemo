
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





# a bit of fun ----------------------------------

hh <- icesDatras::getDATRAS("HH", "NS-IBTS", c(2000, 2001), 1)
hl <- icesDatras::getDATRAS("HL", "NS-IBTS", c(2000, 2001), 1)

# keep only haddock
hl <- subset(hl, Valid_Aphia == aphiaID)

# standardise length to mm
hl <- within(hl, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})


# some simple interogation
tab <- with(hl, tapply(HLNoAtLngt, list(Year, length), sum))
tab

length <- as.numeric(colnames(tab))/10
plot(length, tab["2000",], type = "h", ylab = "", las = 1)
points(length + 0.2, tab["2001",], type = "h", col = "red")


# do some merging
keycols <- intersect(names(hh), names(hl))[-1]
hl$key <- apply(hl[keycols], 1, paste, collapse = " ")
hh$key <- apply(hh[keycols], 1, paste, collapse = " ")


N <- tapply(hl$HLNoAtLngt, hl$key, sum, na.rm=TRUE)
# merge
hh$N <- N[hh$key]
hh$N[is.na(hh$N)] <- 0
# convert to npue
hh$npue <- hh$N / hh$HaulDur


plot(hh$ShootLong, hh$ShootLat, cex = sqrt(hh$npue/50))


library(mgcv)
library(gmrf)
require(rgdal)
require(rgeos)
require(sp)
require(spdep)
require(raster)

download.ICESshape <- function(what) {
  if (!dir.exists("zips")) dir.create("zips")

  download.file(paste0("http://gis.ices.dk/shapefiles/", what, ".zip"),
                paste0("zips/", what, ".zip"))

  if (!dir.exists("shapefiles")) dir.create("shapfiles")
  unzip(paste0("zips/", what, ".zip"), exdir = "shapefiles")
}

download.ICESshape("ICES_areas")
download.ICESshape("ICES_StatRec_mapto_ICES_Areas")


# read in spatial datasets
area <- readOGR("shapefiles", "ICES_Areas_20160601_dense")
statrec <- readOGR("shapefiles", "ICES_StatRec_mapto_ICES_Areas")
statrec <- spTransform(statrec, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # transdorm to wgs84

# remember sag?
sag
areas <- gsub("27.", "", strsplit(sag$ICESAreasOfStock, " ~ ")[[1]])

coordinates(hh) <- ~ ShootLong + ShootLat
proj4string(hh) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# keep only sampled areas
tmp <- gContains(area, hh, byid = TRUE)
area <- area[colSums(tmp) > 0,]
statrec <- statrec[statrec$Max_Area %in%
                          gsub(".NA", "",
                               paste(area$SubArea, area$Division, area$SubDivisio, sep = ".")),]

# predict over statsquares that have been sampled at least once ? twice?
tmp <- gContains(statrec, hh, byid = TRUE)
statrec <- statrec[colSums(tmp) > 0,]

# prepare GMRF for spatial model
adj <- spdep::poly2nb(statrec, queen = FALSE)
adj <- spdep::nb2mat(adj, style = "B", zero.policy = TRUE)
Q <- methods::as(adj, "dgTMatrix")

Q @ x[] <- -1/Q @ x
diag(Q) <- rowSums(adj)
Q <- as.matrix(Q)
colnames(Q) <- rownames(Q) <- statrec $ ICESNAME
statrec $ StatRec <- statrec $ ICESNAME




# do some modelling
hh$fStatRec <- factor(hh$StatRec, levels = statrec $ StatRec)
statrec$fStatRec <- factor(statrec$StatRec)

# fit
g2000 <- gam(npue ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = 50),
           data = subset(data.frame(hh), Year == 2000),
           drop.unused.levels = FALSE)

g2001 <- gam(npue ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = 50),
             data = subset(data.frame(hh), Year == 2001),
             drop.unused.levels = FALSE)



# model fit plotting
# set up color scale
cols <- gplots::rich.colors(50)
fitfit2000 <- predict(g2000, newdata = statrec@data)
fit2000 <- predict(g2000, newdata = statrec@data)
fit2001 <- predict(g2001, newdata = statrec@data)

# cut into colour slices
fit <- cbind(fit2000, fit2001)
fit[] <- cols[as.numeric(cut(fit, 50))]


plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
plot(statrec, col = fit[,1], add = TRUE)
plot(area, lwd = 2, add = TRUE)
mtext("2000", font = 2)
points(hh[hh$Year == 2000,])

plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
plot(statrec, col = fit[,2], add = TRUE)
plot(area, lwd = 2, add = TRUE)
mtext("2001", font = 2)
points(hh[hh$Year == 2001,])




top <- 30:35
plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
plot(statrec, col = fit[,1], add = TRUE)
plot(area, lwd = 2, add = TRUE)
mtext("2000", font = 2)
points(hh[hh$Year == 2000,])
plot(statrec[order(fit2000, decreasing = TRUE)[top],], add = TRUE, border = "green", lwd = 2)


keys <- subset(hh@data, fStatRec %in% statrec$StatRec[order(fit2000, decreasing = TRUE)[top]])$key
hl2 <- subset(hl, key %in% keys)

tab <- with(hl2, tapply(HLNoAtLngt, list(Year, length), sum))
tab

length <- as.numeric(colnames(tab))/10
plot(length, tab["2000",], type = "h", ylab = "", las = 1, ylim = c(0, max(tab, na.rm=TRUE)))
points(length + 0.2, tab["2001",], type = "h", col = "red")








