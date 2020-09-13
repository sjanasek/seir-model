library(stringr)

#####################
#                   #
# Als csv-Datei     #
#                   #
#####################

cov19_dat <- str_c("RKI-Faelle-",Sys.Date(),".csv")
if (!file.exists(cov19_dat)) { 
  file_url <- 
    "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv" 
  download.file(url=file_url,destfile= cov19_dat, mode="wb") 
}

data <- read.csv(file = cov19_dat) 
data <- data[,1:18] 

print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

retval <- subset( data, Bundesland == "Sachsen" & Landkreis == "SK Dresden")
print(retval)


#####################
#                   #
# Als geojson-Datei #
#                   #
#####################

library(geojsonR)
file_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")

library(geojsonio)
file= geojson_read("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
