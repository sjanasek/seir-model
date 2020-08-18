#Einlesen und Verwerten der Nowcasting Daten des RKI
#Laden der nötigen Packages

install.packages("xlsx", "stringr", "dplyr", "ggplot2", "scales")

library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)

#Lade neuesten Nowcast Datensatz von der RKI Webseite
daten_file <- str_c("Nowcasting_Zahlen-",Sys.Date(),".xlsx")
if (!file.exists(daten_file)) { 
  file_url <- 
    "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile" 
  download.file(url=file_url,destfile= daten_file, mode="wb") 
}

#Lese Excel-File 
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_261') # for 32-bit version
data <- xlsx::read.xlsx(file = daten_file, sheetName = "Nowcast_R", encoding = "UTF-8") 
data <- data[,1:13] 

#Umbenennung der Spalten
names(data) <- c("Datum", "NeuErkr", "ug_NeuErkr", "og_NeuErkr", "NeuErkr_gl", "ug_NeuErkr_gl", 
                 "og_NeuErkr_gl", "R", "ug_R", "og_R", "R_7Tage", "ug_R_7Tage", "og_R_7Tage")

# Berechnung des 4 Tage R
R_Wert <- rep(NA, nrow(data)) 
for (t in 8:nrow(data)) { 
  R_Wert[t] <- sum(data$NeuErkr[t-0:3]) / sum(data$NeuErkr[t-4:7])
} 
data <- data %>% dplyr::mutate(R_Wert = round(R_Wert, digits = 2))

# Vgl R-Werten in der Excel-Tabelle 
data %>% select(Datum, R, R_Wert) %>% tail()

#Plot
ggplot(data=data, aes(x=Datum)) +
  geom_ribbon(aes(ymin = ug_R, ymax = og_R), stat="identity", fill="seagreen")+
  geom_line(aes(y = R), stat="identity", fill="seagreen")+
  theme_minimal() + 
  labs(title = "", x = "", y = "Basisreproduktionszahl R") + 
  scale_x_date(date_breaks = "2 days", labels = 
                 scales::date_format("%d.%m.")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) + 
  theme(axis.text.x = element_text(angle=90, vjust=0))