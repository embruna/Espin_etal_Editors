Country.Codes <- function(DATASET) {
  
  # DATA CLEANUP
  
  #Remove (trim) the leading and trailing white spaces (not can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  DATASET$COUNTRY<-trim.trailing(DATASET$COUNTRY)
  trim.leading <- function (x)  sub("^\\s+", "", x)
  DATASET$COUNTRY<-trim.leading(DATASET$COUNTRY)
  
  # remove any double spaces
  DATASET$COUNTRY<-gsub("  ", " ", DATASET$COUNTRY, fixed=TRUE)
  
  # USING COUNTRY NAMES AND Package "countrycode" TO ADD A COLUMN WITH THE 3 DIGIT CODE 
  
  library(countrycode)
  
  # ELIMINATE ANY ROWS WITH BLANKS IN THE 
  # DATASET=Author.Geo for debugging purposes only
  DATASET<-subset(DATASET, COUNTRY!="Unknown")
  
  # Chnage countries as needed, either because country names have changed or to reflect political organization
  
  DATASET$COUNTRY[DATASET$COUNTRY == "Scotland"]  <- "UK" #With apologies to Scots everywhere
  DATASET$COUNTRY[DATASET$COUNTRY == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
  DATASET$COUNTRY[DATASET$COUNTRY == "Wales"]  <- "UK"
  DATASET$COUNTRY[DATASET$COUNTRY == "England"]  <- "UK"
  DATASET$COUNTRY[DATASET$COUNTRY == "German Democratic Republic"]  <- "Germany" #removing old names
  DATASET$COUNTRY[DATASET$COUNTRY == "US"]  <- "USA" #in case any snuck in
  DATASET$COUNTRY[DATASET$COUNTRY == "Yugoslavia"]  <- "Croatia" #Authors from Pretinac
  # DATASET$COUNTRY[DATASET$COUNTRY == "French Guiana"]  <- "France" 
  
  #WOS DATA COME CAPITALIZED, THIS CORRECTS DSOME OF THE ODD ONES
  DATASET$COUNTRY[DATASET$COUNTRY == "BOPHUTHATSWANA"]  <- "South Africa"
  DATASET$COUNTRY[DATASET$COUNTRY == "BYELARUS"]  <- "Belarus"
  DATASET$COUNTRY[DATASET$COUNTRY == "CISKEI"]  <- "South Africa"
  DATASET$COUNTRY[DATASET$COUNTRY == "ENGLAND"]  <- "UK"
  DATASET$COUNTRY[DATASET$COUNTRY == "YEMEN ARAB REP"]  <- "Yemen"
  DATASET$COUNTRY[DATASET$COUNTRY == "WALES"]  <- "UK"
  
  DATASET$COUNTRY[DATASET$COUNTRY == "TRANSKEI"]  <- "South Africa"
  DATASET$COUNTRY[DATASET$COUNTRY == "NETH ANTILLES"]  <- "Netherland Antilles"
  DATASET$COUNTRY[DATASET$COUNTRY == "MONGOL PEO REP"]  <- "Mongolia"
  DATASET$COUNTRY[DATASET$COUNTRY == "FR POLYNESIA"]  <- "French Polynesia"
  DATASET$COUNTRY[DATASET$COUNTRY == "FED REP GER"]  <- "Germany"
  DATASET$COUNTRY[DATASET$COUNTRY == "GER DEM REP"]  <- "Germany"  
  DATASET$COUNTRY[DATASET$COUNTRY == "GUINEA BISSAU"]  <- "GUINEA-BISSAU"  
  DATASET$COUNTRY[DATASET$COUNTRY == "PAPUA N GUINEA"]  <- "PNG"  
  DATASET$COUNTRY[DATASET$COUNTRY == "W IND ASSOC ST"]  <- NA
  
  # West Indies Associated States: collective name for a number of islands in
  # Eastern Caribbean whose status changed from being British colonies to states 
  # in free association with the United Kingdom in 1967. 
  # Included Antigua, Dominica, Grenada, Saint Christopher-Nevis-Anguilla, 
  # Saint Lucia, and Saint Vincent.
  # Treat as NA for now because would need to check each authors home island
  
  #step3: change all the country names to the codes used in mapping
  #Add a column with the 3 letter country codes to be consistent with the other datasets
  #Maptools uses the ISO 3166 three letter codes: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
  #The packahge countrycode will take your column of country names and convert them to ISO3166-3 Codes
  #I began by checking the values of COUNTRY to see if there are any mistakes. To do so I just created a vector 
  
  #called CODECHECK
  
  DATASET$geo.code<-countrycode(DATASET$COUNTRY, "country.name", "iso3c", warn = TRUE)
  #By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
  DATASET$geo.code<-as.factor(DATASET$geo.code)
  
  
  #Deleting rows without country
  # DATASET <- DATASET[!is.na(DATASET$geo.code),] 
  
  #You can correct these as follows in the dataframe with all the data, then add a new column to the dataframe with the country codes
  
  
  return(DATASET)
  
}
  