
# R CODE FOR GENERATING THE DATASET "AuthorCountries_1985_2014.csv" used in espin et al 2017 Plos Biology

# After conducting a search in the Web of Science Core Colleciton for articles published in the focal journals in a given year, 
# we then did a  "Results Analysis" of the resulting articles (https://images.webofknowledge.com/WOKRS526R11/help/WOK/hp_analyze_results.html)
# and downloaded the frequency data of "Author Country") as a text file. We saved these in a folder called "AuthorCountries1985_2014" 
# located in the "Data" folder with the file name AU1985-AU2014.
 
# These files were then stiched together into the file used in the article (AuthorCountries_1985_2014.csv) with the following code: 

# Binds the files in the folder together and adds the year represented by each file 
# as a column by estracting it from the file name  
FileNames <- list.files("./Data/AuthorCountries1985_2014", full.names = T)
AuthorCountries = lapply(FileNames, function(x) {
  dat = read_tsv(x, col_names = TRUE,skip = 0, comment="(") %>% select(-(3)) 
  # Add column names
  names(dat) = c("COUNTRY", "Articles")
  # Add a column with the year
  dat$YEAR = substr(x,35,38)
  return(dat)
})

#This is returned as a list, when binding below converts to dataframe with country as chr
AuthorCountries<-bind_rows(AuthorCountries)
AuthorCountries$YEAR<-as.numeric(AuthorCountries$YEAR)

# One of the countries no longer exists, and the authors need to be allocated to the correct (new) country 
# based on their institutional address. These were found manually by reviewing article pdfs.
AuthorCountries<-AuthorCountries %>% filter(COUNTRY!="CZECHOSLOVAKIA")
CZK<-rep("Czech Republic",10)
SVK<-rep("Slovakia",10)
CZK_N<-c(7,2,2,4,5,4,4,13,11,1) #The number each year (prior to breakup of Czechoslovakia) that are in what is today CZK
SVK_N<-c(1,0,2,0,2,0,1,2,0,0) #The number each year (prior to breakup of Czechoslovakia) that are in what is today SVK
CZK<-cbind(CZK,CZK_N)
SVK<-cbind(SVK,SVK_N)
CZK_SVK<-as.data.frame(rbind(CZK, SVK))
CZK_SVK<-rename(CZK_SVK, COUNTRY=CZK, Articles=CZK_N)
CZK_SVK$COUNTRY<-as.character(CZK_SVK$COUNTRY)
CZK_SVK$Articles<-as.integer(CZK_SVK$Articles)
str(CZK_SVK)
YEAR<-rep(seq(1985,1994,by=1),2)
CZK_SVK<-cbind(CZK_SVK, YEAR)

AuthorCountries<-bind_rows(CZK_SVK,AuthorCountries)
AuthorCountries<-AuthorCountries %>% arrange(YEAR, desc(Articles))
str(AuthorCountries)
#Delete the WOS percentage, add a column in which you generate it yourself
AuthorCountries<-AuthorCountries %>%  group_by(YEAR) %>% mutate(Pcnt_Pubs= (Articles/sum(Articles)*100)) %>% rename(N_Articles = Articles)
AuthorCountries
AuthorCountries$COUNTRY[AuthorCountries$COUNTRY=="FED REP GER"] <- "GERMANY"
write.csv(AuthorCountries, file="./Data/AuthorCountries_1985_2014.csv", row.names = F) #export it as a csv file
# 
# This is to calclulate the total articles over all years.

Articles<-c(2828,2830,3045,2528,2708,2756,2833,2934,2928,2925,2982,3296,3204,3308,3332,3571,3848,3908,4237,4433,4636,4780,4668,5067,4958,4941,4822,4857,5095,4998)
Year<-seq(1985,2014,by=1)
ArtPerYear<-as.data.frame(cbind(Year,Articles))
sum(ArtPerYear$Articles)
