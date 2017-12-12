# Retrieval of AIRBNB-Listings in the canton of Zurich
# August / September 2017

#load packages
library(pacman)
pacman::p_load(jsonlite,curl,dplyr,leaflet,readxl)

#Load file with locality names & postalcodes (PLZ / Ort)
ort<- read.csv("ortschaften.csv", sep=";")

#Filter: Only localities in the canton of Zurich
ortzh<-ort %>% filter(KTKZ=="ZH")

#extract main string in the locality-name to drop suffixes like a.A., a.I.,etc.
ortzh$ORTNAME2 <- sub(" .*","",ortzh$ORTNAME)

# AIRBNB-API  ---------------
# The API delivers a JSON-file with listings that can be stored in a list

testlist<-fromJSON("https://api.airbnb.com/v2/search_results?client_id=3092nxybyb0otqw18e8nh5nty&_limit=10&_offset=0&&location=Adlikon--8106-Regensdorf--ZH")

# Extract the attributes of interest
df <- subset(testlist$search_results$listing, select=c("id","room_type","lat","lng","beds","bedrooms","person_capacity","bathrooms","city","instant_bookable","property_type","name","reviews_count","star_rating","public_address"))

# Retrieval-strategy with postal area codes and localities  ------------------------------
# if the loop breaks, just restart from the last postalcode (lastplz)

#the baseurl to be dynamically modified within the loop (parameters: postal codes and price categories)
baseurl <- "https://api.airbnb.com/v2/search_results?client_id=3092nxybyb0otqw18e8nh5nty&_limit=50&fetch_facets=true&ib=false&sort=1&locale=de-CH&&zoom=10"


lastplz <- 1

#######################
# if the loop breaks, it can be re-executed from here on (it will restart from the rowindex where it has been stuck)
#######################

for (plz in lastplz:nrow(ortzh)){
  for (p in c(seq(20,100,20),seq(200,500,100),seq(500,1000,250))){
    for (i in seq(0,350,50)){
      
      zhlist <- fromJSON(paste0(baseurl,"&sort=1&_offset=",i ,"&&location=",ortzh$ORTNAME2[plz],"-",ortzh$PLZ4[plz],"-ZH")) #flatten=TRUE
      
      ls(zhlist$search_results$listing)
      
      zhdf2 <- subset(zhlist$search_results$listing, select=c("id","room_type","lat","lng","beds","bedrooms","person_capacity","bathrooms","city","instant_bookable","property_type","name","reviews_count","star_rating","public_address"))
      
      zhdf2$curr <-unlist(zhlist$search_results$pricing_quote$localized_currency)
      zhdf2$price <-unlist(zhlist$search_results$pricing_quote$localized_nightly_price)
      zhdf2$userid<- zhlist$search_results$listing$user$id
      zhdf2$scrapedate <- Sys.Date()
      zhdf2$i <- i
      zhdf2$p <- p
      zhdf2$plz <- ortzh$PLZ4[lastplz]
      
      #store the postal code which has just been processed for the case the loop breaks
      lastplz <-plz
      
      #add the retrieved listings to the listings-dataframe incrementally
      if (exists("zhdfplz")){ zhdfplz<- rbind(zhdfplz,zhdf2)} else { zhdfplz <- zhdf2  } 
      
      #message
      message("Retrieving listings ", i, " max price:", p, " PLZ:  ", ortzh$PLZ4[lastplz], ", NUMBER of listings retrived: ", nrow(zhdfplz))
      
      #break the loop if the last result page has been reached
      if (zhlist$metadata$listings_count == zhlist$metadata$pagination$next_offset){break}
    }
  }
}  


#######################
## Alternative retrieval-strategy with coordinates, finer price-categories and capacity (number of guests) ------------------
#######################

#pages<-list()
for (lat in c(0,1)){
  for (p in seq(20,1000,5)){
    for (g in c(1:16)){
      for (i in seq(0,350,50)){
        
        #modify URL in loop
        zhlist <- fromJSON(paste0(baseurl,"&sort=1&_offset=",i,"&guests=",g,"&price_max=",p,"&price_min=",p-5, "&search_by_map=true&sw_lat=",47.10674756746862+lat,"&sw_lng=8.26497214993151&ne_lat=",47.605009102870554+lat,"&ne_lng=9.05324119290026&zoom=10")) 
        
        if (zhlist$metadata$pagination$result_count<1){break}
        
        zhdf1 <- subset(zhlist$search_results$listing, select=c("id","room_type","lat","lng","beds","bedrooms","person_capacity","bathrooms","city","instant_bookable","property_type","name","reviews_count","public_address"))
        
        zhdf1$curr <-unlist(zhlist$search_results$pricing_quote$localized_currency)
        zhdf1$price <-unlist(zhlist$search_results$pricing_quote$localized_nightly_price)
        zhdf1$userid<- zhlist$search_results$listing$user$id
        zhdf1$scrapedate <- Sys.Date()
        zhdf1$g <- g
        zhdf1$i <- i
        zhdf1$p <- p
        zhdf1$geo <- lat
        row.names(zhdf1) <- NULL
        
        
        if (exists("zhdfplz2")){ zhdfplz<- rbind(zhdfplz2,zhdf1)} else { zhdfplz2 <- zhdf1  } 
        
        message("Retrieving listings ", i, " max price:", p, " guests: ", g, ", NUMBER of listings retrived: ", nrow(zhdfplz2))
        
        if (zhlist$metadata$listings_count == zhlist$metadata$pagination$next_offset){break}  
        
      }
    }
  }
}





