library(sf)
library(dplyr)
library(fuzzyjoin)
library(stringr)
ED <- st_read(dsn = "./ElectoralDistrictMaps/ElectoralDistricts_Simplified.shp",options = "ENCODING=WINDOWS-1252") 
ED <- ED %>% arrange(FEDENAME)
ED <- ED %>% mutate(FEDENAME = gsub("--","—",FEDENAME),
                    FEDFNAME = gsub("--","—",FEDFNAME),
                    FEDNAME = gsub("--","—",FEDNAME))



members <- read.csv("./members.csv")
members <- members %>% arrange(Constituency)
members <- members %>% mutate(Constituency = gsub("--","—",Constituency))


member_ED <- stringdist_join(ED,members,by=c("FEDENAME"="Constituency"),
                      mode="left",method="jw",max_dist=0.02,distance_col="dist") %>%
  group_by(FEDENAME) %>% slice_min(order_by=dist,n=1)


no_member_ED <- ED[which(ED$FEDUID%in%setdiff(ED$FEDUID,member_ED$FEDUID)),]

ok <- bind_rows(data.frame(no_member_ED),data.frame(member_ED))
ok$geometry <- c(no_member_ED$geometry, member_ED$geometry)
ok <- st_as_sf(ok)

View(ok %>% select(FEDENAME,Constituency,dist))

library(XML)
library(plyr)
library(RCurl)
nvotes <- 261
# for(j in 1:nsession)
# for(i in 1:nvotes){
#   
#   vote_xml_url <- paste0("https://www.ourcommons.ca/Members/en/votes/44/1/",i,"/xml")
#   download_URL <- getURL(vote_xml_url)
#   temp_raw_xml <- xmlParse(download_URL)
#   temp_list <- xmlToList(temp_raw_xml) ###is to convert xml doc into List
#   data <- ldply(temp_list, data.frame)
#   
#   write.csv(data,paste0("./VoteData/vote_",i,"_data.csv"))
# }

#Change above code to reflect xmlToDataFrame?

# vote_xml_url <- "https://www.ourcommons.ca/Members/en/votes/xml"
#    download_URL <- getURL(vote_xml_url)
#    temp_raw_xml <- xmlParse(download_URL,ignoreBlanks = F)
#    data <- xmlToDataFrame(temp_raw_xml) ###is to convert xml doc into List
#    write.csv(data,"./VoteData/MetaDataVotes.csv")
# 
# vote1 <- read.csv("./VoteData/vote_1_data.csv")
# vote_data <- vote1 %>% select(PersonId,
#                           .id,
#                           PersonShortSalutation,
#                           PersonOfficialFirstName,
#                           PersonOfficialLastName,
#                           ConstituencyName,
#                           ConstituencyProvinceTerritoryName,
#                           VoteValueName) %>%
#   mutate(Name = paste0(PersonShortSalutation,
#                        PersonOfficialFirstName,
#                        " ",
#                        PersonOfficialLastName)) %>% 
#     relocate(Name,.after=.id) %>% 
#       select(-c(PersonShortSalutation,
#                 PersonOfficialFirstName,
#                 PersonOfficialLastName))
# names(vote_data)[6] <- "Vote_1"
# 
# for(i in 2:nvotes){
#   print(i)
#   votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
#   votetemp <- votetemp %>% 
#     select(PersonId,
#            .id,
#            PersonShortSalutation,
#            PersonOfficialFirstName,
#            PersonOfficialLastName,
#            ConstituencyName,
#            ConstituencyProvinceTerritoryName,
#            VoteValueName) %>%
#       mutate(Name = paste0(PersonShortSalutation,
#                            PersonOfficialFirstName,
#                            " ",
#                            PersonOfficialLastName)) %>% 
#         relocate(Name,.after=.id) %>% 
#           select(-c(PersonShortSalutation,
#                     PersonOfficialFirstName,
#                     PersonOfficialLastName))
#   names(votetemp)[6] <- paste("Vote_",i)
#   vote_data <- full_join(vote_data,votetemp,by=c("PersonId",
#                                                  ".id",
#                                                  "Name",
#                                                  "ConstituencyName",
#                                                  "ConstituencyProvinceTerritoryName"))
# }


