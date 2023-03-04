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
ok <- ok %>% mutate(Name = paste0(Honorific.Title,
                                  First.Name,
                                  " ",
                                  Last.Name)) %>% 
  relocate(Name,.after=PRUID) %>% select(-c(Honorific.Title,
                                                            First.Name,
                                                            Last.Name))
                                 #                        PersonOfficialFirstName,
                                 #                        " ",
                                 #                        PersonOfficialLastName))
View(ok %>% select(FEDENAME,Constituency,dist))

library(XML)
library(plyr)
library(RCurl)
# nvotes <- 261
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
# 
# # vote_xml_url <- "https://www.ourcommons.ca/Members/en/votes/xml"
#    download_URL <- getURL(vote_xml_url)
#    temp_raw_xml <- xmlParse(download_URL,ignoreBlanks = F)
#    data <- xmlToDataFrame(temp_raw_xml) ###is to convert xml doc into List
#    write.csv(data,"./VoteData/MetaDataVotes.csv")

#Get parliament member data

vote1 <- read.csv("./VoteData/vote_1_data.csv")

members_tenure <- vote1 %>% select(PersonId)

for(i in 2:nvotes){
  print(i)
  votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
  membertemp <- votetemp %>%
    select(PersonId)

  members_tenure <- full_join(members_tenure,membertemp,by="PersonId")

  
}

# for(i in 1:nrow(members_tenure)){
#   print(i)
#   member_csv_url <- paste0("https://www.ourcommons.ca/members/en/",members_tenure$PersonId[i],"/csv")
#   download.file(member_csv_url,paste0())
# }

for(i in 1:nrow(members_tenure)){
  temp_members <- read.csv(paste0("./Members/",members_tenure$PersonId[i],".csv"),skip=1,header = T)
  members_tenure$StartDate[i] <- temp_members[1,7]
  members_tenure$EndDate[i] <- temp_members[1,8]
}

members_caucus <- members_tenure
members_caucus$Caucus <- NA

for(i in 1:nrow(members_caucus)){
  temp_members <- read.csv(paste0("./Members/",members_caucus$PersonId[i],".csv"),skip=1,header = T)
  if(!is.na(temp_members[4,1])){
    members_caucus[i,]$Caucus <- temp_members[4,1]
    members_caucus[i,]$StartDate <- temp_members[4,2]
    members_caucus[i,]$EndDate <- temp_members[4,3]
  }
  if(is.na(temp_members[4,1])){
    members_caucus[i,]$Caucus <- temp_members[1,6]
    }
}

# for(i in 2:nvotes){
#   print(i)
#   votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
#   membertemp <- votetemp %>%
#     select(PersonId)
#   membertemp$StartDate <- NA
#   membertemp$EndDate <- NA
#   membertemp <- membertemp[membertemp$PersonId%in%setdiff(membertemp[,1:2],members[,1:2])$PersonId,]
#   
#   if(nrow(membertemp)!=0){
#     members <- rbind(members,membertemp)
#   }
#   
# }












# get affiliation for each member
members$
for(i in 1:nrow(members)){
  temp <- read.csv(paste0("./Members/",members$PersonId[i],".csv"),skip=1,header=T)
}



vote1 <- read.csv("./VoteData/vote_1_data.csv")
vote_data <- vote1 %>% select(PersonId,
                          .id,
                          PersonShortSalutation,
                          PersonOfficialFirstName,
                          PersonOfficialLastName,
                          CaucusShortName,
                          ConstituencyName,
                          ConstituencyProvinceTerritoryName,
                          DecisionEventDateTime,
                          VoteValueName) %>%
  mutate(Name = paste0(PersonShortSalutation,
                       PersonOfficialFirstName,
                       " ",
                       PersonOfficialLastName)) %>%
    relocate(Name,.after=.id) %>%
      select(-c(PersonShortSalutation,
                PersonOfficialFirstName,
                PersonOfficialLastName))
names(vote_data)[7] <- "Date_Vote_1"
names(vote_data)[8] <- "Vote_1"

for(i in 2:nvotes){
  print(i)
  votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
  votetemp <- votetemp %>%
    select(PersonId,
           .id,
           PersonShortSalutation,
           PersonOfficialFirstName,#need to include caucus?
           PersonOfficialLastName,
           CaucusShortName,
           ConstituencyName,
           ConstituencyProvinceTerritoryName,
           VoteValueName) %>%
      mutate(Name = paste0(PersonShortSalutation,
                           PersonOfficialFirstName,
                           " ",
                           PersonOfficialLastName)) %>%
        relocate(Name,.after=.id) %>%
          select(-c(PersonShortSalutation,
                    PersonOfficialFirstName,
                    PersonOfficialLastName))
  names(votetemp)[7] <- paste("Vote_",i)
  
  votetemp_new <- votetemp[votetemp$PersonId%in%setdiff(votetemp$PersonId,vote_data$PersonId),]
  votetemp <- votetemp %>% select(PersonId,CaucusShortName,paste("Vote_",i))
  vote_data <- full_join(vote_data,votetemp,by=c("PersonId","CaucusShortName"))
}


