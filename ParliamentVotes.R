library(sf)
library(dplyr)
library(fuzzyjoin)
library(stringr)
library(XML)
# library(plyr)
library(RCurl)
`%not_in%` <- Negate(`%in%`)
#Take vote xml data and use this to create a csv file for each vote

nvotes <- 408 #number of votes in the first session of parliament so far
#add for(j in 1:nsession) if parliament is into 2nd session

### GET THE DATA FOR EACH VOTE ###
#This may take some time to download.
#if you are updating your data, start at your last nvotes+1, instead of downloading 
#the original n again
for(i in 1:nvotes){
  print(i)
  vote_xml_url <- paste0("https://www.ourcommons.ca/Members/en/votes/44/1/",i,"/xml")
  download_URL <- getURL(vote_xml_url)
  temp_raw_xml <- xmlParse(download_URL)
  tempvotedata <- xmlToDataFrame(temp_raw_xml)
  #write.csv(tempvotedata,paste0("./VoteData/vote_",i,"_data.csv"))
}

### GET THE METADATA FOR THE VOTES ###
#Get metadata on votes (name of bill, subject, vote results, time of vote, etc.)
#because this is not included in the above csv for each vote
vote_xml_url <- "https://www.ourcommons.ca/Members/en/votes/xml"
download_URL <- getURL(vote_xml_url)
temp_raw_xml <- xmlParse(download_URL,ignoreBlanks = F)
data <- xmlToDataFrame(temp_raw_xml)
write.csv(data,"./VoteData/MetaDataVotes.csv")


#Getting the constituencies of all past (voting) HoC members. Speaker does not vote
#and must be added manually.

vote1 <- read.csv("./VoteData/vote_1_data.csv")

members_constituency <- vote1 %>% select(PersonId,ConstituencyName)

for(i in 2:nvotes){
  print(i)
  votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
  membertemp <- votetemp %>%
    select(PersonId,ConstituencyName)
  members_constituency <- full_join(members_constituency,
                                    membertemp,
                                    by=c("PersonId","ConstituencyName"))
}
#add the speaker
members_constituency <- rbind(members_constituency,
                              data.frame(PersonId=25452,
                                         ConstituencyName = "Nipissing—Timiskaming"))
write.csv(members_constituency,"members_constituency.csv") #can write now, but will add 
#to it at the bottom of this file



#members_constituency <- members_constituency[-184,]

#Get parliament member tenure data

for(i in 1:nrow(members_constituency)){
  print(i)
  member_csv_url <- paste0("https://www.ourcommons.ca/members/en/",members_constituency$PersonId[i],"/csv")
  download.file(member_csv_url,paste0("./Members/",members_constituency$PersonId[i],".csv"))
}

members_tenure <- members_constituency
members_tenure$StartDate <- NA
members_tenure$EndDate <- NA

for(i in 1:nrow(members_constituency)){
  temp_members <- read.csv(paste0("./Members/",members_constituency$PersonId[i],".csv"),skip=1,header = T)
  members_tenure$StartDate[i] <- temp_members[1,7]
  members_tenure$EndDate[i] <- temp_members[1,8]
}

write.csv(members_tenure,"members_tenure.csv")

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
# These CSV files seem pretty randomly formatted, in Marc Garneau's case the formatting is 
#slightly different so the start and end dates don't follow the same rules as above
members_caucus[118,]$StartDate <- read.csv(paste0("./Members/",10524,".csv"),skip=1,header = T)[1,7]
members_caucus[118,]$EndDate <- read.csv(paste0("./Members/",10524,".csv"),skip=1,header = T)[1,8]
members_caucus[118,]$Caucus <- read.csv(paste0("./Members/",10524,".csv"),skip=1,header = T)[1,6]

#Add Alain Rayes and Han Dong, members who changed caucuses mid session
members_caucus <- rbind(members_caucus,data.frame(PersonId=c(88600,105091),
                                                  ConstituencyName=c("Richmond-Arthabaska","Don Valley North"),
                                                  StartDate=c("2021-09-20 12:00:00 AM","2021-09-20 12:00:00 AM"),
                                                  EndDate=c("2022-09-13 1:05:00 PM","2023-03-22 5:00:00 PM"),
                                                  Caucus=c("Conservative","Liberal")))

write.csv(members_caucus,"members_caucus.csv")

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

# combine vote data to get a large table of votes. 
# Need to be careful here because people who did not vote are omitted from the voting
# data. Did people who didn't vote not vote because they weren't present, they chose not to,
# or because they were not elected yet (Charles Sousa prior to Jan 2023) or had resigned prior? 
# Need to be manually add such members based on the members of parliament roll from 
# defined by members_caucus dates

library(data.table)
vote1 <- fread("./VoteData/vote_1_data.csv",drop=1)
vote_data <- vote1 %>%
  mutate(VoteId = paste0(ParliamentNumber,
                         SessionNumber,
                         DecisionDivisionNumber)) %>%
  select(-c(ParliamentNumber,
            SessionNumber,
            DecisionDivisionNumber,
            PersonShortSalutation))

vote_date <- as.POSIXct(unique(vote1$DecisionEventDateTime))

vote_i_members <- rbind(members_caucus %>% 
                          filter(EndDate ==""& vote_date > as.POSIXct(StartDate)),
                        members_caucus %>% 
                          filter(EndDate!="") %>% 
                          filter(as.POSIXct(StartDate) <= vote_date & vote_date <= as.POSIXct(EndDate)))

vote_i_members <- vote_i_members[vote_i_members$PersonId %in% setdiff(vote_i_members$PersonId,vote_data$PersonId),]

for(j in 1:nrow(vote_i_members)){
  
  member_temp <- fread(paste0("./Members/",vote_i_members$PersonId[j],".csv"),skip=1,fill=T,header=T,sep=",",select=c(1:5),nrows=1)
  vote_data <- rbind(vote_data,
                     data.frame(
                       DecisionEventDateTime = vote_date,
                       ConsituencyName = member_temp[1,4],
                       VoteValueName = "Did not vote",
                       PersonOfficialFirstName = member_temp[1,2],
                       PersonOfficialLastName = member_temp[1,3],
                       ConstituencyProvinceTerritoryName = member_temp[1,5],
                       CaucusShortName = vote_i_members$Caucus[j],
                       IsVoteYea = FALSE,
                       IsVoteNay = FALSE,
                       IsVotePaired = FALSE,
                       DecisionResultName = vote_data$DecisionResultName[1],
                       PersonId = vote_i_members$PersonId[j],
                       VoteId = 4411),
                     use.names=F)
}  

for(i in 2:nvotes){
  print(i)
  votetemp <- fread(paste0("./VoteData/vote_",i,"_data.csv"),drop=1)
  votetemp <- votetemp %>%
    mutate(VoteId = paste0(ParliamentNumber,
                           SessionNumber,
                           DecisionDivisionNumber)) %>%
    select(-c(ParliamentNumber,
              SessionNumber,
              DecisionDivisionNumber,
              PersonShortSalutation))
  vote_data <- rbind(vote_data, votetemp)
  
  vote_date <- as.POSIXct(unique(votetemp$DecisionEventDateTime))
  
  vote_i_members <- rbind(members_caucus %>% 
                            filter(EndDate ==""& vote_date > as.POSIXct(StartDate)),
                          members_caucus %>% 
                            filter(EndDate!="") %>% 
                            filter(as.POSIXct(StartDate) <= vote_date & vote_date <= as.POSIXct(EndDate)))
  
  vote_i_members <- vote_i_members[vote_i_members$PersonId %in% setdiff(vote_i_members$PersonId,votetemp$PersonId),]
  
  
  #if there are members who were employed but did not vote, add them as "Did not vote" 
  if(nrow(vote_i_members)>0){
    for(j in 1:nrow(vote_i_members)){
      
      member_temp <- fread(paste0("./Members/",vote_i_members$PersonId[j],".csv"),skip=1,fill=T,header=T,sep=",",select=c(1:5),nrows=1)
      vote_data <- rbind(vote_data,
                         data.frame(
                           DecisionEventDateTime = vote_date,
                           ConsituencyName = member_temp[1,4],
                           VoteValueName = "Did not vote",
                           PersonOfficialFirstName = member_temp[1,2],
                           PersonOfficialLastName = member_temp[1,3],
                           ConstituencyProvinceTerritoryName = member_temp[1,5],
                           CaucusShortName = vote_i_members$Caucus[j],
                           IsVoteYea = FALSE,
                           IsVoteNay = FALSE,
                           IsVotePaired = FALSE,
                           DecisionResultName = vote_data$DecisionResultName[1],
                           PersonId = vote_i_members$PersonId[j],
                           votetemp$VoteId[1]),
                         use.names=F)
    }  
  }
}


#combine casewhen adn decisioneventdatetime

# 25524 pierre
# 59110 bergen September 10, 2022
#  toole February 2, 2022 ended 72773
# green after November 19 2022 2897

vote_data <- vote_data %>%
  mutate(IsLeader = case_when(DecisionEventDateTime < "2022-02-02" & PersonId %in%c(58733,104669,71588,72773)~1,
                              DecisionEventDateTime < "2022-02-02" & PersonId %not_in%c(58733,104669,71588,72773)~0,
                              DecisionEventDateTime >= "2022-02-02" & DecisionEventDateTime < "2022-09-10" & PersonId %in%c(58733,104669,71588,59110)~1,
                              DecisionEventDateTime >= "2022-02-02" & DecisionEventDateTime < "2022-09-10" & PersonId %not_in%c(58733,104669,71588,59110)~0,
                              DecisionEventDateTime >= "2022-09-10" & DecisionEventDateTime < "2022-11-19" & PersonId %in%c(58733,104669,71588,25524)~1,
                              DecisionEventDateTime >= "2022-09-10" & DecisionEventDateTime < "2022-11-19" & PersonId %not_in%c(58733,104669,71588,25524)~0,
                              DecisionEventDateTime >= "2022-11-19" & PersonId %in%c(2897,58733,104669,71588,25524)~1,
                              DecisionEventDateTime >= "2022-11-19" & PersonId %not_in%c(2897,58733,104669,71588,25524)~0))

vote_data$IsLeader <- as.logical(vote_data$IsLeader)  
vote_data <- vote_data %>%
  group_by(VoteId, CaucusShortName) %>%
  mutate(
    LeaderAgree = ifelse(as.logical(IsLeader) | any(IsLeader),
                         ifelse(VoteValueName=="Paired",
                                NA,
                                VoteValueName == VoteValueName[as.logical(IsLeader)]
                         ),
                         NA),
    LeaderAgree = as.integer(LeaderAgree),
    Participated = ifelse(VoteValueName=="Did not vote",0,1)
  ) %>%ungroup()


write.csv(vote_data,"vote_data.csv")

members_caucus <- inner_join(vote_data %>% group_by(PersonId,CaucusShortName) %>% summarise(VotesParticipated = sum(Participated),
                                                                                            Percentage_Whipped = round(100*mean(LeaderAgree,na.rm=T),1)),
                             members_caucus, by=c("PersonId"="PersonId","CaucusShortName"="Caucus"))

members_caucus[which(is.na(members_caucus$Percentage_Whipped)),]$Percentage_Whipped <- 0

write.csv(members_caucus,"members_caucus.csv")

Metadata_votes <- fread("./VoteData/MetaDataVotes.csv",drop=1)
Metadata_votes$Link <- paste0("https://www.ourcommons.ca/Members/en/votes/44/1/",Metadata_votes$DecisionDivisionNumber)
write.csv(Metadata_votes,"./VoteData/MetaDataVotes.csv")

# Get pictures of members from parliament website
# Pictures have the form LastNameFirstName_ShortCaucus
# but there are a few members with name or party changes that affect
# their url. need to download those manually. 

Member_pic <- vote_data %>% select(PersonOfficialLastName,
                     PersonOfficialFirstName,
                     CaucusShortName,
                     PersonId) %>%
  mutate(CaucusShortName = case_when(CaucusShortName == "Liberal"~"LIB",
                                     CaucusShortName == "Conservative"~"CPC",
                                     CaucusShortName == "Independent"~"Ind",
                                     CaucusShortName == "Bloc Québécois"~"BQ",
                                     CaucusShortName == "Green Party"~"GP",
                                     CaucusShortName == "NDP"~"NDP"
                                     )) %>%
  mutate(PersonOfficialLastName = gsub("'", '', PersonOfficialLastName),
         PersonOfficialFirstName = gsub("'", '', PersonOfficialFirstName),
         PersonOfficialLastName = gsub(" ", '', PersonOfficialLastName),
         PersonOfficialFirstName = gsub(" ", '', PersonOfficialFirstName)) %>%
  mutate(Pic_URL = paste0(PersonOfficialLastName,PersonOfficialFirstName,"_",CaucusShortName)) %>%
  select(Pic_URL,PersonId) %>%
  distinct(Pic_URL,PersonId)

Member_pic<-Member_pic[-c(339),] # Remove alain rayes ind, since his photo was taken as a CPC
#Some members that need edits to their urls. 
Member_pic$Pic_URL[12] <- "ArseneaultRené_Lib"
Member_pic$Pic_URL[65] <- "ChongMichaelD_CPC"
Member_pic$Pic_URL[107] <- "FindlayKerryLynne_CPC"
Member_pic$Pic_URL[119] <- "GaronJeanDenis_BQ"
Member_pic$Pic_URL[121] <-"GaudreauMarieHélène_BQ"
Member_pic$Pic_URL[142] <-"HussenAhmedD_Lib"
Member_pic$Pic_URL[168] <-"LalondeMarieFrance_Lib"
Member_pic$Pic_URL[190] <- "MacDonaldHeath_CPC"
Member_pic$Pic_URL[191] <-"MacGregorAllistair_NDP"
Member_pic$Pic_URL[192] <- "MacKenzieDavid_CPC"
Member_pic$Pic_URL[204] <- "McGuintyDavidJ._Lib"
Member_pic$Pic_URL[208] <- "McLeodMichaelV_Lib"
Member_pic$Pic_URL[220] <- "MorrisseyRobertJ._Lib"
Member_pic$Pic_URL[234] <- "PaulHusPierre_CPC"
Member_pic$Pic_URL[246] <- "RempelMichelle_CPC"
Member_pic$Pic_URL[256] <- "SajjanHarjit_Lib"
Member_pic$Pic_URL[266] <- "SgroJudyA_Lib"
Member_pic$Pic_URL[274] <- "SinclairDesgagnéNathalie_BQ"
Member_pic$Pic_URL[281] <- "SteMarieGabriel_BQ"
Member_pic$Pic_URL[296] <- "TurnbullRyan_CPC"
Member_pic$Pic_URL[328] <- "BarsalouDuvalXavier_BQ"
Member_pic$Pic_URL[333] <- "SavardTremblaySimonPierre_BQ"
Member_pic$Pic_URL[339] <- "RayesAlain_CPC"


#Something to do with encoding is resulting in errors with names with special
# (accented) letters.
for(i in 1:nrow(Member_pic)){
    print(i)
    print(Member_pic$PersonId[i])
    member_pic_url <- paste0("https://www.ourcommons.ca/Content/Parliamentarians/Images/OfficialMPPhotos/44/",
                             Member_pic$Pic_URL[i],
                             ".jpg")
    download.file(member_pic_url,paste0("./Members/Photos/",Member_pic$PersonId[i],".jpg"),mode = 'wb')
}

#I hosted the members on my github so scraping photos isn't necessary for others
members_constituency$Pic_URL <- paste0("https://raw.githubusercontent.com/dgrant59/ParliamentVotes/main/Members/Photos/",
                                       members_constituency$PersonId,
                                       ".jpg")
write.csv(members_constituency,"members_constituency.csv")




# Code for potential geographical map

# ED <- st_read(dsn = "./ElectoralDistrictMaps/ElectoralDistricts_Simplified.shp",options = "ENCODING=WINDOWS-1252") 
# ED <- ED %>% arrange(FEDENAME)
# ED <- ED %>% mutate(FEDENAME = gsub("--","—",FEDENAME),
#                     FEDFNAME = gsub("--","—",FEDFNAME),
#                     FEDNAME = gsub("--","—",FEDNAME))
# 
# 
# 
# members <- read.csv("./members.csv")
# members <- members %>% arrange(Constituency)
# members <- members %>% mutate(Constituency = gsub("--","—",Constituency))
# 
# 
# member_ED <- stringdist_join(ED,members,by=c("FEDENAME"="Constituency"),
#                       mode="left",method="jw",max_dist=0.02,distance_col="dist") %>%
#   group_by(FEDENAME) %>% slice_min(order_by=dist,n=1)
# 
# 
# no_member_ED <- ED[which(ED$FEDUID%in%setdiff(ED$FEDUID,member_ED$FEDUID)),]
# 
# ok <- bind_rows(data.frame(no_member_ED),data.frame(member_ED))
# ok$geometry <- c(no_member_ED$geometry, member_ED$geometry)
# ok <- st_as_sf(ok)
# ok <- ok %>% mutate(Name = paste0(Honorific.Title,
#                                   First.Name,
#                                   " ",
#                                   Last.Name)) %>% 
#   relocate(Name,.after=PRUID) %>% select(-c(Honorific.Title,
#                                                             First.Name,
#                                                             Last.Name))
#                                  #                        PersonOfficialFirstName,
#                                  #                        " ",
#                                  #                        PersonOfficialLastName))
# View(ok %>% select(FEDENAME,Constituency,dist))
# ED2 <- stringdist_join(ED,members_constituency,by=c("FEDENAME"="ConstituencyName"),
#                        mode="full",method="jw",max_dist=0.99,distance_col="dist") %>%
#   group_by(FEDENAME) %>% slice_min(order_by=dist,n=1) %>% ungroup()
# ED2 <- ED2 %>% select(-c(FEDNAME,FEDENAME,FEDFNAME))
# ED2 <- ED2[-c(184),]
# st_write(ED2,"./ElectoralDistrictMaps/EDPersonId.shp",layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
