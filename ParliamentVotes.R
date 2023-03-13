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


vote1 <- read.csv("./VoteData/vote_1_data.csv")

members_constituency <- vote1 %>% select(PersonId,ConstituencyName)

for(i in 2:nvotes){
  print(i)
  votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
  membertemp <- votetemp %>%
    select(PersonId,ConstituencyName)
  
  members_constituency <- full_join(members_constituency,membertemp,by=c("PersonId","ConstituencyName"))
  
  
}
members_constituency <- rbind(members_constituency,data.frame(PersonId=25452,
                                                              ConstituencyName = "Nipissing—Timiskaming"))
#members_constituency <- members_constituency[-184,]
write.csv(members_constituency,"members_constituency.csv")


ED2 <- stringdist_join(ED,members_constituency,by=c("FEDENAME"="ConstituencyName"),
                             mode="full",method="jw",max_dist=0.99,distance_col="dist") %>%
  group_by(FEDENAME) %>% slice_min(order_by=dist,n=1) %>% ungroup()
ED2 <- ED2 %>% select(-c(FEDNAME,FEDENAME,FEDFNAME))
ED2 <- ED2[-c(184),]
st_write(ED2,"./ElectoralDistrictMaps/EDPersonId.shp",layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

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

#add the speaker of the house, Anthony Rota (doesn't vote)
members_tenure <- rbind(members_tenure, data.frame(PersonId=25452,
                                                   StartDate="2021-09-20 12:00:00 AM",
                                                   EndDate=NA))
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
members_caucus <- rbind(members_caucus,data.frame(PersonId=88600,
                                                  StartDate="2021-09-20 12:00:00 AM",
                                                  EndDate="2022-09-13 1:05:00 PM",
                                                  Caucus="Conservative"))

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
                     data.frame(.id = "VoteParticipant",
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


# data.frame(.id = NULL,
#            DecisionEventDateTime = NULL,
#            ConsituencyName = NULL,
#            VoteValueName = NULL,
#            PersonOfficialFirstName = NULL,
#            PersonOfficialLastName = NULL,
#            ConstituencyProvinceTerritoryName = NULL,
#            CaucusShortName = NULL,
#            IsVoteYea = NULL,
#            IsVoteNay = NULL,
#            IsVotePaired = NULL,
#            DecisionResultName = NULL,
#            PersonId = NULL,
#            VoteId = NULL)


  
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
  
  
  
  if(nrow(vote_i_members)>0){
    for(j in 1:nrow(vote_i_members)){
      
      member_temp <- fread(paste0("./Members/",vote_i_members$PersonId[j],".csv"),skip=1,fill=T,header=T,sep=",",select=c(1:5),nrows=1)
      vote_data <- rbind(vote_data,
                         data.frame(.id = "VoteParticipant",
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











# get affiliation for each member
# members$
# for(i in 1:nrow(members)){
#   temp <- read.csv(paste0("./Members/",members$PersonId[i],".csv"),skip=1,header=T)
# }



vote1 <- read.csv("./VoteData/vote_1_data.csv")
vote_data <- vote1 %>%
  mutate(VoteId = paste0(ParliamentNumber,
                         SessionNumber,
                         DecisionDivisionNumber)) %>%
  select(-c(ParliamentNumber,
            SessionNumber,
            DecisionDivisionNumber,X))

for(i in 2:nvotes){
  print(i)
  votetemp <- read.csv(paste0("./VoteData/vote_",i,"_data.csv"))
  votetemp <- votetemp %>%
    mutate(VoteId = paste0(ParliamentNumber,
                           SessionNumber,
                           DecisionDivisionNumber)) %>%
    select(-c(ParliamentNumber,
              SessionNumber,
              DecisionDivisionNumber,X))
  vote_data <- rbind(vote_data, votetemp)
}
# vote_data <- vote_data %>% pivot_wider(names_from = PersonId,values_from = VoteValueName)

write.csv(vote_data,"vote_data.csv")



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

Member_pic$Pic_URL[246] <- "RempelMichelle_CPC"
Member_pic$Pic_URL[12] <- "ArseneaultRené_Lib"
Member_pic$Pic_URL[65] <- "ChongMichaelD_CPC"
Member_pic$Pic_URL[107] <- "FindlayKerryLynne_CPC"
Member_pic$Pic_URL[119] <- "GaronJeanDenis_BQ"

# 121"GaudreauMarieHélène_BQ"
# 142 "HussenAhmedD_Lib"
# 168"LalondeMarieFrance_Lib"
# 190 "MacDonaldHeath_CPC"
# 192 "MacKenzieDavid_CPC"
# 204 "McGuintyDavidJ._Lib"
# 208 "McLeodMichaelV_Lib"
# 220 "MorrisseyRobertJ._Lib"
# 234 "PaulHusPierre_CPC"
# 256 "SajjanHarjit_Lib"
# 266 "SgroJudyA_Lib"
# 274 "SinclairDesgagnéNathalie_BQ"
# 281 "SteMarieGabriel_BQ"
# 296 "TurnbullRyan_CPC"
# 328 "BarsalouDuvalXavier_BQ"
# 333 "SavardTremblaySimonPierre_BQ"
# 339 "RayesAlain_CPC"
Member_pic$Pic_URL[191] <-"MacGregorAllistair_NDP"
for(i in 340:nrow(Member_pic)){
    print(i)
    member_pic_url <- paste0("https://www.ourcommons.ca/Content/Parliamentarians/Images/OfficialMPPhotos/44/",Member_pic$Pic_URL[i],".jpg")
    download.file(member_pic_url,paste0("./Members/Photos/",i,".jpg"),mode = 'wb')
}

file.rename(paste0("./Members/Photos/",list.files(path="./Members/Photos/",pattern="*.jpg")), paste0("./Members/Photos/",Member_pic$PersonId,".jpg"))
library(base64enc)
base64encode()


members_constituency$Pic_URL <- paste0("https://raw.githubusercontent.com/dgrant59/ParliamentVotes/main/Members/Photos/",members_constituency$PersonId,".jpg")
write.csv(members_constituency,"members_constituency.csv")
