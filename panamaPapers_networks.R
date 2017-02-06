# rm(list=ls())
setwd("~/Dropbox/eliteNetworks/data")

library(foreign)
library(dplyr)
library(reshape2)
library(foreach)
library(doParallel)
registerDoParallel(cores=3)  # register no. cores for parallelization
library(network)
library(sna)
library(lmtest)
library(sandwich)
library(KRLS)


###
# PP: Edges: Give same node ID to duplicate entities
###

edges <- read.csv("all_edges.csv", header=T)  # contains all types of edges (directed) between all entities/officers/intermediaries/addresses

# NOTE: 3,146 obs correspond to entities w multiple node_1/node_2 IDs, denoted by rel_type=="same name and registration date as"-- possible that 2 officers connected, but one tied to node_1 ID, other to node_2 ID of same entity; recode entity IDs to avoid this 
# NOTE: 622 obs correspond to  edges$rel_type=="related entity" -- L: diff incorp_date for node_1 & node_2, unclear which correct

problNodes <- edges$node_1[edges$rel_type=="same name and registration date as"][edges$node_1[edges$rel_type=="same name and registration date as"] %in% edges$node_2[edges$rel_type=="same name and registration date as"]]  # 7 unique entities (30 obs) where i=j & j=i or i=j & j=k (contain vice-versa too)
edges <- edges[!(edges$node_1 %in% problNodes | edges$node_2 %in% problNodes),]  # drop edges w above entities; L: imperfect
duplNodes <- subset(edges, rel_type=="same name and registration date as", select=c(node_1,node_2))  # IDs for recoding
for(i in 1:nrow(duplNodes)){  
  edges$node_2[edges$node_2==duplNodes$node_1[i]] <- duplNodes$node_2[i]  # give id of rhs entity to all rhs obs of lhs entity  
}


###
# PP: Edges: Drop irrelevant ties
###

## Find irrelevant types of ties & drop respective edges 
irrelevTies <- c("Alternate Director of", "Auditor of", "Assistant Secretary of", "Auth. Representative of", "Authorised Person / Signatory of", "Authorized signatory of", "Bank Signatory of", "Board Representative of", "Co-Trustee of Trust of", "Custodian of", "Correspondent Addr. of", "General Accountant of", "Grantee of a mortgage of", "intermediary of", "Investment Advisor of", "Legal Advisor of", "Nominated Person of", "Nominee Beneficial Owner of", "Nominee Beneficiary of", "Nominee Director of", "Nominee Investment Advisor of", "Nominee Name of", "Nominee Protector of", "Nominee Secretary of", "Nominee Shareholder of", "Nominee Trust Settlor of", "Officer of", "Partner of", "President of", "Records & Registers of", "Register of Director of", "Register of Shareholder of", "registered address", "related entity", "Reserve Director of", "Resident Director of", "Safekeeping of", "same address as", "same name and registration date as", "similar name and address as", "Stockbroker of","Tax Advisor of", "Treasurer of", "Trustee of Trust of", "Unit Trust Register of", "Vice President of")
edges <- edges[!(edges$rel_type %in% irrelevTies), ]
# NOTE: drop 960 obs w rel_type=="same address as" bc their node_1 & node_2 IDs don't correspond to any officers$node_id nor entities$node_id 
# NOTE: 46,761 obs correspond to officers w multiple node_1/node_2 IDs, denoted by edges$rel_type=="similar name and address as"; ~all of these IDs match to some officers$node_id, so officer info will be merged-in; collapse officers names later 


###
# PP: Edges: Drop frontmen & duplicate edges
### 

## Find no. times each node holds each type of tie, drop nodes that are directors or secretaries for too many entities -- TODO: check that no frontmen left at end of officersEntities prep -- RC: increase number of ties allowed
nodeTieFreq <- edges %>%
  group_by(node_1, rel_type) %>%
  summarize(freq=length(node_1))
frontMen <- nodeTieFreq$node_1[(nodeTieFreq$rel_type=="Director of" | nodeTieFreq$rel_type=="Protector of" | nodeTieFreq$rel_type=="Secretary of" | nodeTieFreq$rel_type=="Trust Settlor of") & nodeTieFreq$freq>5]  # types of frontmen
edges <- edges[!(edges$node_1 %in% frontMen),]  # drop frontmen


# Remove any node1-node2 edges w more than one type of edge (e.g. x is shareholder y & x is director of y) (also removes 31 exact duplicates)
edges <- edges[!duplicated(edges[,-2]),]  
# NOTE: if an officer has multiple ties to same company and exclude some bc they are irrelevant, any relevant ones will still be included (e.g. a shareholder to b, a secretary of b)


###
# PP: Officers: Merge officer info
###

officers <- read.csv("Officers.csv")  # all recorded info for all officers
officersEntities <- merge(edges, officers, by.x="node_1", by.y="node_id", all.x=T)  # merge in officer name, country, icij_id (node_id as merge var)
names(officersEntities)[names(officersEntities)=="node_1"] <- "officerID"  # rename officer ID var
names(officersEntities)[names(officersEntities)=="node_2"] <- "entityID"  # rename node ID var
names(officersEntities)[names(officersEntities)=="name"] <- "officerName"  # rename name var
names(officersEntities)[names(officersEntities)=="countries"] <- "officerCountry"  # rename country var
officersEntities <- subset(officersEntities, select=-c(country_codes, valid_until, sourceID))  # drop redundant vars


###
# PP: Officers: Drop problem officers
###

officersEntities$officerName <- as.character(officersEntities$officerName)  # for speed
officersEntities <- officersEntities[!is.na(officersEntities$officerName),]  # drop officers w missing name (also miss node_id & icij_id)
officersEntities$officerName <- tolower(officersEntities$officerName)  # make letters lower case
officersEntities <- officersEntities[!(officersEntities$officerName %in% c("el portador", "bear", "bearer", "the  bearer", "the bearer", " the bearer", "beareer", "the bearar", "the baerer", "to the bearer", "al portador", "elportador", "bearer1", "bearer 01", "bearer 1", "bearer2", "bearer 2", "???")),]  # drop bearer officers
# NOTE: missing/NA icij_id's given to some officers w names that appear to be companies (eg International Administartors Limited), bearers (eg The Bearer), or agents (eg Matthew Charles Stokes); BUT some such companies given multiple unique icij_id's & some bearer officers given unique icij_id --> can't drop these companies or agents (hard to detect), but can drop diff bearer officers


###
# PP: Officers: Impute officer country
###

# RC: drop all officers w/o country info

officersEntities$officerCountry <- as.character(officersEntities$officerCountry)  # for speed

# Names of officers w missing country that have synonymous officers w non-missing country
impCountryName <- sort(unique(officersEntities$officerName[officersEntities$officerCountry=="" & officersEntities$officerName %in% officersEntities$officerName[officersEntities$officerCountry!=""]]))
# NOTE: tens of thousands entries w/o country info but w name (checked that none of their icij_id's correspond to other entries w country info) 

## DF to impute country w modal country of synonymous officer/s
impCountry <- officersEntities %>%  
  filter(officerName %in% impCountryName) %>%  # only look at officers that can be imputed
  group_by(officerName) %>%  # 1 row per officer
  summarize(countryImp=list(names(table(officerCountry[!officerCountry==""]))),  # list all country associated w officer name (drop blank)  
            countryImp=countryImp[[1]][1])  # keep modal country associated w officer name

## Impute country w modal country of synonymous officer/s (if any) 
for(i in 1:length(impCountryName)){
  officersEntities$officerCountry[officersEntities$officerCountry=="" & officersEntities$officerName==impCountryName[i]] <- impCountry$countryImp[i]  # only gives country to officers w/o info, preserves country of synonymous officers w diff country 
}

officersEntities <- officersEntities[!officersEntities$officerCountry=="",]  # drop officers w missing country


###
# PP: Entities: Merge entity info
###

entities <- read.csv("Entities.csv")

## Drop problem entities
entities <- entities[!entities$note=="This is not an offshore entity even though it was logged as such in original database ICIJ received. It isi an internal account created by the agent to record miscellaneous charges of an officer or intermediary",]  # drop ~8k that aren't really offshores

## Gen incorporation, inactivation & struckoff year
entities <- entities[!entities$incorporation_date=="",]  # drop 434 missing incorp. date -- NOTE: only 3 have same name as an entity w non-missing incorporation_date; ignore match
entities$incorporation_date <- as.Date(entities$incorporation_date, format="%d-%b-%Y")  # covert to date object
entities$incorpYr <- as.numeric(substr(entities$incorporation_date, start=1, stop=4))  # gen incorporation yr
entities$inactivation_date <- as.Date(entities$inactivation_date, format="%d-%b-%Y")  # covert to date object
entities$inactYr <- as.numeric(substr(entities$inactivation_date, start=1, stop=4))  # gen incorporation yr
entities$struck_off_date <- as.Date(entities$struck_off_date, format="%d-%b-%Y")  # covert to date object
entities$struckoffYr <- as.numeric(substr(entities$struck_off_date, start=1, stop=4))  # gen incorporation yr


## Merge-in entity info
officersEntities <- merge(officersEntities, entities, by.x="entityID", by.y="node_id", all.x=T)  # merge-in entity info
names(officersEntities)[names(officersEntities)=="name"] <- "entityName"  # rename entity name var
names(officersEntities)[names(officersEntities)=="countries"] <- "entityCountry"  # rename entity country var


# save(officersEntities, file="officersEntities.RData")


###
# GWF: Create TSCS
###

## Import gwf tcsc & merge correct format dates
gwftscs <- read.dta("GWFtscs2015.dta")  # latest gwf TSCS data from coups paper
gwftscs <- subset(gwftscs, select=-c(gwf_startdate, gwf_enddate))  # drop startdate, enddate; wrong format
load("gwf_cases2015.RData")  # latest gwf regime cases data from coups paper; date format recoded (see repression_monthly.R gwf section)
gwf_cases <- subset(gwf_cases, select=c(gwf_casename, gwf_startdate, gwf_enddate, gwf_startyr, gwf_endyr))  # keep startdate, enddate, startyr, endyr; correct format
gwftscs <- merge(gwftscs, gwf_cases, all.x=T)  # merge-in correct format dates
rm(gwf_cases)  # remove gwf_cases from memory

gwftscs <- gwftscs[gwftscs$year>1989,]  # drop pre-90 obs

## Recode gwf country names that don't match pp country names (only post-89 regimes)
gwftscs$country <- gwftscs$gwf_country  # create new country names var for merging
gwftscs$country[gwftscs$countries=="Cen African Rep"] <- "Central African Republic"
gwftscs$country[gwftscs$countries=="Guinea Bissau"] <- "Guinea-Bissau"
gwftscs$country[gwftscs$countries=="Congo/Zaire"] <- "DR Congo"
gwftscs$country[gwftscs$countries=="Ivory Coast"] <- "Côte d'Ivoire"


###
# PP: TSCS: country-year-officerName w entity list
###

years <- seq(1990,2015)  # years for tscs

## Collapse by country-officerName
# NOTE: synonymous officers from same country treated as same even if diff icij_id (or one or both missing icij_id)
t_oe <- system.time({
  for(i in 1:length(years)){
    oe_tscs[[i]] <- officersEntities %>%
      filter(officerCountry %in% gwftscs$country[gwftscs$year==years[i]]) %>%  # only officers in dicts at t
      filter(incorpYr<years[i]) %>%
      # filter(node_2 %in% entities$node_id[entities$incorpYr<years[i]]) %>%  # only entities formed <t
      group_by(officerCountry, officerName) %>%
      summarize(entities=list(unique(entityID)))  # some duplicate icij_id's have diff entities; unlist entities to retain unique in each list, then list together
  }
})

# save(oe_tscs, file="oe_tscs.RData")


###
# PP: TSCS: Networks
###

networks_tscs <- vector("list", length=length(years))  # empty year list

#### Create network TSCS
networks_tscs <- foreach(t=1:length(years), .packages=c('reshape2','network')) %dopar% {  # parallelize year loop; load packages used in loop (can't search globally)
  
  ## List for countries in t
  countries_t <- gwftscs$country[gwftscs$year==years[t]]  # names of countries in t
  networks_t <- vector("list", length(countries_t))  # empty country list for t
  names(networks_t) <- countries_t  # list's elements = countries
  
  ### Create networks forall countries in t
  for(k in 1:length(countries_t)){  # loop thru countries in t
    ## Create list of tk's officers tied to same entities 
    oe_tk <- subset(oe_tscs[[as.character(years[t])]], officerCountry==countries_t[k]) # subset officer-entities (list) for t then k
    connections_tk <- lapply(oe_tk$entities, function(y){oe_tk$officerName[lapply(oe_tk$entities, function(x){any(unlist(x) %in% unlist(y))})==T]})  # unlist within function to ensure all entities of officers x & y checked for intersection --- NB: slowest part of code
    names(connections_tk) <- oe_tk$officerName  # list elements = officers
    
    ## Create tk network 
    networks_t[[k]] <- network.initialize(n=nrow(oe_tk), directed=F)  # network object w no. nodes = officers
    network.vertex.names(networks_t[[k]]) <- names(connections_tk)  # node name = officerName
    
    ## Add edges to tk network
    if(!all(connections_tk==names(connections_tk))){  # only if >0 connections in tk
      edgeList_tk <- as.matrix(melt(connections_tk))  # tk edgelist from tk connections list
      networkii_tk <- network(edgeList_tk, directed=F)  # tk network object from tk edgelist; contains i-i edges
      networks_t[[k]] <- network(as.matrix(networkii_tk), directed=F)  # remove i-i edges by converting tk network to tk matrix, then back to tk network object; assign to k position in 
    }
  }
  networks_t  # output of each iter of foreach loop;iu assigned to networks[[t]] bc of vectorization
}
names(networks_tscs) <- years  # list's elements = years; outside foreach loop bc latter nullifies names(networks)


# save(networks_tscs, file='networks_tscs.RData')


####
# PP: TSCS: Network Stats
###

### DF: network-level stats
t_netStats <- system.time({
  netStats_tscs <- data.frame(country=melt(sapply(networks_tscs, names))$value,
                              year=melt(sapply(networks_tscs, names))$L1,
                              nodes=melt(sapply(networks_tscs, function(x){sapply(x,function(y){network.size(y)})}))$value,  # no. nodes
                              edges=melt(sapply(networks_tscs, function(x){sapply(x,function(y){network.edgecount(y)})}))$value,  # no. edges 
                              dyads=melt(sapply(networks_tscs, function(x){sapply(x,function(y){network.dyadcount(y)})}))$value,  # no. dyads
                              transitivity=melt(sapply(networks_tscs, function(x){sapply(x,function(y){gtrans(y,mode="graph",diag=T)})}))$value,  # transitivity
                              density=melt(sapply(networks_tscs, function(x){sapply(x,function(y){network.density(y)})}))$value,  # tie density
                              degreeCent=melt(sapply(networks_tscs, function(x){sapply(x,function(y){centralization(y,degree,mode="graph")})}))$value,  # degree centrality
                              eigenCent=melt(sapply(networks_tscs, function(x){sapply(x,function(y){centralization(y,evcent,mode="graph")})}))$value,  # eigenvector centrality
                              betweenCent=melt(sapply(networks_tscs, function(x){sapply(x,function(y){centralization(y,betweenness,mode="graph")})}))$value,  # betweenness centrality
                              stressCent=melt(sapply(networks_tscs, function(x){sapply(x,function(y){centralization(y,stresscent,mode="graph")})}))$value,
                              nonIsolateFrac=melt(sapply(networks_tscs, function(x){sapply(x,function(y){1-length(isolates(y))/network.size(y)})}))$value,  # fraction of nodes that aren't isolates
                              diffuseFrac=melt(sapply(networks_tscs, function(x){sapply(x, function(y){mean((degree(y,gmode="graph")+1)/network.size(y))})}))$value
  )
})
netStats_tscs[netStats_tscs$nodes<2, 6:ncol(netStats_tscs)] <- NA  # for countries w empty/singleton networks all stats=NA

# save(netStats_tscs, file="netStats_tscs.RData")


###
# END
###