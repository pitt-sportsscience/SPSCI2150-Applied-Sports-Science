# CLEAN THE ENVIRONMENT
rm(list = ls())

# import relevant packages
library(dplyr)
library(ggplot2)
library(data.table)
library(forcats)
library(plyr)
library(tidyr)
library(stringr)
library(pals)

#define positions
S <- c("Akeo", "Faribanks")
RS <- c("Buzzerio", "Posada") 
L <- c("Klika", "Browske", "Griffin")
MB <- c("Jepsen", "Nwokolo", "Gray", "Starks")
OH <- c("Flood", "Dalton", "Gomez", "Cam", "Ennis")

##color package
pitt_fill_colors = c("#003594", "#FFB81C", "grey51", 'black', 'brown')

# figure aesthetics
aesthetics <- list(theme(axis.title = element_text(face = "bold"),
                         panel.background = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.line = element_line(color = "black"),
                         legend.position = "none"))

# Vertec data first
root_dir <- getwd()

# import jump data from Hawkin

# LIBRARIES
for (package in c('data.table',
                  'jsonlite',
                  'httr')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# EXPORT FUNCTION
data_from_kinduct = function(clientname,
                             client_id,
                             client_secret,
                             start_date, 
                             end_date,
                             segments = NULL,
                             leagues = NULL,
                             organizations = NULL,
                             metrics = NULL,
                             na_strings = NULL
){
  
  # Generate NA strings
  if(!is.null(na_strings)){
    na_strings <- c("NA", " ", "",na_strings)
  }else{
    na_strings <- c("NA", " ", "")
  }
  
  ## Generating Bearer Token
  grant_type = "client_credentials"
  performanceData = data.frame(client_secret,client_id,grant_type)
  samplePost = POST(url = paste("https://",clientname,".kinduct.com/api/oauth/token", sep = "") ,body = performanceData, encode = "form")
  returnToken = (content(samplePost, "parsed"))
  accessToken = returnToken$access_token
  tokenType = returnToken$token_type
  accessKey <<- paste(tokenType,accessToken)
  
  ## Default metrics list
  metrics_default = c("Date", "Source", "Segment","User UID", "External UID", "Organization", "First Name", "Last Name", "Position", "League", "Alias")
  rename_default = c("date", "source", "segment", "user_uid", "external_uid", "organization", "first_name", "last_name", "position", "league", "alias")
  metrics_remove = c(metrics_default, "First name", "first name", "Last name", "last name", "date", "position")
  
  
  #### Generating Query
  if(is.null(start_date) | is.null(end_date)){
    stop("No date range defined. Date parameters cannot be NULL. Please define a start and end date.")
  } else{
    query = paste("https://",clientname,".kinduct.com/api/export/export_json?start_date=",start_date,"&end_date=",end_date,  sep = "")
    
    # League Filter
    if(!is.null(leagues) & length(leagues) == 1){
      query = paste(query, "&leagues=", leagues, sep = "")
    } else if(!is.null(leagues) & length(leagues) > 1) {
      join_leagues = paste(leagues, collapse = "|")
      query = paste(query, "&leagues=", join_leagues, sep = "")
    }
    
    # Segments Filter
    if(!is.null(segments) & length(segments) == 1){
      segments = gsub(" ", "%20", segments)
      query = paste(query, "&segments=", segments, sep = "")
    } else if(!is.null(segments) & length(segments) > 1) {
      segments = gsub(" ", "%20", segments)
      join_segments = paste(segments, collapse = "|")
      query = paste(query, "&segments=", join_segments, sep = "")
    }
    
    # Organizations Filter
    if(!is.null(organizations) & length(organizations) == 1){
      organizations = gsub(" ", "%20", organizations)
      query = paste(query, "&organizations=", organizations, sep = "")
    } else if(!is.null(organizations) & length(organizations) > 1) {
      organizations = gsub(" ", "%20", organizations)
      join_organizations = paste(organizations, collapse = "|")
      query = paste(query, "&organizations=", join_organizations, sep = "")
    }
  }
  
  
  # POST to get data
  if(is.null(metrics)){
    samplePOST = POST(url = query, body = NULL, add_headers(Authorization = accessKey, Connection = 'keep-alive'), encode="json")
  } else {
    samplePOST = POST(url = query, body = list(metrics = metrics), add_headers(Authorization = accessKey, Connection = 'keep-alive'), encode="json")
  }
  
  stop_for_status(samplePOST)
  x_list <<- (fromJSON(content(samplePOST,type="text", encoding = "UTF-8")))
  
  # Reformat the returned data
  data_list <- x_list$results
  if (length(data_list) == 0){
    print("No data in selected export. Returning an empty data frame.")
    kinduct_export <<- data.table()
  } else {
    dt_list <- lapply(data_list, function(x){
      # Get data frame of the metrics
      DF <- x[["metrics"]]
      # Get all metric names of current record
      record_metric <- DF$name
      idx_remove = which(record_metric %in% metrics_remove)
      if(length(idx_remove) > 0){
        record_metric = record_metric[-idx_remove]
      }
      
      # Set a list to extract all default metrics, which are
      # Date, Source, Segment, User UID, External UID, Organization, First Name, Last Name, Position, League
      record <- x[which(names(x) %in% metrics_default)]
      record = record[!sapply(record, is.null)]
      
      # for each metric in list - assign the corresponding value
      for (m in record_metric) {
        value <-  DF[DF$name == m, "value"]
        # Convert Na strings
        if(value %in% na_strings){
          record[[m]] <- NA
        }else{
          if(suppressWarnings(!is.na(as.numeric(value)))){
            record[[m]] <- as.numeric(value)
          }else{
            record[[m]] <- value
          }
        }
      }
      return(record)
    })
    
    # Combine all lists into data.table
    DT <- rbindlist(dt_list[!sapply(dt_list, is.null)], fill = T)
    
    # Remove columns when all values are nas
    DT <- DT[,which(unlist(lapply(DT, function(x)!all(is.na(x))))), with=F]
    
    
    # Final formatting - return kinduct_export as a data.frame
    kinduct_export = data.table(DT)
    idx_rename = which(metrics_default %in% names(kinduct_export))
    setnames(kinduct_export, metrics_default[idx_rename], rename_default[idx_rename])
    setcolorder(kinduct_export, c(rename_default[idx_rename], setdiff(names(kinduct_export), rename_default[idx_rename])))
    kinduct_export = kinduct_export[order(last_name, first_name, date)]
    kinduct_export <<- as.data.frame(kinduct_export)
  }
}


# Import wellness data from Kinduct
df <- data_from_kinduct(clientname = "pittsburghpanthers",
                        client_id = "kkc701nm37s2qaph",
                        client_secret = "umf1wyu98odkhalf",
                        start_date = Sys.Date() - 800, 
                        end_date = Sys.Date(),
                        organization = "4oj7b9nownvocc8n") %>% #volleyball ID
  filter(external_uid != "athlete_test") %>%
  filter(grepl("Hawkin", source)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         Name = stringr::str_c(first_name, " ", last_name),
         BW = `System Weight(N)` * 0.2248090795) #N to lbs

df_clean <- df %>%
  filter(grepl("Countermovement Jump", segment)) %>%
  filter(date > "2021-01-01") %>%
  distinct(date, last_name, .keep_all = TRUE) %>%
  select(`Avg. Braking Force(N)`, `Avg. Braking Power(W)`,
       `Avg. Braking Velocity(m/s)`,`Braking RFD(N/s)`,
       `Braking Phase(s)`, `Relative Braking Net Impulse(N.s/kg)`,
       `Relative Braking Impulse(N.s/kg)`, `Braking Impulse(N.s)`, `Braking Net Impulse(N.s)`,
       `Peak Relative Braking Power(W/kg)`, `Peak Relative Braking Force(%)`,
       `L|R Avg. Braking Force(%)`, `Right Avg. Braking RFD(N/s)`, `Left Avg. Braking RFD(N/s)`,
       `Peak Braking Force(N)`, `Left Avg. Braking RFD(N/s)`, `Right Avg. Braking Force(N)`,
       `Left Force at Peak Braking Force(N)`, `Right Force at Peak Braking Force(N)`,
       `L|R Peak Braking Force(%)`, `L|R Braking Impulse Index(%)`, `L|R Avg. Braking RFD(%)`)

library(corrplot)

df_num_clean <- df_clean %>%
  select_if(is.numeric) %>%
  na.omit()

df_num_clean_cor <- cor(df_num_clean)

setwd('/Users/fsp5/OneDrive - University of Pittsburgh/work/athletics/sports_science/mentorship/teaching/lectures/force_plates')

png('Hawkin_colinearity.png',
    width = 7, height = 5,
    units = "in", res = 300)
corrplot(df_num_clean_cor,
         col= colorRampPalette(c(pitt_fill_colors[1], "white", pitt_fill_colors[2]))(100),
         method = "color",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         addrect = 4)
dev.off()

library(factoextra)
pca <- prcomp(df_num_clean, center = TRUE, scale = TRUE)

fviz_screeplot(pca)
fviz_

fviz_pca_biplot(pca,
                alpha.ind = "contrib",
                #alpha.var = "contrib",
                col.ind = "contrib",
                #col.var = "contrib",
                invisible = "ind",
                labelsize = 3, 
                repel = TRUE,
#                geom = "arrow",
                label = "var") +
  ggtitle(" ") +
  theme(axis.line = element_line(color = "black"),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggsave('pca_biplot.png',
       width = 6, height = 5,
       units = "in", dpi = 300)

pc1 <- as.matrix(pca$rotation[,1])
pc2 <- as.matrix(pca$rotation[,2])
pc3 <- as.matrix(pca$rotation[,3])

library(psych)
KMO(df_num_clean_cor)

rotations <- bind_cols(pc1, pc2)
colnames(rotations) <- c("PC1", "PC2")
rownames(rotations) <- rownames(pc1)

rotations_sorted <- rotations %>%
  mutate(names = rownames(rotations)) %>%
  arrange(PC1, decreasing = TRUE)

rotations_sorted_short <- rotations_sorted %>% select(-names)

rownames(rotations_sorted_short) <- rotations_sorted$names


png('force_pca_loading.png',
    width = 4,
    height = 3,
    units = "in",
    res = 300)
corrplot(as.matrix(rotations_sorted_short), 
         method = "color", 
         is.corr = FALSE,
         tl.cex = 0.6,
         cl.pos = "r",
         cl.cex = 0.6,
         cl.offset = 0.5,
         cl.ratio = 2,
         cl.align.text = 'c',
         col= colorRampPalette(c(pitt_fill_colors[1], "white", pitt_fill_colors[2]))(100),
         tl.col = "black")
dev.off()



