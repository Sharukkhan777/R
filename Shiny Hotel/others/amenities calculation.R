# read csv file
df_ame <- read.csv("others/allAmenitySouth.csv")

deg2rad <- function(deg) {
  return(deg * (pi/180))
}

getDistanceFromLatLonInKm <- function(lat1,lon1,lat2,lon2) {
  R = 6371 # Radius of the earth in km
  dLat = deg2rad(lat2-lat1)  # deg2rad below
  dLon = deg2rad(lon2-lon1) 
  a = 
    sin(dLat/2) * sin(dLat/2) +
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * 
    sin(dLon/2) * sin(dLon/2)
  
  c = 2*atan2(sqrt(a), sqrt(1-a)) 
  d = R * c # Distance in km
  return(d)
}
# getDistanceFromLatLonInKm(12.7881765,80.2054503,12.8470659,80.2077105)


get_table_amenity <- function(lat1, lon1){
  # count the amenity
  count025 = 0
  count05 = 0
  count1 = 0
  count2 = 0
  count5 = 0
  
  
  ame_025 = c()
  ame_05 = c()
  ame_1 = c()
  ame_2 = c()
  ame_5 = c()
  # lat1 = 13.0783831
  # lon1 = 80.2680937
  
  for (i in 1:dim(df_ame)[1]){
    
    lat2 = df_ame$lattitude[i]
    lon2 = df_ame$longitude[i]
    val = getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2)
    if (val <= 0.25){
      count025 = count025 + 1
      ame_025 <- c(df_ame$amenity[i],ame_025) 
    }
    if (val <= 0.5){
      count05 = count05 + 1
      ame_05 <- c(df_ame$amenity[i],ame_05) 
    }
    if (val <= 1){
      count1 = count1 + 1
      ame_1 <- c(df_ame$amenity[i],ame_1) 
    }
    if (val <= 2){
      count2 = count2 + 1
      ame_2 <- c(df_ame$amenity[i],ame_2) 
    }
    if (val <= 5){
      count5 = count5 + 1
      ame_5 <- c(df_ame$amenity[i],ame_5) 
    }
    else{
      next
    }
  }
  
  
  if (is.null(ame_025)){
     df_025 <- data.frame(
       AMENITIES = c("NULL"),
       COUNT = c("NULL")
     )
  }else{
    df_025 = data.frame(table(ame_025))
    colnames(df_025) = c("AMENITIES","COUNT")
  }
  
  
  if (is.null(ame_05)){
    df_05 <- data.frame(
      AMENITIES = c("NULL"),
      COUNT = c("NULL")
    )
  }else{
    df_05 = data.frame(table(ame_05))
    colnames(df_05) = c("AMENITIES","COUNT")
  }
  
  
  if (is.null(ame_1)){
    df_1 <- data.frame(
      AMENITIES = c("NULL"),
      COUNT = c("NULL")
    )
  }else{
    df_1 = data.frame(table(ame_1))
    colnames(df_1) = c("AMENITIES","COUNT")
  }
  
  
  if (is.null(ame_2)){
    df_2 <- data.frame(
      AMENITIES = c("NULL"),
      COUNT = c("NULL")
    )
  }else{
    df_2 = data.frame(table(ame_2))
    colnames(df_2) = c("AMENITIES","COUNT")
  }
  
  
  if (is.null(ame_5)){
    df_5 <- data.frame(
      AMENITIES = c("NULL"),
      COUNT = c("NULL")
    )
  }else{
    df_5 = data.frame(table(ame_5))
    colnames(df_5) = c("AMENITIES","COUNT")
  } 
  
  ans <- list(df_025, df_05, df_1,df_2,df_5,
              sum(df_025$COUNT),
              sum(df_05$COUNT),
              sum(df_1$COUNT),
              sum(df_2$COUNT),
              sum(df_5$COUNT)
                   )
  
  return(ans)
}





# competitors
# read csv file
df_comp <- read.csv("others/CompetitorsSouth.csv")

get_table_comp <- function(lat1, lon1){
  new_df <- data.frame()
  for(i in 1:dim(df_comp)[1]){
    lat2 = df_comp$latitude[i]
    lon2 = df_comp$longitude[i]
    val = getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2)
    if (val <= 0.35){
      new_df <- rbind(new_df, df_comp[i,])
    }
    else{
      next
    }
  }
  if (dim(new_df)[1] == 0){
    new_df = data.frame(
      Data = "NULL"
    )
  }
  else{
    new_df = select(new_df, -latitude, -longitude)  
  }
  # ans = list(new_df,
             # min(new_df$))
  return(list(new_df,
         min(new_df$average_cost_for_two),
         round(mean(new_df$average_cost_for_two), digits = 0),
         max(new_df$average_cost_for_two),
         min(new_df$aggregate_rating),
         round(mean(new_df$aggregate_rating), digits = 1),
         max(new_df$aggregate_rating)))
}



