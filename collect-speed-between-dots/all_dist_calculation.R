library('ggplot2')
library('ggmap')
library('dplyr')
library('sp')
library('geosphere')

route_ID <- 712988

df <- read.csv('all_paths.csv')

all <- read.csv('tidy_busdata_220717_1230.csv')
#all$datetime <- as.POSIXct(all$datetime)

one_route <- df %>% filter(route_id == route_ID)

all_routes <- all %>% 
  filter(routeid == route_ID & state == 1) %>%
  mutate(new_datetime = as.POSIXct(datetime)) %>%
#  arrange(all_routes, vehicleid)
  filter(new_datetime > '2017-07-19 00:00:00' & new_datetime < '2017-07-19 23:59:59')
#  filter(vehicleid == 38789)

sorted_routes <- arrange(all_routes, vehicleid, new_datetime)


get_distance <- function(lon1, lat1, lon2, lat2, one_route) {
  min_dist1 <- 10000
  min_dist1_i <- 0
  
  min_dist2 <- 10000
  min_dist2_i <- 0  
  
  dir_to <- 0
  
  xes <- one_route$lng_start
  yes <- one_route$lat_start
  dists <- one_route$dist
  dirs <- one_route$dir
  one_route_len <- length(xes)
  
  
  i <- 1  
  for (xx in xes) {
    yy <- yes[i]
    real_dist = distm (c(lon1, lat1), c(xx, yy), fun = distHaversine)
    if(real_dist < min_dist1) {
      min_dist1 <- real_dist
      min_dist1_i <- i
      dir_to <- dirs[i]
    }
    i <- i + 1
  }
  
  i <- min_dist1_i      
  for (xx in xes[min_dist1_i:one_route_len]) {
    yy <- yes[i]    
    real_dist = distm (c(lon2, lat2), c(xx, yy), fun = distHaversine)
    if(real_dist < min_dist2 & dir_to == dirs[i]) {# 
      min_dist2 <- real_dist
      min_dist2_i <- i      
    }
    i <- i + 1    
  }
  
  #print(min_dist1_i)
  #print(min_dist2_i)
  #print(one_route_len)
  
  return(sum(dists[min_dist1_i:min_dist2_i]))
  
}

i <- 1
count <- nrow(sorted_routes)

calc_data <- NULL

for (datetime in sorted_routes$datetime) {
  if(
    count > i 
     & sorted_routes$new_datetime[i+1] - sorted_routes$new_datetime[i] > 0 
     & sorted_routes$new_datetime[i+1] - sorted_routes$new_datetime[i] < 10
     & sorted_routes$vehicleid[i+1] == sorted_routes$vehicleid[i]
    ){
    one_dist <- get_distance(sorted_routes$lon[i], sorted_routes$lat[i], sorted_routes$lon[i+1], sorted_routes$lat[i+1], one_route)
    if(one_dist < 100) {
      #print(one_dist)
      #print(as.numeric(sorted_routes$datetime[i+1] - sorted_routes$datetime[i])*60)
      #print(c(sorted_routes$lon[i], sorted_routes$lat[i], sorted_routes$lon[i+1], sorted_routes$lat[i+1]))
      
      vector_to_ins <- c(
        sorted_routes$routeid[i],
        sorted_routes$vehicleid[i],
        sorted_routes$new_datetime[i],
        datetime,        
        sorted_routes$lon[i],
        sorted_routes$lat[i],
        sorted_routes$lon[i+1],
        sorted_routes$lat[i+1],
        one_dist*1000, 
        as.numeric(sorted_routes$new_datetime[i+1] - sorted_routes$new_datetime[i])*60, 
        one_dist*60/as.numeric(sorted_routes$new_datetime[i+1] - sorted_routes$new_datetime[i]))
      
      calc_data <- rbind(calc_data, vector_to_ins)
      
    }
  }
  
  if(i %% 100 == 0){
    print(i)
    print(Sys.time())
  }
  
  i <- i + 1
}

calc_data_cols <- c('routeid', 'vehicleid', 'new_datetime', 'datetime', 'lonS', 'latS', 'lonF', 'latF', 'dist', 'time', 'speed')
write.table(calc_data, file = "MyData.csv", row.names = FALSE, col.names = calc_data_cols, sep=",")
