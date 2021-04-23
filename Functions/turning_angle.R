# Determine Turning Angle ------------------------------------------------------
# https://www.r-bloggers.com/calculate-turning-angles-and-step-lengths-from-location-data/
bearing.ta <- function(loc1,loc2,loc3,as.deg=FALSE){
  ## calculates the bearing and length of the two lines
  ##    formed by three points
  ## the turning angle from the first bearing to the
  ##    second bearing is also calculated
  ## locations are assumed to be in (X,Y) format.
  ## Options:
  ## as.deg = TRUE returns degrees instead of radians
  if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
    print("Locations must consist of either three vectors, length == 2,
          or three two-column dataframes")
    return(NaN)
  }
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  locdiff1<-loc2-loc1
  locdiff2<-loc3-loc2
  bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=F)
  bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=F)
  
  if(is.data.frame(locdiff1)){
    dist1<-sqrt(rowSums(locdiff1^2))
    dist2<-sqrt(rowSums(locdiff2^2))
  }else{
    dist1<-sqrt(sum(locdiff1^2))
    dist2<-sqrt(sum(locdiff2^2))
  }
  
  ta=(bearing2-bearing1)
  
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  return(list(bearing1=unlist(bearing1*c),bearing2=unlist(bearing2*c),
              ta=unlist(ta*c),dist1=unlist(dist1),dist2=unlist(dist2)))
}

anglefun <- function(xx,yy,bearing=TRUE,as.deg=FALSE){
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:
  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  b<-sign(xx)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(yy<0)*pi+atan(xx/yy)
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  return(tempangle*c)
}


determine_turn_angle <- function(i, addis_adama_points, turn_angle_window){
  
  indices <- c((i-turn_angle_window),i,(i+turn_angle_window))
  if((TRUE %in% (indices < 0)) | (TRUE %in% (indices > nrow(addis_adama_points)))){
    ta <- NA
  }else{
    addis_adama_points_i <- addis_adama_points[c((i-turn_angle_window),i,(i+turn_angle_window)),]
    
    A <- c(addis_adama_points_i$latitude[1], addis_adama_points_i$longitude[1])
    O <- c(addis_adama_points_i$latitude[2], addis_adama_points_i$longitude[2])
    B <- c(addis_adama_points_i$latitude[3], addis_adama_points_i$longitude[3])
    
    ta <- abs(bearing.ta(A,O,B,as.deg=T)$ta)
  }
  
  return(ta)
}
