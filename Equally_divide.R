## equally divide line shapefile


split_lines_func <- function(input_lines, max_length) {
  ## get attributeI
  ## retrieve coordinate reference system
  # input_crs <- sf::st_crs(input_lines)
  ## calculate length 
  input_lines$length = as.numeric(st_length(input_lines))
  pts_100 = rgeos::gInterpolate(sf::as_Spatial(input_lines), seq(0, input_lines$length, max_length))

  split_nodes = data.frame(pts_100@coords)
  split_nodes = dplyr::mutate(split_nodes,len  = c(0, rep(max_length, (length(pts_100)-1))),cumlen = cumsum(len),newID = NA) 
  
  
  coords <- dplyr::select(data.frame(sf::st_coordinates(input_lines)), X,Y) 
  coords <- dplyr::mutate(coords,len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2))))
  coords[['len']] = ifelse(is.na(coords[['len']]), 0, coords[['len']])
  coords = dplyr::mutate(coords, cumlen = cumsum(coords[['len']]), newID = " ")
  
  colnames(coords) = c("x", "y","len", "cumlen", "newID")
  
  total_pts = data.frame(rbind(split_nodes[2:nrow(split_nodes),], coords))
  total_pts02 = total_pts[order(total_pts$cumlen),]
  
  rownames(total_pts02) = c(1:nrow(total_pts02))
  
  split_points = data.frame(start_nID = c(1,which(is.na(total_pts02$newID))), stop_nID = c(which(is.na(total_pts02$newID)), nrow(total_pts02)))
  split_points$FID = c(1:nrow(split_points))
  
  new_line <- function(start_stop, coords) {
    return(sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("x", "y")])))
  }
  
  split_lines <-plyr::alply(as.matrix(split_points[,c("start_nID", "stop_nID")]),
                            .margins = 1, .fun = new_line, coords = total_pts02)
  
  return(split_lines)
}
## Read the shapefile
str = read_sf('F:\\github file\\Equally divide the street segment\\street.shp')
str$Shape__Len = as.numeric(st_length(str))
split_lines = split_lines_func(str[198,], 100)

sf_lines = st_sf(geometry = st_sfc(split_lines, crs = sf::st_crs(str)))
attr_line = data.frame(ID = c(1,2), length = c('',''))
lines_new = st_as_sf(cbind(attr_line, sf_lines))
lines_new$length = as.numeric(st_length(lines_new))









