#my plot##################################################################
#' Plot HYSPLIT trajectory model output onto a map
#' @description The function plots modeled wind
#' trajectories onto a map with information provided
#' at every air transport time interval.
#' @param traj_df a trajectory data frame, typically
#' created from use of the \code{hysplit_trajectory}
#' function with the value for \code{return_traj_df}
#' set to \code{TRUE}.
#' @param show_hourly an option to show hourly
#' positions and associated data along trajectories.
#' @param color_scheme defines the appearance of
#' multiple trajectories in a single plot. Current
#' options are \code{cycle_hues} (the default), and
#' \code{increasingly_gray}.
#' @import leaflet
#' @import scales
#' @export trajectory_plot

trajectory_plot_mine <- function(traj_df_list,
                                 show_hourly = TRUE,
                                 color_scheme = "cycle_hues"){
  
  traj_names=names(traj_df_list)
  
  j=1
  traj_df<-traj_df_list[[j]]
  
  traj_plot <- leaflet()
  
  traj_plot <- 
    addProviderTiles(
      traj_plot,
      "OpenStreetMap",
      group = "OpenStreetMap") 
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "CartoDB.DarkMatter",
      group = "CartoDB Dark Matter")
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "CartoDB.Positron",
      group = "CartoDB Positron")
  
  traj_plot <- 
    addProviderTiles(
      traj_plot,
      "Esri.WorldTerrain",
      group = "ESRI World Terrain")
  
  traj_plot <-
    addProviderTiles(
      traj_plot,
      "Stamen.Toner",
      group = "Stamen Toner")
  
  traj_plot <- 
    fitBounds(
      traj_plot,
      min(traj_df$lon),
      min(traj_df$lat),
      max(traj_df$lon),
      max(traj_df$lat))
  
  if (color_scheme == "cycle_hues"){
    colors <- 
      hue_pal(c = 90, l = 70)(
        length(traj_df_list))
  }
  
  if (color_scheme == "increasingly_gray"){
    colors <-
      grey_pal(0.7, 0.1)(length(traj_df_list))
  }
  
  
  for (j in 1:length(traj_df_list)){
    
    
    traj_df<-traj_df_list[[j]]
    traj_df$date_local<-ymd_hms(traj_df$date_local,tz="America/Los_Angeles")
    
    
    
    
    # Correct longitude values near prime meridian
    traj_df$lon[which(traj_df$lon > 0)] <- 
      traj_df$lon[which(traj_df$lon > 0)] - (180*2)
    
    # Get different trajectories by date
    for (i in 1:length(sort(unique(traj_df$date)))){
      
      if (i == 1){
        sorted_dates <- sort(unique(traj_df$date_local))
        wind_trajectories_by_date <- list()
      }
      
      wind_trajectories_by_date[[i]] <-
        subset(traj_df, date_local == sorted_dates[i])
      
      wind_trajectories_by_date[[i]] <-
        wind_trajectories_by_date[[i]][
          order(wind_trajectories_by_date[[i]][,"hour.inc"]),]
    }
    
    for (i in 1:length(wind_trajectories_by_date)){
      
      popup <- 
        paste0("<strong>trajectory </strong> ",
               unique(wind_trajectories_by_date[[i]][, "date_local"]),
               "<br><strong>total duration: </strong> ",
               length(wind_trajectories_by_date[[i]][, "hour.inc"]) - 1,
               " h<br>")
      
      traj_plot <-
        addPolylines(
          traj_plot,
          wind_trajectories_by_date[[i]][,"lon"],
          wind_trajectories_by_date[[i]][,"lat"],
          group = traj_names[j],
          weight = 2,
          smoothFactor = 1,
          color = colors[(1:length(unique(names(traj_df_list))))
                         [names(traj_df_list)[j]==unique(names(traj_df_list))]],
          popup = popup)
      
      traj_plot <- 
        addCircleMarkers(
          traj_plot,
          radius= 3,
          fillOpacity = 1,
          stroke = FALSE,
          fillColor = colors[(1:length(unique(names(traj_df_list))))
                         [names(traj_df_list)[j]==unique(names(traj_df_list))]],
          wind_trajectories_by_date[[i]][,"lon"],
          wind_trajectories_by_date[[i]][,"lat"],
          group = traj_names[j],
          label = wind_trajectories_by_date[[i]][,"hour.inc"],
          popup = paste("Time Point:",
                        wind_trajectories_by_date[[i]][,"date_local"] +
                        wind_trajectories_by_date[[i]][,"hour.inc"]*60*60)
        )
          
      
    }
  }
  traj_plot <-
    addLayersControl(
      traj_plot,
      position = "topright",
      baseGroups = c("CartoDB Positron",
                     "CartoDB Dark Matter",
                     "Stamen Toner",
                     "ESRI World Terrain"),
      overlayGroups = c("traj_names","traj_circles"))
  
  traj_plot
}
