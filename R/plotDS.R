plotDS = function(speed,
                  direction,
                  pressure,
                  ctd,
                  seafet,
                  puc = NULL,
                  read.csv = TRUE,
                  write.csv = FALSE,
                  plot.tz = "UTC",
                  time.step = "4 hour",
                  temp.range = NULL,
                  sal.range = NULL,
                  pH.range = NULL) {
  # Read csv files
  if (read.csv == TRUE) {
    speed = read.csv(speed, check.names = FALSE)
    direction = read.csv(direction, check.names = FALSE)
    pressure = read.csv(pressure)
    ctd = read.csv(ctd)
    seafet = read.csv(seafet)
  } else {
    speed = speed
    direction = direction
    pressure = pressure
    ctd = ctd
    seafet = seafet
  }
  
  # Plot ADCP data
  speed$DateTimeUTC = ymd_hms(speed$DateTimeUTC)
  direction$DateTimeUTC = ymd_hms(direction$DateTimeUTC)
  pressure$DateTimeUTC = ymd_hms(pressure$DateTimeUTC)
  
  adcp.long.speed = melt(speed, id.vars = 'DateTimeUTC')
  adcp.long.direction = melt(direction, id.vars = 'DateTimeUTC')
  
  colnames(adcp.long.speed) = c("DateTimeUTC", "Height", "Speed")
  colnames(adcp.long.direction) = c("DateTimeUTC", "Height", "Direction")
  
  time.min = min(adcp.long.speed$DateTimeUTC)
  time.max = max(adcp.long.speed$DateTimeUTC)
  height.max = floor(max(pressure$Pressure))
  
  adcp.long.speed.sub = subset(adcp.long.speed, as.numeric(as.character(Height)) < height.max)
  adcp.long.speed.sub$Depth = -1 * (as.numeric(as.character(adcp.long.speed.sub$Height)) - max(as.numeric(
    as.character(adcp.long.speed.sub$Height)
  )))
  
  adcp.long.direction.sub = subset(adcp.long.direction, as.numeric(as.character(Height)) < height.max)
  adcp.long.direction.sub$Depth = -1 * (as.numeric(as.character(adcp.long.direction.sub$Height)) - max(as.numeric(
    as.character(adcp.long.direction.sub$Height)
  )))
  
  speed.plot = ggplot() +
    geom_raster(
      aes(
        x = adcp.long.speed.sub$DateTimeUTC,
        y = adcp.long.speed.sub$Depth,
        fill = adcp.long.speed.sub$Speed
      ),
      interpolate = TRUE
    ) +
    scale_fill_viridis(name = 'Speed (m/s)') +
    scale_x_datetime(
      date_breaks = time.step,
      limits = c(time.min, time.max),
      labels = date_format("%H:%M", tz = plot.tz),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0),
                       trans = 'reverse') +
    theme_bw() +
    xlab('Time') +
    ylab('Depth (m)') +
    theme(
      legend.position = 'top',
      legend.key.height =  unit(0.1, "in"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      axis.title.x = element_blank()
    )
  
  direction.plot = ggplot() +
    geom_raster(
      aes(
        x = adcp.long.direction.sub$DateTimeUTC,
        y = adcp.long.direction.sub$Depth,
        fill = adcp.long.direction.sub$Direction
      ),
      interpolate = TRUE
    ) +
    scale_fill_viridis(name = 'Direction',
                       limits = c(0, 360)) +
    scale_x_datetime(
      date_breaks = time.step,
      limits = c(time.min, time.max),
      labels = date_format("%H:%M", tz = plot.tz),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0),
                       trans = 'reverse') +
    xlab('Time') +
    ylab('Depth (m)') +
    theme(
      legend.position = 'top',
      legend.key.height =  unit(0.1, "in"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      axis.title.x = element_blank()
    )
  
  pressure.plot = ggplot() +
    geom_line(
      aes(x = pressure$DateTimeUTC, y = pressure$Pressure),
      col = 'dodgerblue',
      na.rm = TRUE,
      cex = 1
    ) +
    scale_x_datetime(
      breaks = date_breaks(time.step),
      limits = c(time.min, time.max),
      labels = date_format("%H:%M", tz = plot.tz),
      expand = c(0, 0)
    ) +
    xlab('Time') +
    ylab('P (dbar)') +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  # Plot CTD data
  ctd$DateTime = ymd_hms(ctd$DateTime)
  
  ctd.time = subset(ctd, DateTime >= time.min &
                      DateTime <= time.max)
  
  temp.plot = ggplot() +
    geom_line(
      aes(x = ctd$DateTime, y = ctd$Temp_DegC),
      col = 'dodgerblue',
      na.rm = TRUE,
      cex = 1
    ) +
    scale_x_datetime(
      breaks = date_breaks(time.step),
      limits = c(time.min, time.max),
      labels = date_format("%H:%M",  tz = plot.tz),
      expand = c(0, 0)
    ) +
    xlab('Time') +
    ylab("Temp (deg C)") +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    if (is.null(temp.range) == FALSE) {
      scale_y_continuous(limits = c(temp.range[1], temp.range[2]))
    }
  
  sal.plot = ggplot() +
    geom_line(
      aes(x = ctd$DateTime, y = ctd$Saln_PSU),
      col = 'dodgerblue',
      na.rm = TRUE,
      cex = 1
    ) +
    scale_x_datetime(
      breaks = date_breaks(time.step),
      limits = c(time.min, time.max),
      labels = date_format("%H:%M",  tz = plot.tz),
      expand = c(0, 0)
    ) +
    xlab('Time') +
    ylab('S') +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    if (is.null(sal.range) == FALSE) {
      scale_y_continuous(limits = c(sal.range[1], sal.range[2]))
    }
  
  
  # Plot pH and PUC data
  seafet$DateTime = ymd_hms(seafet$DateTime)
  
  seafet.time = subset(seafet, DateTime >= time.min &
                         DateTime <= time.max)
  
  if (is.null(puc) == TRUE) {
    pH.plot = ggplot() +
      geom_line(aes(x = seafet.time$DateTime, y = seafet.time$pH),
                col = 'dodgerblue',
                cex = 1)  +
      scale_x_datetime(
        breaks = date_breaks(time.step),
        limits = c(time.min, time.max),
        labels = date_format("%H:%M",  tz = plot.tz),
        expand = c(0, 0)
      ) +
      xlab('Time') +
      ylab('pH') +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      if (is.null(pH.range) == TRUE) {
        scale_y_continuous(limits = c(round(min(
          seafet.time$pH
        ), digits = 2), round(max(
          seafet.time$pH
        ), digits = 2)))
      } else {
        scale_y_continuous(limits = c(pH.range[1], pH.range[2]))
      }
  } else {
    puc = read.csv(puc)
    puc$DateTimeUTC_Start = ymd_hms(puc$DateTimeUTC)
    puc$DateTimeUTC_End = puc$DateTimeUTC_Start + lubridate::minutes(45)
    puc.subset = subset(puc,
                        DateTimeUTC_Start >= time.min & DateTimeUTC_Start <= time.max)
    
    pH.plot = ggplot() +
      geom_line(aes(x = seafet.time$DateTime, y = seafet.time$pH),
                col = 'dodgerblue',
                cex = 1)  +
      geom_rect(
        data = puc.subset,
        mapping = aes(
          xmin = DateTimeUTC_Start,
          xmax = DateTimeUTC_End,
          ymin = pH - 0.002,
          ymax = pH + 0.002
        ),
        fill = 'tomato1'
      ) +
      scale_x_datetime(
        breaks = date_breaks(time.step),
        limits = c(time.min, time.max),
        labels = date_format("%H:%M",  tz = plot.tz),
        expand = c(0, 0)
      ) +
      xlab('Time') +
      ylab('pH') +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      if (is.null(pH.range) == TRUE) {
        scale_y_continuous(limits = c(round(min(
          seafet.time$pH
        ), digits = 2), round(max(
          seafet.time$pH
        ), digits = 2)))
      } else {
        scale_y_continuous(limits = c(pH.range[1], pH.range[2]))
      }
  }
  
  if (write.csv == TRUE) {
    write.csv(ctd.time,
              paste0(ctd.time$ShallowCTDID[1], '_trimmed.csv'),
              row.names = FALSE)
    
    write.csv(seafet.time,
              paste0("seafet", '_trimmed.csv'),
              row.names = FALSE)
  }
  
  # Combine all plots
  
  all.ds = ggarrange(
    speed.plot,
    direction.plot,
    pressure.plot,
    temp.plot,
    sal.plot,
    pH.plot,
    nrow = 6,
    align = 'hv'
  )
  
  return(
    list(
      speed.plot = speed.plot,
      direction.plot = direction.plot,
      pressure.plot = pressure.plot,
      temp.plot = temp.plot,
      sal.plot = sal.plot,
      pH.plot = pH.plot,
      all.ds = all.ds
    )
  )
  
  print(all.ds)
  
}
