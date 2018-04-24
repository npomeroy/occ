plotDS = function(speed,
                  direction,
                  pressure,
                  ctd,
                  seafet,
                  puc = NULL,
                  read.csv = TRUE,
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

  speed.plot = ggplot() +
    geom_raster(
      aes(
        x = adcp.long.speed$DateTimeUTC,
        y = as.numeric(as.character(adcp.long.speed$Height)),
        fill = adcp.long.speed$Speed
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
    scale_y_continuous(limits = c(NA, floor(max(
      pressure$Pressure
    ))), expand = c(0, 0)) +
    theme_bw() +
    xlab('Time') +
    ylab('Height (m)') +
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
        x = adcp.long.direction$DateTimeUTC,
        y = as.numeric(as.character(adcp.long.direction$Height)),
        fill = adcp.long.direction$Direction
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
    scale_y_continuous(limits = c(NA, floor(max(
      pressure$Pressure
    ))), expand = c(0, 0)) +
    theme_bw() +
    xlab('Time') +
    ylab('Height (m)') +
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

  temp.plot = ggplot() +
    geom_line(aes(x = ctd$DateTime, y = ctd$Temp_DegC),
              col = 'dodgerblue',
              na.rm = TRUE,
              cex = 1) +
    scale_x_datetime(
      breaks = date_breaks(time.step),
      limits = c(time.min, time.max),
      labels = date_format("%H:%M",  tz = plot.tz),
      expand = c(0, 0)
    ) +
    xlab('Time') +
    ylab(expression(atop(paste("Temp (", degree,"C)")))) +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    if (is.null(temp.range) == FALSE) {
      scale_y_continuous(limits = c(temp.range[1], temp.range[2]))
    }

  sal.plot = ggplot() +
    geom_line(aes(x = ctd$DateTime, y = ctd$Saln_PSU),
              col = 'dodgerblue',
              na.rm = TRUE,
              cex = 1) +
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
    if (read.csv == TRUE) {
      puc = read.csv(puc)
      puc$DateTimeUTC = ymd_hms(puc$DateTimeUTC)
    } else {
      puc = puc
    }

    pH.plot = ggplot() +
      geom_line(aes(x = seafet.time$DateTime, y = seafet.time$pH),
                col = 'dodgerblue',
                cex = 1)  +
      geom_point(
        aes(x = puc$DateTimeUTC, y = puc$pH),
        col = 'tomato1',
        pch = 15,
        cex = 2
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

  # Combine all plots

  all.ds = ggarrange(speed.plot,
            direction.plot,
            pressure.plot,
            temp.plot,
            sal.plot,
            pH.plot,
            nrow = 6,
            align = 'v')

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
