library(dplyr)
library(ggplot2)
library(readr)
library(sf)

sf::sf_use_s2(FALSE)

script_path <- sub(
  "^--file=",
  "",
  commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))[1]]
)
project_dir <- if (!is.na(script_path) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

local_sources <- file.path(project_dir, "code", "local_map_sources.R")
if (file.exists(local_sources)) {
  source(local_sources)
}

resolve_input <- function(env_var, local_var, default_path, label, required = TRUE) {
  candidates <- c(
    Sys.getenv(env_var, unset = NA_character_),
    if (exists(local_var, inherits = TRUE)) get(local_var, inherits = TRUE) else NA_character_,
    default_path
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    normalized <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (file.exists(normalized)) return(normalized)
  }
  if (required) {
    stop(
      paste0(
        "Could not find ", label, ". Set ", env_var,
        ", define ", local_var, " in code/local_map_sources.R, ",
        "or place the file at ", default_path, "."
      ),
      call. = FALSE
    )
  }
  NA_character_
}

site_coords_path <- resolve_input(
  "PUMPING_SITE_COORDS",
  "site_coords_path",
  file.path(project_dir, "data", "map_sources", "monitoring_site_coords.csv"),
  "monitoring site coordinates"
)
well_points_path <- resolve_input(
  "PUMPING_WELL_POINTS",
  "well_points_path",
  file.path(project_dir, "data", "map_sources", "monitoring_well_points.csv"),
  "monitoring well points"
)
linked_wells_path <- resolve_input(
  "PUMPING_LINKED_WELLS",
  "linked_wells_path",
  file.path(project_dir, "data", "map_sources", "monitoring_site_linked_wells.csv"),
  "monitoring site linked wells"
)
parcels_path <- resolve_input(
  "PUMPING_PARCELS_GEOJSON",
  "parcels_path",
  file.path(project_dir, "data", "map_sources", "parcels.geojson"),
  "parcel geometry",
  required = FALSE
)
out_dir <- file.path(project_dir, "site_images", "site_maps")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

site_coords <- readr::read_csv(site_coords_path, show_col_types = FALSE)
well_points <- readr::read_csv(well_points_path, show_col_types = FALSE)
linked_wells <- readr::read_csv(linked_wells_path, show_col_types = FALSE)

parcels_sf <- NULL
if (file.exists(parcels_path)) {
  parcels_sf <- sf::st_read(parcels_path, quiet = TRUE) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid() %>%
    mutate(
      parcel_id = trimws(as.character(.data$PCL)),
      gb_type = toupper(trimws(as.character(dplyr::coalesce(.data$TYPE, .data$GB_TYPE)))),
      gb_type = ifelse(gb_type %in% LETTERS[1:5], gb_type, NA_character_)
    )
}

gb_type_palette <- c(
  A = "#FFFFFF",
  B = "#FDD835",
  C = "#2E7D32",
  D = "#B39DDB",
  E = "#1E88E5"
)

gb_type_alpha <- c(
  A = 0.01,
  B = 0.28,
  C = 0.24,
  D = 0.36,
  E = 0.24
)

gb_type_line <- c(
  A = "#FFFFFF",
  B = "#C9A227",
  C = "#1B5E20",
  D = "#6A1B9A",
  E = "#1565C0"
)

haversine_m <- function(lat1, lon1, lat2, lon2) {
  p1 <- lat1 * pi / 180
  p2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlmb <- (lon2 - lon1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(p1) * cos(p2) * sin(dlmb / 2)^2
  a <- pmax(pmin(a, 1), 0)
  6371000 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

expand_limits <- function(values, fraction, minimum) {
  rng <- range(values, na.rm = TRUE)
  pad <- diff(rng) * fraction
  if (!is.finite(pad) || pad < minimum) pad <- minimum
  rng + c(-pad, pad)
}

make_site_map <- function(site_id) {
  site <- site_coords %>%
    filter(.data$site_tacbg == site_id) %>%
    transmute(
      site_tacbg = .data$site_tacbg,
      site_monsites = .data$site_monsites,
      site_lat = .data$lat,
      site_lng = .data$lng
    )
  linked <- linked_wells %>%
    filter(.data$site == site_id) %>%
    transmute(site_tacbg = .data$site, staid = .data$linked_well)
  wells <- well_points %>%
    transmute(
      staid = .data$staid,
      well_lat = .data$lat,
      well_lng = .data$lng
    ) %>%
    inner_join(linked, by = "staid")

  if (nrow(site) == 0 || nrow(wells) == 0) return(invisible(NULL))

  map_rows <- wells %>%
    mutate(
      site_lat = site$site_lat[1],
      site_lng = site$site_lng[1],
      dist_m = haversine_m(.data$well_lat, .data$well_lng, .data$site_lat, .data$site_lng),
      distance = ifelse(
        .data$dist_m >= 1000,
        paste0(round(.data$dist_m / 1000, 2), " km"),
        paste0(round(.data$dist_m), " m")
      ),
      mid_lat = (.data$well_lat + .data$site_lat) / 2,
      mid_lng = (.data$well_lng + .data$site_lng) / 2
    )

  xlim <- expand_limits(c(map_rows$well_lng, map_rows$site_lng), fraction = 0.75, minimum = 0.016)
  ylim <- expand_limits(c(map_rows$well_lat, map_rows$site_lat), fraction = 0.85, minimum = 0.012)
  crop_bbox <- sf::st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]), crs = sf::st_crs(4326))

  site_point <- sf::st_as_sf(site, coords = c("site_lng", "site_lat"), crs = 4326, remove = FALSE)
  parcel_window <- NULL
  site_parcel <- NULL
  site_parcel_text <- "Site parcel: unavailable"

  if (!is.null(parcels_sf)) {
    parcel_window <- suppressWarnings(sf::st_crop(parcels_sf, crop_bbox))
    site_hit <- lengths(sf::st_intersects(site_point, parcels_sf)) > 0
    if (isTRUE(site_hit)) {
      site_parcel <- parcels_sf[sf::st_intersects(site_point, parcels_sf)[[1]][1], ]
    } else if (nrow(parcels_sf) > 0) {
      site_parcel <- parcels_sf[sf::st_nearest_feature(site_point, parcels_sf), ]
    }
    if (!is.null(site_parcel) && nrow(site_parcel) > 0) {
      site_parcel_text <- paste0(
        "Site parcel: ",
        site_parcel$parcel_id[1],
        " (GB type ",
        ifelse(is.na(site_parcel$gb_type[1]), "NA", site_parcel$gb_type[1]),
        ")"
      )
    }
  }

  p <- ggplot()
  if (!is.null(parcel_window) && nrow(parcel_window) > 0) {
    p <- p +
      geom_sf(
        data = parcel_window,
        aes(fill = .data$gb_type, alpha = .data$gb_type, color = .data$gb_type),
        linewidth = 0.2
      ) +
      scale_fill_manual(
        values = gb_type_palette,
        na.value = "#eef2f7",
        name = "GB type",
        drop = FALSE
      ) +
      scale_alpha_manual(
        values = gb_type_alpha,
        na.value = 0.08,
        guide = "none",
        drop = FALSE
      ) +
      scale_color_manual(
        values = gb_type_line,
        na.value = "#78909C",
        guide = "none",
        drop = FALSE
      )
  }
  if (!is.null(site_parcel) && nrow(site_parcel) > 0) {
    p <- p +
      geom_sf(
        data = site_parcel,
        fill = NA,
        color = "#1b5e20",
        linewidth = 1.1
      )
  }

  p <- p +
    geom_segment(
      data = map_rows,
      aes(x = .data$site_lng, y = .data$site_lat, xend = .data$well_lng, yend = .data$well_lat),
      color = "#b36b2c",
      linewidth = 0.9,
      linetype = "longdash",
      alpha = 0.9
    ) +
    geom_label(
      data = map_rows,
      aes(x = .data$mid_lng, y = .data$mid_lat, label = .data$distance),
      size = 5.4,
      linewidth = 0,
      label.padding = unit(0.18, "lines"),
      fill = "#fff8ed",
      color = "#7a4a1d",
      alpha = 0.96
    ) +
    geom_point(
      data = map_rows,
      aes(x = .data$well_lng, y = .data$well_lat),
      size = 6.3,
      shape = 21,
      stroke = 1,
      fill = "#c9a97b",
      color = "#5c3f2b"
    ) +
    geom_label(
      data = map_rows,
      aes(x = .data$well_lng, y = .data$well_lat, label = .data$staid),
      nudge_y = 0.0014,
      size = 5.5,
      linewidth = 0,
      label.padding = unit(0.14, "lines"),
      fill = "white",
      color = "#263238",
      fontface = "bold"
    ) +
    geom_point(
      data = site,
      aes(x = .data$site_lng, y = .data$site_lat),
      size = 8.5,
      shape = 21,
      stroke = 1.2,
      fill = "#2e7d32",
      color = "white"
    ) +
    geom_label(
      data = site,
      aes(x = .data$site_lng, y = .data$site_lat, label = paste0(.data$site_tacbg, " / ", .data$site_monsites)),
      nudge_y = -0.0018,
      size = 5.5,
      linewidth = 0,
      label.padding = unit(0.16, "lines"),
      fill = "#eef8ef",
      color = "#1b5e20",
      fontface = "bold"
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(
      title = paste0(site_id, " Linked Wells"),
      subtitle = site_parcel_text,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#fbfbf7", color = NA),
      panel.grid.major = element_line(color = "#d9ded5", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 11),
      plot.title = element_text(face = "bold", color = "#1f2937", size = 22),
      plot.subtitle = element_text(color = "#475569", size = 14),
      plot.margin = margin(8, 10, 6, 10)
    )

  ggsave(
    file.path(out_dir, paste0(site_id, "_linked_wells_map.png")),
    p,
    width = 9,
    height = 9,
    dpi = 260
  )
}

for (site_id in unique(linked_wells$site)) {
  make_site_map(site_id)
}
