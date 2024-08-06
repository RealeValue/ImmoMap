add_Objektart <- function (df) {
  df %>% dplyr::collect() %>% dplyr::mutate(Objektart_num = Objektart,
                                            Objektart = dplyr::case_when(Objektart_num == "10" ~
                                                                           "Eigentumswohnung", Objektart_num == "20" ~ "Einfamilienhaus",
                                                                         Objektart_num == "21" ~ "Zweifamilienhaus", Objektart_num ==
                                                                           "30" ~ "Reihenhaus", Objektart_num == "31" ~
                                                                           "Doppelhaushaelfte", Objektart_num == "37" ~
                                                                           "Gebaeude", Objektart_num == "40" ~ "Kleingarten",
                                                                         Objektart_num == "50" ~ "Grundstueck", Objektart_num ==
                                                                           "60" ~ "PKW-Abstellplatz", TRUE ~ "Unbekannt") %>%
                                              factor(levels = c("Eigentumswohnung", "Einfamilienhaus",
                                                                "Zweifamilienhaus", "Reihenhaus", "Doppelhaushaelfte",
                                                                "Gebaeude", "Kleingarten", "Grundstueck", "PKW-Abstellplatz")),
                                            Modelltyp = dplyr::case_when(Objektart_num %in% c(10) ~
                                                                           "ETW", Objektart_num %in% c(20, 21, 30, 31, 37, 40) ~
                                                                           "EFH", Objektart_num %in% c(50) ~ "GST", Objektart_num %in%
                                                                           c(60) ~ "PKW") %>% factor(levels = c("EFH", "ETW",
                                                                                                                "GST", "PKW")))
}

## Returns an sf object
search_vergleichspreise_polygon <- function(reference_object, buffer_radius = 5000) {
  search_polygon_sf <- reference_object %>% st_buffer(dist = buffer_radius)
  return(search_polygon_sf)
}



# Define the function
# returns sf object
find_ComparableObjectsWithinPolygon <- function(search_polygon_sf, search_vergleichspreise_params) {

  search_polygon_sf <<- search_polygon_sf
  search_vergleichspreise_params <<- search_vergleichspreise_params

  bbox_sel <- search_polygon_sf %>% st_bbox()

  xmin <- bbox_sel["xmin"] %>% as.numeric()
  xmax <- bbox_sel["xmax"] %>% as.numeric()
  ymin <- bbox_sel["ymin"] %>% as.numeric()
  ymax <- bbox_sel["ymax"] %>% as.numeric()

  ComparableObjectsWithinPolygon <- df_immonet %>%
    dplyr::filter(
      xco_wgs84 >= xmin,
      xco_wgs84 <= xmax,
      yco_wgs84 >= ymin,
      yco_wgs84 <= ymax
    ) %>%
    dplyr::filter(
      Kaufdatum <= !!search_vergleichspreise_params$Aenderungsdatum,
      Modelltyp %in% !!search_vergleichspreise_params$Modelltyp,
      Kaufpreis >= !!search_vergleichspreise_params$Marktwert * 0.85,
      Kaufpreis <= !!search_vergleichspreise_params$Marktwert * 1.15
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(xco_copy = xco_wgs84, yco_copy = yco_wgs84) %>%
    st_as_sf(coords = c("xco_copy", "yco_copy"), crs = st_crs(zsp_dat))

  ComparableObjectsWithinPolygon <- ComparableObjectsWithinPolygon %>%
    ## st_intersection(search_polygon_sf) %>%
    dplyr::mutate(Distanz = st_distance(search_vergleichspreise_params)) %>%
    dplyr::arrange(Distanz)

  ComparableObjectsWithinPolygon
}

find_selected_objects <- function(ComparableObjectsWithinPolygon) {
  ComparableObjects <- ComparableObjectsWithinPolygon

  n                <- nrow(ComparableObjects)
  selected_objects <- ComparableObjects[1:min(5, n), ]
  avg_Kaufpreis    <- mean(selected_objects$Kaufpreis, na.rm = TRUE)


  if (n > 5 && abs(avg_Kaufpreis - search_vergleichspreise_params$Marktwert) / search_vergleichspreise_params$Marktwert > 0.10) {
    for (i in 6:nrow(ComparableObjects)) {
      for (j in 1:5) {
        temp_objects <- selected_objects
        temp_objects[j, ] <- ComparableObjects[i, ]
        avg_Kaufpreis <- mean(temp_objects$Kaufpreis, na.rm = TRUE)
        if (abs(avg_Kaufpreis - search_vergleichspreise_params$Marktwert) / search_vergleichspreise_params$Marktwert <= 0.10) {
          selected_objects <- temp_objects
          break
        }
      }
      if (abs(avg_Kaufpreis - search_vergleichspreise_params$Marktwert) / search_vergleichspreise_params$Marktwert <= 0.10) {
        break
      }
    }
  }

  ComparableObjectsWithinPolygon <- selected_objects %>%
    st_drop_geometry() %>%
    dplyr::select(BewertungId) %>%
    dplyr::mutate(Selected = 1L) %>%
    dplyr::right_join(ComparableObjectsWithinPolygon, by = "BewertungId") %>%
    dplyr::mutate(Selected = if_else(is.na(Selected), 0L, Selected)) %>%
    st_as_sf()


  ## ComparableObjectsWithinPolygon <<- ComparableObjectsWithinPolygon
  return(ComparableObjectsWithinPolygon)
}


find_ComparableObjects <- function(ComparableObjectsWithinPolygon) {
  ComparableObjectsWithinPolygon %>% dplyr::filter(Selected == 1)
}




