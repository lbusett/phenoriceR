#' @title pr_extract
#' @description Extract data from PhenoRice mosaics
#' @param main_folder location of the "Data" folder (e.g., "/home/lb/my_data/prasia/Data")
#' @param region region to extract(e.g., "Region_3_-_Central_Luzon").
#'   If "All", all regions are extracted. Multoiple values
#'   are possible. (e.g., region = c("Region_3_-_Central_Luzon", "Chippalonga"))
#' @return
#' - Cropped rasters are stored in the `Data/subsets/_region_name_/orig` and
#'  `Data/subsets/_region_name_/decirc` folders. #'
#' - Extracted data are saved in the `Data/subsets/_region_name_/RData` folder. One
#'  separate RData file is created for each PhenoRice parameter (e.g., "Dhaka_sos_stats.RData",
#'   "Dhaka_eos_stats.RData", "Dhaka_sos_decirc_stats.RData")
#' @examples
#' \dontrun{
#' # set the main folder ----
#' main_folder <- "/home/lb/my_data/prasia/Data"
#'
#' # Suppose you want to extract data for Region: "Region_3_-_Central_Luzon" :
#' # --> extract it from the full shapefile
#' Region_name <- "Region_3_-_Central_Luzon"
#'
#' # "1) run "pr_extract" on the region ----
#'
#' pr_extract(main_folder,
#'           region = Region_name)
#' }
#'
#' @rdname pr_extract
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom dplyr filter select rename
#' @importFrom lubridate year
#' @importFrom sprawl reproj_vect get_proj4string make_folder extract_rast
#' @importFrom tools file_path_sans_ext
pr_extract = function(main_folder, region) {
  # Load the reshuffled shapefile ----

  in_shp <- read_vect(file.path(main_folder,
                                "vector/Ricetlas/riceatlas_asia_reshuffled.shp"))

  if (region == "All") {
    regions <- unique(in_shp$Region)
  } else {
    regions = region
    if (!all(regions %in% unique(in_shp$Region))) {
      stop("pr_extract --> At least one of the specified regions does not
           exist in the RiceAtlas data base. Aborting!")
    }
  }

  for (Region_name in regions) {

    ## "1) Get the polygons of a specific region from the shapefile") ----

    in_vect <- dplyr::filter(in_shp, Region == Region_name)
    # this is needed to remove duplicate polygons and keep only useful columns
    # of the vector
    in_vect <- unique(in_vect[c(1:4, 19)])
    # reproject to the CRS of the rasters
    in_vect <- sprawl::reproj_vect(in_vect, sprawl::get_proj4string(
      file.path(main_folder,"orig_mosaic/param_series/decirc/eos_decirc.tif")),
      verbose = FALSE)

    # in_vect contains one polygon for each sub_region of "Region_3_-_Central_Luzon"
    # plot_vect(in_vect, fill_var = "ID_name")

    # Create cropped rasters and put them in the \"subsets\" subfolder" ----

    in_rast_folder  <- file.path(main_folder, "orig_mosaic/param_series/")
    out_folder      <- file.path(main_folder, "subsets", Region_name)
    sprawl::make_folder(out_folder, type = "dirname", verbose = FALSE)

    # you have all you need to crop the mosaics: folder of the mosaic rasters, a
    # vector where to cut and an output folder. Cropped rasters will be available
    # in out_folder

    message("- ------------------------------------------------------- - ")
    message("- Cropping Mosaics on ", Region_name)
    message("- ------------------------------------------------------- - ")

    pr_extract_subarea(in_mosaics_folder = in_rast_folder,
                       in_mask           = in_vect,
                       out_folder        = out_folder)

    ## 2. Extract the data from the different provinces of the region" ----

    in_files <- list.files(
      file.path(out_folder, "param_series/"),
      pattern = "*.RData",
      full.names = TRUE, recursive = TRUE)
    in_files

    #### Cycle on all RData files - adjust nodata where needed.  ----

    message("- ------------------------------------------------------- - ")
    message("- Extracting data on ", Region_name)
    message("- ------------------------------------------------------- - ")

    for (file in in_files) {

      variable <- tools::file_path_sans_ext(basename(file))

      message("Extracting data for ", basename(file))

      in_rast <- get(load(file))
      if (basename(file) %in% c("eos_decirc.RData",
                                "pos_decirc.RData",
                                "sos_decirc.RData")) {
        na.value <- 32767
      }
      if (basename(file) %in% c("cumevi.RData",
                                "eos.RData",
                                "pos.RData",
                                "sos.RData")) {
        na.value <- -999
      }

      if (basename(file) %in% c("veglgt.RData",
                                "totlgt.RData",
                                "nseas.RData")) {
        na.value <- 0
      }

      ## 2. Use "sprawl::extract_rast() to extract data for each
      ## subregion ----

      out <- sprawl::extract_rast(in_rast,
                                  in_vect,
                                  na.value = na.value,
                                  join_geom = FALSE,
                                  join_feat_tbl = FALSE,
                                  id_field = "ID_name",
                                  verbose = FALSE)

      # Reshuffle a bit the outputs:
      #
      # 1. Add an "avgdate" column to out$stats and a "date" column to "out$alldata"
      # 2. Add a "year" column to both "out$alldata" and "out$stats"
      # 3. Add a column with an "identifier" for the phenorice parameter (e.g., eos, pos, etc.)

      if (basename(file) %in% c("eos_decirc.RData",
                                "pos_decirc.RData",
                                "sos_decirc.RData")) {
        names(out$stats)[3] <- "year"
        out$stats$year     <- lubridate::year(out$stats$year)
        out$stats$avgdate  <- as.Date("2000-01-01") + out$stats$avg
        out$stats$variable <- variable

        names(out$alldata)[3] <- "year"
        out$alldata$year      <- lubridate::year(out$alldata$year)
        out$alldata$date      <- as.Date("2000-01-01") + out$alldata$value
        out$alldata$variable  <- variable

        # reorder columns for easier reading ---
        out$stats <- out$stats %>%
          dplyr::select(ID_name, variable, band_n, year, avgdate, avg, med, sd, min, max,
                        n_pix_val, n_pix)

        out$alldata <- out$alldata %>%
          dplyr::select(ID_name, variable, band_n, year, date, value,N, n_pix_val)

        # reorder columns for easier reading ---

        out$stats <- out$stats %>%
          dplyr::select(ID_name, variable, band_n, year, avgdate, avg, med, sd, min, max,
                        n_pix_val, n_pix) %>%
          dplyr::rename(avgdoy = avg, meddoy = med, mindoy = min, maxdoy = max)


        out$alldata <- out$alldata %>%
          dplyr::select(ID_name, variable, band_n, year, date, value, N, n_pix_val)   %>%
          dplyr::rename(doy = value)

      }

      if (basename(file) %in% c("eos.RData",
                                "pos.RData",
                                "sos.RData")) {

        out$stats$avgdate   <- out$stats$date + out$stats$avg
        out$stats$variable  <- variable
        names(out$stats)[3] <- "year"
        out$stats$year      <- lubridate::year(out$stats$year)

        names(out$alldata)[3] <- "year"
        out$alldata$date      <- out$alldata$year + out$alldata$value
        out$alldata$year      <- lubridate::year(out$alldata$year)
        out$alldata$variable  <- variable

        # reorder columns for easier reading ---
        out$stats <- out$stats %>%
          dplyr::select(ID_name, variable, band_n, year, avgdate, avg, med, sd, min, max,
                        n_pix_val, n_pix) %>%
          dplyr::rename(avgdoy = avg, meddoy = med, mindoy = min, maxdoy = max)


        out$alldata <- out$alldata %>%
          dplyr::select(ID_name, variable, band_n, year, date, value, N, n_pix_val)   %>%
          dplyr::rename(doy = value)

      }



      ## 3. Save results as an RData file for future use" ----
      out_rdata_file <- file.path(out_folder, "RData",
                                  paste(Region_name, "stats.RData", sep = "_"))

      sprawl::make_folder(file.path(out_folder, "RData"))
      save(out, file = file.path(out_folder, "RData",
                                 paste(Region_name,
                                       basename(tools::file_path_sans_ext(file)),
                                       "stats.RData", sep = "_")))
    }
  }
}
