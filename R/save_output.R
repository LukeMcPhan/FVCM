#' Save State Transition Model Outputs
#'
#' Writes raster and summary table outputs from the matrix projection step to disk.
#'
#' @param matrix_projection_output Output list from `matrix_projection()`.
#' @param rule_set_name Name of the transition rule set used.
#' @param vegetation_layer_name Name of the vegetation GDB layer.
#' @param inundation_raster_name Name of the inundation raster file.
#' @param flow_data_file_name Name of the flow data file.
#' @param scenario_name Optional scenario identifier to include in filenames.
#' @param water_years Vector of evaluated water years.
#' @param inundation_timeseries Optional list of inundation timeseries outputs.
#'
#' @return No return value. Files are written to disk.
#' @export
save_output <- function(matrix_projection_output,
                        rule_set_name,
                        vegetation_layer_name,
                        inundation_raster_name,
                        flow_data_file_name,
                        scenario_name = "",
                        water_years,
                        inundation_timeseries = FALSE) {

  matrix_projection_output$summarised_annual_states %>%
    readr::write_csv(stringr::str_c("Output/summarised_annual_states_", scenario_name, ".csv"))

  message("Summarised annual states saved!\n")

  if (!identical(inundation_timeseries, FALSE)) {
    names <- names(inundation_timeseries)

    purrr::map2(.x = inundation_timeseries,
                .y = names,
                ~readr::write_csv(.x, stringr::str_c("Output/inundation_timeseries_", scenario_name, "_", .y, ".csv")))

    message("Inundation timeseries saved!\n")
  }

  for (i in seq_along(water_years)) {
    filename <- stringr::str_c(
      stringr::str_c("Output/state_raster",
                     rule_set_name,
                     vegetation_layer_name,
                     inundation_raster_name,
                     flow_data_file_name,
                     scenario_name, sep = "__"),
      "_",
      water_years[i],
      ".grd")

    raster::writeRaster(matrix_projection_output$annual_raster_list[[i]],
                        filename = filename,
                        format = "raster",
                        overwrite = TRUE)
  }

  message("Writing complete!!!\n####____####\n\nFilename structure in folder is as follows = \n`Output/state_raster`\n__`rule set file name`\n__`vegetation layer`\n__`inundation layer`\n__`flow_gauge`\n__`scenario(if included)`\n`water_year`.grd/gri")
}
