# get_hdf_dates returns the dates of the hdf files
#' Get the dates of the MODIS hdf files.
#' @param hdf_path The path of the downloaded hdf files.
#' @param pattern Pattern to look for. Pass to \code{\link[stringr]{str_extract}}.
#' @return A character vector.
get_hdf_dates <- function(hdf_path, pattern = "\\d{4}M\\d{2}"){
  hdf_list <- list.files(hdf_path, pattern = ".hdf$", full.names = T)
  # get the length of the time series
  return(unique(stringr::str_extract(hdf_list, pattern = pattern)))
}
