# Notes:
# Frank, 2016-12-06, fanhongxiang13@mails.ucas.ac.cn
# Please install MRT tools in advance if you want to use these functions,
# The suggested path for MRT is "C:/MRT/bin", which is the default setting of these functions
# Customized function for reprojecting MOD16 HDF files and saving in desired format
# Modified from the original version in the 'rts' pacakge by Babak Naimi

#' Reproject MODIS hdf file and save it in desired format.
#' @param hdf_name The name of the hdf file.
#' @param dst_fpname The full name of the output file. e.g. "C:/out.tif".
#' @param mrt_path The path of the MRT executable file. If missing, default is "C:/MRT/bin".
#' @param UL The upper-left corner of the processing extent.
#' @param LR The lower-right corner of the processing extent.
#' @param resample_type The resample method used. Default is "NEAREST_NEIGHBOR".
#' @param proj_type The projection type used. Default is "UTM".
#' @param bands_subset A character vector indicating the spectral subset of the input hdf file.
#' @param proj_params A character vector indicating the project parameters.
#' @param datum Character of datum to use. Default is "WGS84".
#' @param utm_zone Number of the UTM zones when projection. Only used when proj_type = "UTM".
#' @param pixel_size Number of the output pixel size. Only used when proj_type != "GEO".
#' @description Modified from the original version in the 'rts' pacakge by Babak Naimi.
#' It internally use MODIS Reproject Tool (MRT) software to reproject the mosaiced image
#' to a specified coordinate system. As the format of the source images in LP DAAC is HDF,
#' this tool can also convert them into other formats (i.e. Geotif, hdr).
#' @note To have the functionality for Mosaic and reprojecting of the images,
#' you need to first install MRT software (\url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool})
#' on your machine, and introduce its' path through the
#' mrt_path argument.

reproject_modis_hdf <- function(hdf_name,
                                dst_fpname,
                                mrt_path,
                                UL = "",
                                LR = "",
                                resample_type = 'NEAREST_NEIGHBOR',
                                proj_type = 'UTM',
                                bands_subset = '',
                                proj_params = '0 0 0 0 0 0 0 0 0 0 0 0 0 0 0',
                                datum = 'WGS84',
                                utm_zone = 50,
                                pixel_size = 1000){
  if(missing(mrt_path)) mrt_path <- "C:/MRT/bin"
  proj_prm_fpname <- normalizePath(tempfile(pattern = "temp_proj",
                                            tmpdir = "./temp",
                                            fileext = ".prm"),
                                   mustWork = F,
                                   winslash = "/")
  proj_prm_file = file(proj_prm_fpname, open = "wt")
  write(paste('INPUT_FILENAME = ',hdf_name, sep = ""), proj_prm_file)
  if (bands_subset != '') {
    write(paste('SPECTRAL_SUBSET = ( ',bands_subset,' )', sep = ''), proj_prm_file, append = TRUE)
  }
  if (UL[1] != '' & LR[1] != '') {
    write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', proj_prm_file, append=TRUE)
    write(paste('SPATIAL_SUBSET_UL_CORNER = ( ', as.character(UL[1]),' ',as.character(UL[2]),' )',sep = ''), proj_prm_file, append = TRUE)
    write(paste('SPATIAL_SUBSET_LR_CORNER = ( ', as.character(LR[1]),' ',as.character(LR[2]),' )',sep = ''), proj_prm_file, append = TRUE)
  }
  write(paste('OUTPUT_FILENAME = ', dst_fpname, sep = ""), proj_prm_file, append = TRUE)
  write(paste('RESAMPLING_TYPE = ',resample_type, sep = ''), proj_prm_file, append = TRUE)
  write(paste('OUTPUT_PROJECTION_TYPE = ',proj_type, sep = ''), proj_prm_file, append = TRUE)
  write(paste('OUTPUT_PROJECTION_PARAMETERS = ( ',proj_params,' )', sep = ''), proj_prm_file, append = TRUE)
  write(paste('DATUM = ',datum,sep=''), proj_prm_file, append = TRUE)
  if (proj_type == 'UTM') write(paste('UTM_ZONE = ',utm_zone, sep = ''), proj_prm_file, append = TRUE)
  if (proj_type != 'GEO') write(paste('OUTPUT_PIXEL_SIZE = ',as.character(pixel_size), sep = ''), proj_prm_file, append = TRUE)
  close(proj_prm_file)
  e <- system(paste(mrt_path, '/resample -p ', proj_prm_fpname, sep = ''))
  if (e == 0) return (TRUE)
  else return(FALSE)
}
