#' Starting SNOW Clusters
#' @description Functions to start a SNOW cluster and to set default cluster options.
#' @param noCores Either a character vector of host names on which to run the worker copies of R,
#' or a positive integer (in which case that number of copies is run on localhost).
#' @param logfile The logfile can be used to specify the file to which slave node output is to be directed.
#' The default is /dev/null; during debugging of an installation it can be useful to set this to a proper file.
#' On some systems setting logfile to "" or to /dev/tty will result in worker output being sent to
#' the terminal running the master process.
#' @param export Character list of variables to export. Pass to \code{\link[snow]{clusterExport}}.
#' @param lib Character list of packages to export. Pass to \code{\link[snow]{clusterEvalQ}}.
#' @export
create_snow_cluster = function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  cl <- snow::makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) snow::clusterExport(cl, export)
  if(!is.null(lib)) {
    plyr::l_ply(lib, function(dum) {
      snow::clusterExport(cl, "dum", envir = environment())
      snow::clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  doSNOW::registerDoSNOW(cl)
  return(cl)
}
