# GRASS session initialisation
# Sourced by entrypoint scripts after config.R.
grass_session_metadata <- tryCatch(gmeta(), error = function(e) NULL)
if (is.null(grass_session_metadata)) {
  print("Initializing GRASS session")
  initGRASS(gisBase = config$gisBase, gisDbase = config$GrassDataBase, override = TRUE)
  print("Successfully initialized GRASS session")
} else {
  print("GRASS session is already running")
}
