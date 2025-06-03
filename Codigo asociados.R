if (!require("shiny")) {
  install.packages("shiny")
}
runGitHub("jventural/RenCal_app")

renv::snapshot()

rsconnect::writeManifest()

txt <- readLines("app.R", encoding = "latin1", warn = FALSE)
writeLines(enc2utf8(txt), "app.R", useBytes = TRUE)


# 1) Leer todas las líneas asumiendo UTF-8 (porque tu app.R ya está en UTF-8)
texto_original <- readLines("app.R", encoding = "UTF-8")

# 2) Simplemente volver a escribirlas en un nuevo archivo, forzando UTF-8 puro
#    (sin pasar por latin1 ni otra codificación)
writeLines(texto_original, "app_utf8_fixed.R", useBytes = TRUE)

parse(file = "app_utf8_fixed.R", encoding = "UTF-8")

