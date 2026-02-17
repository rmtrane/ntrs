# .onLoad <- function(libname, pkgname) {
#   ## Run all .setup_CLASS_versions()
#   ns <- getNamespace("NpsychBatteryNormsS3")
#   lapply(
#     grep(pattern = "^.setup_.+_versions", x = names(ns), value = T),
#     \(x) do.call(x, args = list())
#   )
# }
