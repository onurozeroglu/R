#Size report using R

sizeReport <- function(path, patt = ".*", dironly = FALSE, level = Inf) {
  files <- data.frame(name = character(), size = numeric())
  walkDir <- function(path, level) {
    filesInDir <- list.files(path, recursive = FALSE)
    for (file in filesInDir) {
      fullPath <- file.path(path, file)
      if (dironly && !dir.exists(fullPath)) {next}
      if (dir.exists(fullPath) && level > 0) {walkDir(fullPath, level - 1)
      } else {
        
        if (!dir.exists(fullPath) && grepl(patt, file)) {
          files <<- rbind(files, data.frame(name = fullPath, size = file.size(fullPath)))
        }
      }
    }
  }
  walkDir(path, level)
  
  
  return(files)
}
sizeReport(path = "C:/Users/XXXX")
sizeReport(path = "C:/Users/XXXX" , level=0)
#sizeReport(path = "C:/Users/XXXX", dironly = TRUE)
#sizeReport(path = "C:/Users/XXXX", patt = "png$")
#sizeReport(path = "C:/Users/XXXX", patt = "fig[1-4]")
#sizeReport(path = "C:/Users/XXXX", patt = "proje[ck]t", dironly = TRUE, level = 1)
