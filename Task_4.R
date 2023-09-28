
# Load necessary libraries
library(R6)
library(tools)
library(ggplot2)
library(hexbin)           # required for the 'stat_bin_hex()' function of ggplot2


# Domain class
Domain <- R6::R6Class("Domain",
                      
                      lock_objects = FALSE,
                      
                      public = list(
                        
                        #' @field file_path The file path parameter as a character string
                        file_path = NULL,
                        
                        #' @field resolution A numeric vector that includes the resolutions that should be plotted
                        resolution = NULL,
                        
                        #' @description
                        #' Create a new Domain object
                        #' @param file_path A character string specifying a valid path to a .csv file
                        #' @return A new `Domain` object.
                        #' 
                        initialize = function(file_path,
                                              resolution = c(0.3, 0.5, 1.0)) {
                          
                          # check that the input file is of type .csv
                          ext_file <- tools::file_ext(x = file_path)
                          if (ext_file != 'csv') stop("The input file must be a .csv file!")
                          
                          # read the data and make it available for the other methods of the 'Domain' R6 class
                          self$data <- read.csv(file = file_path, stringsAsFactors = FALSE, header = TRUE)
                          
                          # make the resolution available for the other methods
                          self$resolution <- resolution
                          
                          # the length of the resolution vector
                          len_res = length(resolution)
                          if (len_res == 0) stop("The 'resolution' vector must be not empty!")
                          
                          self$max_res <- 10
                        },
                        
                        
                        #' @description
                        #' The public method 'adjust_range' increases the upper and lower x,y axis limits during visualization
                        #' @param vec a vector with the current upper and lower bounds
                        #' @examples
                        #' 
                        #' Domain$public_methods$adjust_range(c(0, 1))
                        #' 
                        adjust_range = function(vec) {
                          vec[1] <- vec[1] - 2
                          vec[2] <- vec[2] + 2
                          return(vec)
                        },
                        
                        
                        #' @description
                        #' The public method 'color_name_to_hex' converts a color name to a hexadecimal value
                        #' @param color_name A character string specifying a color name
                        #' @examples
                        #' 
                        #' Domain$public_methods$color_name_to_hex('green')
                        #'
                        color_name_to_hex = function(color_name) {
                          if (!is.na(color_name)) {
                            # Convert the color name to RGB
                            rgb_values <- col2rgb(color_name, alpha = TRUE)
                            # Convert the RGB values to hexadecimal
                            hex_name <- rgb(rgb_values[1,]/255, rgb_values[2,]/255, rgb_values[3,]/255, rgb_values[4,]/255)
                          }
                          return(hex_name)
                        },
                        
                        
                        #' @description
                        #' The public method 'color_name_to_hex' converts a color name to a hexadecimal value
                        #' @param color_name A character string specifying a color name
                        #' @examples
                        #' 
                        #' Domain$public_methods$rgb_to_hex(rgb_vec = c(0, 1, 1, .3))
                        #'
                        rgb_to_hex = function(rgb_vec) {
                          if (length(rgb_vec) != 4) stop("The input vector 'rgb_vec' must be of length four (including the 'alpha' image band)!")
                          hex_name <- rgb(rgb_vec[1], rgb_vec[2], rgb_vec[3], rgb_vec[4])
                          return(hex_name)
                        },
                        
                        
                        #' @description
                        #' The 'plotDomain' method which takes plot parameters and displays the results
                        #' @param resolutionIndex An integer value specifying the index of the value from the input 'resolution' vector
                        #' @param ... Usage of additional ggplot2 parameters such as 'color' and 'fill' (see the 'examples' section)
                        #' @param color_border A character string specifying the color of the hexagonal borders
                        #' @param fill_color_cells A character string specifying the filled color inside the hexagonal cells
                        #' @examples
                        #'
                        #' pth_file = 'Project1_test_data_1.csv'
                        #' if (!file.exists(pth_file)) stop('The file does not exist!')
                        #'
                        #' init = Domain$new(file_path = pth_file, resolution = c(0.3, 0.5, 1.0))
                        #' 
                        #' plt = init$plotDomain(resolutionIndex = 3, color = 'darkblue', fill = 'lightblue')
                        #' plt
                        #' 
                        #' plt = init$plotDomain(resolutionIndex = 2, color = 'yellow', fill = 'orange')
                        #' plt
                        #' 
                        #' plt = init$plotDomain(resolutionIndex = 1, color = 'green', fill = 'red')
                        #' plt
                        #'
                        plotDomain = function(resolutionIndex, ...) {

                          # error handling
                          if (length(resolutionIndex) != 1) stop("The 'resolutionIndex' must be a single numeric or integer value!")
                          if (resolutionIndex <= 0 | resolutionIndex > length(self$resolution)) stop("The 'resolutionIndex' must be a positive number and the maximum value must be equal to the length of the 'resolution' input vector!")
                          
                          # get the resolution for the selected index
                          res_selected <- self$resolution[resolutionIndex]
                          
                          # if the ggplot object does not exist create it and save it to the private list
                          if (length(private$PLT) == 0) {
                            plt <- ggplot2::ggplot(data = self$data, mapping = ggplot2::aes(x = x, y = y)) +
                              ggplot2::xlim(self$adjust_range(vec = range(self$data$x))) +
                              ggplot2::ylim(self$adjust_range(vec = range(self$data$y)))
                            
                            private$PLT[['init_layer']] <- plt
                          } else {
                            # .. else create the object 'plt' from the existing private ggplot object
                            plt <- private$PLT[['init_layer']]
                          }
                          
                          # every time I run the function the private list of layers will be updated
                          layer <- ggplot2::stat_bin_hex(bins = self$max_res / res_selected,
                                                         show.legend = FALSE,
                                                         ...)

                          # the private layer will be saved to the private list
                          private$LAYERS[[as.character(resolutionIndex)]] <- layer
                          
                          # all existing layers will be visualized iteratively
                          for (item_layer in private$LAYERS) {
                            plt <- plt + item_layer
                          }
                          
                          # if the points layer exists then visualize the points
                          if (length(private$POINTS) > 0) {
                            plt <- plt + private$POINTS[['points']]
                          }
                          
                          return(plt)
                        },
                        
                        
                        #' @description
                        #' The 'plotPoints' method takes the x,y coordinates and displays the points on the hexagonal map
                        #' @examples
                        #' 
                        #' pth_file = 'Project1_test_data_1.csv'
                        #' if (!file.exists(pth_file)) stop('The file does not exist!')
                        #'
                        #' init = Domain$new(file_path = pth_file, resolution = c(0.3, 0.5, 1.0))
                        #' 
                        #' plt_pnts = init$plotPoints(color = 'purple')
                        #' plt_pnts
                        #' 
                        plotPoints = function(...) {
                          
                          # if the ggplot object does not exist create it and save it to the private list
                          if (length(private$PLT) == 0) {
                            plt <- ggplot2::ggplot(data = self$data, mapping = ggplot2::aes(x = x, y = y)) +
                              ggplot2::xlim(self$adjust_range(vec = range(self$data$x))) +
                              ggplot2::ylim(self$adjust_range(vec = range(self$data$y)))
                            
                            private$PLT[['init_layer']] <- plt
                          } else {
                            # .. else create the object 'plt' from the existing private ggplot object
                            plt <- private$PLT[['init_layer']]
                          }
                          
                          # create the geometry points layer
                          layer_pnts <- geom_point(...)
                          
                          # the private layer will be saved to the private list
                          private$POINTS[['points']] <- layer_pnts
                          
                          # all existing layers will be visualized iteratively
                          if (length(private$LAYERS) > 0) {
                            for (item_layer in private$LAYERS) {
                              plt <- plt + item_layer
                            }
                          }

                          # add the geometry points to the plot
                          plt <- plt + layer_pnts
                          
                          return(plt)
                        },
                        
                        
                        #' @description
                        #' The 'reset_plots' method resets the existing private objects and clears the graphics
                        #' @examples
                        #' 
                        #' pth_file = 'Project1_test_data_1.csv'
                        #' if (!file.exists(pth_file)) stop('The file does not exist!')
                        #'
                        #' init = Domain$new(file_path = pth_file, resolution = c(0.3, 0.5, 1.0))
                        #' 
                        #' plt_pnts = init$plotPoints(color = 'purple')
                        #' plt_pnts
                        #' 
                        #' # reset the plots
                        #' reset_plots()
                        #' 
                        reset_plots = function() {
                          private$PLT = list()
                          private$LAYERS = list()
                          private$POINTS = list()
                          graphics.off()
                        }
                      ),
                      
                      
                      # lists of private objects required to create the hexagonal and point plots
                      private = list(
                        PLT = list(),
                        LAYERS = list(),
                        POINTS = list()
                      )
)


#................................................  Examples using the file 'Project1_test_data_1.csv'   ................................................

pth_1st_file <- 'C:/Users/XXXXX/data_1.csv'
if (!file.exists(pth_1st_file)) {
  stop("The file 'Project1_test_data_1.csv' does not exist! Make sure to specify the full file path!")
}

# initialize the 'Domain' R6 class
init <- Domain$new(file_path = pth_1st_file, resolution = c(0.3, 0.5, 1.0))

# plot the hexagonals using each time a different resolution index. The function also takes
# additional ggplot2 visualization parameters and works also with color names
plt <- init$plotDomain(resolutionIndex = 3, color = 'darkblue', fill = 'lightblue')
plt

plt <- init$plotDomain(resolutionIndex = 2, color = 'yellow', fill = 'orange')
plt

plt <- init$plotDomain(resolutionIndex = 1, color = 'green', fill = 'red')
plt

# plot the points
plt_pnts <- init$plotPoints(color = 'purple')
plt_pnts

# reset the existing objects and clear the graphics
init$reset_plots()

#................................................  Examples using the file 'Project1_test_data_2.csv'   ................................................

pth_2nd_file <- 'C:/Users/XXXXX/data_2.csv'
if (!file.exists(pth_2nd_file)) {
  stop("The file 'Project1_test_data_2.csv' does not exist! Make sure to specify the full file path!")
}

# initialize the 'Domain' R6 class (using a different resolution compared to the 1st example)
init_2nd <- Domain$new(file_path = pth_2nd_file, resolution = c(0.25, 0.65))

# takes a color name and returns the hexadecimal value
hex_pnts <- Domain$public_methods$color_name_to_hex(color_name = 'purple')
hex_pnts

# plot the points (use as input color a hexadecimal value)
plt_pnts_2nd <- init_2nd$plotPoints(color = hex_pnts)
plt_pnts_2nd


# plot the hexagonals using each time a different resolution index (for this example we only use 2 resolution
# values). The function also takes additional ggplot2 visualization parameters and works also with color names
plt_2nd <- init_2nd$plotDomain(resolutionIndex = 2, color = 'darkblue', fill = 'lightblue')
plt_2nd

plt_2nd <- init_2nd$plotDomain(resolutionIndex = 1, color = 'yellow', fill = 'orange')
plt_2nd

# reset the existing objects and clear the graphics
init_2nd$reset_plots()

#................................................  Examples using the file 'Project1_test_data_3.csv'   ................................................

pth_3rd_file <- 'C:/Users/XXXXX/data_3.csv'
if (!file.exists(pth_3rd_file)) {
  stop("The file 'Project1_test_data_3.csv' does not exist! Make sure to specify the full file path!")
}

# initialize the 'Domain' R6 class (using one value as a resolution)
init_3rd <- Domain$new(file_path = pth_3rd_file, resolution = c(0.45))

# plot the hexagonals using a resolution index (for this example we only have 1 resolution value. The function 
# also takes a vector of RGB values and covert it to a hexadecimal value to be used in the ggplot2 method

# RGB color vector (including the 'alpha' image band)
color_border <- c(0, 0, 1, .4)
hex_border <- Domain$public_methods$rgb_to_hex(rgb_vec = color_border)
hex_border

# function to make the color lighter
lighter <- function(color, percentLighter = 50) {
  r <- strtoi(paste0("0x", substr(color, 2, 3))) / 255
  g <- strtoi(paste0("0x", substr(color, 4, 5))) / 255
  b <- strtoi(paste0("0x", substr(color, 6, 7))) / 255
  a <- strtoi(paste0("0x", substr(color, 8, 9))) / 255
  a <- a * percentLighter / 100
  return(rgb(r, g, b, a))
}

# the color to fill the hexagonal cells (as an RGB vector)
color_fill <- c(1, 0, 0, 0.4)
hex_fill <- Domain$public_methods$rgb_to_hex(rgb_vec = color_fill)
hex_fill

# make the color lighter
col_lighter <- lighter(color = hex_fill)
col_lighter

# plot the hexagonal cells
plt_3rd <- init_3rd$plotDomain(resolutionIndex = 1, color = hex_border, fill = col_lighter)
plt_3rd

# plot also the points
plt_pnts_3rd <- init_3rd$plotPoints(color = 'purple')
plt_pnts_3rd

# reset the existing objects and clear the graphics
init_3rd$reset_plots()

#................................................  End of Examples section   ................................................
