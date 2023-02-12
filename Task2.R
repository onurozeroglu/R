#Finding the end of the maze map

readMap <- function(fPath) {
  lines <- readLines(fPath)
  line_array <- lapply(lines, function(line) {
    strsplit(line, "")[[1]]
  })
  res_arr <- do.call(rbind, line_array)
  
  return(res_arr)
}

findWay <- function(map = map) {
  
  for (row in 1:dim(map)[1] ) {
    for (col in 1:dim(map)[2]) {
      if (map[row,col]=="s") {
        begin <- list(c(row, col))[[1]]
        mat_double <- sapply(begin, as.double)
      }
      if (map[row,col]=="e") {
        goal<- list(c(row, col))[[1]]
        mat_double <- sapply(goal, as.double)
      }
    }
  }
  
  open_l = list()
  close_l = list()
  
  row_bnd = dim(map)[1]
  column_bnd = dim(map)[2]
  
  base = list(
    loc = c(begin[1], begin[2]),
    parent = NULL,
    f = 0,
    g = 0,
    h = 0
  )
  
  open_l <- append(open_l, list(base))
  
  new_node <- function(loc,p,f,g,h) {list(list(loc = loc,parent = p,
      f = f,
      g = g,
      h = h
    ))
  }
  
  while(length(open_l)!=0) {unpacked_open = sapply(open_l, '[[', 'f')
    idx_current = which(unpacked_open == min(unpacked_open))
    current = open_l[idx_current]
    
    open_l <- open_l[-idx_current]
    close_l <- append(close_l, current)
    
    z <- current[[1]]$loc
    df <- data.frame(r = c(z[1]-1,z[1]+1,z[1],z[1]),
                     c = c(z[2],z[2],z[2]+1,z[2]-1))
    
    df <- df[!(df$r < 1 | df$r > row_bnd),]
    df <- df[!(df$c < 1 | df$c > column_bnd),]
    
    for(i in 1:nrow(df)) { n_loc = c(df$r[i],df$c[i])
      
      if(map[n_loc[1],n_loc[2]]== "e") {trail <- data.frame(
          r = c(n_loc[1]),
          c = c(n_loc[2])
        )

        while(!is.null(current[[1]]$parent)){
          walk = current[[1]]$loc
          trail <- rbind(trail, walk)
          current = current[[1]]$parent
        }
        
        trail <- rbind(trail, begin)
        return(trail)
      }
      
      unpacked_closed = sapply(close_l, '[[', 'loc')
      idx_closed = which(apply(unpacked_closed, 2, function(col) all(col == n_loc)))
      if(length(idx_closed) | map[n_loc[1],n_loc[2]]!= " ") {
        next
      }
      

      g = current[[1]]$g + sum((n_loc - current[[1]]$loc)**2) 

      h = sum((n_loc - goal)**2)
      f = g + h # total cost
      
      
      unpacked_open_loc = sapply(open_l, '[[', 'loc')
      if(length(unpacked_open_loc)) {
        idx_open_loc = which(apply(unpacked_open_loc, 2, function(col) all(col == n_loc)))
      }
      else {
        idx_open_loc = c()
      }
      
      if(length(idx_open_loc)) {
        if(g > open_l[[idx_open_loc]]$g) {
          next 
        }
      }
      
      open_l = append(open_l, new_node(n_loc, current,f ,g, h))
    }
  }
}

plotMap <- function(map = map, wallCol = "black") {
  plot(NULL,
       xlim = c(1,dim(map)[2]),ylim = c(dim(map)[1],1),
       type = "l",asp = 1,axes = F,ylab = "",xlab = "")
  walls <- which(map == "x", arr.ind = T)
  pad = 1/2
  rect(walls[,2]-pad,walls[,1]-pad,walls[,2]+pad,walls[,1]+pad, col = wallCol)
}


plotPath <- function (trc, col_path = "maroon"){
  if (is.null(dev.list())) {
    stop("Please plot the map first!")
  }
  lines(r ~ c, data = trc, lwd = 3, col = col_path)
  points(r ~ c, data = trc, lwd = 2, pch = 16, col = col_path)
  points(trc$c[1],trc$r[1], pch = 21, lwd = 4)
  points(trc$c[nrow(trc)],trc$r[nrow(trc)], pch = 1, lwd = 5)
}


m = readMap('XXXX')
trail = findWay(m)
plotMap(m, "black")
plotPath(trail)

