test_that("Share data in ccLines and ccPoints",{
  sectors <- c('a','a','a','a','b','b','b','b','c','c','c','c','d','d','d','d')
  x1 <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
  y1 <- c(1,2,3,4,4,3,2,1,1,1,1,1,1,2,1,2)
  cc <- ccPlot(initMode = "initialize", sectors = sectors, x = x1)
  cells <- ccCells(sector.indexes = letters[1:4])
  cc_point <- ccPoints()
  cells <- cells + cc_point + ccLines()
  track2 <- ccTrack(sectors = sectors, x=x1, y = y1)
  track2 <- track2 + cells
  show(cc  + track2)
  succeed()
})

test_that("Two standard tracks", {
  par1=ccPar("track.height" = 0.1)

  set.seed(999)
  n = 1000
  sectors = sample(letters[1:8], n, replace = TRUE)
  x1 = rnorm(n)
  y1 = runif(n)
  cc=ccPlot(initMode = "initialize", sectors = sectors, x = x1)

  track1 = ccTrack(sectors = sectors, y = y1)
  col = rep(c("#FF0000", "#00FF00"), 4)
  trackPoint1 = ccTrackPoints(sectors = sectors, x = x1, y = y1, col = col,
                              pch = 16, cex = 0.5)
  track1 = track1 + trackPoint1

  cells = ccCells(sector.indexes = letters[1:8])
  ind = sample(length(x1), 10)
  x2 = x1[ind]
  y2 = y1[ind]
  od = order(x2)
  cc_line = ccLines(x = x2[od], y = y2[od])
  cells = cells + cc_line

  track2 = ccTrack(sectors = sectors, y = y1)
  track2 = track2 + cells


  show(cc + par1 + track1 + track2)
  succeed()
})
