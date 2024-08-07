test_that("Share data in genomics track", {
  data = generateRandomBed(nr =30, nc = 2)
  all_chr = c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY")
  cc = ccPlot(initMode = "initializeWithIdeogram", plotType=NULL)
  t1 = ccGenomicTrack(data=data, numeric.column = 4,
                      panel.fun=function(region,value,...){
                        circos.genomicPoints(region,value,...)
                      })
  cells1 = ccCells(sector.indexes = all_chr) + ccGenomicLines(numeric.column=2) + ccGenomicPoints(region=\(region,value){region}, value=\(region,value){value}, numeric.column=2)
  t1 = t1 + cells1
  show(cc+t1)
  succeed()
})
