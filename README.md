## Installation
It is recommended that you have the latest version of the R environment installed. You need to install devtools and load it into the R environment, and then you can install circlizePlus from Github.

```install.packages("devtools")```

```library(devtools)```

```install_github("TianzeLab/circlizePlus")```

Every time you reboot the R environment, you need to load circlizePlus again.

```library(circlizePlus)```
## Acknowledgements
We are grateful to Dr. Zuguang Gu from DFKZ for his invaluable guidance and support throughout the project.
## Sample code 
[Dr. Gu](https://github.com/jokergoo) has written a book(["Circular Visualization in R"](https://jokergoo.github.io/circlize_book/book/)) explaining how circlize works. To illustrate the similarities and differences between circlize and cirzelizePlus, we've rewritten the code in the book as sample code. Visit [circlizePlusBook](https://tianzelab.github.io/circlizePlusBook/) for details.
## Addition rules
ccPlot(contain n ccPars) + 𝑐𝑐𝑃𝑎𝑟 = 𝑐𝑐𝑃𝑙𝑜𝑡(contain 𝑛 + 1 𝑐𝑐𝑃𝑎𝑟𝑠), 𝑛 ≥ 0  
ccPlot(contain n ccTracks) + 𝑐𝑐𝑇𝑟𝑎𝑐𝑘 = 𝑐𝑐𝑃𝑙𝑜𝑡(contain 𝑛 + 1 𝑐𝑐𝑇𝑟𝑎𝑐𝑘𝑠), 𝑛 ≥ 0  
ccPlot(contain n ccLinks) + 𝑐𝑐𝐿𝑖𝑛𝑘 = 𝑐𝑐𝑃𝑙𝑜𝑡(𝑐𝑜𝑛𝑡𝑎𝑖𝑛 𝑛 + 1 𝑐𝑐𝐿𝑖𝑛𝑘𝑠), 𝑛 ≥ 0  
ccTrak(contain n ccTrakGeoms) + 𝑐𝑐𝑇𝑟𝑎𝑐𝑘𝐺𝑒𝑜𝑚 = 𝑐𝑐𝑇𝑟𝑎𝑐𝑘(𝑐𝑜𝑛𝑡𝑎𝑖𝑛 𝑛 + 1 𝑐𝑐𝑇𝑟𝑎𝑐𝑘𝐺𝑒𝑜𝑚𝑠), 𝑛 ≥ 0  
ccTrack(contain n ccCells) + 𝑐𝑐𝐶𝑒𝑙𝑙 = 𝑐𝑐𝑇𝑟𝑎𝑐𝑘(𝑐𝑜𝑛𝑡𝑎𝑖𝑛 𝑛 + 1 𝑐𝑐𝐶𝑒𝑙𝑙𝑠), 𝑛 ≥ 0  
ccCell(contain n ccCellGeoms) + 𝑐𝑐𝐶𝑒𝑙𝑙𝐺𝑒𝑜𝑚 = 𝑐𝑐𝐶𝑒𝑙𝑙(𝑐𝑜𝑛𝑡𝑎𝑖𝑛 𝑛 + 1 𝑐𝑐𝐶𝑒𝑙𝑙𝐺𝑒𝑜𝑚𝑠), 𝑛 ≥ 0  
## S4 class ccCell and ccCells
- ccCell: Generate a cell container that belongs to a particular sector.
- ccCells: A list of multiple ccCells. Any ccCellGeom and ccCells are added together as if they were added to each ccCell contained in the ccCells.
## Data auto-population from ccTrack to ccGenomicCellGeom
|                                             |                   |                  |                  |                  |           |            |             |           |                              |                |
|---------------------------------------------|-------------------|------------------|------------------|------------------|-----------|------------|-------------|-----------|------------------------------|----------------|
| ccTrack Constructor                         | ccGenomicTrack()  | ccGenomicTrack() | ccGenomicTrack() | ccGenomicTrack() | ccTrack() | ccTrack()  | ccTrack()   | ccTrack() | ccTrack()                    | ccTrack()      |
| Parameters in ccTrack  Constructor          | data              | data             | data             | data             | x, y      | x, y       | x, y        | x, y      | x, y                         | x, y           |
| ccGenomicCellGeom Constructor               | ccGenomicPoints() | ccGenomicLines() | ccGenomicRect()  | ccGenomicText()  | ccLines() | ccPoints() | ccPolygon() | ccText()  | ccRect()                     | ccSegments()   |
| Parameters in ccGenomicCellGeom Constructor | region, value     | region, value    | region, value    | region, value    | x, y      | x, y       | x, y        | x, y      | xleft, ybottom, xright, ytop | x0, y0, x1, y1 |
