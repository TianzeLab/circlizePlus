## Citation
Zhang Z, Cao T, Huang Y and Xia Y (2025) CirclizePlus: using ggplot2 feature to write readable R code for circular visualization. Front. Genet. 16:1535368. doi: 10.3389/fgene.2025.1535368

## Highlights

-   Programming in circlizePlus is summarized into 5 addition rules, which are simple and clear.
-   Like ggplot2, it makes programming circular visualizations in the form of addition of plotting functions.
-   In ggplot2, the data parameter of the function that draws geometric figures can be missing, and it will use the data parameter value in the ggplot() function. circlizePlus implements similar functionality. In circlizePlus, the function that draws geometric figures can get the default coordinate data (such as x, y) from the track it belongs to.
-   Like ggplot2, it supports data mapping. The coordinate parameters are mapped to the default values ​​by passing in an anonymous function of the form "function(x,y){...}".

## Installation

It is recommended that you have the latest version of the R environment installed. You need to install devtools and load it into the R environment, and then you can install circlizePlus from Github.

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
install_github("TianzeLab/circlizePlus")
```

Every time you reboot the R environment, you need to load circlizePlus again.

`library(circlizePlus)`

## Acknowledgements

We are grateful to Dr. Zuguang Gu from DFKZ for his invaluable guidance and support throughout the project.

## Sample code

[Dr. Gu](https://github.com/jokergoo) has written a book(["Circular Visualization in R"](https://jokergoo.github.io/circlize_book/book/)) explaining how circlize works. To illustrate the similarities and differences between circlize and cirzelizePlus, we've rewritten the code in the book as sample code. Visit [circlizePlusBook](https://tianzelab.github.io/circlizePlusBook/) for details.


