#########
# Bivariate scatterplots
# Add lines
# Add points
# control colors, symbols and labels
manipulate(
  plot(cars, xlim=c(0,x.max)),  
  x.max=slider(15,45))

# ===========       


#### http://rstudio.org/docs/advanced/manipulate
#### http://www.statmethods.net/graphs/scatterplot.html
#### http://www.statmethods.net/advgraphs/index.html
#### http://www.statmethods.net/advgraphs/parameters.html

