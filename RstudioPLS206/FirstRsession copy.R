# A first session in R
# *************************************************************************
# Text after # is ignored
# Use these comments to fully document what you are doing
# The Mac R GUI has a code editor. Windows has just a text window.

# R is an object-oriented environment and interpreted programming language.
# "Interpreted" means that the lines of code are processed one at a time
# wihtout compilation of whole programs. This limits speed, but to a level
# that is plenty for anything we do.

# In Mac OS X you type lines of code here and press Command-Return to submit your
# code to the console. In Windows you press CTRL-R.

# Change directory to the directory that contains the file "xmpl_filaree.csv".

setwd("/Users/ealaca/Documents/TeachServ/CLASSES/AGR206/PLS206F10/Examples")

# Read a data set into a data frame. Data frames are the basic objects to hold typical data.

filaree<-read.csv("xmpl_filaree.csv", header=TRUE)

# Help is readily and abundantly available.

help(read.csv)

# List the objects in the workspace:

ls()

# The up and down arrows are useful to recall lines of code already submitted.
# This allows you to fix mistakes and add code rapidly.

# We can view the characteristics of our data as follows:
# First, all objects in R have class.

class(filaree) 

# The object "filaree" is a data frame. It has dimensions and length.

dim(filaree)
length(filaree)

# The lenght of a data frame is the number of columns.
# Columns have names because we specified header=T and had names in our file.

names(filaree)

# Notice that R does not like symbols in the names of things, so it changed the
# "-" to "." R likes to use periods to separate parts of names for easy reading.
# Periods in names do not mean anything in particular; they are just good to
# improve the readability of meaningful names. You can use any names you want.
# Objects are created when the result of a function is "sent" into a name.

my.summary<-summary(filaree)
class(my.summary)
ls()
my.summary # A line of code with the name of an object shows the object.

# The Mac OS GUI also shows the objects through the Workspace browser window.

# *********************************************************************
# PACKAGES
# *********************************************************************
# R programs are available in packages. Some packages are installed and
# loaded automatically, but others need to be installed and manually loaded
# before you can use them.

install.packages()

# Rcmndr and sos are very important packages