# A first session in R
# *************************************************************************
# The objective of this script is to introduce you (operatively) to the use of R in more or less 300 lines of code.
# Text after # is ignored by R, then we will put comments there.
# Use these comments to fully document what you are doing
# The Mac R GUI has a code editor. Windows has just a text window. Now, RStudio, a multi-platform GUI, displays everything that you need only in one window

# R is an object-oriented environment and interpreted programming language.
# "Interpreted" means that the lines of code are processed one at a time without compilation of whole programs. This limits speed, but to a level that is plenty for anything we do.

#You have to understand that R as any language has a structure, syntax and a grammar (basically). In R we have packages, which contain functions that perform routines. In order to do that you have to load a package (or also called library) and after that apply the function on an object (lets say a data set).

#In R, basically, you will have that a function usually looks like: function(object, option1, option2, … optionN). Thus, you have options into each function. These functions are useful to perform correctly your analysis or to make that your graphs look well 

# In Mac OS X you type lines of code here (in the script editor window) and press Command-Return to submit your code to the console. In Windows you press CTRL-R.

# A very basic and important step is to input and manipulate data. We will be doing this routinely. Professional statisticians spend about 80% of the "data analysis" time manipulating, organizing and cleaning the data. 

# First, create a folder in your desktop, let say that the name is FirstRsession

# Change directory to the directory that contains the file "xmpl_filaree.csv". Lets say that you are using the zip file "first R session", you unzipped on your desktop, and then, you use:

setwd("~/Desktop/PLS206F12/FirstRsession/") #set working directory

# Check if you are in the correct path.

getwd() # get the working directory, the default directory for files

#If you are in the correct path, then, you can load files directly
# Read a data set into a data frame. Data frames are the basic objects to hold typical data.

filaree<-read.csv("xmpl_filaree.csv", header=TRUE)

#In this case we are inputting a comma-separated values file type, but we can input tab-delimited text file by using the function read.txt()

# List the objects in the workspace, which objects are already loaded and available in the workspace?

ls()

# The up and down arrows are useful to recall lines of code already submitted. This allows you to fix mistakes and add code rapidly.

# We can view the characteristics of our data as follows:
# First, all objects in R have class.

class(filaree) 

# The object "filaree" is a data frame. It has dimensions and length.

dim(filaree)
length(filaree)

# The length of a data frame is the number of columns.

#If you want to know the structure of the object:

str(filaree)

# Columns have names because we specified header=T and had names in our csv file. You can check each one of the components of the object "filaree"

names(filaree)
filaree$Tgroup
filaree$day
filaree$lnwt
filaree$day.22.4

#In this case, filaree is a small data frame, then, you can see it just typing:

filaree

# Notice that R does not like symbols in the names of things, so it changed the
# "-" to "." in the column names. R likes to use periods to separate parts of names for easy reading.
# Periods in names do not mean anything in particular; they are just good to
# improve the readability of meaningful names. You can use any names you want,
#but stay away from using names that resemble functions or objects that are part of the R program or packages.
# Objects are created when the result of a function is "sent" into a name.
# We read a <- b as "a gets b"



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

# If you know exactly which packages you need to install you can type:

install.packages("car", dependencies=TRUE) #It is jus for one package and its dependencies
install.packages(c("car", "MASS", "ellipse", dependencies=TRUE)) #It is for more than one packages and their dependencies

# Sometimes you need to update your packages, since these are frequently improve by the developers and community, for that, you need to set your repositories (the places in which the packages are stored)

setRepositories()

#Then, you ask for updates of the packages that you already installed, or for an specific package:

update.packages() # updates for all the packages
update.packages("car") # for a specific package

# When you want to use a GUI different to or complementary to RStudio, Rcmdr is a important package, you can install it as follows:

install.packages("Rcmdr", dependencies=TRUE)

#and then, load it in your Workspace

library(Rcmdr)

#if you want to know more about it, type:

??Rcmdr

# *********************************************************************
# HELP & looking for PACKAGES
# *********************************************************************

# Help is readily and abundantly available.

help(read.csv) # when you know the name of the function
help.search("t test") # when you do not know the name of the function for a particular analysis
apropos("anova") # when you are looking for functions relevant for a technique of analysis
?stat.anova # when you need the manual of a specific function

# Also, the package SOS is very useful when you are looking for packages to use

install.packages("sos", dependencies=TRUE)
library(sos)
lr<-findFn('linear regression', 1) # Performing a search for functions and packages for Linear Regression, asking for the first page which will contain 20 matches
print(lr, 'console') # Showing the results of the search

# However, to date, Google is the lord of searchers, a good place to look for several kind of analysis is http://search.r-project.org

#You can perform searches from R by using:

RSiteSearch("mixed model")

# http://crantastic.org , http://dirk.eddelbuettel.com/cranberries/  are good places to look for information of packages

# http://cran.r-project.org/other-docs.html has a large collection of documentation provided by users in different languages and some of them with specific topics

# *********************************************************************
# Start playing with R as a calculator
# *********************************************************************

#R can function like a calculator. 

12*14/(2^2)

#R understands standard mathematical “operators”, like multiplication, division, addition, subtraction, etc. Note the output, which begins with [1], indicating that 42 is the first thing returned by R for your command. Let’s move beyond the calculator and do useful things like make “variables”, that is, objects with names that hold things:

a <- 2

#Here you have created a variable a with the value 2. We did so by using the “assignment operator”, <-. It looks a bit like an arrow pointing from 2 to a. You can read the command above, “a gets 2”, or “a is assigned the value 2”. If we look at a, we see its value is now 2:

a # remember that to run a line you put the cursor in the line and press command-Return or CTL-Return in Windows.

#Now we can do fun things with a:

a*a; a*2; a^2

#The semicolon allows you to put more than one command on a single line, but each command is executed separately and generates its own output (which happens to be 4 in every case). So far, we have dealt with single numbers, or “scalars”. These are a special type of “vector”. The most common way to create vectors, which are lists of items, is with the “concatenate” function:

b <- c(1, 2)
b

#We have assigned to the variable b the stuff to the right of the <- 
#c(1,2) simply says, “make a vector with 1 followed by 2"
# c stands for "combine."

c <-    c   (    "1" , "2") # spaces outside of quotation marks are ignored
c

class(c)
length(c)
class(b)
length(b)
b+c # oops!
# but
TRUE + TRUE # coercion is very practical and intuitive

# whenever possible and meaningful, objects are coerced into the type you want. Coercion follows the principle of no loss of information.

c("A", FALSE, 3.0) # coercion to character

# Alternatively, you can perform the following operations:
2+3
3/2
2^3
# beware of order of operations here
4^2-3*2
(56-14)/6 - 4*7*10/(5^2-5)
sqrt(2)
abs(2-4)
cos(4*pi)
log(0)
factorial(6)
choose(52,5)

# I don't understand how this works, so I look at the help file:
?choose

# hep is everywhere in Rstudio!

# A little of Vector arithmetic. Here we use the "combine" function c() to create "vectors", which are lists of numbers.  We assign them to "variables" using the assignment operator "<-" (a less than sign followed by a dash).

x <- c(1, 2, 3, 4)  # note spaces after commas for legibility/readability
y <- c(5, 6, 7, 8)
x*y

# How did it perform this calculation? The simple * does "element-wise" multiplication each element in one vector is multiplied by the corresponding element in the other.  What happens if vectors aren't the same length?
z <- c(1,2,3)
x*z # works but it does give you a warning: nothing wrong for as long as you know what you just did. Elements are "recycled."

c(1, 2, 3, 4) + c(4, 3) # no warning
c(1, 2, 3, 4) + c(4, 3, 2) # produces warning

# This element-wise effect applies for all the other simple mathematical operators as well
y/x
y-x
x^y
cos(x*pi) + cos(y*pi)

# Note that pi is a special variable that is already assigned a value by R:
pi

# Also notice the distinction here between a scalar (like pi), which is simply a number, and a vector which is a list of numbers.  Of course, a scalar can operate on a vector of any length. It operates on every element of the vector:
pi*x
pi*z
pi*pi

s <- c(1, 1, 3, 4, 7, 11)
length(s)
sum(s)
prod(s)
cumsum(s)
diff(s)
diff(s, lag = 2)

# what is this lag thing?
?diff

# try the examples for diff listed in the help file.
diff(1:10, 2)
diff(runif(20)*10,1)
diff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
diff(x, lag = 2)
diff(x, differences = 2)
diff(.leap.seconds)

# Matrices

# Like scalars and vectors, a matrix (plural matrices) is another way to store data. It is a very powerful way to store and manipulate data.

a <- c(1,2,3,4,5,6,7,8,9,10) # same as 1:10
A <- matrix(a, nrow = 5, ncol = 2)

# Note that variable names are case sensitive: a does not equal A!
a
A
#  Why did it fill down the columns first?
?matrix

#  If you look at the help file, you'll see that one of the arguments to the function "byrow" has a preset value of "FALSE".  Many functions will have arguments with preset values: if you don't tell it differently, it will use this preset value.

#   We can also specify the arguments to change the way the function works:

B <- matrix(a, nrow = 5, ncol = 2, byrow=TRUE)
B

#  Now it filled the matrix "by row" Also notice that we don't have to specify "data=a", which you might think was necessary after reading the help file. R has a default way to interpret anonymous arguments, usually based in the order in which they appear.  These preset arguments are a really important reason to read the help file for a function when you first start using it.  Not only does it enlighten you as to all the options, but it also makes you aware of "hidden" switches that are controlling the behavior of the function.

C <- matrix(a, nrow = 2, ncol = 5, byrow = TRUE)
C

#  More matrix manipulations

t(C)
B%*%C #matrix multiplication
#  This is very different from *!
#  matrix multiplication instead of element-wise multiplication
#  Matrix dimensions matter:

B%*%B

#  You can assign calculations to new variables:

D <- C%*%B
D
det(D)
solve(D)

########## basic indexing #######

x[12]    # 12th element
words[2] # second element
vals[3]     # third element

x[6:15] # elements 6 through 15
x[-(11:100)] # omit elements 11 through 100 (note parentheses)

#########comparison and logical operators######

1 == 2   # note == to test equality
1 != 2
1 <= 2
1 < 1:3  # vectorized
3:1 > 1:3
3:1 >= 1:3
TRUE & c(TRUE, FALSE)  # and
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE)  # or

####### unvectorized logical operators && and ||
#######
z <- "a"
is.numeric(z) && (z <- z + 1)  # works b/c of lazy evaluation
is.numeric(z) & (z <- z + 1)   # fails

! c(T, F)   # abbreviations of TRUE and FALSE, best avoided!
T <- FALSE  # perverse!
T
remove(T)
TRUE <- FALSE  # fails

(z <- x[1:10])
z < -0.5
z > 0.5
z < -0.5 | z > 0.5  #  < and > of higher precedence than |
abs(z) > 0.5  # absolute value
z[abs(z) > 0.5] # indexing by a logical vector

##
# ********************************************************************
# Start playing with R using REAL DATA
# *********************************************************************
########
# R has several datasets available to test packages and functions, you can load them using:

data(iris)
class(iris)
names(iris)
dim(iris)
str(iris)
summary(iris)
length(iris)
?iris
iris
iris$Species

#Look at the data with a simple plot

plot(iris)
pairs(iris[-5], bg=iris$Species, pch=21)
pairs(iris[1:4], main = "Anderson's Iris Data, 3 species", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

#Check the correlation among variables
cor(iris[-5])

#Do you want see it graphically? then:
install.packages("PerformanceAnalytics", dependencies=TRUE)
library(PerformanceAnalytics)
chart.Correlation(iris[-5], bg=iris$Species, pch=21)

# There are many ways to do the same thing in R
# I have the following approach:
# FIRST be able to do it at all
# SECOND devise a good way to do it
