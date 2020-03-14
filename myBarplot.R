# example usage for 1 categorical plot : 
#   myBarplot(Dataset, c(2), "Title", "x axis name", "y axis name")

# example usage for 2 categorical plot : 
#   myBarplot(Dataset, c(2,3), c("Title1","Title2"), c("x axis name 1","x axis name2"), "y axis name")
# Note: ylabel's default value is column name (Gender for example) 

myBarplot <- function(data, columns, plotTitles = c("My Barplot"), xLabs = c(""), yLabs = c(""), xLim = NULL, yLim = NULL, color = "cornflowerblue"){
  
  # number of columns to plot
  numOfCols = length(columns)
  
  # creating 1xnumber of columns area for plot
  par(mfrow=c(1,numOfCols), bty='n')
  
  #draw plots for each column given in columns
  columnCounter = 1
  # column means column index
  for(column in columns){
    
    # finding category names
    categories = table(data[,column])
    categoriNames = dimnames(categories)[[1]]
    
    #number of categories in the given column
    categoryAmount = length(categories)
    
    #max value of the given column will be used for ylim
    maxValue = max(categories)
    
    # identifiers for the specific columns (given in the parameters)
    plotName = plotTitles[columnCounter]
    
    # if ylabels is not given, use column names for ylab. for example: Gender for female and male
    if(length(yLabs) == 1 && yLabs==c("")){
        yLab = dimnames(data)[[2]][column]
    }
    else{
      yLab = yLabs[columnCounter]
    }
    
    
    xLab = xLabs[columnCounter]
    Main = plotTitles[columnCounter]
    

    # creating empty plot with parameters above
    plot(c(0, categoryAmount*10), c(0, maxValue+maxValue/20), type = "n",
         main = plotName, xaxt = "n", xlab = xLab, ylab = yLab, xlim = xLim, ylim = yLim)
    # xaxt parameter removes x axis
    
    
    # plotting the bars
    # number of bars = number of categories (-1 for starting index from 0)
    barAmount = categoryAmount -1
    i <- 10*(0:barAmount) # start point of each bar on x axis, each bar has width of 10
    # as.matrix(categories)
    j <- categories   # hight of each bar
    rect(i+1,0,i+9,j, col = color) # each bar has width of 8 and height of j (j is frequency)
    
    # category names
    text(i+5,maxValue/20,categoriNames)
    
    columnCounter = columnCounter + 1
  }
}