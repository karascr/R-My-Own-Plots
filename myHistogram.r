# params: data = data.frame object.
#         columns = the columns of data.frame to be plotted.
#         plotTitles = name (main) of the plot.( default value is = "My Histogram")
#         xLabs = x axis label. ( default value is = "")
#         yLabs = y axis label. ( default value is = "")
#         xLims = x axis limit.
#         yLims = y axis limit.
#         color = color of bars (default value is = "cornflowerblue")

# example usage for 1 column plot : 
#   myHistogram(Dataset, c(5), "Title", "x axis name", "y axis name")

# example usage for 2 column plot : 
#   myHistogram(Dataset, c(6,7), c("Title 1","Title 2"), c("x1 axis name","x2 axis name"), c("y1 axis name","y2 axis name"))

# example usage for 3 and 4 column plot : 
#   myHistogram(Dataset, c(6,7,8))
#   myHistogram(Dataset, c(6,7,8,9))

# the fucntions in this project plots as many plot as number of inputs given in parameters.

myHistogram <- function(data, columns, plotTitles = c("My Histogram"), xLabs = c(""), yLabs = c("Frequency"), xLim = NULL, yLim = NULL, color = "cornflowerblue"){
  
  # number of rows of data (each row means a bar)
  numOfRows <- nrow(data)
  
  # number of columns to plot
  numOfCols = length(columns)
  
  # creating 1xnumOfCols area for plot
  par(mfrow=c(1,numOfCols))
  
  #draw plots for each column given in columns
  columnCounter = 1
  # column means column index
  for (column in columns){
    classAmount = 10
    
    # max value will be used for y axis limit.
    maxValue = ceiling(max(data[,column]))
    minValue = floor(min(data[,column]))
    plotName = plotTitles[columnCounter]
    xLab = xLabs[columnCounter]
    yLab = yLabs[columnCounter]
    
    classBoundary = (maxValue - minValue) / classAmount
 
    if(length(seq(minValue,maxValue+classBoundary-maxValue %% classBoundary,classBoundary))<4){
      classBoundary = (maxValue - minValue) / classAmount
    }
    
    #creating a matrix for storing the frequencies
    matrix = matrix(seq(minValue, maxValue+classBoundary-maxValue %% classBoundary, classBoundary),nrow = 1)
    # maxValue+classBoundary-maxValue %% classBoundary in 55.row, optimizes the max value for preventing
    # data loss, for example: seq(1,9,3) prompts 1,4,7 but there could be data between 7 and 9
    
    # adds matrix a new row full of zeros
    matrix = rbind(matrix, c(0))
  
    # finding frequences and store them in matrix
    for (i in data[,column]) {
      j = 0
      for (k in matrix[1,]) {
        if(i < k){
          matrix[2,j] = matrix[2,j] + 1
          break()
        }
        j = j + 1
      }
    }
    
    # example matrix 
    # for example there are 3 values between 65 and 66.5, 66.5 is not included.
    
    # [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
    # [1,]   65 66.5   68 69.5   71 72.5   74 75.5   77  78.5    80
    # [2,]    3  5.0    8 14.0   22 20.0   16  6.0    4   2.0     0
    
    
    # for y lim
    maxFreq = max(matrix[2,])
    
    # creating empty plot with parameters above
    plot(c(minValue-minValue/20, maxValue+maxValue/20+classBoundary), c(0, maxFreq+maxFreq/20), type = "n",
         main = plotName, xlab = xLab, ylab = yLab, xlim = xLim, ylim = yLim)
    

    # plotting the bars
    i <- matrix[1,]   # start point of each class
    j <- matrix[2,]   # frequency of each class
    rect(i,0,i+classBoundary,j, col = color) # each bar has width of class boundary and height of frequency
    
    columnCounter = columnCounter + 1
  }
}

