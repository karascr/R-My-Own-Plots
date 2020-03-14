# example usage for 1 column boxplot : 
# myBoxplot(Dataset, c(7), "Title","x axis name", "y axis name")

# example usage for 2 column boxplot : 
# myBoxplot(Dataset, c(6,7), "Title","x axis name", "y axis name")



myBoxplot <- function(data, columns, plotTitle = "My Boxplot", xLab = "", yLab = "", xLim = NULL, yLim = NULL, color = "cornflowerblue"){

  numOfCols = length(columns)
  maxValueOfCols = max(data[,columns])
  minValueOfCols = min(data[,columns])
  
  # creating 1x1 area for plot
  par(mfrow=c(1,1))
  # creating empty plot with parameters above
  plot(c(0, numOfCols), c(minValueOfCols-minValueOfCols/20, maxValueOfCols+maxValueOfCols/20), type = "n",
       main = plotTitle, xlab = xLab, ylab = yLab, xaxt = "n")
  
  
  columnCounter = 0
  
  for (col in columns){
    # colID will used for labeling boxplots later
    colID = col
    # column means column itself in this function
    column = data[,col]
    maxValue = max(column)
    minValue = min(column)
    
    
    Q3 <- quantile(column, 0.75) 
    Q1 <- quantile(column, 0.25)
    median = median(column)
  
    #max line
    segments(columnCounter+0.4,maxValue,columnCounter+0.6,maxValue)
    #upper line (the line between max and q3)
    segments(columnCounter+0.5,maxValue,columnCounter+0.5,Q3)
    #box
    rect(columnCounter+0.3,Q1,columnCounter+0.7,Q3, col = color)
    #median
    segments(columnCounter+0.3,median,columnCounter+0.7,median,lwd = 3)
    #lower line (the line between min and q1)
    segments(columnCounter+0.5,minValue,columnCounter+0.5,Q1)
    #min
    segments(columnCounter+0.4,minValue,columnCounter+0.6,minValue)
    
    # finding column name
    colName = dimnames(data)[[2]][colID]
    
    # putting a text below the box
    text(columnCounter+0.5,minValue-minValue/30,colName)
    
    columnCounter = columnCounter + 1
  }
  
}