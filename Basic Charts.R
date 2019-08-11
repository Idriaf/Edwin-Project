library(plotly)
p <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width,color = ~Species, mode = "markers")
p


library(readxl)
SportsData = read_xlsx("test1.xlsx")



p1 = plot_ly(SportsData, x = ~SportsData$`P2+`, y = ~SportsData$`P18-34`,color = ~SportsData$...1, mode = "markers")
p1

