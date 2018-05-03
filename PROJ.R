


output$mapPlot <- renderPlot({
  
  #Plot actual vote diffrences
  
  #Plot predicted
  #nc[paste0("Y.HAT",year_query)] = y.hat.med[,year_query]
  #plot(nc[paste0("Y.HAT",year_query)], main = paste0("Predicted % Differences in Rep-Dem Voting",
  #                                                (year_query)))
  
  #Plot holdout county map or metric
  plot(hist(y.hat.med[,year_query]))
  
})