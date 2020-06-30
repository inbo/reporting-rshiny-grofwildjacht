#' Plot number of new introductions per pathway at level 2
#' 
#' Split up by pathways at level 1
#' 
#' @param data data.frame with raw data for plotting
#' @importFrom trias visualize_pathways_level2
#' @importFrom plotly ggplotly subplot
#' @return list with plot and data that was used for the plot (i.e. without missing values for the )
#' 
#' @export
countIntroductionPathway <- function(data){
    
  ## generate plot
  
  plots <- lapply(unique(data$pathway_level1), function(x) {
  	p <- visualize_pathways_level2(
      df = data, 
      chosen_pathway_level1 = x,
      x_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
    ) 
    
    ggplotly(p) %>%
        add_annotations(text = x,
            x = 0.5,
            y = 1.1,
            yref = "paper",
            xref = "paper",
            xanchor = "middle",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 15))
  })

  p <- subplot(plots, 
      nrows = ceiling(length(plots) / 2),
      margin = 0.04) #%>%
#      layout(xaxis = list(
#              title = "x Axis"), 
#            yaxis = list(title = "y"))


  
  return(list(plot = p, data = data))
  
  
}