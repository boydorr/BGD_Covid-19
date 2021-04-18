triple_barplot_base <- function(dataframe, title_text="", y_axis_title, show_y_axis, col_pal, stack=NULL){

  if(is.null(stack)){
    pl <- ggplot() +
      geom_col(data = dataframe, aes(x = x, y = y, fill = x), width=0.8) +
      scale_fill_manual(values=col_pal) +
      labs(y=y_axis_title) +
      theme_classic() +
      theme(legend.position = "none",
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size=12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.line.x = element_line(color="#7c6e6e"))
  } else {

    alpha_pal <- c(1,0.5)

    pl <- ggplot() +
      geom_col(data = dataframe, aes(x = x, y = y, fill = x, alpha=cat), width=0.8) +
      scale_fill_manual(values=col_pal) +
      scale_alpha_manual(values=alpha_pal, breaks=c("Lab", "RDT")) +
      labs(y=y_axis_title) +
      theme_classic() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size=16),
            axis.title.y = element_text(size=14),
            axis.text.y = element_text(size=12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.line.x = element_line(color="#7c6e6e")) +
      guides(fill="none", alpha=guide_legend(override.aes = list(fill = "red")))
  }

  if(show_y_axis==FALSE){
    pl <- pl +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y = element_blank())
  }
  
  # if(title_text!=""){
  #   pl <- pl +
  #     ggtitle(title_text) + 
  #     theme(plot.title = element_text(hjust = 0.5,size=18))
  # }

  return(pl)

}