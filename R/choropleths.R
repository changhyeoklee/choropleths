#' Create Choropleths Object from Input Data
#'
#' This function allows you to create \code{choropleths} object from input data indexed by region ID, which you can use to create choropleths maps.
#' @param df Data you want to plot choropleth maps with. Should at least have two columns: region ID and data. Region ID column should be either DMA code or state code. The class type of region ID column should be character.
#' @param which_map Which map to plot. Either "DMA" or "state".
#' @param id_data_index Column index of region ID column and data column. Defaults to c(1, 2) when the first column is region ID and the second column is data.
#' @export 
#' @examples
#' library(choropleths)
#' 
#' choro <- choropleths(sample_data_state, "state")
#' 
#' draw_map(choro)
#' draw_legend(choro)

choropleths <- function(df, which_map, id_data_index=c(1, 2))
{
  map <- get(sprintf("map_%s", which_map))
  metadata <- get(sprintf("metadata_%s", which_map))
  
  # Append data to map
  df_core <- data.frame(id=df[, id_data_index[1]], data=df[, id_data_index[2]], stringsAsFactors=FALSE)
  
  map_with_input <- left_join(map, df_core, by="id") 
  metadata_with_input <- left_join(metadata, df_core, by="id")
  
  extra_id <- setdiff(df_core$id, metadata$id) 
  if (length(extra_id) > 0)
  {
    warning(sprintf("%s %s ignored.", which_map, paste(extra_id, collapse=", ")))  
  }
  
  choro <- list(
    map_with_input = map_with_input,
    metadata_with_input = metadata_with_input,
    extra_id = extra_id,
    which_map = which_map,
    input_name = colnames(df)[id_data_index[2]]
  )
  
  ## Set the name for the class
  class(choro) <- append(class(choro),"choropleths")
  
  return(choro)
}

.color_scaler <- function(colors, domain, input_data)
{
  if(is.null(colors))
  {
    colors <- c("#1A9850", "#66BD63", "#A6D96A", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027")
  }
  
  ramp <- colorRampPalette(colors)
  
  if(is.null(domain))
  {
    domain <- c(min(input_data, na.rm=TRUE), max(input_data, na.rm=TRUE))
  }
  
  num_intervals <- 100
  intervals <- c(-Inf, seq(domain[1], domain[2], length.out=(num_intervals + 1)), Inf)
  
  scaler <- list(
    ramp=ramp,
    intervals=intervals,
    num_intervals=num_intervals
  )
  
  ## Set the name for the class
  class(scaler) <- append(class(scaler),".color_scaler")
  
  return(scaler)
}

.num_to_color <- function(obj, x)
{
  UseMethod(".num_to_color", obj)
}

.num_to_color.default <- function(obj, x)
{
  print("I do not know how to handle this object.")
  return(NULL)
}

.num_to_color..color_scaler <- function(obj, x)
{
  return(as.character(cut(x, obj$intervals, labels=obj$ramp(obj$num_intervals + 2))))
}

draw_map <- function(obj, colors=NULL, domain=NULL, tooltip=TRUE, digits=3, percent=FALSE)
{
  UseMethod("draw_map", obj)
}

draw_map.default <- function(obj, colors=NULL, domain=NULL, tooltip=TRUE, digits=3, percent=FALSE)
{
  print("I do not know how to handle this object.")
  return(NULL)
}

#' Draw Choropleth Map from Choropleths Object
#'
#' This function allows you to draw choropleth map from \code{choropleths} object. The choropleth map will be drawn with ggvis. You can configure colors to scale and domain of input data. You can turn off tooltips, if you wish. You can also configure the format of data in tooltips.
#' @param obj \code{choropleths} object
#' @param colors A vector of color codes for color scales.If not specified, defaults to \code{c("#1A9850", "#66BD63", "#A6D96A", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027")}, which changes from Green, Yellow and to Red as the value of input data increases.
#' @param domain A vector of length 2 for the lower bound and upper bound of input data for color scales. Any values outside of domain will be treated as either lower or upper bound. If not specified, the lower bound and upper bound of input data will be used as domain.
#' @param tooltip Add tooltips. Defaults to \code{TRUE}.
#' @param digits Maximum digits to print after decimal point. Defaults to 3.
#' @param percent wheather to print the data as percentage in tooltips.
#' @export 
#' @examples
#' library(choropleths)
#' 
#' choro <- choropleths(sample_data_state, "state")
#' 
#' draw_map(choro)
#' draw_legend(choro)

draw_map.choropleths <- function(obj, colors=NULL, domain=NULL, tooltip=TRUE, digits=3, percent=FALSE)
{
  scaler <- .color_scaler(colors, domain, obj$metadata_with_input$data)
  
  obj$map_with_input$color <- .num_to_color(scaler, obj$map_with_input$data)
  obj$map_with_input$color <- ifelse(is.na(obj$map_with_input$color), "#F0F0F0", obj$map_with_input$color)
  
  obj$metadata_with_input$color <- .num_to_color(scaler, obj$metadata_with_input$data)
  obj$metadata_with_input$color <- ifelse(is.na(obj$metadata_with_input$color), "#F0F0F0", obj$metadata_with_input$color)

  map_plot <- obj$map_with_input %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%    
    layer_paths(
      fill:=~color,
      strokeWidth:=0.5, 
      stroke:="white") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(keep_aspect=TRUE)
  
  if (tooltip)
  {
    tooltip_values <- function(x) 
    {
      if(is.null(x)) return(NULL)
      
      name <- obj$metadata_with_input %>% filter(id == x$id) %>% select(name)
      data <- obj$metadata_with_input %>% filter(id == x$id) %>% select(data)
      
      if (is.na(data)) {
        data_output <- "NA"
      }
      else {
        if (percent)
        {
          data_output <- paste(round(data$data, digits=digits + 2) * 100, "%")  
        } else
        {
          data_output <- as.character(round(data$data, digits=digits))
        }
      }
      
      sprintf("<table width='100%%'>%s</table>",
        paste0(
          "<tr>",
          "<td><td style='text-align:left'>", obj$which_map, "</td>",
          "<td><td style='text-align:right'>", x$id, ": ", format(name), "</td>",
          "</tr>",
          "<tr>",
          "<td><td style='text-align:left'>", obj$input_name, "</td>",
          "<td><td style='text-align:right'>", data_output, "</td>",
          "</tr>",
          collapse=""))
    }
    
    map_plot <- map_plot %>%
      add_tooltip(tooltip_values, "hover")
  }
  
  return(map_plot)
}

draw_legend <- function(obj, colors=NULL, domain=NULL, digits=3, percent=FALSE)
{
  UseMethod("draw_legend", obj)
}

draw_legend.default <- function(obj, colors=NULL, domain=NULL, digits=3, percent=FALSE)
{
  print("I do not know how to handle this object.")
  return(NULL)
}

#' Draw Choropleth Map Legend from Choropleths Object
#'
#' This function allows you to draw choropleth map legend from \code{choropleths} object. The choropleth map legend will be drawn with ggplot2. You can configure colors to scale and domain of input data. You can also configure the format of data in the legend.
#' @param obj \code{choropleths} object
#' @param colors A vector of color codes for color scales.If not specified, defaults to \code{c("#1A9850", "#66BD63", "#A6D96A", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027")}, which changes from Green, Yellow and to Red as the value of input data increases.
#' @param domain A vector of length 2 for the lower bound and upper bound of input data for color scales. Any values outside of domain will be treated as either lower or upper bound. If not specified, the lower bound and upper bound of input data will be used as domain.
#' @param digits Maximum digits to print after decimal point. Defaults to 3.
#' @param percent wheather to print the data as percentage in the legend.
#' @export 
#' @examples
#' library(choropleths)
#' 
#' choro <- choropleths(sample_data_state, "state")
#' 
#' draw_map(choro)
#' draw_legend(choro)

draw_legend.choropleths <- function(obj, colors=NULL, domain=NULL, digits=3, percent=FALSE)
{
  scaler <- .color_scaler(colors, domain, obj$metadata_with_input$data)
  
  legend_data <- data.frame(x=rep(1, scaler$num_intervals + 1), color=.num_to_color(scaler, scaler$intervals[2:(scaler$num_intervals + 2)]))
  breaks <- seq(0, scaler$num_intervals, scaler$num_intervals / 5) + 1
  
  if (percent)
  {
    labels <- paste(round(scaler$intervals[breaks + 1], digits=digits + 2) * 100, "%")  
  } else
  {
    labels <- as.character(round(scaler$intervals[breaks + 1], digits=digits))
  }
  
  map_legend <- ggplot(legend_data, aes(x, x)) +
    geom_bar(stat="identity", fill=legend_data$color) +
    coord_flip() + 
    theme_bw() + 
    ggtitle(obj$input_name) + 
    scale_y_continuous(breaks=breaks, labels=labels) +
    theme(
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_blank(),
      axis.title=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      plot.title=element_text(face="bold", hjust = 0.05),
      aspect.ratio=0.1
    )
  
  return(map_legend)
}