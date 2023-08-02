
###############################################################
#' bar_styles
#' Initialize a set of parameter values for use with the
#' bar_timelines function
#'
#' @return A data frame with default parameter values
#' @export
#'
#' @examples column <- bar_styles()
bar_styles <- function(){

  column <- NULL  ## Needed to get started

  ## Defaults
  column$color <- "grey"
  column$height <- 0.8
  column$alpha <- 0.8
  column$outline_color <- "black"
  column$text <- ""
  column$text_size <- 6
  column$text_color <- "black"
  column$key_title <- "key"
  column$x_axis_label <- ""
  column$background_color <- "lightblue"
  column$title <- ""
  column$source_info <- ""
  return(column)
} ## end function bar_styles

###############################################################
#' bar_timelines
#' Builds charts using "ggplot2" with bars that span time
#' periods.
#'
#' @param datatable A table with event, start, and end columns
#' @param styles A data frame with the style parameters
#' @param key_color_table A table linking events to colors
#'
#' @return A "ggplot2" object ready for plotting
#' @export
#'
#' @examples bar_timelines(data=data,styles=column)
bar_timelines <- function(datatable,
                         styles=column,
                         key_color_table=NULL){

  ## Global binding
  . <- event <- start <- end <- row <- NULL

  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    add_column(!!!styles[!names(styles) %in% names(.)])

  ## Calculations

  ## If there aren't row values, add them as a sequence
  ## First, make the "not in" operator
  "%!in%" <- Negate("%in%")
  if("row" %!in% names(datatable)){
    datatable$row <- 1:nrow(datatable)}

  ## Make top and bottom for each row
  datatable <- datatable |>
    mutate(y2 = row + (height/2)) |>
    mutate(y1 = row - (height/2)) |>
    rename(x1 = start) |>
    rename(x2 = end)

  ## Initialize
  make_key <- "none"

  ## Process if a key is needed
  if(is.null(key_color_table)){key_test <- FALSE} else
  {key_test <- TRUE}
  if(key_test == TRUE){
    ## Use code to make a key (i.e., legend)
    make_key <- "legend"

    ## Sort the key color table by color name
    key_color_table <- dplyr::arrange(
      key_color_table, color)
  } ## end if  key_test is TRUE

    ## Generate the bar timeline
  ggplot(datatable, aes(xmin=x1,
                        xmax=x2,
                        ymin=y1,
                        ymax=y2)) +
    geom_rect(aes(fill=color),
              color=datatable$outline_color,
              alpha=datatable$alpha) +
    geom_text(mapping=aes(x=x1+(x2-x1)/2,
                          y=y1+(y2-y1)/2,
                          label=event),
              size=datatable$text_size,
              lineheight = 0.75, ## spacing between rows of text
              color=datatable$text_color) +
    labs(y = NULL,
         x = datatable$x_axis_label,
         title = datatable$title,
         caption = datatable$source_info) +
    theme(panel.background =
            element_rect(fill=datatable$background_color),
          axis.text.y=element_blank(),     #remove labels
          axis.ticks.y=element_blank()) +  #remove ticks
    scale_y_reverse(breaks = NULL) +       #remove y grid
    scale_fill_identity(datatable$key_title,
                        guide = make_key,
                        labels = key_color_table$text)
} ## end function bar_timeline

###############################################################

#' lolli_styles
#' Initialize a set of parameter values for use with the
#' milestones function
#'
#' @return A dataframe with default parameter values
#' @export
#'
#' @examples column <- lolli_styles()
lolli_styles <- function(){

  column <- NULL  ## Needed to get started

  ## Defaults
  column$color             <- "red"
  column$point_size        <- 2
  column$outline           <- "black"
  column$stroke            <- 1
  column$text_size         <- 3
  column$text_color        <- "black"
  column$background_color  <- "slategray1"
  column$grid_color        <- "slategray2"
  column$y_extend_pct      <- 0.1
  column$x_axis_label      <- ""
  column$title             <- ""
  column$source_info       <- ""

  return(column)
} ## end function lolli_styles

###############################################################

#' milestones
#' Builds charts using "ggplot2" with lollipop shape symbols
#' marking events on a timeline
#'
#' @param datatable A data frame with columns for events and dates
#' @param styles A data frame with the style parameters
#' @param add_row_above A binary variable to add spacing at the top
#' @param add_row_below A binary variable to add spacing below
#'
#' @return A "ggplot2" object ready for plotting
#' @export
#'
#' @examples milestones(data=data,styles=column,add_row_below=TRUE)
milestones <- function(datatable=data,
                       styles = column,
                       add_row_above = 0.5,
                       add_row_below = 0.5){
  ## Global binding
  . <- event <- date <- row <- color <- NULL

  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    add_column(!!!styles[!names(styles) %in% names(.)])

  ## Calculations

  ## If there aren't row values, add them as a sequence
  ## First, make the "not in" operator
  "%!in%" <- Negate("%in%")
  if("row" %!in% names(datatable)){
    datatable <- datatable |>
      mutate(row = nrow(datatable):1)}

  ## Adjust the panel height by increasing the Y axis range.
  ## Ordinarily, this doesn't need to be changed.
  y_axis_max <- max(datatable$row) + add_row_above
  y_axis_min <- min(datatable$row) - add_row_below

  ## X axis limits
  axis_start <- min(datatable$date)
  axis_end   <- max(datatable$date)
  axis_range <- axis_end-axis_start
  axis_end   <- axis_end + (axis_range * datatable$y_extend_pct)

  ## Place text above or below the lollipop point.
  vjust <- ifelse(datatable$row > 0, "bottom", "top")
  v_nudge <- ifelse(datatable$row > 0, 0.3, -0.3)

  tline <- ggplot(datatable,aes(date, row)) +

    geom_text(aes(x     = date,
                  y     = row,
                  label = event),
              hjust = "left",
              vjust = vjust,
              nudge_y = v_nudge,
              size  = datatable$text_size,
              lineheight = 0.75, ## spacing between rows of text
              colour = datatable$text_color) +
    ##position = position_nudge(y = 0.2)) +

    ##    geom_lollipop(point.size  = datatable$point_size,
    ##                   point.colour = datatable$color) +
    ## Draw the vertical lines for each point
    annotate("segment",
             x    = datatable$date,
             xend = datatable$date,
             y    = 0,
             yend = datatable$row) +

    geom_point(shape = 21,
               aes(x     = date,
                   y     = row,
                   fill  = color),
               colour = datatable$outline,
               size  = datatable$point_size,
               stroke = datatable$stroke) +

    labs(y = NULL,
         x = datatable$x_axis_label,
         title = datatable$title,
         caption = datatable$source_info) +

    theme(axis.title   = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line    = element_blank(),
          axis.text.x  = element_text(size = 8),
          panel.border = element_rect(linetype = "solid",
                                      fill=NA,
                                      color="black"),
          panel.background   =
            element_rect(fill=datatable$background_color),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x =
            element_line(color=datatable$grid_color),
          panel.grid.minor.x =
            element_line(color=datatable$grid_color)) +
    expand_limits(x = c(axis_start, axis_end),
                  y = c(y_axis_min,y_axis_max)) +

    annotate("segment",
             y=0,
             yend=0,
             x=axis_start,
             xend=axis_end)+

    scale_fill_identity() +
    scale_size_identity()

  return(tline)

}  ## end function milestones


