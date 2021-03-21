#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


# Use this to show select / hover Plotly data
renderPlotly2 <-
  function (expr,
            env = parent.frame(),
            quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    }
    htmlwidgets::shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
  }

# Javascript to get the output
# Found d.label by looking at source code when hovering (i.e. look for fullData)
addHoverBehavior <- "function(el, x){
  el.on('plotly_hover', function(data){
    var infotext = data.points.map(function(d){
      console.log(d)
      return ('label: '+d.label+' parent: ' + d.parent);
    });
    console.log(infotext)
    Shiny.onInputChange('hover_data', infotext)
  })
}"
# Same but with capturing the click data. ALso disables animation.
addClickBehavior <- "function(el, x){
  el.on('plotly_click', function(data){
    var infotext = data.points.map(function(d){
      console.log(d)
      return (d.label);
    });
    console.log(infotext)
    Shiny.onInputChange('click_data', infotext)
    gd.on('plotly_sunburstclick', () => false)
  })
}"

# disable_sunburst_animation <-
# "gd => {
#   gd.on('plotly_sunburstclick', () => false)
# }"
#
# "function(el, x){
#   el.on('plotly_click', function(data){
#     var infotext = data.points.map(function(d){
#       console.log(d)
#       return (d.label);
#     });
#     console.log(infotext)
#     Shiny.onInputChange('click_data', infotext)
#   })
# }"



