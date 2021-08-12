#' @name bitr_templ_html
#' @rdname bitr_templ_html
#' @title bitR HTML template
#'
#' Loads additional style and template file
#'
#' @references https://github.com/UNHCR-WEB/unhcRstyle/blob/master/R/unhcr_templ_html.R
#'
#' @param toc logical. should a table of contents be displayed?
#' @param maintitle character. title in header.
#' @param subtitle character. subtitle in header.
#' @param menu_url character. menu url in header.
#' @param menu_name character. munu url name in header.
#' @param logo_img character. name of logo image file on top left.
#' @param path name of directory to generate report file. default is "."
#' @param ... additional arguments provided to \code{html_document}
#' @return An R Markdown output format.
#' @export bitr_templ_html
#' @importFrom rmarkdown html_document includes
#' @importFrom glue glue
#' @importFrom stringr str_split
#'
bitr_templ_html <- function(toc = TRUE, maintitle = NULL, subtitle = NULL,
                            menu_url = "https://r2bit.com/", menu_name = "bitR",
                            logo_img = NULL, path = ".", ...) {
  header <- "header_temp.html"
  footer <- "footer_temp.html"
  logo <- "r2bit.png"

  header_path <- paste(tempdir(), header, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy header
  header_file <- file.path(system.file(package = "bitRtemplate"),
                           "resources", header)
  flag <- file.copy(from = header_file, to = tempdir(), recursive = TRUE)

  #--Store parameters ----------------------------------------------------------
  # store maintitle
  header_content <- sub("\\$maintitle\\$", maintitle, readLines(header_path))
  cat(header_content, file = header_path, sep = "\n")

  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(header_path))
  cat(header_content, file = header_path, sep = "\n")

  css <- file.path(system.file(package = "bitRtemplate"), "resources",
                   "bitr-bootstrap.css")

  # store the menus
  menu_url <- stringr::str_split(menu_url, ",", simplify = TRUE)
  menu_name <- stringr::str_split(menu_name, ",", simplify = TRUE)

  menus <- NULL
  for (i in seq(menu_url)) {
    menus <- paste(menus, glue::glue("<li><a href=\"{menu_url[i]}\" target=\"_blank\">{menu_name[i]}</a></li>"))
  }

  header_content <- sub("\\$menu\\$", menus, readLines(header_path))
  cat(header_content, file = header_path, sep = "\n")

  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "bitRtemplate"),
                           "resources/image", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- sub("\\$logo\\$", base64_logo, readLines(header_path))
  cat(header_content, file = header_path, sep = "\n")

  
  footer_file <- file.path(system.file(package = "bitRtemplate"),
                           "resources", footer)
  
  # call the base html_document function
  rmarkdown::html_document(
    toc = toc,
    toc_float = TRUE,
    fig_caption = TRUE,
    fig_height = 5,
    fig_width = 8,
    toc_depth = 2,
    code_folding = "show",
    css = css,
    includes = rmarkdown::includes(in_header = header_path,  
                                   after_body = footer_file),
    ...
  )
}
