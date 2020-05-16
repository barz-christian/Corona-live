#### global parameters
palette <- "Reds"
use_mapshaper <- TRUE

#### functions
generate_labels <- function(dt, Namen = NULL, format_integers = TRUE)
{
  # generates labels from a given data frame, i.e. information which will be shown on mouse hover
  # input : dt = data frame with sf-extension, i.e. a data frame with a "geometry column"
  # consider only the columns given in Namen, if they are missing all non geometry columns are used
  # format_integers = True : formats integer columns from 1234567 to 1 234 567
  
  # define helper functions
  paste2 <- function(x, y, sep = "") paste(x, y, sep = sep)
  fformat <- function(x){format(x, big.mark = " ")}
  
  # if names not specified use as labels all non geometry columns
  if(is.null(Namen)){Namen <- dt %>% sf::st_drop_geometry() %>% names()}
  
  # format numbers
  if(format_integers) {dt <- dt %>% mutate_if(is.integer, fformat)}
  
  #generate HTML strings creating the tables shown at each label
  lapply(Namen, function(x){
    #generate the rows of the table
    paste("<tr><td><strong>", x,"</strong></td>",
          "<td class='alignRight'>", dt[[x]], "</td></tr>",
          sep = "")
  }) %>% 
    purrr::reduce(paste2, .dir = "forward") %>% # fold left
    paste("<table style='width: 100%;'>", . , "</table>", # add HTML table boarders
          sep = "") %>% 
    lapply(htmltools::HTML) %>% # convert plain text to HTML
    return()
}