

## See also <https://wpdatatables.com/datatables-alternative/>

## Especially 


toHTML.data.frame <- function(x, ...)
{
    row_info <- .row_names_info(x, type = 1L)
    xx <- do.call(cbind, format(x)) # list -> matrix of characters
    if (row_info > 0)
        rownames(xx) <- rownames(x)
    toHTML(xx, ...)
}



toHTML.matrix <- function(x, ..., standalone = TRUE, use_json = FALSE, datatable = TRUE)
{
    if (!is.character(x)) x <- format(x) # list of characters
    result <- ## HTMLheader(...) # or
        c("<!DOCTYPE html>",
          "<html>",
          "<head>",
          "<meta charset='utf-8' />",
          "<title>R Object Viewer</title>",
          "<meta name='viewport' content='width=device-width, initial-scale=1.0, user-scalable=yes' />",
          "<link rel='stylesheet' href='https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css' integrity='sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh' crossorigin='anonymous'>",
          "<link rel='stylesheet' href='https://cdn.datatables.net/1.11.3/css/dataTables.bootstrap4.min.css'>",
          "<style type='text/css'>",
          "  body { padding-top: 10px; }",
          "  #ftable { font-family: monospace; }",
          "  #isrc { width: 100%; height: 90%; }",
          "</style>",
          "</head>",
          "<body>",
          "<div class='container-fluid'>")
    footer <- "</div></html></body>"
    c(result,
      if (use_json) matrix2html_json(x)
      else matrix2html_table(x, datatable = datatable),
      footer)
}

## Data loaded from JSON --- more efficient for large tables, but
## requires javasript

matrix2html_json <- function(x)
{
    stop("not implemented")
}

vec2tr <- function(x, tag = "td", prefix = "", suffix = "")
{
    out <- sprintf("<%s>%s</%s>", tag, x, tag)
    if (length(out))
        paste0(prefix,
               "<tr>",
               paste(out, collapse = ""),
               "</tr>",
               suffix)
    else character(0)
}

## Creates HTML tables with optional datatable 
matrix2html_table <-
    function(x,
             datatable = TRUE,
             id = "rtable",
             dtopts = "{ 'paging': false, scrollY : '70vh', scrollCollapse: true, scrollX : true, }")
{
    have_rownames <- !is.null(rownames(x))
    have_colnames <- !is.null(colnames(x))
    if (have_rownames) x <- cbind(rownames(x), x)
    if (have_colnames) colnames(x)[[1]] <- ""
    out <- NULL
    addLine <- function(...) out <<- c(out, ...)
    addLine(sprintf("<table class='table table-striped table-bordered' id='%s'>", id))
    addLine(if (have_colnames)
                vec2tr(colnames(x), tag = "th",
                       prefix = "   <thead>", suffix = "</thead>")
            else NULL)
    apply(x, 1,
          function(u) {
              addLine(vec2tr(u, tag = "td", prefix = "   "))
          })
    addLine("</table>")
    if (datatable) {
        addLine("<script src='https://code.jquery.com/jquery-3.4.1.min.js' integrity='sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=' crossorigin='anonymous'></script>",
                "<script src='https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js' crossorigin='anonymous'></script>",
                "<script src='https://cdn.datatables.net/1.11.3/js/dataTables.bootstrap4.min.js' crossorigin='anonymous'></script>",
                "<script src='https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js' integrity='sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6' crossorigin='anonymous'></script>",
                "<script type='text/javascript'>",
                sprintf("$(document).ready(function() { $('#%s').DataTable(%s); } ); </script>", id, dtopts))
    }
    out
}


show_html <- function(x, ..., file = tempfile(fileext = ".html"))
{
    out <- toHTML(x, ...)
    cat(out, file = file, sep = "\n")
    browseURL(file)
    file
}


