# helper function
md_write_as_json <- function(file,ls)
{
  x[["names"]] <- NULL
  x[["row.names"]] <- NULL
  x[["problems"]] <- NULL
  x[["spec"]] <- NULL

  jsonlite::write_json(x,file,simplifyVector=T,pretty=T)
}

# helper function
md_read_json <- function(file,prefix="",max_lines=-1L)
{
  pat <- paste0("^\\s*",prefix)
  lns <- grep(pat,readLines(file,n=max_lines),value=T)

  parse <- NULL
  pat <- paste0(pat,"(.*)")
  for (ln in lns)
    parse <- paste0(parse,gsub(pat,"\\1",ln))

  jsonlite::parse_json(parse,simplifyVector=T)
}

