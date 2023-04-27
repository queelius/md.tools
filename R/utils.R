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

