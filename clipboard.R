#function to export to clipboard
clipboard <- function(df) {
  clip <- pipe("pbcopy", "w")                       
  write.table(df, file=clip,row.names=F,col.names=F)                               
  close(clip)
}