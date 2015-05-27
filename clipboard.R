#function to export to clipboard
clipboard <- function(df,csv=FALSE) {
  clip <- pipe("pbcopy", "w")
  if (csv == F) {
    write.table(df,file=clip,row.names=F,col.names=F,quote=F)      
  } else if (csv == T){
    write.table(df,file=clip,row.names=F,col.names=F,quote=F,sep=',')  
  }                          
  close(clip)
}