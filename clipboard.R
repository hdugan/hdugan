# Function to export to clipboard
# Default is for 'space separated'. Set csv = T for comma separated 
clipboard <- function(df,csv=FALSE) {
  clip <- pipe("pbcopy", "w")
  if (csv == F) {
    write.table(df,file=clip,row.names=F,col.names=F,quote=F)      
  } else if (csv == T){
    write.table(df,file=clip,row.names=F,col.names=F,quote=F,sep=',')  
  }                          
  close(clip)
}