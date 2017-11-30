getAccessToken <-
function(api.key){
  tokenUrl = paste("https://api.cognitive.microsoft.com/sts/v1.0/issueToken?Subscription-Key=", api.key , sep="")
  return(
    cleanFun(
      POST(tokenUrl, body="")
    )
  )
  
    
}
