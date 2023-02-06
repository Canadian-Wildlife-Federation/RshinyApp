library(rsconnect)

#authenticate
setAccountInfo(name='tomasmk',
			  token='139CE58825DBE28ECF5CD30B12B1874C',
			  secret='w1OIPuW7EyXXPkd36Ozjk7tMRCuMMdzXWLVQ2SCg')

#deploy
deployApp(appDir = "./", appFiles = "app.R")
