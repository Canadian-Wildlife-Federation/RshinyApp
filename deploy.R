library(rsconnect)

#authenticate
setAccountInfo(name='tomasmk',
			  token=${{ secrets.TOKEN}},
			  secret=${{ secrets.SECRET}})

#deploy
deployApp("./")
