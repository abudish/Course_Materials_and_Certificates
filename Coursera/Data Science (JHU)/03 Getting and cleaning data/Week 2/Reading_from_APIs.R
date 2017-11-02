# accessing Twitter from R
library(httr)

myapp = oauth_app("twitter",
                  key="znuLseutM9eIX9lbszydS9S4m ", #your Consumer Key here
                  secret="GkYAzaru28KcwRcOWyyVBsh0qfOsnK7pAWUSMv8vpIaD4uV3wN") #your Consumer Secret here
sig = sign_oauth1.0(myapp,
                    token="4622930056-FILq8V8oKBQTb1sVkTG2CxHH4sxxBGDWlLijxzy", #your Access Token here
                    token_secret="JmWyQSBrTM4Asx6d4iBXhs4FGX86DenHZw9PkSr2Nwf8B") #your Token Secret here
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)

# converting the json object
library(jsonlite)

json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1, 1:4]
