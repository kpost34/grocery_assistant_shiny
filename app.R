#Created by Keith Post on 3/19/23

# Set options for google sheets
options(
  #whenever there is one account token found, use the cached token
  gargle_oauth_email=TRUE,
  #specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache="grocery_assistant_shiny/.secrets"
)

# Run the application
app<-shinyApp(ui=ui,server=server)
runApp(app)