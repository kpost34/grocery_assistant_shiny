#Created by Keith Post on 3/19/23

#run at start to connect to gmail account
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

#pull id of Google sheet "main"
sheet_id<-drive_get("main")$id

# Run the application
app<-shinyApp(ui=ui,server=server)
runApp(app)