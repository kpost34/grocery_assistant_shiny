#Created by Keith Post on 3/20/23


#### UI Functions===================================================================================
### Set linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}


### Create blocks of ingredients
add_ingredients<-function(n_prev=0,n) {
  
  ns<-(n_prev+1):(n_prev+n)
  
  pos<-ordinal(ns)
  
  map2(pos,ns,function(x,y) {
    f7Block(h3(paste("Enter",x,"ingredient")),
            hairlines=FALSE,
      f7Text(inputId=paste0("txt_ingred",y,"_nm_ingredSheets"),
           label="Name"),
      splitLayout(cellWidths=c("75%","25%"),
        f7Text(inputId=paste0("txt_ingred",y,"_size_ingredSheets"),
           label="Size (e.g., 12 oz can)"),
        div(class="label-left",
          f7Stepper(inputId=paste0("stp_ingred",y,"_n_ingredSheets"),
              label="Count",
              min=0.5,
              max=20,
              value=1,
              step=0.5,
              size="large",
              raised=TRUE),
          #provides margins around text (which helps center stepper label with input)
          style="padding: 40px 0px 0px 0px; font-size: 20px;"
        ),
        #aligns stepper input with text input
        tags$style(type='text/css', paste0("#stp_ingred",y,"_n_ingredSheets"," { vertical-align: middle;
                   width: 80%}"))
      ),
      hr(),
    )
    })
}


### Create appliance and protein checkboxes with pre-selected values
edit_recipe_info<-function(app_edit=NA,prot_edit=NA,id){
  #create objects
  app_value<-app_choices_sheet1 %in% app_edit
  protein_value<-protein_choices_sheet1 %in% prot_edit

  #create appliance checkboxes
  app_list<-purrr::map2(.x=1:5,.y=app_value,
    function(num,log){
      p(f7Checkbox(inputId=paste("chk_app",num,id,"recipe_popup",sep="_"),
         label=app_choices_sheet1[num],
         value=log))
        # style="font-size: 16px")
    })

  #create protein checkboxes
  protein_list<-purrr::map2(.x=1:6,.y=protein_value,
    function(num2,log2){
      p(f7Checkbox(inputId=paste("chk_protein",num2,id,"recipe_popup",sep="_"),
         label=protein_choices_sheet1[num2],
         value=log2))
        # style="font-size: 16px")
    })

  #generate app & protein checkboxes
  splitLayout(cellArgs=list(style="padding: 15px"),
    f7Block(
      strong("Appliance(s) used"),
      # strong("Appliance(s) used:",style="font-size: 18px"),
      app_list
    ),
    f7Block(
      strong("Protein source(s):"),
      # strong("Protein source(s):",style="font-size: 18px"),
      protein_list
    )
  )
}


### Create ingredient inputs with pre-selected values
# edit_ingred_info<-function(df){
edit_ingred_info<-function(df,id){
# Create objects
  #recipe, nrows (with values), nblanks (to balance out all ingred slots), 
    #num (track ingred #), nm, size, and ns (ingred data)
  # recipe<-df %>% pull(recipe) %>% unique()

  nrows<-nrow(df)
  nblanks<-8-nrows

  num<-1:8
  nm<-df %>% pull(name) %>% c(.,rep(NA,nblanks))
  size<-df %>% pull(size) %>% c(.,rep(NA,nblanks))
  ns<-df %>% pull(n) %>% c(.,rep(1,nblanks))

  ingred_list<-list(w=num,x=nm,y=size,z=ns)


  # Build UI
  pmap(ingred_list,function(w,x,y,z) {
    #all info in one row
    splitLayout(cellWidths=c("40%","35%","25%"),
      # f7Text(inputId=paste("txt_ingred",w,"nm","ingred_popup",sep="_"),
      f7Text(inputId=paste("txt_ingred",w,id,"nm","ingred_popup",sep="_"),
             label="Name",
             value=x),
      # f7Text(inputId=paste("txt_ingred",w,"size","ingred_popup",sep="_"),
      f7Text(inputId=paste("txt_ingred",w,id,"size","ingred_popup",sep="_"),
             label="Size",
             value=y),
      #aligns stepper with text boxes
      div(class="label-left",
        # f7Stepper(inputId=paste("stp_ingred",w,"n","ingred_popup",sep="_"),
        f7Stepper(inputId=paste("stp_ingred",w,id,"n","ingred_popup",sep="_"),
                  label="",
                  min=0.5,
                  max=20,
                  value=z,
                  step=0.5,
                  size="large",
                  raised=TRUE),
        #provides margins around text (which helps center stepper label with input)
        style="padding: 45px 0px 0px 0px; font-size: 18px;"
      )
    )
  })
}


### Create inputs programmatically
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}



### Check whether an image exists
checkImageExists <- function(user_id, recipe) {
  
  #grab and store all recipe names
  recipes<-drive_ls(user_id) %>%
    mutate(name=str_extract(name,"^.+(?=\\.)")) %>%
    pull(name)
  
  #check if recipe matches any in files
  tolower(recipe) %in% recipes
}



### Retrieve image once it is found
retrieveImage <- function(user_id, recipe) {
  #search for the image file in the Google Drive folder
  drive_file<-drive_ls(user_id) %>%
    mutate(name=str_extract(name,"^.+(?=\\.)")) %>%
    filter(name==tolower(recipe))
  
  if(length(drive_file) > 0) {
    #if an image file is found, download it to a temporary location
    temp_file <- tempfile()
    drive_download(drive_file, path = temp_file)
    
    #return the file path to be displayed in the renderImage function
    return(temp_file)
  }
  
  return(NULL)  #return NULL if no image file is found
}








