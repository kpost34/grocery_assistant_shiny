#Created by Keith Post on 3/20/23


#### UI Functions===================================================================================
### Set linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}



### Create block of accordion inputs
add_accordions <- function(list_input) {
  purrr::imap(list_input,function(x,y) {
    f7AccordionItem(
      title=em(y),
      div(
        x,
        linebreaks(2),
        style="margin-left: 2%; margin-right: 2%"
      )
    )
  })
}


list_example <- list(planner_list_txt) %>%
  set_names("Plan meals and build a grocery list")




### Create blocks of ingredients
add_ingredients<-function(n_prev=0,n=4) {
  
  ns<-(n_prev+1):(n_prev+n)
  
  pos<-ordinal(ns)
  
  #create multiple blocks of ingred info boxes
  ingred_block <- map2(pos,ns,function(x,y) {
    f7Block(h3(paste("Enter",x,"ingredient")),
            hairlines=FALSE,
      f7Text(inputId=paste0("txt_ingred",y,"_nm_ingredSheets"),
           label="Name"),
      splitLayout(cellWidths=c("65%","35%"),
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
          style="padding: 30px 0px 0px 0px; font-size: 20px;"
        ),
        #aligns stepper input with text input
        tags$style(type='text/css', paste0("#stp_ingred",y,"_n_ingredSheets"," { vertical-align: middle;
                   width: 80%}"))
      ),
      hr(),
    ) 
    }) 
  
  #outputs them together into two splitLayouts of two blocks
  tagList(
    splitLayout(ingred_block[1], ingred_block[2]),
    splitLayout(ingred_block[3], ingred_block[4])
  )

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
        style="padding: 25px 0px 0px 0px; font-size: 18px;"
      )
    )
  })
}




#### Server Functions===============================================================================
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



### Convert and downgrade image before uploading to Google drive
reduce_image_size <- function(name, img_fp, extension){
  
  #grab and change filename
  new_filename <- str_replace(name, paste0(extension,"$"),"jpg")
  
  #create output dir (if does not exist)
  out_dir <- file.path(tempdir(), "temp")
  
  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  } 
  
  out_fp <- file.path(out_dir, new_filename)
    
  
  #read selected file, convert to jpg (if nec), and write a compressed version to temp dir
  img_fp %>%
    image_read() %>%
    {if(extension!="jpg") image_convert(.,format="jpg") else .} %>%
    image_write(out_fp,quality=50)
  
  #return output dir
  return(out_fp)
}






