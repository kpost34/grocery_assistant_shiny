#Created by Keith Post on 3/20/23


#### UI Functions===================================================================================
### Set linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}


### Create block of recipe info
edit_recipe_info<-function(root_id,app_edit=NA,prot_edit=NA){
# add_recipe_sheet<-function(type="entry",root_id,app_edit=NA,prot_edit=NA){
  # if(type=="entry"){
  #   app_value<-rep(FALSE,5)
  #   protein_value<-FALSE
  # }
  # else if(type=="edit"){
    app_value<-app_choices_sheet1 %in% app_edit
    protein_value<-protein_choices_sheet1 %in% prot_edit
  # }

  #create appliance checkboxes
  app_list<-purrr::map2(.x=1:5,.y=app_value,
    function(num,log){
      p(f7Checkbox(inputId=paste0("chkGrp_app_",tolower(app_choices_sheet1[num]),root_id),
         label=app_choices_sheet1[num],
         value=log))
        # style="font-size: 16px")
    })

  #create protein checkboxes
  protein_list<-purrr::map2(.x=1:6,.y=protein_value,
    function(num2,log2){
      p(f7Checkbox(inputId=paste0("chkGrp_protein_",tolower(protein_choices_sheet1[num2]),root_id),
         label=protein_choices_sheet1[num2],
         value=log2))
        # style="font-size: 16px")
    })

  #right-aligns text
  p("Click outside sheet to return to main menu",style="text-align: right")
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



# add_ingredients<-function(n_prev=0,n,counts=NA) {
#   
#   ns<-(n_prev+1):(n_prev+n)
#   
#   pos<-ordinal(ns)
#   
#   if(!is.na(counts)){
#     
#     
#     
#   }
#   
#   map2(pos,ns,function(x,y) {
#     f7Block(h3(paste("Enter",x,"ingredient")),
#             hairlines=FALSE,
#       f7Text(inputId=paste0("txt_ingred",y,"_nm_ingredSheets"),
#            label="Name"),
#       splitLayout(cellWidths=c("75%","25%"),
#         f7Text(inputId=paste0("txt_ingred",y,"_size_ingredSheets"),
#            label="Size (e.g., 12 oz can)"),
#         div(class="label-left",
#           f7Stepper(inputId=paste0("stp_ingred",y,"_n_ingredSheets"),
#               label="Count",
#               min=0.5,
#               max=20,
#               value=1,
#               step=0.5,
#               size="large",
#               raised=TRUE),
#           #provides margins around text (which helps center stepper label with input)
#           style="padding: 40px 0px 0px 0px; font-size: 20px;"
#         ),
#         #aligns stepper input with text input
#         tags$style(type='text/css', paste0("#stp_ingred",y,"_n_ingredSheets"," { vertical-align: middle;
#                    width: 80%}"))
#       ),
#       hr(),
#     )
#     })
# }


### Create inputs programmatically
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}



