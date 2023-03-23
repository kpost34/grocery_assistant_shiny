#Created by Keith Post on 3/20/23


#### UI Functions===================================================================================
### Set linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}


### Create blocks of ingredients
add_ingredients<-function(n_prev=0,n,sheet_num) {
  
  ns<-(n_prev+1):(n_prev+n)
  
  pos<-ordinal(ns)
  
  map2(pos,ns,function(x,y) {
    f7Block(paste("Enter",x,"ingredient"),
            hairlines=FALSE,
      f7Text(inputId=paste0("txt_ingred",y,"_nm_sheet",sheet_num),
           label="Name"),
      splitLayout(cellWidths=c("75%","25%"),
        f7Text(inputId=paste0("txt_ingred",y,"_size_sheet",sheet_num),
           label="Size (e.g., 12 oz can)"),
        div(class="label-left",
          f7Stepper(inputId=paste0("txt_ingred",y,"_n_sheet",sheet_num),
              label="Count",
              min=0,
              max=20,
              value=1,
              step=0.5,
              size="large",
              raised=TRUE),
          #provides margins around text (which helps center stepper label with input)
          style="padding: 40px 0px 0px 0px; font-size: 20px;"
        ),
        #aligns stepper input with text input
        tags$style(type='text/css', paste0("#txt_ingred",y,"_n_sheet",sheet_num," { vertical-align: middle;
                   width: 80%}"))
      ),
      hr(),
    )
    })
}



