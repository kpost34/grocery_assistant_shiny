#Created by Keith Post on 3/19/23


#### Recipe Sheet Choices===========================================================================
### Appliance Choices
app_choices_sheet1<-c(
  "Stove",
  "Oven",
  "Slow cooker",
  "Grill",
  "Broiler"
  )


### Protein Source Choices
protein_choices_sheet1<-c(
  "Chicken",
  "Beef",
  "Steak",
  "Pork",
  "Fish",
  "Vegetarian"
)



##### View Recipes==================================================================================
#### Add/Update Image by File--image file extensions
img_ext<-c(
  "jpg",
  "png",
  "gif",
  "webp",
  "tiff",
  "psd",
  "raw",
  "bmp",
  "heif",
  "indd",
  "jpeg 2000",
  "svg",
  "ai",
  "eps",
  "pdf"
)




#### App Test=======================================================================================
### Generate recipe & ingredient data
demo_recipeDF <- tibble(
  id=1:3,
  recipe=c("Chicken alfredo","Chicken tacos","Pot roast"),
  appliance=c("Stove, Slow cooker","Slow cooker","Slow cooker"),
  protein=c("Chicken","Chicken","Beef")
)


demo_ingredDF <- tibble(
  recipe=c(
    rep("Chicken alfredo",3),
    rep("Chicken tacos",4),
    rep("Pot roast",4)
  ),
  name=c(
    "chicken breast","alfredo sauce","pasta",
    "chicken breast","salsa","taco seasoning", "corn tortillas",
    "chuck roast","golden potatoes","baby carrots","beef broth"
  ),
  size=c(
    "large","12 oz jar","16 oz package",
    "large","12 oz jar","packet","30-pack",
    "3 lb","whole","16 oz bag","15 oz can"
  ),
  n=c(
    2,3,1,
    2,1,1,1,
    1,6,1,1
  )
)




#### Meal Plan & Shopping List======================================================================
### Send email copy
## String for detecting legitimate emails
email_str_detect<-"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"



