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
  "Vegetarian (e.g, beans)"
)




#### App Test=======================================================================================
### Generate recipe & ingredient data
demo_recipeDF <- tibble(
  recipe=c("chicken alfredo","chicken tacos"),
  appliance=c("stove, slow cooker","slow cooker"),
  protein=c("chicken","chicken")
)


demo_ingredDF <- tibble(
  recipe=c(
    rep("chicken alfredo",3),
    rep("chicken tacos",3)
  ),
  name=c(
    "chicken breast","alfredo sauce","pasta",
    "chicken breast","salsa","taco seasoning"
  ),
  size=c(
    "large","12 oz jar","16 oz package",
    "large","12 oz jar","packet"
  ),
  n=c(
    2,3,1,
    2,1,1
  )
)


demo_recipeDF
demo_ingredDF
 