# Purpose: To create a Logo (footer) that will be on every page of the application

logo <- function(){
  
  div(class ="row",
      
      column(
        
        width=4,
        img(class ="img-rounded img-responsive",src="JSGS_CCSC_Logo_CMYK-highnobg.jpg", 
            style="width:200px; margin:auto; display:block; padding-top: 50px; padding-bottom: 25px;")),
      column(
        width=4,
        img(class ="img-rounded img-responsive",src="USask_CHASR_Logo.png", 
            style="width:200px; margin:auto; display:block; padding-top: 50px; padding-bottom: 25px;")),
      column(
        width=4,
        img(class ="img-rounded img-responsive",src="1200px-United_Farmers_of_Alberta_Logo.jpg", 
            style="width:200px; margin:auto; display:block; padding-top: 50px; padding-bottom: 25px;"))
  
  )
  
}