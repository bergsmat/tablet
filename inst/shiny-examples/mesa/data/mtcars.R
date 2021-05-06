data(mtcars)
library(haven)
library(dplyr)
library(magrittr)
library(yamlet)
library(SASxport)

x <- mtcars
x$car <- rownames(x)
x %<>% select(car, everything())

x %<>% decorate('
car:  [ Automobile ]
mpg:  [ Mileage, mi/gal ]
cyl:  [ Number of Cylinders ]
disp: [ Displacement, in^3 ]
hp:   [ Gross Horsepower, hp ]
drat: [ Rear Axle Ratio ]
wt:   [ Weight, klb ]
qsec: [ Quarter-mile Time, sec ]
vs:   [ Engine, [ V-shaped: 0, Straight: 1 ]]
am:   [ Transmission, [ Automatic: 0, Manual: 1 ]]
gear: [ Number of Forward Gears ]
carb: [ Number of Carburetors ]
')

x %>% io_csv('mtcars.csv')
x %>% write_sas('mtcars.sas7bdat')
x %>% write.xport(file = 'mtcars.xpt')
