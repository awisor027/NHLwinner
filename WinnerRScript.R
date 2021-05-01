Winner<-function(var1, var2){
newdata = read.csv("HockeyStats.csv", header=TRUE)
attach(newdata)
lm.final=lm(Shots.GP~GP+GF+FOWpercent)
lookuptable=cbind(Team, lm.final$fitted.values)
for(i in 1:length(Team)) if(var1==lookuptable[i,1])varone=as.numeric(lookuptable[i,2])
for(i in 1:length(Team)) if(var2==lookuptable[i,1])vartwo=as.numeric(lookuptable[i,2])
detach(newdata)
if (varone>vartwo) print(paste(var1, "wins!")) else if (vartwo>varone) print(paste(var2, "wins! ") ) else print("It is a tie")
}