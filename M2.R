#hard coded
x = c(3,5,7,9,11,13)
movave = function(x) {c((x[1] + x[2])/2, (x[2] + x[3])/2 ,(x[3] + x[4])/2, (x[4] + x[5])/2 , (x[5] + x[6])/2)}
movave(x)


#returns value for only 6
x = c(3,5,7,9,11,13)
movave = function(x) {  c((x[length(x)-1] + x[length(x)])/2) }
movave(x)

#trying to iterate, doesnt return a value
x = c(3,5,7,9,11,13)
movave = function(x) {for (i in 2:length(x)) {c((x[i-1] + x[i])/2)} }
movave(x)

#trying to test just the loop, only returns last value
for (i in 2:6) { y = c((x[i-1] + x[i])/2)}


#8.1
x = c(3,5,7,9,11,13)
reverse = function (x) { c(x[length(x):1]) }
reverse(x)

#7.13
factorial(49/50)
sum(prod(3:49)/prod(4:50)+prod(3:47)/prod(4:48)+prod(3:45)/prod(4:46)+prod(3:43)/prod(4:44)+prod(3:41)/prod(4:42)+prod(3:39)/prod(4:40)+prod(3:37)/prod(4:38)+prod(3:35)/prod(4:36)+prod(3:33)/prod(4:34)+prod(3:31)/prod(4:32)+prod(3:29)/prod(4:30)+prod(3:27)/prod(4:28)+prod(3:25)/prod(4:26)+prod(3:23)/prod(4:24)+prod(3:21)/prod(4:22)+prod(3:19)/prod(4:20)+prod(3:17)/prod(4:18)+prod(3:15)/prod(4:16)+prod(3:13)/prod(4:14)+prod(3:11)/prod(4:12)+prod(3:9)/prod(4:10)+prod(3:7)/prod(4:8)+prod(3:5)/prod(4:6)+prod(3)/prod(4))



#8.9
x = c(3,5,7,9,11,13)
mad = function(x) { sum(abs(x[1:length(x)]-mean(x)))}
mad(x)


       

