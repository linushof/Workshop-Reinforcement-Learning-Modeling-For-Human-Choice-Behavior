#Aim:
#this code generates from a gui select list for the data that is going to be used for sampling

set_datatype<-function(){

mydatatype   =dlg_list(list('empirical','artificial'), multiple = TRUE)$res

return(mydatatype)
}

