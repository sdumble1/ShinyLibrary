#Function to modify the ODK form into an R object that can be used to link the form to the data to the analysis
#adds in some extra columns to the original form to do this

ODKForm4R<-function(formfile,language=NULL){
form<-openxlsx::read.xlsx(formfile,"survey")
labels<-openxlsx::read.xlsx(formfile,"choices")   

if(!"label"%in%colnames(form)){
  if(is.null(language)){
    form$label<-form[,min(grep("label",colnames(form)))]
  }
  else{
    form$label<-form[,paste0("label::",language)]  
  }
}

  form$groupname<-""
  form$repeatlevel<-""
  form$grouplabel<-""
  form$type<-str_replace(form$type,"begin ","begin_")
  form$type<-str_replace(form$type,"end ","end_") 
  
  form$type<-gsub("  "," ",form$type)
  
  form$class<-str_split_fixed(form$type," ",2)[,1]
  
  grp<-NULL
  grp_type<-NULL
  for(i in 1:nrow(form)){
    
    if(form$type[i]=="begin_group"|form$type[i]=="begin_repeat"){
      grp<-c(grp,form$name[i])
      grp_type<-c(grp_type,str_remove(form$class[i],"begin_"))
      form$grouplabel[i]<-ifelse(is.na(form$label[i]),form$name[i],form$label[i])
    }
    else{
      if(i==1){   form$grouplabel[i]<-""}
      else{
      form$grouplabel[i]<-form$grouplabel[i-1]  
      }
    }
    if(form$type[i]=="end_group"|form$type[i]=="end_repeat"){
      grp<-grp[-length(grp)]
      grp_type<-grp_type[-length(grp_type)]
    }
    form$groupname[i]<-paste(grp,collapse=".")
    
    form$repeatlevel[i]<-sum(str_detect(grp_type,"repeat"))
  }
  form$varname<-ifelse(form$groupname=="",form$name,paste(form$groupname,form$name,sep="."))
  
  form$varname<-ifelse(form$type%in%c("begin_group","begin_repeat","end_group","end_repeat"),
                       "",form$varname)
  

  form$list_name<-str_split_fixed(form$type," ",4)[,2]
  

  
  if(!"label"%in%colnames(labels)){
    if(is.null(language)){
      labels$label<-labels[,min(grep("label",colnames(labels)))]
    }
    else{
      labels$label<-labels[,paste0("label::",language)]  
    }
  }
return(list(form=form,labels=labels))  
}


#links the data downloaded from the server into a friendlier format
#merges variable labels and missing values handled appropriately and numbers treated as numbers and logicals treated as logicals


formt_odk<-function(data,form1,drop=TRUE){
  
  form<-form1$form
  labels<-form1$labels
  
  #merge in factor levels
  singles<-which(form$class=="select_one")
  for(i in singles){
    tmp_levs<-subset(labels,list_name==form$list[i])
    if(form$varname[i]%in%colnames(data)){
      data[,form$varname[i]]<-factor(data[,form$varname[i]],levels=tmp_levs$name,labels=tmp_levs$label)
    }
  }
  
  numbrers<-which(form$class%in%c("integer","decimal"))
  for(i in numbrers){
    if(form$varname[i]%in%colnames(data)){
      data[,form$varname[i]]<-as.numeric(as.character(data[,form$varname[i]]))
    }
  }
  
  #sort out date formats
  dates<-which(form$class%in%c("date"))
  for(i in dates){
    if(form$varname[i]%in%colnames(data)){
      data[,form$varname[i]]<-as.Date(data[,form$varname[i]])
    }
  }
  
  #identify logicals and recode
  
  TF<-function(x){
   mean(as.numeric(x%in%c("True","False","n/a",NA)))==1
  }
  
  logicals<-which(apply(data,2,TF)==TRUE)
  
  for(i in logicals){
    data[,i]<-as.logical(ifelse(data[,i]=="True",TRUE,
                     ifelse(data[,i]=="False",FALSE,NA)))
  }
  
  
  #other formats worth coding in?
  if(drop==TRUE){
    data=droplevels(data)
  }
  return(data1=data)
}

#modify functions from koboloader to show avaialble datasets linked to account
available_data<-function (user = NULL, api = "kobo") 
{
  URL <- sprintf(fmt = "%sdata.csv", koboloadeR:::host(api))
  u <- koboloadeR:::pwd_parse(user)
  x <-GET(URL, httr::authenticate(u$username, u$password))
  return(content(x))
}


#modify functions from koboloader to download data with no bells or whistles
get_data<-function (user = NULL, formid,api = "kobo") 
{
  URL <- sprintf(fmt = "%sdata/%s.csv", koboloadeR:::host(api),formid)
  u <- koboloadeR:::pwd_parse(user)
  x <-GET(URL, httr::authenticate(u$username, u$password), write_disk("data.csv",overwrite=TRUE))
  x1<-read.csv("data.csv")
  return(x1)
}


