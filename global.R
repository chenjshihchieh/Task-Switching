##Load in the image names to be called later
images       <- read.csv(file.path("www", "csv", "images.csv"), stringsAsFactors = FALSE)
images.info  <- images$info #list of the image names for information pages
stimuli.info <- read.csv( #files containing the types of stimuli and the correctResp
  file.path("www", "csv", "stimuli.csv"), 
  stringsAsFactors = FALSE
  )



#Setting up variables to be used####
#Position of stimuli
position 　　     <- reactiveVal("stimuli_top")
train.fillFirst   <- sample(0:1, 1) #1 = fill first 0 = shape first
data.fillFirst    <- sample(0:1, 1) #1 = fill first 0 = shape first

#Variable for displaying pages and setup timing
timesup           <- reactiveVal(FALSE)
wrongbutton       <- reactiveVal(FALSE)
wrongbutton.count <- reactiveVal(0)
countdown         <- reactiveVal(TRUE)
countdown.count   <- reactiveVal(0)
background        <- reactiveVal("")
stimuli.object    <- reactiveVal("empty.png")
emptyframe        <- reactiveVal(TRUE)
emptyframe.count  <- reactiveVal(0)
readyprompt       <- reactiveVal(TRUE)
instruction       <- reactiveVal(TRUE)
instruction.break <- reactiveVal(FALSE)
instruction.count <- reactiveVal(0)
onTrialQ          <- reactiveVal(FALSE)
spacebar          <- reactiveVal(FALSE)

#relevant page numbers and number of trials
transition        <- reactiveVal(1) #Page number
stimuli           <- 1:4 #stimuli numbering to be randomized
triallen.train    <- 12 #Number of train trials
triallen.mixtrain <- 20 #Number of train trials for mixed trials
triallen.data     <- 20 #Number of data collection trials
triallen.mixdata  <- 60 #Number of data collection trials for mixed trials
triallen.total    <- sum(triallen.train * 2,
                         triallen.mixtrain,
                         triallen.data * 2,
                         triallen.mixdata)
info.end          <- 11 #end of the information 
train.trial1      <- info.end + triallen.train #end of train trial 1
train.trial2      <- train.trial1 + triallen.train #end of train trial 1
train.trialMix    <- train.trial2 + triallen.mixtrain #end of train trial mix
data.trial1       <- train.trialMix + triallen.data #end of collection trial 1
data.trial2       <- data.trial1 + triallen.data #end of collection trial 2
data.trialMix     <- data.trial2 + triallen.mixdata #end of collection trial mix
trialEnd.list     <- c(train.trial1,
                       train.trial2,
                       train.trialMix,
                       data.trial1,
                       data.trial2,
                       data.trialMix)

# Initialize the timer, 4 seconds, inactive
timer             <- reactiveVal(0)
active            <- reactiveVal(FALSE)


## Generating train Sets####
## generating train set Fill stimuli
train.fill.index  <- sample(rep(stimuli, triallen.train/4), triallen.train)
train.fill        <- stimuli.info[train.fill.index, 'stimuli']

## generating train set Shape stimuli
train.shape.index <- sample(rep(stimuli, triallen.train/4), triallen.train)
train.shape       <- stimuli.info[train.shape.index, 'stimuli']


## generating train set shape and fill
#First shape and fill set stimuli
train.mix.index   <- sample(rep(stimuli, triallen.mixtrain/4), triallen.mixtrain)
train.mix         <- stimuli.info[train.mix.index, 'stimuli']
train.mix.position <- sample(rep(c("stimuli_top", "stimuli_bottom"), 
                                    triallen.mixtrain/2), 
                            triallen.mixtrain)

##Generating Trial set####
#Data fill set
data.fill.index <- sample(rep(stimuli, triallen.data/4), triallen.data)
data.fill       <- stimuli.info[data.fill.index, 'stimuli']

#Data shape set
data.shape.index <- sample(rep(stimuli, triallen.data/4), triallen.data)
data.shape       <- stimuli.info[data.shape.index, 'stimuli']

#First data set stimuli
data.mix.index   <- sample(rep(stimuli, triallen.mixdata/4), triallen.mixdata)
data.mix         <- stimuli.info[data.mix.index, 'stimuli']
data.mixPosition <- sample(rep(c("stimuli_top", "stimuli_bottom"), 
                               triallen.mixdata/2), 
                           triallen.mixdata)

#Empty vector for recording response
response <- vector(mode = "character", 
                   length = triallen.total)

#For storing reaction time
reactiontime <- vector(mode = "numeric", 
                       length = triallen.total)


# answer to stimuli and name of stimuli
if(train.fillFirst){
  answer <- c(stimuli.info[train.fill.index, "fill"],
              stimuli.info[train.shape.index, "shape"])
  
  stimuli <- c(stimuli.info[train.fill.index, "stimuli"],
               stimuli.info[train.shape.index, "stimuli"])
  
  Trial <- c(rep("Training-PureFill", triallen.train),
             rep("Training-PureShape", triallen.train),
             rep("Training-Mix", triallen.mixtrain))
  
}else{
  answer <- c(stimuli.info[train.shape.index, "shape"],
              stimuli.info[train.fill.index, "fill"])
  
  stimuli <- c(stimuli.info[train.shape.index, "stimuli"],
               stimuli.info[train.fill.index, "stimuli"])
  
  Trial <- c(rep("Training-PureShape", triallen.train),
             rep("Training-PureFill", triallen.train),
             rep("Training-Mix", triallen.mixtrain))
  
}

#Whether or not the task is switched into
#The first stimuli of mix trial is always not switch
taskSwitch <- rep(0, (triallen.train * 2) + 1) 

#Answer and stimuli for mix condition
i <- 1
while(i <= triallen.mixtrain){
  if(train.mix.position[i] == "stimuli_top"){
    answer <- c(answer, 
                stimuli.info[train.mix.index[i], "shape"])
    
    stimuli <- c(stimuli,
                 stimuli.info[train.mix.index[i], "stimuli"])
    
  }else{
    answer <- c(answer, 
                stimuli.info[train.mix.index[i], "fill"])
    
    stimuli <- c(stimuli, 
                 stimuli.info[train.mix.index[i], "stimuli"])
  }
  
  i <- i + 1
  if(i <= triallen.mixtrain){
    taskSwitch <- c(taskSwitch, 
                as.numeric(train.mix.position[i-1] != train.mix.position[i]))
  }
  
}

# answer to stimuli and name of stimuli
if(data.fillFirst){
  answer <- c(answer,
              stimuli.info[data.fill.index, "fill"],
              stimuli.info[data.shape.index, "shape"])
  
  stimuli <- c(stimuli,
               stimuli.info[data.fill.index, "stimuli"],
               stimuli.info[data.shape.index, "stimuli"])
  
  Trial <- c(Trial,
             rep("Data-PureFill", triallen.data),
             rep("Data-PureShape", triallen.data),
             rep("Data-Mix", triallen.mixdata))
                  
  
}else{
  answer <- c(answer,
              stimuli.info[data.shape.index, "shape"],
              stimuli.info[data.fill.index, "fill"])
  
  stimuli <- c(stimuli,
               stimuli.info[data.shape.index, "stimuli"],
               stimuli.info[data.fill.index, "stimuli"])
  
  Trial <- c(Trial,
             rep("Data-PureShape", triallen.data),
             rep("Data-PureFill", triallen.data),
             rep("Data-Mix", triallen.mixdata))
  
}

#The first stimuli of mix trial is always not switch
taskSwitch <- c(taskSwitch, rep(0, (triallen.data * 2) + 1))

#Answer and stimuli for mix condition
i <- 1
while(i <= triallen.mixdata){
  if(data.mixPosition[i] == "stimuli_top"){
    answer <- c(answer,  
                stimuli.info[data.mix.index[i], "shape"])
    
    stimuli <- c(stimuli,
                 stimuli.info[data.mix.index[i], "stimuli"])
    
  }else{
    answer <- c(answer, 
                stimuli.info[data.mix.index[i], "fill"])
    
    stimuli <- c(stimuli,
                 stimuli.info[data.mix.index[i], "stimuli"])
  }
  
  i <- i + 1
  if(i <= triallen.mixdata){
    taskSwitch <- c(taskSwitch, 
                as.numeric(data.mixPosition[i-1] != data.mixPosition[i]))
  }
}

