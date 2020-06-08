function(input, output, session){
  #Event listener for key presses####
  
  #observes up arrow and down arrow keys####
  #for navigating through info images
  observeEvent(input$keypress, {
    if(input$start > 0 & transition() <= info.end){
      
      if (input$keypress == 40 & transition() < info.end) {
        transition(transition() + 1) #arrow down to advance info
        
        
      } else if (input$keypress == 38 & transition() > 1) {
        transition(transition() - 1) #arrow up to previous info
        
        
      } else if (input$keypress == 81 & transition() == 11) { #observes q
        transition(transition() + 1) #press 'q' to start first trial
      }
    }
    
    #observes b button and n button####
    #for responding to the trials
    if(onTrialQ()){
      
      if(input$keypress == 66 | input$keypress == 78){
        #Current Row
        RT <- Sys.time() - RT.before
        i  <- transition() - info.end
        
        #Registering key press. Assess correctness later
        response[i]     <<- as.character(input$keypress)
        #Record reaction time
        reactiontime[i] <<- RT
        
        #Make sure an empty frame comes up
        emptyframe(TRUE)
        #disable onTrialQ
        onTrialQ(FALSE)
        
        
        # Check if it's end of the trial
        trialEnd <- transition() %in% trialEnd.list
        if(trialEnd){
          readyprompt(TRUE)
          instruction(TRUE)
          countdown(TRUE)
        }
        
        
        if(response[i] != answer[i]){
          wrongbutton(TRUE)
        }
        
        #adding to transition() to advance page
        transition(transition() + 1)
        
        
        
      }
    }
    
    
    #observes space bar####
    #for starting tasks, moving on to next section of tasks
    if(input$keypress == 32 & transition() < 11 & input$start > 0){
      transition(transition() + 1)
    }else if(input$keypress == 32 & readyprompt() & spacebar()){
      readyprompt(FALSE)
    }else if(input$keypress == 32 & instruction() & spacebar()){
      instruction(FALSE)
    }else if(input$keypress == 32 & spacebar() & transition() > data.trialMix){
      transition(transition() + 1)
    }
    
    
    
    timer(0)
    
  })

  #Transitioning through the images####
  #beginning info images
  output$info <- renderImage({
    image.path <- file.path("www", "info", images.info[transition()])
    list(src   = image.path, border = "0", 
         alt   = images.info[transition()],
         class = "center")
  }, deleteFile = FALSE)
  
  #For images that triggers with a timer####
  observe({
    invalidateLater(800, session)
    isolate({
      if(countdown.count() == 1) {
        background(file.path("countdown", "countdown.png"))
        stimuli.object("empty.png")
        countdown.count(countdown.count() + 1)
        
      }else if(countdown.count() == 2){
        background(file.path("countdown", "countdown3.png"))
        countdown.count(countdown.count() + 1)
        
      }else if(countdown.count() == 3){
        background(file.path("countdown", "countdown2.png"))
        countdown.count(countdown.count() + 1)
        
      }else if(countdown.count() == 4){
        background(file.path("countdown", "countdown1.png"))
        countdown.count(0)
        countdown(FALSE)
        
        
        
      }else if(emptyframe.count() == 1){
        background("frame.png")
        stimuli.object("empty.png")
        timer(5)
        emptyframe.count(emptyframe.count() + 1)
        
      }else if(emptyframe.count() == 2){
        emptyframe.count(0)
        emptyframe(FALSE)
        
      }else if(timer() > 1){
        timer(timer() - 1)
        
      }else if(timer() == 1 & !timesup()){
        timesup(TRUE)
        timer(0)
        
        
      }else if(timesup()){
        #Record the response as 9999
        i            <- transition() - info.end
        response[i]     <<- 9999
        reactiontime[i] <<- 4
        
        
        # Check if it's end of the trial
        trialEnd <- transition() %in% trialEnd.list
        if(trialEnd){
          readyprompt(TRUE)
          instruction(TRUE)
          countdown(TRUE)
        }
        
        emptyframe(TRUE)
        instruction.break(TRUE)
        timesup(FALSE)
        transition(transition() + 1)
        
      }
      
      if(wrongbutton()){
        Sys.sleep(1)
        wrongbutton(FALSE)
        instruction.break(TRUE)
      }
      
      if(instruction.break() & instruction.count() == 0){
        instruction.count(4)
        
      }else if(instruction.count() > 1){
        instruction.count(instruction.count() - 1)
      }else if(instruction.count() == 1){
        instruction.break(FALSE)
        instruction.count(0)
      }
      
    })
  })
  
  #For instructions that display for a certain amount of time
  observe({
    if(emptyframe()){
      Sys.sleep(1)
      emptyframe(TRUE)
    }
  })
  
  #First train trials####
  observe({
    if(transition() > info.end & transition() <= train.trial1){
      
      if(instruction.break()){
        if(train.fillFirst){ #If fill is first, then 1st trial is fill
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }else{
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        #Ready training 
       background(file.path("readyGo", "readytraining.png")) 
       stimuli.object("empty.png")
       spacebar(TRUE)
        
      }else if(instruction()){
        #Instruct for which condition is being done
        spacebar(TRUE)
        if(train.fillFirst){
          background(file.path("readyGo", "gojustfilling.png"))
          stimuli.object("empty.png")
        }else{
          background(file.path("readyGo", "gojustshape.png"))
          stimuli.object("empty.png")
        }
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
      
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - info.end #indicate trial number
        position(switch(train.fillFirst + 1, #assigning stimuli position
                            "stimuli_top",
                            "stimuli_bottom"
                       )
        )
        
        
        
        if(train.fillFirst){
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", train.fill[stimuli.index]))
          
          
        }else{
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", train.shape[stimuli.index]))
          
          
        }
        
        onTrialQ(TRUE) #Activate buttons for this page
        timer(5) #start timer
        RT.before <<- Sys.time()
        
        
      }
    }
  # })
  
  
  
  #Second train trials####
  # observe({
    if(transition() > train.trial1 & transition() <= train.trial2){
      
      if(instruction.break()){
        if(train.fillFirst){#If fill was first, then second trial is shape
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }else{
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        background(file.path("readyGo", "readytraining.png"))
        stimuli.object("empty.png")
        
        
      }else if(instruction()){
        #show the ready prompt
        stimuli.object("empty.png")
        if(train.fillFirst){
          background(file.path("readyGo", "gojustshape.png"))
        }else{
          background(file.path("readyGo", "gojustfilling.png"))
        }
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
        
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - train.trial1 #indicate trial number
        
        #assign position of the stimuli depending
        position(switch(2 - train.fillFirst, #assigning stimuli position 
                        "stimuli_top", #For second trial, if fillFirst was TRUE(1),
                        "stimuli_bottom"     #then 1 should be top for trial two.
        )
        )
        
        
        if(train.fillFirst){
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", train.shape[stimuli.index]))
          
          
        }else{
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", train.fill[stimuli.index]))
          
          
        }
        
        onTrialQ(TRUE) #activate buttons for this page
        timer(5) #start timer
        RT.before <<- Sys.time()
        
      }
    }
  # })
  
  
  
  #Mixed train trials####
  # observe({
    if(transition() > train.trial2 & transition() <= train.trialMix){
      
      if(instruction.break()){
        if(position() == "stimuli_top"){
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }else if(position() == "stimuli_bottom"){
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        background(file.path("readyGo", "readytraining.png")) 
        stimuli.object("empty.png")
        spacebar(TRUE)
        
        
      }else if(instruction()){
        spacebar(TRUE) #Activate spacebar
        #show the ready prompt
        background(file.path("readyGo", "gomix.png"))
        stimuli.object("empty.png")
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
        
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - train.trial2 #indicate trial number
        position(train.mix.position[stimuli.index])
        stimuli.object(file.path("stimuli", train.mix[stimuli.index]))
        
        
        onTrialQ(TRUE) #Activate buttons for this page
        timer(5) #start the timer
        RT.before <<- Sys.time()
        
        
      }
    }
  # })
  
  
  
  #First data trials####
  # observe({
    if(transition() > train.trialMix & transition() <= data.trial1){
      
      if(instruction.break()){
        if(position() == "stimuli_top"){
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }else if(position() == "stimuli_bottom"){
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        #Ready training 
        background(file.path("readyGo", "readyreal.png")) 
        stimuli.object("empty.png")
        spacebar(TRUE)
        
        
      }else if(instruction()){
        #Instructions to signal which condition P is doing
        stimuli.object("empty.png")
        spacebar(TRUE) #Activate spacebar
        if(train.fillFirst){
          background(file.path("readyGo", "gojustfilling.png"))
        }else{
          background(file.path("readyGo", "gojustshape.png"))
        }
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
        
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - train.trialMix #indicate trial number
        position(switch(data.fillFirst + 1, #assigning stimuli position
                        "stimuli_top",
                        "stimuli_bottom"
        )
        )
        
        if(data.fillFirst){
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", data.fill[stimuli.index]))
          
          
        }else{
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", data.shape[stimuli.index]))
          
          
        }
        
        onTrialQ(TRUE) #Activate buttons for this page
        timer(5) #Start timer
        RT.before <<- Sys.time()
        
        
      }
    }
  # })
  
  
  
  #Second data trials####
  # observe({
    if(transition() > data.trial1 & transition() <= data.trial2){
      
      if(instruction.break()){
        if(position() == "stimuli_top"){
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }else if(position() == "stimuli_bottom"){
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        background(file.path("readyGo", "readyreal.png")) 
        stimuli.object("empty.png")
        
        
      }else if(instruction()){
        #show the ready prompt
        stimuli.object("empty.png")
        if(data.fillFirst){
          background(file.path("readyGo", "gojustshape.png"))
        }else{
          background(file.path("readyGo", "gojustfilling.png"))
        }
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - data.trial1 #indicate trial number
        position(switch(2 - data.fillFirst, #assigning stimuli position
                        "stimuli_top",
                        "stimuli_bottom"
        )
        )
        
        if(!data.fillFirst){
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", data.fill[stimuli.index]))
          
          
        }else{
          #file path to the image for the stimuli
          stimuli.object(file.path("stimuli", data.shape[stimuli.index]))
          
          
        }
        
        onTrialQ(TRUE) #Activate buttons for this page
        timer(5) #Start timer
        RT.before <<- Sys.time()
        
        
      }
    }
  # })
  
  
  
  #Mixed data trials####
  # observe({
    if(transition() > data.trial2 & transition() <= data.trialMix){
      
      if(instruction.break()){
        if(position() == "stimuli_top"){
          background(file.path("instructions", "shapeinstruction.png"))
          stimuli.object("empty.png")
          
        }else if(position() == "stimuli_bottom"){
          background(file.path("instructions", "fillinginstruction.png"))
          stimuli.object("empty.png")
          
        }
        
      }else if(readyprompt()){
        background(file.path("readyGo", "readyreal.png"))
        stimuli.object("empty.png")
        spacebar(TRUE)
        
        
      }else if(instruction()){
        #Instruct for which condition P is doing
        spacebar(TRUE) #Activate spacebar
        background(file.path("readyGo", "gomix.png"))
        stimuli.object("empty.png")
        
        
      }else if(wrongbutton()) { #Trigger only with logical statement
        #Wrong button messages - pop up only when triggered
        # change logical to FALSE
        background(file.path("comments", "wrongkey.png"))
        stimuli.object("empty.png")
        
        
      }else if(countdown()) { #Need Logical
        #Count down to starting the trial
        countdown.count(1)
        
        
      }else if(emptyframe()){
        #display empty frame before each transition
        emptyframe.count(1)
        
        
      }else if(timesup()) { #Trigger only with logical statement
        #Times up messages - pop up only when triggered
        background(file.path("comments", "tooslow.png"))
        stimuli.object("empty.png")
        
        
      }else{
        #The trials
        background("frame.png")
        stimuli.index <- transition() - data.trial2 #indicate trial number
        position(data.mixPosition[stimuli.index])
        stimuli.object(file.path("stimuli", data.mix[stimuli.index]))
        
        
        onTrialQ(TRUE) #Activate buttons for this page
        timer(5) #Set the timer
        RT.before <<- Sys.time()
        
        
      }
    }
    
    if(transition() == data.trialMix + 1){
      
      #Generate result
      Result <<- response == answer
      i <- 1
      while(i <= length(response)){
        Result[i] <<- switch(Result[i] + 1,
                            "Incorrect",
                            "Correct")
        i <- i + 1
      }
      
      Result[response == 9999] <<- "Times up"
      
      #Congruency of task
      Congruency <<- vector(mode = "character", length = length(stimuli))
      i <- 1
      while(i <= length(stimuli)){
        
        Congruency[i] <<- switch(stimuli[i], 
                                "shape1fill1.png" = "Congruent",
                                "shape1fill2.png" = "Incongruent",
                                "shape2fill1.png" = "Incongruent",
                                "shape2fill2.png" = "Congruent")
        
        i <- i + 1
      }
      
      #Combining the variables
      result <<- data.frame(Trial, 
                           Stimuli = stimuli, 
                           Result, 
                           Congruency, 
                           RT = reactiontime)
    }
  })
  
  #For testing, See if answers are being saved####
  observe({
    transition()
    output$responseTable <- renderTable(data.frame(response,
                                              reactiontime))
  })
  
  output$answerTable   <- renderTable(answer)
  
  #Main display to transition through pages based on a transitional variable####
  output$main.display <- renderUI({
    #output only when start button is pressed
    if (input$start > 0) {
      
      if (transition() <= 11) {
        #Display for introduction
        imageOutput("info")
        
      } else if(transition() <= data.trialMix) {
        #First pratice trial
        h3("First training trial")
        HTML(paste0("<center>", 
                    tags$div(
                      img(src = background(), class = "stimuli_frame"),
                      img(src = stimuli.object(), class = position()),
                      class = "stimuli_parent"
                      )
                    , "</center>"))
        
        
        
      }else if(transition() == data.trialMix + 1){
        background(file.path("comments", "thankyou.png"))
        stimuli.object("empty.png")
        
        
      }else{
        #Results trial where participants see and downloads their result
        HTML(paste0("<center>RT in pure trials: ", 
             mean(reactiontime[Trial %in% c("Data-PureShape", "Data-PureFill")]), 
             "</center>
             <br/>
             <center>RT in mixed trials: ", 
             mean(reactiontime[ Trial == "Data-Mix"]), 
             "</center>
             <br/>
             <center>Mixing Cost: ", 
             '-'(mean(reactiontime[ Trial == "Data-Mix"]),
                 mean(reactiontime[Trial %in% c("Data-PureShape", "Data-PureFill")])), 
             "</center>"
             
             #downloadButton("downloadData", "Download")
                    )
             )
        
        
      }
    }else{
      img(src    = file.path("icons", "kpuLogo.jpg"), 
          width  = "1040px", 
          height = "25%", 
          alt    = "KPU Logo", 
          class  = "center")
    }
    

  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}