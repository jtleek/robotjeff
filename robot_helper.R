run_robot = function(txt,wav){
 words = str_split(txt, " ")[[1]] %>% str_to_lower()
 clap = which(str_detect(words,"applause|clap"))
 
 
 txt2 = str_replace_all(txt,"to my left","left2")
 txt2 = str_replace_all(txt2,"to my right","right2")
 words2 = str_split(txt2, " ")[[1]] %>% str_to_lower()
 word_count = rep(1,length(words2))
 
 
 point_left = str_detect(words2,"left2")
 point_right = str_detect(words2,"right2")
 word_count[point_left] = 3
 word_count[point_right] = 3
 
 left = cumsum(word_count)[which(point_left)]-2
 right = cumsum(word_count)[which(point_right)]-2
 
 action_times = c(left,right,clap)
 actions = c(rep("left",length(left)),
             rep("right",length(right)),
             rep("clap",length(clap)))
 actions = actions[order(action_times)]
 action_times = action_times[order(action_times)]
 
 len = length(wav)/wav@samp.rate
 nwords = length(words)
 word_len = len/nwords
 
 action_times = c(0,action_times,nwords)
 sleep_times = diff(action_times)
 nsleep = length(sleep_times)-1
 
 if(length(action_times) == 2){
   system("audacious ~/text.wav",wait=FALSE)
   start_mouth()
   Sys.sleep(len)
   stop_mouth()
 }else{
   system("audacious ~/text.wav",wait=FALSE)
   start_mouth()
   for(i in 1:nsleep){
     Sys.sleep(sleep_times[i]*word_len)
     do_action(actions[i])
   }
   Sys.sleep(sleep_times[(i+1)]*word_len)
   stop_mouth()
 }

 
}

start_mouth = function(){
  GET("http://192.168.7.2:9000/?m=1")
}

stop_mouth = function(){
  GET("http://192.168.7.2:9000/?m=0")
}

clap_hands = function(nclap=2,dur=0.25){
    GET("http://192.168.7.2:9000?l=-0.5&r=0.5")
  for(i in 1:nclap){
    Sys.sleep(dur)
    GET("http://192.168.7.2:9000?l=0.3&r=-0.3")
    Sys.sleep(dur)
    GET("http://192.168.7.2:9000?l=-0.5&r=0.5")
  }
  Sys.sleep(dur)
  GET("http://192.168.7.2:9000?l=0&r=0")
}

point_left = function(dur=1){
  GET("http://192.168.7.2:9000?l=-1")
  Sys.sleep(dur)
  GET("http://192.168.7.2:9000?l=0")
}

point_right = function(dur=1){
  GET("http://192.168.7.2:9000?r=1")
  Sys.sleep(dur)
  GET("http://192.168.7.2:9000?r=0")
}


do_action = function(action){
  if(action=="left"){
    point_left()
  }else if(action=="right"){
    point_right()
  }else if(action=="clap"){
    clap_hands()
  }
}
