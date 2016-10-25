// Declare the arrays globally
var events = [];
var time_raw = [];
var users = [];
var date = [];
var time = [];
$.getJSON('sessionevents.json', function(info){
	// Collect the events, raw time (i.e. time with date) and the user ids.
   		   	for(var i = 0; i < info.length; i++){
   		   		events[i] = info[i].type;
   		   		time_raw[i] = info[i].time.$date;
   		   		// The user id can be obtained from in data as user, or object or in data as an answeree
   		   		if(info[i].data.user === undefined){
   		   		    users[i] = info[i].data.answeree.$oid;
   		   		}
   		   		else{
   		   			if(info[i].data.user.$oid === undefined){
   		   				users[i] = info[i].data.user;
   		   			}
   		   			else{
                         users[i] = info[i].data.user.$oid;	
   		   			}
   		   		}
   		   			 
   		    } 
   		 });
