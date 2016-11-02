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
console.log(events);
console.log(time_raw);
console.log(users);
setTimeout(function()
{
for(var j = 0; j < time_raw.length; j++){
   date[j] = time_raw[j].substring(0,10);
   time[j] = time_raw[j].substring(11,23);
}
if events = 
}, 1000);
console.log(date);
console.log(time);
	

var time_seconds = [];

// These variables store the binary state of each user for a certain time interval

var exercise = [];
var connected = [];
var focus = [];
var idle = [];
var input = [];
var submitted = [];
var students = [];

for(var k = 0; k < time_raw.length; k++){

 // Convert time in seconds only

time_seconds[k] = 3600*(parseInt(time[k].substring(0,2))) + 60*(parseInt(time[k].substring(3,5))) + parseFloat(time[k].substring(6,12));
 if(date[k] === "2016-01-04"){
 	
 	// Extract all students using ASQ in that time interval

 	if(time_seconds[k] <= (time_seconds[k] + 10.000)){
 		if (i === 0){
 		students[i] = users[k];
 		i++;	
 		}
 		if(i > 0){
 			for(var m = 0; m < students.length; m++){
 				if(users[k] === students[m]){
                  break;
 				}
 				else
 					if(m === (students.length-1)){
                     students[i] = users[k];
                     i++;
                     break;
 					}
 			}
 		}

 	}
  }
}

/*function ExtractAllstudents()
{
// Written Above
}*/
 
 // Converts the events to the six indicators

  function EventstoIndicators()
  {
    for(var l = 0; l < time_raw.length; l++){

    	// To check for connected user

    	if(events[l] === "folo-connected"){
    		connected[index]++;
    		break;
    	}
    	if(events[l] === "folo-disconnected"){
    		connected[index] = 0;
    		break;
    	}
    	
    	// To check if a user is in an exercise

    	if(events[l] === "exercise-activated"){
            exercise[index]++;
            break;
        }
        if(events[l] === "exercise-deactivated"){
        	exercise[index] = 0;
        	break;
        }

        // To check for user focus

        if(events[l] === "windowfocus" || events[l] === "exercisefocus"){
            focus[index]++;
            break;
        }
        /*if(events[l] === "windowblur" || events[l] === "exerciseblur"){
        	focus[index] = 0;
        	break;
        }*/

        // To check for user input

        if(events[l] === "input" || events[l] === "questioninput"){
        	input[index]++;
        	break;
        }

        // To check for user submission

        if(events[l] === "exercise-submit"){
        	submitted[index]++;
        	break;
        }

        // To check for idle user

        if(events[l] === "viewer-idle"){
        	idle[index]++;
        	break;
        }
        if(events[l] === "tabhidden" || events[l] === "tabvisible" || events[l] === "windowfocus" || events[l] === "windowblur" || events[l] === "focusin" || events[l] === "focusout" || events[l] === "exercisefocus" || events[l] === "exerciseblur" || events[l] === "input" || events[l] === "questioninput" || events[l] === "exercise-submit" || events[l] === "answer-submitted"){
        	idle[index] = 0;
        	break;
        }
    }
  }
  
  // Converts the binary state of six indicators to decimal values which indicate different annotated states

  function IndicatorstoNumbers()
  {
  }


  students.forEach(function(item) {
    console.log(students.indexOf(item));
});
