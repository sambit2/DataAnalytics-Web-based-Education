// Declare the arrays globally

// Variables to store the event types, date and time, processed date and time and time in seconds

var events = [];
var time_raw = [];
var users = [];
var date = [];
var time = [];
var time_seconds = [];

// These variables store the binary state of each user as a string for a certain time interval

var exercise = [];
var connected = [];
var focus = [];
var idle = [];
var input = [];
var submitted = [];

// Variables store all the students using ASQ in a time duration, the number state of each student also their category

var students = [];
var number = [];
var category = [];
var y = 0;

$.getJSON('sessionevents.json', function(info){

	// Collect the events, raw time (i.e. time with date) and the user ids

   		   	for(var i = 0; i < info.length; i++){
   		   		events[i] = info[i].type;
   		   		time_raw[i] = info[i].time.$date;

   		   		// The user id can be obtained in data as user, or object or in data as an answeree

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
             process();
          });

function process()
{
 for(var j = 0; j < time_raw.length; j++){
   date[j] = time_raw[j].substring(0,10);
   time[j] = time_raw[j].substring(11,23);
 }
 for(var k = 0; k < time_raw.length; k++){

  // Convert time in seconds only

  time_seconds[k] = 3600*(parseInt(time[k].substring(0,2))) + 60*(parseInt(time[k].substring(3,5))) + parseFloat(time[k].substring(6,12));
  // fix the index for time duration
  if(date[k] === "2016-01-04"){
   
   // Extract all students using ASQ in that time interval

   if(time_seconds[k] <= (time_seconds[k] + 10.000)){
      if (y === 0){
      students[y] = users[k];
      y++;  
      }
      if(y > 0){
         for(var m = 0; m < students.length; m++){
            if(users[k] === students[m]){
                  continue;
            }
            else
               if(m === (students.length-1)){
                     students[y] = users[k];
                     y++;
                     continue;
               }
         }
      }

   }
}
}

}

 // Converts the events to the six indicators

  function EventstoIndicators()
  {
    for(var l = 0; l < time_raw.length; l++){

      // To check for idle user

        if(events[l] === "viewer-idle"){
         idle[index] = "1";
         continue;
        }
        if(events[l] === "tabhidden" || events[l] === "tabvisible" || events[l] === "windowfocus" || events[l] === "windowblur" || events[l] === "focusin" || events[l] === "focusout" || events[l] === "exercisefocus" || events[l] === "exerciseblur" || events[l] === "input" || events[l] === "questioninput" || events[l] === "exercise-submit" || events[l] === "answer-submitted"){
         idle[index] = "0";
        }

      // To check for connected user

      if(events[l] === "folo-connected"){
         connected[index] = "1";
         continue;
      } // fix the index value
      if(events[l] === "folo-disconnected"){
         connected[index] = "0";
         continue;
      }
      
      // To check if a user is in an exercise

      if(events[l] === "exercise-activated"){
            exercise[index] = "1";
            continue;
        }
        if(events[l] === "exercise-deactivated"){
         exercise[index] = "0";
         continue;
        }

        // To check for user focus

        if(events[l] === "windowfocus" || events[l] === "exercisefocus"){
            focus[index] = "1";
            continue;
        }
        /*if(events[l] === "windowblur" || events[l] === "exerciseblur"){
         focus[index] = 0;
         continue;
        }*/

        // To check for user input

        if(events[l] === "input" || events[l] === "questioninput"){
         input[index] = "1";
         continue;
        }

        // To check for user submission

        if(events[l] === "exercise-submit"){
         submitted[index] = "1";
         continue;
        }
    }
  }
  
  // Converts the binary state of six indicators to decimal values

  function IndicatorstoNumbers()
  {
   for(var x = 0; x < students.length; x++){
      number[x] = parseInt((submitted[x]+input[x]+idle[x]+focus[x]+connected[x]+exercise[x]),2);
   }
  }

  // Assign students to different categories

  function Categories()
  {
   for(var y = 0; y < students.length; y++){
      
      // 1 for in-attentive, 2 for semi-attentive and 3 for attentive categories

      if(number[y] === 0 || number[y] === 1 || number[y] === 33 || number[y] === 2 || number[y] === 10 || number[y] === 35 || number[y] === 43){
         category[y] = 1;
         continue;
      }
      if(number[y] === 11 || number[y] === 14 || number[y] === 15 || number[y] === 47){
         category[y] = 2;
         continue;
      }
      if(number[y] === 3 || number[y] === 6 || number[y] === 7 || number[y] === 39 || number[y] === 23 || number[y] === 55){
         category[y] = 3;
         continue;
      }
   }

  }



students.forEach(function(item) {
    console.log(students.indexOf(item));
});

/*setTimeout(function()
{
for(var j = 0; j < time_raw.length; j++){
   date[j] = time_raw[j].substring(0,10);
   time[j] = time_raw[j].substring(11,23);
}
}, 1000);*/
/*for(var k = 0; k < time_raw.length; k++){
   // Convert time in seconds only

 if (events[k] === "exercise-activated"){
   index = events.indexOf("exercise-activated");
  }
}*/


// Yet to Complete
