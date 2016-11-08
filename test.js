// Declare the arrays globally

// Variables to store the event types, raw time (with date), processed date and time and time in seconds

var events = [];
var time_raw = [];
var users = [];
var date = [];
var time = [];
var time_seconds = [];

// These variables store the binary state of each user as a string for a certain time interval

var exercise_check = "0";
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

var student_counter = 0;
var p = 0;
var date_index = [];
var counter = 0;
var student_index = 0;
var annotation_index = 0;
var annotated = [];
var one_counter = 0;
var two_counter = 0;
var three_counter = 0;
var z = 0;

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
  
  // Extract the dates to verify

  if(date[k] === "2016-01-04"){
   date_index[p] = k;
   p++;
  }
}
//console.log(users[358871]);
//console.log(date_index);

  for(z = date_index[0]; z < (date_index[0] + date_index.length); z = counter){
   
   // Extract all students using ASQ in that time interval

   student_counter = 0;
   counter = z;
   //counter = 360072;


   while(time_seconds[counter] <= (time_seconds[z] + 10.000)){

      if (student_counter === 0){
      students[student_counter] = users[counter];
      student_counter++;  
      }
      if(student_counter > 0){
         for(var m = 0; m < students.length; m++){
            if(users[counter] !== students[m]){
               if(m === students.length-1){
                     students[student_counter] = users[counter];
                     student_counter++;
                  }
                     continue;
            }
            else{
               break;
            }    
         }
      }

counter++;
   }

   //console.log(students);
   //break;
   
   number = [];
   category = [];
   students.forEach(function(item) {
    EventstoIndicators(students.indexOf(item), z, counter);
});

   IndicatorstoNumbers();

   //console.log(number);
   //console.log(submitted);
   Categories();
   Annotation(annotation_index);
   annotation_index++;
   
   //students = [];
   

   /*exercise = [];
   connected = [];
   focus = [];
   idle = [];
   input = [];
   submitted = [];*/

}

}

 // Converts the events to the six indicators

  function EventstoIndicators(index, start, end)
  {
   
   // Initialize the state of a new user to avoid undefined problem 

   if(idle[index] === undefined){
    idle[index] = "0";
   }
   if(focus[index] === undefined){
    focus[index] = "0";
   }
   if(input[index] === undefined){
    input[index] = "0";
   }
   if(submitted[index] === undefined){
    submitted[index] = "0";
   }
   if(connected[index] === undefined){
    connected[index] = "0";
   }
   if(exercise[index] === undefined){
    exercise[index] = "0";
   }
    
    for(var l = start; l <= end; l++){

      // To check if an exercise is activated

      if(events[l] === "exercise-activated"){
            exercise_check = "1";
        }
      if(events[l] === "exercise-deactivated"){
            exercise_check = "0";
        }
      
      if(users[l] === students[index]){

         // To check if a user is in exercise

         if(exercise_check === "1"){
            exercise[index] = "1";
         }

         // To check for idle user

        if(events[l] === "viewer-idle"){
         idle[index] = "1";
         //continue;
        }
        if(events[l] === "tabhidden" || events[l] === "tabvisible" || events[l] === "windowfocus" || events[l] === "windowblur" || events[l] === "focusin" || events[l] === "focusout" || events[l] === "exercisefocus" || events[l] === "exerciseblur" || events[l] === "input" || events[l] === "questioninput" || events[l] === "exercise-submit" || events[l] === "answer-submitted"){
         idle[index] = "0";
        }

      // To check for connected user

      if(events[l] === "folo-connected"){
         connected[index] = "1";
         //continue;
      } 
      if(events[l] === "folo-disconnected"){
         connected[index] = "0";
         //continue;
      }

        // To check for user focus

        if(events[l] === "windowfocus" || events[l] === "exercisefocus"){
            focus[index] = "1";
            //continue;
        }

        if(events[l] === "windowblur" || events[l] === "exerciseblur"){
         focus[index] = "0";
         //continue;
        }

        // To check for user input

        if(events[l] === "input" || events[l] === "questioninput"){
         input[index] = "1";
         //continue;
        }

        // To check for user submission

        if(events[l] === "exercise-submit"){
         submitted[index] = "1";
         //continue;
        }
      }
    }
  }
  
  // Converts the binary state of six indicators to decimal values

  function IndicatorstoNumbers()
  {
   for(var x = 0; x < students.length; x++){
      number[x] = parseInt((submitted[x]+input[x]+idle[x]+focus[x]+connected[x]+exercise[x]),2);
      //console.log(submitted[x]+input[x]+idle[x]+focus[x]+connected[x]+exercise[x]);
      //console.log(submitted[x]);
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

  // Annotate with Red, Yellow or Green as R, Y or G

  function Annotation(s){
   one_counter = 0;
   two_counter = 0;
   three_counter = 0;
   for(var o = 0; o < students.length; o++){
      if(category[o] === 1){
         one_counter++;
      }
      if(category[o] === 2){
         two_counter++;
      }
      if(category[o] === 3){
         three_counter++;
      }
   }
   if((one_counter/students.length) >= 0.5){
      annotated[s] = "R";
   }
   if((one_counter/students.length) >= 0.4 && (one_counter/students.length) < 0.5){
      annotated[s] = "Y";
   }
   annotated[s] = "G";
  }


//console.log(annotated);

//Use of settimeout

/*setTimeout(function()
{
console.log(annotated);
}, 60000);*/

// Presenter ID is "564c6ecce69d92a4682a99fc"
