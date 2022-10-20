  const trials_per_block = 28;

  window.block_number = 1;
  window.detection_instruction_repetitions=0
  window.discrimination_instruction_repetitions=0


  const Cstar = 49;
  const lab_origin = [54,21.5,11.5];

  function polar2lab(Csatr,hstar, origin) {
    astar = Cstar*Math.cos(hstar)+origin[1];
    bstar = Cstar*Math.sin(hstar)+origin[2];
    return([origin[0],astar,bstar])
  }

  function polar2rgb(Cstar, hstar,origin) {
    return(lab2rgb(polar2lab(Cstar,hstar,origin)))
  }

  function makeTimeline() {

    timeline = [];

    const std = 0.35;
    const mu = window.colormapping==1? 1.85:2.1;
    const delta = window.colormapping==1? 0.25:-0.25;
    const boost = window.colormapping==1? 0.1:-0.1;

    // this plugin reproduces the colorwheel from Schurgin, M. W., Wixted,
    // J. T., & Brady, T. F. (2020). Nature human behaviour, 4(11), 1156-1172.

      // timeline.push({
      //   type: 'colorwheel'
      // });

      timeline.push({
        type: 'fullscreen',
        fullscreen_mode: true
      });

      var welcome = {
        type: "p5Text",
        title: 'Welcome!',
        text: "In this experiment, you will look for tiny differences in the average "+
        "color of flickering patches. You will also rate your confidence " +
        "in your decisions. The experiment consists of 6 parts + some instructions "+
        "and practice trials, and should take about 20 minutes to complete."
      };
      timeline.push(welcome);

      // var epilepsy = {
      //   type: "p5Text",
      //   title: 'Warning',
      //   text: "Flickering stimuli in this experiment may potentially trigger "+
      //   "seizures for people with photosensitive epilepsy. Viewer discretion is advised."
      // };
      // timeline.push(epilepsy);

      var keyFlashFunc = function(p, trial) {
        var keyCode=p.keyCode
        if (keyCode==49) {
          trial.flickering_frame = 0
          trial.hue = [[],[],[],[]];
          for (i=0; i<4;i++) {
            for (j=0; j<12; j++) {
              trial.hue[i][j] = p.randomGaussian(mu,std)
            }
            trial.hue[i][12] =128;
          }
        }
        if (keyCode==50) {
          trial.special_flickering_frame = 0
          trial.special_hue = [[],[],[],[]];
          for (i=0;i<4;i++) {
            for (j=0; j<12; j++) {
              trial.special_hue[i][j] = p.randomGaussian(mu+delta,std)
            }
            trial.special_hue[i][12] =128;
          }
        }
      }

      var drawFlashFunc = function(p, trial) {
        if (trial.flickering_frame > -1) {
            p.push()
            // draw left square
            p.strokeWeight(0)
            for (i=0;i<4;i++) {
              p.fill(polar2rgb(Cstar, trial.hue[i][trial.flickering_frame],lab_origin));
              p.rect(p.width/2+300+(14*i), 320, 14, 56);
            }
            p.pop()

            trial.flickering_frame++
            if (trial.flickering_frame>12) {
              trial.flickering_frame=undefined;
            }
        }
        if (trial.special_flickering_frame > -1) {
            p.push()
            // draw left square
            p.strokeWeight(0)
            for (i=0;i<4;i++) {
              p.fill(polar2rgb(Cstar,trial.special_hue[i][trial.special_flickering_frame],lab_origin));
              p.rect(p.width/2+300+(14*i), 320, 14, 56);
            }
            p.pop()
            trial.special_flickering_frame++
            if (trial.special_flickering_frame>12) {
              trial.special_flickering_frame=undefined;
            }
        }
      }

      var introduce_squares = {
        type: "p5Text",
        title: 'Flickering Patches',
        text: "Flickering patches are colorful bars that change"+
        " their color very rapidly (25 times a second)." +
        `\nThe average color of a flickering patch is orange-greenish.\n` +
        "Press 1 to see a flickering patch. (You can try this several times).",
        key_function: keyFlashFunc,
        draw_function: drawFlashFunc
      };
      timeline.push(introduce_squares)

      var introduce_special = {
        type: "p5Text",
        title: `Special Flickering Patches`,
        text: `Special flickering patches are similar to ordinary flickering patches, except that` +
        ` on average they are a tiny bit ${window.colormapping==1? 'more green' : 'more orange'}.\n` +
        "Press 2 to see a Special flickering patch and 1 to see an ordinary flickering patch.",
        key_function: keyFlashFunc,
        draw_function: drawFlashFunc
      };
      timeline.push(introduce_special);

      var introduce_det_1 = {
        type: "p5Text",
        title: "Your First Challenge",
        text: "In the first part of the experiment you will see pairs of " +
              `flickering patches side by side, on an orange background. On half of the trials, both ` +
              "patches will be ordinary. On the other half, exactly one of " +
              `the patches (but never both!) will be special (a little ${window.colormapping==1? 'more green' : 'more orange'} than the orange background). `

      };
      timeline.push(introduce_det_1);

      var introduce_det_2 = {
        type: "p5Text",
        title: "Your First Challenge",
        text:  "Your challenge is to tell whether one of the patches was special, " +
          "or whether both were ordinary.\n" +
          "Remember that the special patch can " +
          "appear on both sides (right or left), and that on " +
          "exactly half of the trials both patchs will be ordinary. "
      };
      timeline.push(introduce_det_2);

      var introduce_det_practice = {
        type: "p5Text",
        text: "Let's practice this new challenge. Press Y if you think one patch was " +
              "special and N if both were ordinary. We'll do 4 trials of this." +
              `The special patch will be extra ${window.colormapping==1? 'green' : 'orange'} for these first trials.`
      };
      timeline.push(introduce_det_practice);


      var det_practice_stimulus = {
        type: "flicker",
        choices: ['y','n'],
        detection: true,
        signal_presence: jsPsych.timelineVariable('signal_presence'),
        special_side: jsPsych.timelineVariable('special_side'),
        boost: jsPsych.timelineVariable('boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        std: jsPsych.timelineVariable('std'),
        mu: mu,
        delta: delta,
        on_finish: function(data){
          data.correct = data.response == data.correct_response;
        },
        data: jsPsych.timelineVariable('data')
      };

      var det_stimulus = {
        type: "flicker",
        choices: ['y','n'],
        detection: true,
        signal_presence: jsPsych.timelineVariable('signal_presence'),
        boost: jsPsych.timelineVariable('boost'),
        std: jsPsych.timelineVariable('std'),
        delta: delta,
        mu: mu,
        on_finish: function(data){
          data.correct = data.response == data.correct_response;
        },
        data: jsPsych.timelineVariable('data')
      };

      var det_practice_stimuli = [
        { special_side: "right", signal_presence: true, boost: 0,
          rate_confidence:false, std:std/2,
          data: { test_part: 'practice', correct_response: 'y' } },
        { special_side: "left", signal_presence: true, boost: 0,
          rate_confidence:false, std:std/2,
          data: { test_part: 'practice', correct_response: 'y' } },
        { special_side: "right", signal_presence: false, boost: 0,
          rate_confidence:false, std:std/2,
          data: { test_part: 'practice', correct_response: 'n' } },
        { special_side: "left", signal_presence: false, boost: 0,
          rate_confidence:false, std:std/2,
          data: { test_part: 'practice', correct_response: 'n' } }
      ];

      var feedback = {
        type: 'html-keyboard-response',
        on_start: function() {$('body').css('cursor', 'none')},
        on_finish: function() {$('body').css('cursor', 'default')},
        stimulus: function(){
          var last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
          if(last_trial_correct){
            return "<div style=\"width:150px;height:56px;font-size:30px;text-align:center;\">correct!</div>";
          } else {
            return "<div style=\"width:150px;height:56px;border:0px;font-size:30px;text-align:center\">wrong.</div>"
          }
        },
        trial_duration: function(){
          var last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
          if(last_trial_correct){
            return 1000;
          } else {
            return 1500
          }
        },
        response_ends_trial: false
      }

      var det_practice_procedure = {
        timeline: [det_practice_stimulus, feedback],
        timeline_variables: det_practice_stimuli,
        randomize_order: true
      }

      var det_practice_feedback = {
        type: "p5Text",
        text: function() {

          var trials = jsPsych.data.get().last(8).filter({test_part: 'practice'});
          var num_correct = trials.filter({correct: true}).count();

          if (num_correct>2) {
          return "Excellent, you responded correctly on "+num_correct+" out "+
          "of 4 trials. There's one more bit of explanation before we can move on to the "+
          "actual experiment."
          } else {
            return "This time you responded correctly on "+num_correct+" out "+
          "of 4 trials. Let's do 4 more practice trials. Press Y if you think " +
          `one of the patches was special (a little ${window.colormapping==1? 'more green' : 'more orange'}) and N if both were ordinary.`
          }
        }
      }

      var det_practice_loop_node = {
          timeline: [det_practice_procedure,det_practice_feedback],
          loop_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              var trials = data.filter({test_part: 'practice'});
              var num_correct = trials.filter({correct: true}).count();
              if(num_correct>2){
                  return false;
              } else {
                  return true;
              }
          }
      }
      timeline.push(det_practice_loop_node);

      var lets_practice_confidence = {
        type: "p5Text",
        title: 'Confidence Ratings',
        text: "Immediately after you decide whether one of the patches was special " +
        "you will indicate your confidence in your decision. You will do " +
        "this by controlling the size of a circle with your mouse: "+
        "a large circle means you are certain that you got it right, and a small "+
        "circle means there's a good chance you might be wrong.\n"+
        "Let's practice this before we start the actual experiment.\n"+
        "This time things will be harder!"
      };
      timeline.push(lets_practice_confidence)

      var conf_practice_stimuli = [
        { special_side: "right", signal_presence: true, boost: 0,
          rate_confidence:true, delta: delta, std:std,
          data: { test_part: 'practice', correct_response: 'y' } },
        { special_side: "left", signal_presence: true, boost: 0,
          rate_confidence:true, delta: delta,std:std,
          data: { test_part: 'practice', correct_response: 'y' } },
        { special_side: "right", signal_presence: false, boost: 0,
          rate_confidence:true, delta: delta,std:std,
          data: { test_part: 'practice', correct_response: 'n' } },
        { special_side: "left", signal_presence: false, boost: 0,
          rate_confidence:true, delta: delta,std:std,
          data: { test_part: 'practice', correct_response: 'n' } }
      ];

      var conf_practice_procedure = {
        timeline: [det_practice_stimulus, feedback],
        timeline_variables: conf_practice_stimuli,
        repetitions: 1,
        randomize_order: true,
      }
      timeline.push(conf_practice_procedure)

      var multichoice_detection = {
        type: 'survey-multi-choice',
        questions: [{
        prompt: "If I pressed N and made the circle " +
        "very large, it means that I am:",
        options: ['1. very confident that the special patch was on the right',
                  '2. very confident that both patches were ordinary.',
                  '3. guessing that both patches were ordinary.',
                  '4. very confident that one of the two patches (left or right) was special.',
                  '5. guessing that one fo the two patches was special.'],
        name: 'multichoice_detection'
      }],
      on_finish: function(data){
          window.followed_detection = JSON.parse(data.responses)['multichoice_detection'][0][0]==2;
          window.detection_instruction_repetitions++
      }
      }
      timeline.push(multichoice_detection);

      var explain_detection_again = {
        type: "p5Text",
        title: "Let's try again",
        text: `Indicate whether exactly one of the flickering patches was special (a little ${window.colormapping==1? 'more green' : 'more orange'}; Y key) `+
        "or whether both were ordinary (N key), and " +
        "rate your confidence by controlling the size of the circle.",
        draw_function: function(p, trial) {
          p.push()
          p.fill(150);
          p.stroke(255);
          p.strokeWeight(2);
          p.rect(p.width/2+365, p.height/2, 70, 70);
          p.rect(p.width/2+365, p.height/2+70, 70, 70);
          p.fill(255)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(150)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('Y',p.width/2+400, p.height/2+35)
          p.text('N',p.width/2+400, p.height/2+105)
          p.textSize(15);
          p.fill(0)
          p.text('One special',p.width/2+400, p.height/2-15)
          p.text('Both ordinary',p.width/2+400, p.height/2+155)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      };

      var multichoice_detection_loop_node = {
          timeline: [explain_detection_again,multichoice_detection],
          loop_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              return(!window.followed_detection & window.detection_instruction_repetitions<3)
          },
          conditional_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              return(!window.followed_detection & window.detection_instruction_repetitions<3)
          }
      }
      timeline.push(multichoice_detection_loop_node);


      var starting_experiment = {
        type: "p5Text",
        title: 'Part 1 out of 6',
        text: "We are now ready to start the first part of the experiment."
      };
      timeline.push(starting_experiment)

      var start_det = {
        type: "p5Text",
        title: function() {return 'Part '+window.block_number+ ' out of 6'},
        text: `Indicate whether exactly one of the flickering patches was special (a little ${window.colormapping==1? 'more green' : 'more orange'}; Y key) `+
        "or whether both were ordinary (N key), and " +
        "rate your confidence by controlling the size of the circle. This part "+
        "has "+trials_per_block+" repetitions and should take about three and a half minutes to complete.",
        draw_function: function(p, trial) {
          p.push()
          p.fill(150);
          p.stroke(255);
          p.strokeWeight(2);
          p.rect(p.width/2+365, p.height/2, 70, 70);
          p.rect(p.width/2+365, p.height/2+70, 70, 70);
          p.fill(255)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(150)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('Y',p.width/2+400, p.height/2+35)
          p.text('N',p.width/2+400, p.height/2+105)
          p.textSize(15);
          p.fill(0)
          p.text('One special',p.width/2+400, p.height/2-15)
          p.text('Both ordinary',p.width/2+400, p.height/2+155)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      }
      timeline.push(start_det);

      var det_test_stimuli = [
        { signal_presence: true, boost: 0,std:std,
          data: { test_part: 'test', correct_response: 'y' } },
        { signal_presence: false, boost: 0,std:std,
          data: { test_part: 'test', correct_response: 'n' } },
        { signal_presence: true, boost: boost,std:std,
          data: { test_part: 'test', correct_response: 'y' } },
        { signal_presence: false, boost: boost,std:std,
          data: { test_part: 'test', correct_response: 'n' } }
      ];

      var det_test_procedure = {
        timeline: [det_stimulus, feedback],
        timeline_variables: det_test_stimuli,
        repetitions: trials_per_block/4,
        randomize_order: true
        }

      var update_block_number = {
        type: 'call-function',
        func: function() {
        window.block_number++
        console.log(window.block_number)
        }
      }
      timeline.push(det_test_procedure,update_block_number);

      var take_a_break = {
        type: "p5Text",
        text: "Great job. You can now take a short break. Press Space when you are ready " +
              "to continue.",
        footer: ''
      };

      var debrief_block = {
        type: "p5Text",
        text: function() {

          // take trials_per_block*4 because feedback is also counted
          var trials = jsPsych.data.get().last(trials_per_block*2).filter({test_part: 'test'});
          var correct_trials = trials.filter({correct: true});
          var incorrect_trials = trials.filter({correct: false});
          var accuracy = Math.round(correct_trials.count() / trials.count() * 100);
          var confidenceCorrect = Math.round(correct_trials.select('confidence').mean()*100);
          var confidenceIncorrect = Math.round(incorrect_trials.select('confidence').mean()*100);


          return `You responded correctly on ${accuracy}% of the trials.
Your average confidence level was ${confidenceCorrect}/100 when you were correct and ${confidenceIncorrect}/100 when you were wrong.
You can now take a short break before we continue.`
        }
      };
      timeline.push(debrief_block);

      var introduce_discrimination = {
        type: "p5Text",
        title: 'Your Second Challenge',
        text: "In the first part of the experiment, you observed pairs of " +
        "flickering patches and decided whether one of them was special. This time there will always be one "+
        `patch that is special (a little ${window.colormapping==1? 'more green' : 'more orange'}) and one that is ordinary. ` +
        "Your task is to pay close attention and tell which is which."
      };
      timeline.push(introduce_discrimination)

      var lets_practice_discrimination = {
        type: "p5Text",
        title: 'Your Second Challenge',
        text: "Lets start with 4 practice trials. \n" +
        "Press F if the special patch was on the right and D if it was " +
        "on the left. Then, rate your confidence using the circle as before. "+
        `The special patches will be extra-${window.colormapping==1? 'green' : 'orange'} for these first `+
        "practice trials."
      };
      timeline.push(lets_practice_discrimination)

      var dis_practice_stimulus = {
        type: "flicker",
        choices: ['d','f'],
        special_side: jsPsych.timelineVariable('special_side'),
        std: std/3,
        delta: delta,
        mu: mu,
        boost: jsPsych.timelineVariable('boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        give_confidence_feedback:  jsPsych.timelineVariable('give_confidence_feedback'),
        on_finish: function(data){
          data.correct = data.response == data.key_mapping[data.special_side];
        },
        data: jsPsych.timelineVariable('data')
      };

      var dis_stimulus = {
        type: "flicker",
        choices: ['d','f'],
        std: std,
        delta: delta,
        mu: mu,
        boost: jsPsych.timelineVariable('boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        give_confidence_feedback:  jsPsych.timelineVariable('give_confidence_feedback'),
        on_finish: function(data){
          data.correct = data.response == data.key_mapping[data.special_side];
        },
        data: jsPsych.timelineVariable('data')
      };


      var dis_practice_stimuli = [
        { special_side: "right",
          boost: 0,
          data: { test_part: 'practice', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } },
        { special_side: "left",
          oost: 0,
          data: { test_part: 'practice', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } }
      ];

      var dis_practice_procedure = {
        timeline: [dis_practice_stimulus, feedback],
        timeline_variables: dis_practice_stimuli,
        repetitions: 2,
        randomize_order: true
      }

      var dis_practice_feedback = {
        type: "p5Text",
        text: function() {

          var trials = jsPsych.data.get().last(8).filter({test_part: 'practice'});
          var num_correct = trials.filter({correct: true}).count();

          if (num_correct>2) {
          return "Excellent, you responded correctly on "+num_correct+" out "+
          "of 4 trials."
          } else {
            return "This time you responded correctly on "+num_correct+" out "+
          "of 4 trials. Let's do 4 more practice trials. Press D if the special " +
          `patch (the one that is a little ${window.colormapping==1? 'more green' : 'more orange'}) was on the right and F if it was on the left.`
          }
        }
      }

      var dis_practice_loop_node = {
          timeline: [dis_practice_procedure,dis_practice_feedback],
          loop_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              var trials = data.filter({test_part: 'practice'});
              var num_correct = trials.filter({correct: true}).count();
              if(num_correct>2){
                  return false;
              } else {
                  return true;
              }
          }
      }
      timeline.push(dis_practice_loop_node)

      var multichoice_discrimination = {
        type: 'survey-multi-choice',
        questions: [{
        prompt: "If I pressed D and made the circle very large, it means that I am:",
        options: ['1. very confident that the special patch was on the left.',
                  '2. guessing that the special patch was on the left.',
                  '3. very confident that the special patch was on the right.',
                  '4. guessing that the special patch was on the right.'],
        name: 'multichoice_discrimination'
      }],
      on_finish: function(data){
          window.followed_discrimination = JSON.parse(data.responses)['multichoice_discrimination'][0][0]==1;
          window.discrimination_instruction_repetitions++
      }
      }
      timeline.push(multichoice_discrimination)

      var explain_discrimination_again = {
        type: "p5Text",
        title: "Let's try again",
        text: `Indicate which flickering patch was special (a little ${window.colormapping==1? 'more green' : 'more orange'} `+
        "using your keyboard (D for left and F for right), and " +
        "rate your confidence by controlling the size of the circle.",
        draw_function: function(p, trial) {
          p.push()
          p.fill(150);
          p.stroke(255);
          p.strokeWeight(2);
          p.rect(p.width/2+330, p.height/2+50, 70, 70);
          p.rect(p.width/2+400, p.height/2+50, 70, 70);
          p.fill(255)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(150)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('F',p.width/2+435, p.height/2+90)
          p.text('D',p.width/2+365, p.height/2+90)
          p.textSize(15);
          p.fill(0)
          p.text('Special on right',p.width/2+470, p.height/2+30)
          p.text('Special on left',p.width/2+330, p.height/2+30)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      };

      var multichoice_discrimination_loop_node = {
          timeline: [explain_discrimination_again,multichoice_discrimination],
          loop_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              return(!window.followed_discrimination & window.discrimination_instruction_repetitions<3)
          },
          conditional_function: function(data){
              // get the data from the previous trial,
              // and check which key was pressed
              return(!window.followed_discrimination & window.discrimination_instruction_repetitions<3)
          }
      }
      timeline.push(multichoice_discrimination_loop_node);

      // DISCRIMINATION

      var start_dis = {
        type: "p5Text",
        title: function() {return 'Part '+window.block_number+ ' out of 6'},
        text: `Indicate which flickering patch was special (a little ${window.colormapping==1? 'more green' : 'more orange'} `+
        "using your keyboard (D for left and F for right), and " +
        "rate your confidence by controlling the size of the circle. This part "+
        "has "+trials_per_block+" repetitions and should take about three and a half minutes to complete.",
        draw_function: function(p, trial) {
          p.push()
          p.fill(150);
          p.stroke(255);
          p.strokeWeight(2);
          p.rect(p.width/2+330, p.height/2+50, 70, 70);
          p.rect(p.width/2+400, p.height/2+50, 70, 70);
          p.fill(255)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(150)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('F',p.width/2+435, p.height/2+90)
          p.text('D',p.width/2+365, p.height/2+90)
          p.textSize(15);
          p.fill(0)
          p.text('Special on right',p.width/2+470, p.height/2+30)
          p.text('Special on left',p.width/2+330, p.height/2+30)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      };
      timeline.push(start_dis);

      var dis_test_stimuli = [
        { delta: delta, rate_confidence: true, boost: 0,
          data: { test_part: 'test', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } },
        { delta: delta, rate_confidence: true, boost: boost,
          data: { test_part: 'test', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } }
      ];


      var dis_test_procedure = {
        timeline: [dis_stimulus, feedback],
        timeline_variables: dis_test_stimuli,
        repetitions: trials_per_block/2,
        randomize_order: true
        }

      timeline.push(dis_test_procedure, update_block_number);
      timeline.push(debrief_block);


      var low_performance = {
        type: "p5Text",
        text: "Your performance did not reach our high bar, so we will not " +
              "continue with the rest of the experiment. You will be paid for "+
              "your time!"
      };

      low_performance_if_node = {
        timeline: [low_performance],
        conditional_function: function() {
          var all_trials = jsPsych.data.get().last(trials_per_block*4).filter({test_part: 'test'})
          var detection_trials = all_trials.first(trials_per_block);
          var discrimination_trials = all_trials.last(trials_per_block);
          var detection_acc = detection_trials.filter({correct:true}).count()/trials_per_block;
          var discrimination_acc = discrimination_trials.filter({correct:true}).count()/trials_per_block;
          if (detection_acc<=0.55 || discrimination_acc<=0.55 || !window.followed_detection || !window.followed_discrimination) {
            return true
            window.message = 'FAILED'
          } else {
            return false
            window.message = 'PASSED'
          }
        }
      }
      timeline.push(low_performance_if_node)

      main_loop = {
        timeline: [start_det, det_test_procedure, update_block_number, debrief_block,
                  start_dis, dis_test_procedure, update_block_number, debrief_block,
                  start_det, det_test_procedure, update_block_number,debrief_block,
                  start_det, det_test_procedure, update_block_number, debrief_block],
        repetitions:  1
       }

      main_if_node = {
        timeline: [main_loop],
        conditional_function: function() {
          var all_trials = jsPsych.data.get().last(trials_per_block*4).filter({test_part: 'test'})
          var detection_trials = all_trials.last(trials_per_block);
          var discrimination_trials = all_trials.first(trials_per_block);
          var detection_acc = detection_trials.filter({correct:true}).count()/trials_per_block;
          var discrimination_acc = discrimination_trials.filter({correct:true}).count()/trials_per_block;
          if ((detection_acc>0.55 && discrimination_acc>0.55) && window.followed_detection && window.followed_discrimination) {
            return true
          } else {
            return false
          }
        }
      }
      timeline.push(main_if_node)



      var worker_comments = {
        type: 'survey-text',
        preamble: '<h1>Your Thoughts<h1>',
        questions: [{
        prompt: "Before we thank you, we would appreciate if you could share " +
        "any thoughts you had about the experiment, or anything we should "+
        "take into account when analyzing your data.",
        pleaceholder: "your comments here",
        rows:8,
        columns:60,
        name: 'worker_comments'
      }]
      }
      timeline.push(worker_comments)

      var thank_you = {
        type: 'p5Text',
        title: "Thank you!",
        text: "Thanks for your time and effort. Your contribution will help us " +
              "learn about the way people make and form confidence " +
              "in their decisions."
      };

      timeline.push(thank_you)
      return timeline
    }

    function hexToBytes(hex) {
        for (var bytes = [], c = 0; c < hex.length; c += 2)
        bytes.push(parseInt(hex.substr(c, 2), 16));
        return bytes;
    }

  /* start the experiment */
  jatos.onLoad(function () {
    console.log('version 3');
    window.participant_number = jatos.workerId;
    console.log(window.participant_number);
    // jatos.batchSession.set('participant_number',window.participant_number+1);
    window.colormapping = window.participant_number%2==0? 1: 2;
    console.log(window.colormapping)
    var m = new MersenneTwister();
    Math.random = function() {return m.random()};
    var protocol_sum =  jatos.batchSession.get("protocol_sum")
    console.log(protocol_sum)
    subject_sum = hexToBytes(
      CryptoJS.SHA256(
        protocol_sum+window.participant_number).toString()
      )
    console.log(subject_sum)
    m.init_by_array(subject_sum, subject_sum.length)
    jsPsych.data.addProperties({'protocol_sum':protocol_sum,
    'participant_number':participant_number, 'subject_sum':subject_sum});
    timeline=makeTimeline()
      jsPsych.init({
          timeline: timeline,
          on_finish: function() {
              // jsPsych.data.addProperties(jatos.urlQueryParameters);
              jsPsych.data.addProperties({colormapping: window.window.colormapping,
                                          participant_number:window.participant_number,
                                        followed_detection: window.followed_detection,
                                      followed_discrimination: window.followed_discrimination,
                                    detection_instruction_repetitions: window.detection_instruction_repetitions,
                                  discrimination_instruction_repetitions:window.discrimination_instruction_repetitions});
              var resultJson = jsPsych.data.get().json();
              jatos.submitResultData(resultJson, jatos.startNextComponent);
          }
      });
  });
