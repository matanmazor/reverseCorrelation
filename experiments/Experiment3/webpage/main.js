  const std = 10;
  const delta = 5;
  const trials_per_block = 28;
  var followed_instructions = true;
  window.block_number = 1;


  function makeTimeline() {

    timeline = [];


      timeline.push({
        type: 'fullscreen',
        fullscreen_mode: true
      });

      var welcome = {
        type: "p5Text",
        title: 'Welcome!',
        text: "In this experiment, you will look for tiny differences in the average "+
        "brightness of flickering patches. You will also rate your confidence " +
        "in your decisions. The experiment consists of 6 parts + some instructions "+
        "and practice trials, and should take about 20 minutes to complete."
      };
      timeline.push(welcome);

      var epilepsy = {
        type: "p5Text",
        title: 'Warning',
        text: "Flickering stimuli in this experiment may potentially trigger "+
        "seizures for people with photosensitive epilepsy. Viewer discretion is advised."
      };
      timeline.push(epilepsy);

      var keyFlashFunc = function(p, trial) {
        var keyCode=p.keyCode
        if (keyCode==49) {
          trial.flickering_frame = 0
          trial.luminance = [[],[],[],[]];
          for (i=0; i<4;i++) {
            for (j=0; j<12; j++) {
              trial.luminance[i][j] = p.round(p.randomGaussian(128,std))
            }
            trial.luminance[i][12] =128;
          }
        }
        if (keyCode==50) {
          trial.bright_flickering_frame = 0
          trial.bright_luminance = [[],[],[],[]];
          for (i=0;i<4;i++) {
            for (j=0; j<12; j++) {
              trial.bright_luminance[i][j] = p.round(p.randomGaussian(128+delta,std))
            }
            trial.bright_luminance[i][12] =128;
          }
        }
      }

      var drawFlashFunc = function(p, trial) {
        if (trial.flickering_frame > -1) {
            p.push()
            // draw left square
            p.strokeWeight(0)
            for (i=0;i<4;i++) {
              p.fill(trial.luminance[i][trial.flickering_frame]);
              p.rect(p.width/2+280+(14*i), 320, 14, 56);
            }
            p.pop()

            trial.flickering_frame++
            if (trial.flickering_frame>12) {
              trial.flickering_frame=undefined;
            }
        }
        if (trial.bright_flickering_frame > -1) {
            p.push()
            // draw left square
            p.strokeWeight(0)
            for (i=0;i<4;i++) {
              p.fill(trial.bright_luminance[i][trial.bright_flickering_frame]);
              p.rect(p.width/2+280+(14*i), 320, 14, 56);
            }
            p.pop()
            trial.bright_flickering_frame++
            if (trial.bright_flickering_frame>12) {
              trial.bright_flickering_frame=undefined;
            }
        }
      }

      var introduce_squares = {
        type: "p5Text",
        title: 'Flickering Patches',
        text: "Flickering patches are gray bars that change"+
        " brightness very rapidly (25 times a second)." +
        "\nThe average brightness of a flickering patch is dark gray, just like the background.\n" +
        "Press 1 to see a flickering patch. (You can try this several times).",
        key_function: keyFlashFunc,
        draw_function: drawFlashFunc
      };
      timeline.push(introduce_squares)

      var introduce_bright = {
        type: "p5Text",
        title: 'Bright Flickering Patches',
        text: "Bright flickering patches are similar to ordinary flickering patches, except that" +
        " on average they are a tiny bit brighter compared to the background.\n" +
        "Press 2 to see a Bright flickering patch and 1 to see an ordinary flickering patch.",
        key_function: keyFlashFunc,
        draw_function: drawFlashFunc
      };
      timeline.push(introduce_bright);

      var introduce_det_1 = {
        type: "p5Text",
        title: "Your First Challenge",
        text: "In the first part of the experiment you will see pairs of " +
              "flickering patches side by side. On half of the trials, both " +
              "patches will be ordinary. On the other half, exactly one of " +
              "the patches (but never both!) will be bright. "

      };
      timeline.push(introduce_det_1);

      var introduce_det_2 = {
        type: "p5Text",
        title: "Your First Challenge",
        text:  "Your challenge is to tell whether one of the patches was bright, " +
          "or whether both were ordinary.\n" +
          "Remember that the bright patch can " +
          "appear on both sides (right or left), and that on " +
          "exactly half of the trials both patchs will be ordinary. "
      };
      timeline.push(introduce_det_2);

      var introduce_det_practice = {
        type: "p5Text",
        text: "Let's practice this new challenge. Press Y if you think one patch was " +
              "bright and N if both were ordinary. We'll do 4 trials of this." +
              "The bright patch will be extra bright for these first trials, "+
              "but don't get used to it - things will soon become harder!"
      };
      timeline.push(introduce_det_practice);


      var det_practice_stimulus = {
        type: "flicker",
        choices: ['y','n'],
        detection: true,
        signal_presence: jsPsych.timelineVariable('signal_presence'),
        bright_side: jsPsych.timelineVariable('bright_side'),
        brightness_boost: jsPsych.timelineVariable('brightness_boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        std: std,
        delta: jsPsych.timelineVariable('delta'),
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
        brightness_boost: jsPsych.timelineVariable('brightness_boost'),
        std: std,
        delta: delta,
        on_finish: function(data){
          data.correct = data.response == data.correct_response;
        },
        data: jsPsych.timelineVariable('data')
      };

      var det_practice_stimuli = [
        { bright_side: "right", signal_presence: true, brightness_boost: 0,
          rate_confidence:false, delta: delta*3,
          data: { test_part: 'practice', correct_response: 'y' } },
        { bright_side: "left", signal_presence: true, brightness_boost: 0,
          rate_confidence:false, delta: delta*3,
          data: { test_part: 'practice', correct_response: 'y' } },
        { bright_side: "right", signal_presence: false, brightness_boost: 0,
          rate_confidence:false, delta: delta*3,
          data: { test_part: 'practice', correct_response: 'n' } },
        { bright_side: "left", signal_presence: false, brightness_boost: 0,
          rate_confidence:false, delta: delta*3,
          data: { test_part: 'practice', correct_response: 'n' } }
      ];

      var feedback = {
        type: 'html-keyboard-response',
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
          "one of the patches was bright and N if both were ordinary."
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
        text: "Immediately after you decide whether one of the patches was bright " +
        "you will indicate your confidence in your decision. You will do " +
        "this by controlling the size of a colored circle with your mouse: "+
        "a large circle means you are certain that you got it right, and a small "+
        "circle means there's a good chance you might be wrong.\n"+
        "Let's practice this before we start the actual experiment.\n"+
        "Get ready, because this time things will be more difficult"
      };
      timeline.push(lets_practice_confidence)

      var conf_practice_stimuli = [
        { bright_side: "right", signal_presence: true, brightness_boost: 0,
          rate_confidence:true, delta: delta,
          data: { test_part: 'practice', correct_response: 'y' } },
        { bright_side: "left", signal_presence: true, brightness_boost: 0,
          rate_confidence:true, delta: delta,
          data: { test_part: 'practice', correct_response: 'y' } },
        { bright_side: "right", signal_presence: false, brightness_boost: 0,
          rate_confidence:true, delta: delta,
          data: { test_part: 'practice', correct_response: 'n' } },
        { bright_side: "left", signal_presence: false, brightness_boost: 0,
          rate_confidence:true, delta: delta,
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
        options: ['1. very confident that the bright patch was on the right',
                  '2. very confident that both patches were ordinary.',
                  '3. guessing that both patches were ordinary.',
                  '4. very confident that one of the two patches (left or right) was bright.',
                  '5. guessing that one fo the two patches was bright.'],
        name: 'multichoice_detection'
      }],
      on_finish: function(data){
        if (followed_instructions) {followed_instructions =
          JSON.parse(data.responses)['multichoice_detection'][0][0]==2
        }
      }
      }
      timeline.push(multichoice_detection);

      var starting_experiment = {
        type: "p5Text",
        title: 'Part 1 out of 6',
        text: "We are now ready to start the first part of the experiment."
      };
      timeline.push(starting_experiment)

      var start_det = {
        type: "p5Text",
        title: function() {return 'Part '+window.block_number+ ' out of 6'},
        text: "Indicate whether exactly one of the flickering patches was bright (Y key) "+
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
          p.fill(45,120,210)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(210,40,45)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('Y',p.width/2+400, p.height/2+35)
          p.text('N',p.width/2+400, p.height/2+105)
          p.textSize(15);
          p.fill(0)
          p.text('One bright',p.width/2+400, p.height/2-15)
          p.text('Both ordinary',p.width/2+400, p.height/2+155)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      }
      timeline.push(start_det);

      var det_test_stimuli = [
        { signal_presence: true, brightness_boost: 0,
          data: { test_part: 'test', correct_response: 'y' } },
        { signal_presence: false, brightness_boost: 0,
          data: { test_part: 'test', correct_response: 'n' } },
        { signal_presence: true, brightness_boost: 2,
          data: { test_part: 'test', correct_response: 'y' } },
        { signal_presence: false, brightness_boost: 2,
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
      timeline.push(take_a_break);


      var introduce_discrimination = {
        type: "p5Text",
        title: 'Your Second Challenge',
        text: "In the first part of the experiment, you observed pairs of " +
        "flickering patches and decided whether one of them was brighter, "+
        " on average, than the background. This time there will always be one "+
        "patch that is bright and one that is ordinary. " +
        "Your task is to pay close attention and tell which is which."
      };
      timeline.push(introduce_discrimination)

      var lets_practice_discrimination = {
        type: "p5Text",
        title: 'Your Second Challenge',
        text: "Lets start with 4 practice trials. \n" +
        "Press F if the bright patch was on the right and D if it was " +
        "on the left. Then, rate your confidence using the circle as before. "+
        "The bright patches will be extra-bright for these first "+
        "practice trials, but don't get used to it -- things will become more "+
        "challenging soon."
      };
      timeline.push(lets_practice_discrimination)

      var dis_practice_stimulus = {
        type: "flicker",
        choices: ['d','f'],
        bright_side: jsPsych.timelineVariable('bright_side'),
        std: std,
        delta: delta*3,
        brightness_boost: jsPsych.timelineVariable('brightness_boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        give_confidence_feedback:  jsPsych.timelineVariable('give_confidence_feedback'),
        on_finish: function(data){
          data.correct = data.response == data.key_mapping[data.bright_side];
        },
        data: jsPsych.timelineVariable('data')
      };

      var dis_stimulus = {
        type: "flicker",
        choices: ['d','f'],
        std: std,
        delta: delta,
        brightness_boost: jsPsych.timelineVariable('brightness_boost'),
        rate_confidence: jsPsych.timelineVariable('rate_confidence'),
        give_confidence_feedback:  jsPsych.timelineVariable('give_confidence_feedback'),
        on_finish: function(data){
          data.correct = data.response == data.key_mapping[data.bright_side];
        },
        data: jsPsych.timelineVariable('data')
      };


      var dis_practice_stimuli = [
        { bright_side: "right",
          brightness_boost: 0,
          data: { test_part: 'practice', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } },
        { bright_side: "left",
          brightness_boost: 0,
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
          "of 4 trials. Let's do 4 more practice trials. Press D if the bright " +
          "patch was on the right and F if it was on the left."
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
        options: ['1. very confident that the bright patch was on the left.',
                  '2. guessing that the bright patch was on the left.',
                  '3. very confident that the bright patch was on the right.',
                  '4. guessing that the bright patch was on the right.'],
        name: 'multichoice_discrimination'
      }],
      on_finish: function(data){
        if (followed_instructions) {
          followed_instructions =
          JSON.parse(data.responses)['multichoice_discrimination'][0][0]==1
        }
      }
      }
      timeline.push(multichoice_discrimination)



      // DISCRIMINATION

      var start_dis = {
        type: "p5Text",
        title: function() {return 'Part '+window.block_number+ ' out of 6'},
        text: "Indicate which flickering patch was bright "+
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
          p.fill(45,120,210)
          p.circle(p.width/2+400,p.height/2-150,80)
          p.fill(210,40,45)
          p.circle(p.width/2+400,p.height/2-80,20)
          p.fill(255)
          p.textSize(60);
          p.textAlign(p.CENTER, p.CENTER)
          p.strokeWeight(0);
          p.text('F',p.width/2+435, p.height/2+90)
          p.text('D',p.width/2+365, p.height/2+90)
          p.textSize(15);
          p.fill(0)
          p.text('Bright on right',p.width/2+470, p.height/2+30)
          p.text('Bright on left',p.width/2+330, p.height/2+30)
          p.text('Highest confidence',p.width/2+400, p.height/2-200)
          p.text('Lowest confidence',p.width/2+400, p.height/2-50)

          p.pop()

        }
      };
      timeline.push(start_dis);

      var dis_test_stimuli = [
        { delta: delta, rate_confidence: true, brightness_boost: 0,
          data: { test_part: 'test', key_mapping:
            {right: 'f', left: 'd', true: 'y', false: 'n'} } },
        { delta: delta, rate_confidence: true, brightness_boost: 2,
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
      timeline.push(take_a_break);


      var debrief_block = {
        type: "p5Text",
        text: function() {

          // take trials_per_block*4 because feedback is also counted
          var trials = jsPsych.data.get().last(trials_per_block*4).filter({test_part: 'test'});
          var correct_trials = trials.filter({correct: true});
          var accuracy = Math.round(correct_trials.count() / trials.count() * 100);
          var confidence = Math.round(correct_trials.select('confidence').mean()*100);

          return "You responded correctly on "+accuracy+"% of the trials.\n" +
          "Your average confidence level was "+confidence+" on a scale of 0 " +
          "to 100. You can now take a short break before we continue."
        }
      };

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
          if (detection_acc<=0.55 || discrimination_acc<=0.55 || !followed_instructions) {
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
        timeline: [start_det, det_test_procedure, update_block_number, take_a_break,
                  start_dis, dis_test_procedure, update_block_number, debrief_block,
                  start_det, det_test_procedure, update_block_number,take_a_break,
                  start_det, det_test_procedure, update_block_number, debrief_block],
        repetitions:  1
       }

      main_if_node = {
        timeline: [debrief_block, main_loop],
        conditional_function: function() {
          console.log('b')
          var all_trials = jsPsych.data.get().last(trials_per_block*4).filter({test_part: 'test'})
          var detection_trials = all_trials.last(trials_per_block);
          var discrimination_trials = all_trials.first(trials_per_block);
          var detection_acc = detection_trials.filter({correct:true}).count()/trials_per_block;
          var discrimination_acc = discrimination_trials.filter({correct:true}).count()/trials_per_block;
          if ((detection_acc>0.55 || discrimination_acc>0.55) && followed_instructions) {
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
    console.log('version 2');
    window.participant_number = jatos.batchSession.get(`participant_number`);
    var m = new MersenneTwister();
    Math.random = function() {return m.random()};
    var protocol_sum = 'c8c398e9134c072a7c73ea6a24f87079609999df2a40e52c19f94e3d98a58d2c'
    subject_sum = hexToBytes(
      CryptoJS.SHA256(
        protocol_sum+window.participant_number).toString()
      )
    m.init_by_array(subject_sum, subject_sum.length)
    jsPsych.data.addProperties({'protocol_sum':protocol_sum,
    'participant_number':participant_number, 'subject_sum':subject_sum});
    timeline=makeTimeline()
      jsPsych.init({
          timeline: timeline,
          on_finish: function() {
              jsPsych.data.addProperties(jatos.urlQueryParameters);
              var resultJson = jsPsych.data.get().json();
              jatos.batchSession.set('participant_number',jatos.batchSession.get(`participant_number`)+1)
              .then(()=>jatos.submitResultData(resultJson, jatos.startNextComponent));
          }
      });
  });
