module FirstFunction where

makeGreeting salutation person = 
  let messageWithTrailingSpace = salutation <> " "
  in messageWithTrailingSpace <> person
                 
extendedGreeting person = 
  let hello = makeGreeting helloStr person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
      helloStr = "Hello"
  in hello <> "\n" <> goodDay <> "\n" <> goodBye

extendedGreeting' person =
  let joinWithNewLines a b = a <> "\n" <> b
      hello = makeGreeting "Hello" person
      goodBye = makeGreeting "GoodBye" person
  in joinWithNewLines hello goodBye

extendedGreeting'' person =
  let joinWithNewLines a b = a <> "\n" <> b
      joined = joinWithNewLines hello goodbye
      hello = makeGreeting "Hello" person
      goodbye = makeGreeting "Goodbye" person
  in joined

extendedGreeting''' person = 
  let joinWithNewLines a b = a <> "\n" <> b
      helloAndGoodbye hello goodbye = 
        let hello' = makeGreeting hello person
            goodbye' = makeGreeting goodbye person
        in joinWithNewLines hello' goodbye'
  in helloAndGoodbye "Hello" "Goodbye"

extendedGreeting'''' name place =
  let
    salutation = "Hello" <> name
    meetingInfo = location "Tuesday"
  in salutation <> " " <> meetingInfo
  where
    location day = "we met at " <> place <> " on a " <> day

extendedGreeting''''' person = 
  helloAndGoodbye "Hello" "Goodbye"
  where
    helloAndGoodbye hello goodbye = 
      joinWithNewLines hello' goodbye'
      where
        hello' = makeGreeting hello person
        goodbye' = makeGreeting goodbye person
    joinWithNewLines a b = a <> "\n" <> b

-- usually let is used for intermediate values, whereas where is used for ancillary and helper functions
main = print "no salutation to show yet"
