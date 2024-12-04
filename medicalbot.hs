import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Data.List (isInfixOf)

-- Define Severity, ResponseType, and Emotion types
data Severity = Mild | Moderate | Severe deriving (Show, Eq)
data ResponseType = General | FollowUp | Advice | Urgent deriving (Show, Eq)
data Emotion = Neutral | Concerned | Encouraging | Sympathetic deriving (Show, Eq)

-- Structure for each response, containing type, message, severity, and emotion
data Response = Response {
    responseType :: ResponseType,
    message :: String,
    severity :: Severity,
    emotion :: Emotion
} deriving (Show, Eq)

-- Knowledge base for symptoms, each with a solution-oriented message
responseDatabase :: [(String, Response)]
responseDatabase = 
    [ ("fever", Response Advice "Try to rest, stay hydrated, and take acetaminophen if needed. If your fever lasts more than three days, consider visiting a healthcare provider." Moderate Concerned),
      ("cough", Response Advice "For a dry cough, try a humidifier and stay hydrated. For a productive cough, consider seeing a doctor if symptoms persist." Mild Encouraging),
      ("headache", Response Advice "Rest in a dark room, stay hydrated, and avoid screens. If it’s persistent or accompanied by nausea, consult a healthcare provider." Moderate Sympathetic),
      ("stomach pain", Response Advice "Avoid heavy meals, drink clear fluids, and rest. If the pain is sharp or involves vomiting, seek medical attention." Severe Concerned),
      ("sore throat", Response Advice "Gargle with warm salt water, drink warm fluids, and rest. If it doesn’t improve in a few days, consider seeing a healthcare provider." Mild Sympathetic)
    ]

-- General fallback responses
generalResponses :: [Response]
generalResponses = 
    [ Response General "I'm here to help. Could you describe your symptoms in detail?" Mild Neutral,
      Response General "Please share more about your symptoms so I can assist you better." Mild Neutral,
      Response General "Could you provide a bit more detail about what you’re experiencing?" Mild Neutral
    ]

-- Retrieve response based on detected symptom
getResponseForSymptom :: String -> IO Response
getResponseForSymptom input = do
    let detectedResponses = filter (\(symptom, _) -> symptom `isInfixOf` input) responseDatabase
    case detectedResponses of
        ((_, response):_) -> return response
        []                -> getRandomGeneralResponse

-- Select a random general response if symptom is unknown
getRandomGeneralResponse :: IO Response
getRandomGeneralResponse = do
    index <- randomRIO (0, length generalResponses - 1)
    return (generalResponses !! index)

-- Emotion-based formatting for response messages
formatWithEmotion :: Response -> String
formatWithEmotion response = case emotion response of
    Concerned -> "🤔 " ++ message response
    Encouraging -> "💪 " ++ message response
    Sympathetic -> "😔 " ++ message response
    Neutral -> message response

-- Display response with emotional context
displayResponse :: Response -> IO ()
displayResponse response = do
    putStrLn $ "\nBot: " ++ formatWithEmotion response
    if responseType response == Urgent
        then putStrLn "🚨 This seems serious. Seeking medical attention soon is advisable."
        else return ()

-- Main chat loop for reading statements and giving solutions
chatLoop :: IO ()
chatLoop = do
    putStr "\nYou: "
    hFlush stdout
    userInput <- getLine
    if userInput == "bye"
        then putStrLn "\nBot: Take care, and don’t hesitate to reach out to a healthcare provider if you need further help. Goodbye!"
        else do
            -- Fetch response based on symptom detection
            response <- getResponseForSymptom userInput
            displayResponse response
            chatLoop

-- Start the chatbot
main :: IO ()
main = do
    putStrLn "Welcome to the Medical Chatbot! 😊\nDescribe your symptoms, and I'll try to offer helpful advice. Type 'bye' to exit anytime."
    chatLoop

