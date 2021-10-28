import pyttsx3
# initialize Text-to-speech engine
engine = pyttsx3.init()
# convert this text to speech
text = "Python is a great programming language"
engine.say(text)
#voice
voices = engine.getProperty("voices")
engine.setProperty("voice", voices[1].id)
# speed`
engine.setProperty("rate", 120)
# play the speech
engine.runAndWait()

