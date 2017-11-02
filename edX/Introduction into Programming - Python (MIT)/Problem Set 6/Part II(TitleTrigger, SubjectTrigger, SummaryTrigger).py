# Problem 1
import string
class NewsStory(object):
    def __init__(self, guid, title, subject, summary, link):
        self.guid = guid
        self.title = title
        self.subject = subject
        self.summary = summary
        self.link = link
    def getGuid(self):
        return self.guid

    def getTitle(self):
        return self.title

    def getSubject(self):
        return self.subject

    def getSummary(self):
        return self.summary

    def getLink(self):
        return self.link

#======================
# Part 2
# Triggers
#======================

class Trigger(object):
    def evaluate(self, story):
        """
        Returns True if an alert should be generated
        for the given news item, or False otherwise.
        """
        raise NotImplementedError

# Whole Word Triggers
# Problems 2-5

# TODO: WordTrigger
class WordTrigger(Trigger):
    def __init__(self, word):
        self.word = word.lower()
        
    def getWord(self):
        return self.word

    def isWordIn(self, text):
        # 1.Turn text into lowercase string:
        loweredText = text.lower()
        # 2.Change all punctuation in loweredText to spaces:
        textWithoutPunctuation = ''
        for e in loweredText:
            if e in string.punctuation:
                e = ' '
            textWithoutPunctuation += e
        # 3. Split into words with space - ' ':
        listOfText = textWithoutPunctuation.split(' ')
        # 4. Check if the word in the list of  text:
        return self.getWord() in listOfText
        
# TODO: TitleTrigger
class TitleTrigger(WordTrigger):
##    def __init__(self):
##        Trigger.__init__(self)
    def evaluate(self, story):
        
        return self.isWordIn(story.getTitle())
