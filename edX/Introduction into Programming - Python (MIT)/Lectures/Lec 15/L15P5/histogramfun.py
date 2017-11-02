import pylab


WORDLIST_FILENAME = "words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # wordList: list of strings
    wordList = []
    for line in inFile:
        wordList.append(line.strip().lower())
    print "  ", len(wordList), "words loaded."
    return wordList

def stdDev(X):
    mean = sum(X)/float(len(X))
    tot = 0.0
    for x in X:
        tot += (x - mean)**2
    return (tot/len(X))**0.5

def vowelsInWord(word):
    vowels = 'aeiou'
    vowsInWord = 0.0
    for letter in word:
            if letter in vowels:
                vowsInWord +=1
    return vowsInWord

def plotVowelProportionHistogram(wordList, numBins=15):
    """
    Plots a histogram of the proportion of vowels in each word in wordList
    using the specified number of bins in numBins
    """
    # 1. Finding mean:
    sumOfPropVowels = 0.0
    propVowelsList = []
    for word in wordList:
        propVowels = vowelsInWord(word) / len(word)
        propVowelsList.append(propVowels)
    mean = sum(propVowelsList) / len(propVowelsList)
    #print mean
    # 2. Finding standart deviation(StD) and coefficient of variation (CoV):
    StD = stdDev(propVowelsList)
    #print StD
    CoV = StD / mean
    #print CoV
    # 3. Drawing a histogram:
    pylab.title('Proportion of vowels in each word in wordList')
##    xmin,xmax = pylab.xlim()
##    ymin,ymax = pylab.ylim()
    pylab.hist(propVowelsList, bins = numBins)    
if __name__ == '__main__':
    wordList = loadWords()
    plotVowelProportionHistogram(wordList)
pylab.show()
