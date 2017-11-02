from ps4a import *
import time


#
#
# Problem #6: Computer chooses a word
#
#
def compChooseWord(hand, wordList, n):
    """
    Given a hand and a wordList, find the word that gives 
    the maximum value score, and return it.

    This word should be calculated by considering all the words
    in the wordList.

    If no words in the wordList can be made from the hand, return None.

    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)

    returns: string or None
    """
    # BEGIN PSEUDOCODE <-- Remove this comment when you code this function; do your coding within the pseudocode (leaving those comments in-place!)
    # Create a new variable to store the maximum score seen so far (initially 0)
    bestScore = 0
    # Create a new variable to store the best word seen so far (initially None)  
    bestWord = None
    # For each word in the wordList
    for word in wordList:
        # If you can construct the word from your hand
        # (hint: you can use isValidWord, or - since you don't really need to test if the word is in the wordList - you can make a similar function that omits that test)
        if isValidWord(word, hand, wordList) == True:
            # Find out how much making that word is worth
            wordWorth = getWordScore(word, n)
            # If the score for that word is higher than your best score
            if wordWorth > bestScore:
                # Update your best score, and best word accordingly
                bestScore = wordWorth
                bestWord = word
    # return the best word you found.s
    return bestWord



#
# Problem #7: Computer plays a hand
#
def compPlayHand(hand, wordList, n):
    """
    Allows the computer to play the given hand, following the same procedure
    as playHand, except instead of the user choosing a word, the computer 
    chooses it.

    1) The hand is displayed.
    2) The computer chooses a word.
    3) After every valid word: the word and the score for that word is 
    displayed, the remaining letters in the hand are displayed, and the 
    computer chooses another word.
    4)  The sum of the word scores is displayed when the hand finishes.
    5)  The hand finishes when the computer has exhausted its possible
    choices (i.e. compChooseWord returns None).
 
    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    """
    # Keep track of the total score
    totalScore = 0
    # 5)  The hand finishes when the computer has exhausted its possible
    #     choices (i.e. compChooseWord returns None)
    # As long as there are still letters left in the hand:
    while calculateHandlen(hand) > 0:
        # 1) The hand is displayed.
        print 'Current Hand:',
        displayHand(hand)
        # 2) The computer chooses a word.
        chosenWord = compChooseWord(hand, wordList, n)
        if chosenWord == None:
            break
        else: 
            # 3) After every valid word: the word and the score for that word is 
            #    displayed, the remaining letters in the hand are displayed, and the 
            #    computer chooses another word.
            # 3.1) Tell the user how many points the word earned, and the updated total score, in one line followed by a blank line
            totalScore += getWordScore(chosenWord, n)
            print '"' + chosenWord + '" ' + 'earned ' + str(getWordScore(chosenWord, n)) + ' points. Total: ' + str(totalScore) + ' points.'
            print
            # 3.2) Update the hand 
            hand = updateHand(hand, chosenWord)                
    # The sum of the word scores is displayed when the hand finishes.
    print 'Total score: ' + str(totalScore) + ' points.'
    print
    
#
# Problem #8: Playing a game
#
#
def playGame(wordList):
    """
    Allow the user to play an arbitrary number of hands.
 
    1) Asks the user to input 'n' or 'r' or 'e'.
        * If the user inputs 'e', immediately exit the game.
        * If the user inputs anything that's not 'n', 'r', or 'e', keep asking them again.

    2) Asks the user to input a 'u' or a 'c'.
        * If the user inputs anything that's not 'c' or 'u', keep asking them again.

    3) Switch functionality based on the above choices:
        * If the user inputted 'n', play a new (random) hand.
        * Else, if the user inputted 'r', play the last hand again.
      
        * If the user inputted 'u', let the user play the game
          with the selected hand, using playHand.
        * If the user inputted 'c', let the computer play the 
          game with the selected hand, using compPlayHand.

    4) After the computer or user has played the hand, repeat from step 1

    wordList: list (string)
    """
    switch = '' #used for input 'n' or 'r' or 'e'.
    lastHand = {} # empy dic for keeping last hand
    userOrComp = '' # used to input a 'u' or 'c'
    
    # After the computer or user has played the hand, repeat from step 1
    while switch != 'e':
        bool1 = True
        bool2 = True

        # If the user inputs anything that's not 'n', 'r', or 'e', keep asking them again.s
        while  bool1:
            # Ask the user to input 'n' or 'r' or 'e'
            switch = raw_input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
            if switch == 'n' or switch == 'e':
                bool1 = False
                print
            elif switch == 'r' and lastHand == {}:
                print 'You have not played a hand yet. Please play a new hand first!'
                print
            elif switch == 'r':
                bool1 = False
                print
            else:
                print 'Invalid command.'
                print
        # If the user inputs 'e', immediately exit the game.
        if switch == 'e':
            break
        #If the user inputs anything that's not 'c' or 'u', keep asking them again.
        while bool2:
            # Asks the user to input a 'u' or a 'c'.
            userOrComp = raw_input('Enter u to have yourself play, c to have the computer play: ')
            if userOrComp == 'u' or userOrComp == 'c':
                bool2 = False
                print
            else:
                print 'Invalid command.'
                print
        # If the user inputs 'n', let the user play a new (random) hand.
        if switch == 'n':
            lastHand = dealHand(HAND_SIZE)
            if userOrComp == 'u':
                playHand(lastHand, wordList, HAND_SIZE)
            else:
                compPlayHand(lastHand, wordList, HAND_SIZE)
        # If the user inputs 'r', let the user play the last hand again.
        elif switch == 'r':
            if lastHand != {}:
                if userOrComp == 'u':
                    playHand(lastHand, wordList, HAND_SIZE)
                else:
                    compPlayHand(lastHand, wordList, HAND_SIZE)
            else:
                print 'You have not played a hand yet. Please play a new hand first!'
                print
                
        
            
    

        
#
# Build data structures used for entire session and play game
#
if __name__ == '__main__':
    wordList = loadWords()
    playGame(wordList)

#compChooseWord({'a': 2, 'e': 2, 'i': 2, 'm': 2, 'n': 2, 't': 2}, wordList, HAND_SIZE)# Test compChooseWords, HAND_SIZE is 12

#compPlayHand({'a': 2, 'e': 2, 'i': 2, 'm': 2, 'n': 2, 't': 2}, wordList, HAND_SIZE) # Test compChooseWord, HAND_SIZE is 12
#compPlayHand(dealHand(HAND_SIZE), wordList, HAND_SIZE) # Test with random hand, using dealHand function
