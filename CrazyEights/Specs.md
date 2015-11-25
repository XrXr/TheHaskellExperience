Assignment 11 - Crazy Eights (in ~~Java~~ Haskell)

For this assignment, you will be making a simple card game in Java.  The game is the two player version of Crazy Eights.  One of the players is the computer.

- Implement a public class, Card, that represents a standard playing card. This class should include at least a

    1. face value 1,...,13 (1=ace, 11=jack/knave, 12=queen, 13=king) and a
    2. suit 1,...,4 (1=hearts, 2=diamonds, 3=spades, 4=clubs)
    3. a public constructor, Card(face, suit), that initializes the Card.
    4. a public String toString() method that returns a string representation of the card.  For example, the ace of spades might be represented as "AS", the nine of clubs as "9C" or the queen of hearts as "QH".

- Implement a public class, Deck, that represents a standard deck of 52 playing cards. This class should include at least a

    1. list of cards in the Deck.
    2. public constructor, Deck(), that initialzes a new Deque with 52 cards.
    3. a public shuffle() method that shuffles the cards in this Deck.
    4. a public Card draw() method that removes the top card from the Deck and returns it

- Implement a public class, Player, that represents a card player in a game of Crazy Eights.  A Player has a

    1. hand (a list of cards the player is holding)
    2. a public String toString() method that returns a string representation of the player's hand.

- Implement a public class, DiscardPile, which represents a pile (list) of cards that are facing up. This class should implement at least,

    1. a public toString() method that gives a string representation of the top card on the pile.
    2. a public add(Card c) method that places the Card, c, on the top of the pile.

- Implement a runnable public class called CrazyEights(). The main method of this class should implement a text version of a single round Crazy Eights.  A single round lasts until

    1. one player (the winner) discards their last card; or
    2. neither player can discard and there are no cards left on the stock pile, in which case the game is a draw.
    3. You have some flexibility in how you want the interface to look, but here is one possible example:


```
    ...
    Computer is holding 7 cards
    Discard pile: 4H
    Your hand: 3D 4C 6H 9D QD KH
    Which card do you want to discard (0=4C, 1=6H, 2=KH)?: 1
    You discard 6H

    Discard pile: 6H
    Computer discards QH

    Computer is holding 7 cards
    Discard pile: QH
    Your hand: 3D 4C 9D QD KH
    Which card do you want to discard (0=QD, 1=KH)?: 1
    You discard KH

    Discard pile: KH
    Computer discards KS

    Computer is holding 7 cards
    Discard pile: KS
    Your hand: 3D 4C 9D QD
    You draw AS
    Your hand: 3D 4C 9D QD AS
    You draw AS
    Your hand: 3D 4C 9D QD AS
    You discard AS
    ...
```

##Rules

Eight cards are dealt to each player. The remaining cards of the deck are placed face down at the center of the table. The top card is then turned face up to start the game.

Players discard by matching rank or suit with the top card of the discard pile, starting with the player left of the dealer. If a player is unable to match the rank or suit of the top card of the discard pile and does not have an eight, he or she draws cards from the stockpile until getting a playable card. When a player plays an eight, he or she must declare the suit that the next player is to play; that player must then follow the named suit or play another eight. A player can choose to draw as many card as desired during a turn.

As an example: Once the six of clubs is played the next player:

    can play any of the other sixes
    can play any of the clubs
    can play any eight (then must declare a different suit)
    can draw from the stockpile until willing and able to play one of the above
