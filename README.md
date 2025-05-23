# Halatro - A functional twist on Balatro
## Implementing a functional functional back-end
As part of my functional module in my Computer Science course, I was tasked with implementing the back-end logic for Halatro. Halatro is a terminal-based clone on the 
massively popular game Balatro. This implementation involved checking what the highest scoring hand type (e.g. pair, two pair, flush) was from a given list of up to 5 played cards. With this I could then score the played cards in-line with (roughly) Balatro's scoring system.

## Implementing AI
After completing the basic back-end logic, I was then tasked with creating an AI that generates the best score given 3 hands to play and 3 discards to use. Firstly, I had to define a way for the AI to generate the best scoring hand given a list of up to 5 cards. I did this by generating each possible hand as a subsequence of the list of played cards and then comparing the scores of each of this hands to find the highest scoring hand. Unfortunately, I ran out of time to implement the final AI solution. However, I may return to finish the implementation in the future...

## What I learned
During this coursework, I learned how to manipulate data types and type classes in Haskell. I completed this by using list comprehensions and recursion. Most importantly, I learned and expanded upon fundamental functional concepts such as higher-order functions, lambda calculus, beta-reduction and pattern matching.

## How to run
To run this you will need to have Stack installed which can be accessed [here](https://docs.haskellstack.org/en/stable/).
Then use the command ``stack run`` to run the program. Enjoy!