# Team Members
Abdelrahman Madkour -- email: madkour.a@husky.neu.edu
Kyle Mooney  -- email: mooney.kyl@husky.neu.edu

# How we handled comments

We defined a separate comment state which was entered from the initial state
upon encountering the first "/*".  When we do so, we increment a global 
variable "commentInc", which specifies the depth of the comment.  When we 
encounter "*/", we decrement "commentInc" and then check if it is zero.

# How we handled errors

We used Appel's ErrorMsg module to report errors.  There were several cases:
encountering of a new line in the string state (not inside of the
"STRING_ESCAPE" state which accounts for the ignored whitespace/new-line 
sequence), an illegal control character following "\^" inside of a string, 
an illegal character following "\", an illegal character inside of the
above described "ignored whitespace/new-line sequence", and any other 
character not recognized by the language.  The more involved cases 
include how we deal with EOF (discussed in the next section) and the usage of
illegal ASCII codes.  For this we checked if the digit following "\" 
exceeded 255, in which case we threw an error.

# How we handled end-of-file



# Interesting Stuff about our lexer
