# CS4012 Lab 2

## Reverse Interpreter

To run the interpreter, navigate to the project directory in your terminal/console and type:
```
stack ghci
:load Main
main
```

The interpreter accepts a number of user input:

* `n`: interpreter will execute the next line of the program.
* `b`: interpreter will reverse one line.
* `i var1`: inspect the value of the variable `var1`.
* `h var1`: print out the history of values for `var1`.
* `q`: quit the interpreter.

## Step-through

The text file `program.txt` contains a list of statements is read and parsed by the interpreter into a list of statements.
The interpreter runs the first line of code and then waits for the user to enter `n` to run the next line. The interpreter will run lines of code until it gets to the end of the program, where the interpreter informs the user that they have reached the end. Every use of the `n` command after this point will print the message `End of program`.

## Inspect

At any point in the program, the user can ask to see the value of any variable. This can be done by using the `i` command followed by the name of the variable that you want to inspect. If the user asks to inspect a variable that is uninitialised or doesn't exist the interpreter will fail with the message `Unknown variable`.

## History

The interpreter keeps track of the state of the environment throughout the course of program execution. By doing this, the interpreter can find the history of a variable by inspecting the variable in the current and previous environments. The user can request the history of a variable by typing the command `h` followed by the name of the variable. If the user asks for the history of a variable that is uninitialised or doesn't exist the interpreter will fail with the message `Unknown variable`.

## Step Backwards

Because the interpreter saves a list of all the previous environment, we can return to a previous line of code by subbing in the corresponding environment. For example, if we are moving back from line 7 to 6, we have to filter out the environments that correspond to the line 7 and return the environment that corresponds to the line 6. There may be multiple environments for one line of code through the use of `Seq` or `While`. 
