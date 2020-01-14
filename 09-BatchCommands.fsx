(* ======================================
09-BatchCommands.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #9: Batch oriented -- Using a list of commands

In this design, the client creates a list of `Command`s that will be intepreted later.
These commands are then run in sequence using the Turtle library functions.

This approach means that there is no state that needs to be persisted between calls by the client.

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common

// ======================================
// TurtleCommmandHandler
// ======================================

module TurtleCommmandHandler = 

    open FPTurtleLib

    /// Function to log a message
    let log message =
        printfn "%s" message 

    // logged versions    
    let move = Turtle.move log
    let turn = Turtle.turn log
    let penDown = Turtle.penDown log
    let penUp = Turtle.penUp log
    let setColor = Turtle.setColor log

    type TurtleCommand = 
        | Move of Distance 
        | Turn of Angle
        | PenUp
        | PenDown
        | SetColor of PenColor

    // --------------------------------------
    // The Command Handler
    // --------------------------------------

    /// Apply a command to the turtle state and return the new state 
    let applyCommand state command =
        match command with
        | Move distance ->
            move distance state
        | Turn angle ->
            turn angle state
        | PenUp ->
            penUp state
        | PenDown ->
            penDown state
        | SetColor color ->
            setColor color state

    /// Run list of commands in one go
    let run aListOfCommands = 
        aListOfCommands 
        |> List.fold applyCommand Turtle.initialTurtleState

// ======================================
// TurtleCommmandHandler
// ======================================

module TurtleCommmandClient = 
    open TurtleCommmandHandler

    let drawTriangle() = 
        // create the list of commands
        let commands = [
            Move 100.0 
            Turn 120.0<Degrees>
            Move 100.0 
            Turn 120.0<Degrees>
            Move 100.0 
            Turn 120.0<Degrees>
            ]
        // run them
        run commands
            
    let drawThreeLines() = 
        // create the list of commands
        let commands = [
            // draw black line 
            PenDown
            SetColor Black
            Move 100.0 
            // move without drawing
            PenUp
            Turn 90.0<Degrees>
            Move 100.0 
            Turn 90.0<Degrees>
            // draw red line 
            PenDown
            SetColor Red
            Move 100.0
            // move without drawing
            PenUp
            Turn 90.0<Degrees>
            Move 100.0 
            Turn 90.0<Degrees>
            // back home at (0,0) with angle 0
            // draw diagonal blue line 
            PenDown
            SetColor Blue
            Turn 45.0<Degrees>
            Move 100.0
            ]

        // run the commands
        run commands

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n) 
        let angleDegrees = angle * 1.0<Degrees>

        // define a function that draws one side
        let drawOneSide sideNumber = [
            Move 100.0
            Turn angleDegrees
            ]

        // repeat for all sides
        let commands = 
            [1..n] |> List.collect drawOneSide

        // run the commands
        run commands

// ======================================
// TurtleCommmandClient Tests
// ======================================


TurtleCommmandClient.drawTriangle() 
TurtleCommmandClient.drawThreeLines() // Doesn't go back home
TurtleCommmandClient.drawPolygon 4 



