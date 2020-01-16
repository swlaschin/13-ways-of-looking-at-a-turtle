(* ======================================
04-Api_FP_Core.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #4: 4: API (OO/FP hybrid approach) -- OO API calling stateless functions

In this design, an API layer communicates with pure turtle functions
and the client talks to the API layer.

The API layer manages the state (rather than the client) by storing a mutable turtle state.

*This approach has been named "Functional Core/Imperative Shell" by [Gary Bernhardt](https://www.youtube.com/watch?v=yTkzNHF6rMs)*

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common
open FPTurtleLib
open TurtleApiHelpers // helpers for API validation, etc

// ======================================
// Turtle Api Layer
// ======================================

module TurtleApiLayer = 

    open Result
    
    /// Function to log a message
    let log message =
        printfn "%s" message 

    // logged versions    
    let move = Turtle.move log
    let turn = Turtle.turn log
    let penDown = Turtle.penDown log
    let penUp = Turtle.penUp log
    let setColor = Turtle.setColor log

    type TurtleApi() =

        let mutable state = Turtle.initialTurtleState

        /// Update the mutable state value
        let updateState newState =
            state <- newState

        /// Execute the command string, and return a Result
        /// Exec : commandStr:string -> Result<unit,ErrorMessage>
        member this.Exec (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            // lift current state to Result
            let stateR = returnR state

            // calculate the new state
            let newStateR = 
                match tokens with
                | [ "Move"; distanceStr ] -> 
                    // get the distance as a Result
                    let distanceR = validateDistance distanceStr 

                    // call "move" lifted to the world of Results
                    lift2R move distanceR stateR

                | [ "Turn"; angleStr ] -> 
                    let angleR = validateAngle angleStr 
                    lift2R turn angleR stateR

                | [ "Pen"; "Up" ] -> 
                    returnR (penUp state)

                | [ "Pen"; "Down" ] -> 
                    returnR (penDown state)

                | [ "SetColor"; colorStr ] -> 
                    let colorR = validateColor colorStr
                    lift2R setColor colorR stateR

                | _ -> 
                    Error (InvalidCommand commandStr)
        
            // Lift `updateState` into the world of Results and 
            // call it with the new state.
            Result.map updateState newStateR

            // Return the final result (output of updateState)

// ======================================
// Turtle Api Client
// ======================================

module TurtleApiClient = 

    open TurtleApiLayer 
    open Result

    let drawTriangle() = 
        let api = TurtleApi()
        result {
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            }
        // back home at (0,0) with angle 0
            
    let drawThreeLines() = 
        let api = TurtleApi()
        result {

        // draw black line 
        do! api.Exec "Pen Down"
        do! api.Exec "SetColor Black"
        do! api.Exec "Move 100"
        // move without drawing
        do! api.Exec "Pen Up"
        do! api.Exec "Turn 90"
        do! api.Exec "Move 100"
        do! api.Exec "Turn 90"
        // draw red line 
        do! api.Exec "Pen Down"
        do! api.Exec "SetColor Red"
        do! api.Exec "Move 100"
        // move without drawing
        do! api.Exec "Pen Up"
        do! api.Exec "Turn 90"
        do! api.Exec "Move 100"
        do! api.Exec "Turn 90"
        // back home at (0,0) with angle 0
        // draw diagonal blue line 
        do! api.Exec "Pen Down"
        do! api.Exec "SetColor Blue"
        do! api.Exec "Turn 45"
        do! api.Exec "Move 100"
        }

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n) 
        let api = TurtleApi()

        // define a function that draws one side
        let drawOneSide() = result {
            do! api.Exec "Move 100.0"
            do! api.Exec (sprintf "Turn %f" angle)
            }

        // repeat for all sides
        result {
            for i in [1..n] do
                do! drawOneSide() 
        }

    let triggerError() = 
        let api = TurtleApi()
        api.Exec "Move bad"

// ======================================
// Turtle Api Tests
// ======================================

(*
TurtleApiClient.drawTriangle() 
TurtleApiClient.drawThreeLines() 
TurtleApiClient.drawPolygon 4 

// test errors
TurtleApiClient.triggerError()  
// Error (InvalidDistance "bad")
*)

