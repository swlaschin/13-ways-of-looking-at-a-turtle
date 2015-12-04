(* ======================================
07-DependencyInjection_Functions.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #7: Dependency injection using functions

In this design, an API layer communicates via one or more functions that are passed in as parameters to the API call.
These functions are typically partially applied so that the call site is decoupled from the "injection"

No interface is passed to the constructor.



====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open System
open Common
open FPTurtleLib
open TurtleApiHelpers // helpers for API validation, etc


// ======================================
// TurtleApi - all Turtle functions are passed in as parameters
// ======================================

module TurtleApi_PassInAllFunctions = 

    open Result

    // No functions in constructor
    type TurtleApi() =  

        let mutable state = Turtle.initialTurtleState

        /// Update the mutable state value
        let updateState newState =
            state <- newState

        /// Execute the command string, and return a Result
        /// Exec : commandStr:string -> Result<unit,ErrorMessage>
        member this.Exec move turn penUp penDown setColor (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            // return Success of unit, or Failure
            match tokens with
            | [ "Move"; distanceStr ] -> result {
                let! distance = validateDistance distanceStr 
                let newState = move distance state   // use `move` function that was passed in
                updateState newState
                }
            | [ "Turn"; angleStr ] -> result {
                let! angle = validateAngle angleStr   
                let newState = turn angle state   // use `turn` function that was passed in
                updateState newState
                }
            | [ "Pen"; "Up" ] -> result {
                let newState = penUp state
                updateState newState
                }
            | [ "Pen"; "Down" ] -> result {
                let newState = penDown state
                updateState newState
                }
            | [ "SetColor"; colorStr ] -> result {
                let! color = validateColor colorStr
                let newState = setColor color state
                updateState newState
                }
            | _ -> 
                Failure (InvalidCommand commandStr)

// -----------------------------
// Turtle Implementations for "Pass in all functions" design
// -----------------------------

module TurtleImplementation_PassInAllFunctions = 
    
    open FPTurtleLib
    open TurtleApi_PassInAllFunctions

    let log = printfn "%s"
    let move = Turtle.move log 
    let turn = Turtle.turn log 
    let penUp = Turtle.penUp log
    let penDown = Turtle.penDown log
    let setColor = Turtle.setColor log 

    let normalSize() = 
        let api = TurtleApi() 
        // partially apply the functions
        api.Exec move turn penUp penDown setColor 
        // the return value is a function: 
        //     string -> Result<unit,ErrorMessage> 

    let halfSize() = 
        let moveHalf dist = move (dist/2.0)  
        let api = TurtleApi() 
        // partially apply the functions
        api.Exec moveHalf turn penUp penDown setColor 
        // the return value is a function: 
        //     string -> Result<unit,ErrorMessage> 

// -----------------------------
// Turtle API Client for "Pass in all functions" design
// -----------------------------

module TurtleApiClient_PassInAllFunctions = 

    open Result

    // the API type is just a function
    type ApiFunction = string -> Result<unit,ErrorMessage>

    let drawTriangle(api:ApiFunction) = 
        result {
            do! api "Move 100"
            do! api "Turn 120"
            do! api "Move 100"
            do! api "Turn 120"
            do! api "Move 100"
            do! api "Turn 120"
            }
           

// -----------------------------
// Turtle Api Tests for "Pass in all functions" design
// -----------------------------

(*
let apiFn = TurtleImplementation_PassInAllFunctions.normalSize()  // string -> Result<unit,ErrorMessage>
TurtleApiClient_PassInAllFunctions.drawTriangle(apiFn) 

let apiFn = TurtleImplementation_PassInAllFunctions.halfSize()
TurtleApiClient_PassInAllFunctions.drawTriangle(apiFn) 

let mockApi s = 
    printfn "[MockAPI] %s" s
    Success ()
TurtleApiClient_PassInAllFunctions.drawTriangle(mockApi) 
*)

// ======================================
// Turtle Api -- Pass in a single function
// ======================================

module TurtleApi_PassInSingleFunction = 
    
    open Result

    type TurtleCommand = 
        | Move of Distance 
        | Turn of Angle
        | PenUp
        | PenDown
        | SetColor of PenColor

 // No functions in constructor
    type TurtleApi() =  

        let mutable state = Turtle.initialTurtleState

        /// Update the mutable state value
        let updateState newState =
            state <- newState

        /// Execute the command string, and return a Result
        /// Exec : commandStr:string -> Result<unit,ErrorMessage>
        member this.Exec turtleFn (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            // return Success of unit, or Failure
            match tokens with
            | [ "Move"; distanceStr ] -> result {
                let! distance = validateDistance distanceStr 
                let command =  Move distance      // create a Command object
                let newState = turtleFn command state
                updateState newState
                }
            | [ "Turn"; angleStr ] -> result {
                let! angle = validateAngle angleStr 
                let command =  Turn angle      // create a Command object
                let newState = turtleFn command state
                updateState newState
                }
            | [ "Pen"; "Up" ] -> result {
                let command =  PenUp
                let newState = turtleFn command state
                updateState newState
                }
            | [ "Pen"; "Down" ] -> result {
                let command =  PenDown
                let newState = turtleFn command state
                updateState newState
                }
            | [ "SetColor"; colorStr ] -> result {
                let! color = validateColor colorStr
                let command =  SetColor color 
                let newState = turtleFn command state
                updateState newState
                }
            | _ -> 
                Failure (InvalidCommand commandStr)

      

// -----------------------------
// Turtle Implementations for "Pass in a single function" design
// -----------------------------

module TurtleImplementation_PassInSingleFunction = 
    
    open FPTurtleLib
    open TurtleApi_PassInSingleFunction

    let log = printfn "%s"
    let move = Turtle.move log 
    let turn = Turtle.turn log 
    let penUp = Turtle.penUp log
    let penDown = Turtle.penDown log
    let setColor = Turtle.setColor log 

    let normalSize() = 
        let turtleFn = function
            | Move dist -> move dist 
            | Turn angle -> turn angle
            | PenUp -> penUp 
            | PenDown -> penDown 
            | SetColor color -> setColor color

        // partially apply the function
        let api = TurtleApi() 
        api.Exec turtleFn 
        // the return value is a function: 
        //     string -> Result<unit,ErrorMessage> 
        

    let halfSize() = 
        let turtleFn = function
            | Move dist -> move (dist/2.0)  
            | Turn angle -> turn angle
            | PenUp -> penUp 
            | PenDown -> penDown 
            | SetColor color -> setColor color

        // partially apply the function
        let api = TurtleApi() 
        api.Exec turtleFn 
        // the return value is a function: 
        //     string -> Result<unit,ErrorMessage> 
        
// -----------------------------
// Turtle API Client for "Pass in a single function" design
// -----------------------------

module TurtleApiClient_PassInSingleFunction = 

    open Result

    // the API type is just a function
    type ApiFunction = string -> Result<unit,ErrorMessage>

    let drawTriangle(api:ApiFunction) = 
        result {
            do! api "Move 100"
            do! api "Turn 120"
            do! api "Move 100"
            do! api "Turn 120"
            do! api "Move 100"
            do! api "Turn 120"
            }

// -----------------------------
// Turtle Api Tests for "Pass in a single function" design
// -----------------------------

(*
let api = TurtleImplementation_PassInSingleFunction.normalSize()
TurtleApiClient_PassInSingleFunction.drawTriangle(api) 

let api = TurtleImplementation_PassInSingleFunction.halfSize()
TurtleApiClient_PassInSingleFunction.drawTriangle(api) 
*)
