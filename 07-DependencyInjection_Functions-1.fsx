(* ======================================
07-DependencyInjection_Functions-1.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #7: Dependency injection using functions (v1: pass in all functions)

In this design, an API layer communicates via one or more functions that are passed in as parameters to the API call.
These functions are typically partially applied so that the call site is decoupled from the "injection"

No interface is passed to the constructor.



====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

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

            // return Ok of unit, or Error
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
                Error (InvalidCommand commandStr)

// -----------------------------
// Turtle Implementations for "Pass in all functions" design
// -----------------------------

module TurtleImplementation_PassInAllFunctions = 
    
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
            } |> ignore
           

// -----------------------------
// Turtle Api Tests for "Pass in all functions" design
// -----------------------------

do
    let apiFn = TurtleImplementation_PassInAllFunctions.normalSize()  // string -> Result<unit,ErrorMessage>
    TurtleApiClient_PassInAllFunctions.drawTriangle(apiFn) 

do
    let apiFn = TurtleImplementation_PassInAllFunctions.halfSize()
    TurtleApiClient_PassInAllFunctions.drawTriangle(apiFn) 

do
    let mockApi s = 
        printfn "[MockAPI] %s" s
        Ok ()
    TurtleApiClient_PassInAllFunctions.drawTriangle(mockApi) 

