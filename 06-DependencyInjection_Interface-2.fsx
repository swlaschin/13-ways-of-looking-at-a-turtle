(* ======================================
06-DependencyInjection_Interface.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #6: Dependency injection (using interfaces) - v2: records of functions

In this design, an API layer communicates with a Turtle Interface (OO style) or a record of TurtleFunctions (FP style)
rather than directly with a turtle.
The client injects a specific turtle implementation via the API's constructor.

====================================== *)


#load "Common.fsx"
#load "OOTurtleLib.fsx"
#load "FPTurtleLib.fsx"

open Common


// ============================================================================
// Dependency Injection (records of functions)
// ============================================================================

// ----------------------------
// Turtle Interface 
// ----------------------------

// a quick alias for readability
type TurtleState = FPTurtleLib.Turtle.TurtleState 

type TurtleFunctions = {
    move : Distance -> TurtleState -> TurtleState
    turn : Angle -> TurtleState -> TurtleState
    penUp : TurtleState -> TurtleState
    penDown : TurtleState -> TurtleState
    setColor : PenColor -> TurtleState -> TurtleState
    }
// Note that there are NO "units" in these functions, unlike the OO version.


// ----------------------------
// Turtle Api Layer 
// ----------------------------

module TurtleApiLayer_FP = 

    open Result
    open FPTurtleLib
    
    /// Function to log a message
    let log message =
        printfn "%s" message 

    let initialTurtleState = Turtle.initialTurtleState

    type ErrorMessage = 
        | InvalidDistance of string
        | InvalidAngle of string
        | InvalidColor of string
        | InvalidCommand of string

    // convert the distance parameter to a float, or throw an exception
    let validateDistance distanceStr =
        try
            Ok (float distanceStr)
        with
        | ex -> 
            Error (InvalidDistance distanceStr)

    // convert the angle parameter to a float, or throw an exception
    let validateAngle angleStr =
        try
            Ok ((float angleStr) * 1.0<Degrees>)
        with
        | ex -> 
            Error (InvalidAngle angleStr)

    // convert the color parameter to a PenColor, or throw an exception
    let validateColor colorStr =
        match colorStr with
        | "Black" -> Ok Black
        | "Blue" -> Ok Blue
        | "Red" -> Ok Red
        | _ -> 
            Error (InvalidColor colorStr)

    type TurtleApi(turtleFunctions: TurtleFunctions) =

        let mutable state = initialTurtleState

        /// Update the mutable state value
        let updateState newState =
            state <- newState

        /// Execute the command string, and return a Result
        /// Exec : commandStr:string -> Result<unit,ErrorMessage>
        member this.Exec (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            // return Ok of unit, or Error
            match tokens with
            | [ "Move"; distanceStr ] -> result {
                let! distance = validateDistance distanceStr 
                let newState = turtleFunctions.move distance state
                updateState newState
                }
            | [ "Turn"; angleStr ] -> result {
                let! angle = validateAngle angleStr 
                let newState = turtleFunctions.turn angle state
                updateState newState
                }
            | [ "Pen"; "Up" ] -> result {
                let newState = turtleFunctions.penUp state
                updateState newState
                }
            | [ "Pen"; "Down" ] -> result {
                let newState = turtleFunctions.penDown state
                updateState newState
                }
            | [ "SetColor"; colorStr ] -> result {
                let! color = validateColor colorStr
                let newState = turtleFunctions.setColor color state
                updateState newState
                }
            | _ -> 
                Error (InvalidCommand commandStr)
        

// ----------------------------
// Multiple Turtle Implementations 
// ----------------------------

module TurtleImplementation_FP = 
    open FPTurtleLib

    let normalSize() = 
        let log = printfn "%s"
        // return a record of functions
        {
            move = Turtle.move log 
            turn = Turtle.turn log 
            penUp = Turtle.penUp log
            penDown = Turtle.penDown log
            setColor = Turtle.setColor log 
        }

    let halfSize() = 
        let normalSize = normalSize() 
        // return a reduced turtle
        { normalSize with
            move = fun dist -> normalSize.move (dist/2.0) 
        }

// ----------------------------
// Turtle Api Client  
// ----------------------------

module TurtleApiClient_FP = 

    open TurtleApiLayer_FP 
    open Result

    let drawTriangle(api:TurtleApi) = 
        result {
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            } |> ignore

// ----------------------------
// Turtle Api Tests  (FP style)
// ----------------------------

do
    let turtleFns = TurtleImplementation_FP.normalSize()   // a TurtleFunctions type
    let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
    TurtleApiClient_FP.drawTriangle(api) 

do
    let turtleFns = TurtleImplementation_FP.halfSize()
    let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
    TurtleApiClient_FP.drawTriangle(api) 


