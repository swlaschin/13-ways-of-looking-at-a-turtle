(* ======================================
06-DependencyInjection_Interface.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #6: Dependency injection (using interfaces) 

In this design, an API layer communicates with a Turtle Interface (OO style) or a record of TurtleFunctions (FP style)
rather than directly with a turtle.
The client injects a specific turtle implementation via the API's constructor.

====================================== *)


#load "Common.fsx"
#load "OOTurtleLib.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open System
open Common
open TurtleApiHelpers // helpers for API validation, etc

// ============================================================================
// Dependency Injection (OO style)
// ============================================================================


// ----------------------------
// Turtle Interface
// ----------------------------

type ITurtle =
    abstract Move : Distance -> unit
    abstract Turn : Angle -> unit
    abstract PenUp : unit -> unit
    abstract PenDown : unit -> unit
    abstract SetColor : PenColor -> unit
    // Note that there are a lot of "units" in these functions.
    // "unit" in a function implies side effects

// ----------------------------
// Turtle Api Layer (OO version)
// ----------------------------

module TurtleApiLayer_OO = 

    exception TurtleApiException of string

    type TurtleApi(turtle: ITurtle) =

        // convert the distance parameter to a float, or throw an exception
        let validateDistance distanceStr =
            try
                float distanceStr 
            with
            | ex -> 
                let msg = sprintf "Invalid distance '%s' [%s]" distanceStr  ex.Message
                raise (TurtleApiException msg)

        // convert the angle parameter to a float, or throw an exception
        let validateAngle angleStr =
            try
                (float angleStr) * 1.0<Degrees> 
            with
            | ex -> 
                let msg = sprintf "Invalid angle '%s' [%s]" angleStr ex.Message
                raise (TurtleApiException msg)

        // convert the color parameter to a PenColor, or throw an exception
        let validateColor colorStr =
            match colorStr with
            | "Black" -> Black
            | "Blue" -> Blue
            | "Red" -> Red
            | _ -> 
                let msg = sprintf "Color '%s' is not recognized" colorStr
                raise (TurtleApiException msg)
                
        /// Execute the command string, or throw an exception
        /// (Exec : commandStr:string -> unit)
        member this.Exec (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString
            match tokens with
            | [ "Move"; distanceStr ] -> 
                let distance = validateDistance distanceStr 
                turtle.Move distance 
            | [ "Turn"; angleStr ] -> 
                let angle = validateAngle angleStr
                turtle.Turn angle  
            | [ "Pen"; "Up" ] -> 
                turtle.PenUp()
            | [ "Pen"; "Down" ] -> 
                turtle.PenDown()
            | [ "SetColor"; colorStr ] -> 
                let color = validateColor colorStr 
                turtle.SetColor color
            | _ -> 
                let msg = sprintf "Instruction '%s' is not recognized" commandStr
                raise (TurtleApiException msg)

// ----------------------------
// Multiple Turtle Implementations (OO version)
// ----------------------------

module TurtleImplementation_OO = 
    open OOTurtleLib

    let normalSize() = 
        let log = printfn "%s"
        let turtle = Turtle(log)
        
        // return an interface wrapped around the Turtle
        {new ITurtle with
            member this.Move dist = turtle.Move dist
            member this.Turn angle = turtle.Turn angle
            member this.PenUp() = turtle.PenUp()
            member this.PenDown() = turtle.PenDown()
            member this.SetColor color = turtle.SetColor color
        }

    let halfSize() = 
        let normalSize = normalSize() 
        
        // return a decorated interface 
        {new ITurtle with
            member this.Move dist = normalSize.Move (dist/2.0)  // halved!!
            member this.Turn angle = normalSize.Turn angle
            member this.PenUp() = normalSize.PenUp()
            member this.PenDown() = normalSize.PenDown()
            member this.SetColor color = normalSize.SetColor color
        }


// ----------------------------
// Turtle Api Client (OO version)
// ----------------------------


module TurtleApiClient_OO = 
    open TurtleApiLayer_OO

    let drawTriangle(api:TurtleApi) = 
        api.Exec "Move 100"
        api.Exec "Turn 120"
        api.Exec "Move 100"
        api.Exec "Turn 120"
        api.Exec "Move 100"
        api.Exec "Turn 120"
 
// ----------------------------
// Turtle API tests (OO version)
// ----------------------------

(*
let iTurtle = TurtleImplementation_OO.normalSize()  // an ITurtle type
let api = TurtleApiLayer_OO.TurtleApi(iTurtle)
TurtleApiClient_OO.drawTriangle(api) 

let iTurtle = TurtleImplementation_OO.halfSize()
let api = TurtleApiLayer_OO.TurtleApi(iTurtle)
TurtleApiClient_OO.drawTriangle(api) 
*)

// ============================================================================
// Dependency Injection (FP style)
// ============================================================================

// ----------------------------
// Turtle Interface (FP style)
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
// Turtle Api Layer  (FP style)
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
            Success (float distanceStr)
        with
        | ex -> 
            Failure (InvalidDistance distanceStr)

    // convert the angle parameter to a float, or throw an exception
    let validateAngle angleStr =
        try
            Success ((float angleStr) * 1.0<Degrees>)
        with
        | ex -> 
            Failure (InvalidAngle angleStr)

    // convert the color parameter to a PenColor, or throw an exception
    let validateColor colorStr =
        match colorStr with
        | "Black" -> Success Black
        | "Blue" -> Success Blue
        | "Red" -> Success Red
        | _ -> 
            Failure (InvalidColor colorStr)

    type TurtleApi(turtleFunctions: TurtleFunctions) =

        let mutable state = initialTurtleState

        /// Update the mutable state value
        let updateState newState =
            state <- newState

        /// Execute the command string, and return a Result
        /// Exec : commandStr:string -> Result<unit,ErrorMessage>
        member this.Exec (commandStr:string) = 
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            // return Success of unit, or Failure
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
                Failure (InvalidCommand commandStr)
        

// ----------------------------
// Multiple Turtle Implementations  (FP style)
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
// Turtle Api Client  (FP style)
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
            }

// ----------------------------
// Turtle Api Tests  (FP style)
// ----------------------------

(*
let turtleFns = TurtleImplementation_FP.normalSize()   // a TurtleFunctions type
let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
TurtleApiClient_FP.drawTriangle(api) 

let turtleFns = TurtleImplementation_FP.halfSize()
let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
TurtleApiClient_FP.drawTriangle(api) 
*)

