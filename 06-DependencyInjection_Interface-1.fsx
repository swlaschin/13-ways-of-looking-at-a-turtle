(* ======================================
06-DependencyInjection_Interface-1.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #6: Dependency injection (using interfaces) -- v1: OO interface

In this design, an API layer communicates with a Turtle Interface (OO style) or a record of TurtleFunctions (FP style)
rather than directly with a turtle.
The client injects a specific turtle implementation via the API's constructor.

====================================== *)


#load "Common.fsx"
#load "OOTurtleLib.fsx"

open Common

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

do
    let iTurtle = TurtleImplementation_OO.normalSize()  // an ITurtle type
    let api = TurtleApiLayer_OO.TurtleApi(iTurtle)
    TurtleApiClient_OO.drawTriangle(api) 

do
    let iTurtle = TurtleImplementation_OO.halfSize()
    let api = TurtleApiLayer_OO.TurtleApi(iTurtle)
    TurtleApiClient_OO.drawTriangle(api) 
