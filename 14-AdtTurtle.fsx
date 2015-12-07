(* ======================================
14-AdtTurtle.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #14: Abstract Data Turtle - a private type with an associated module of functions 

In this design, the details of the turtle structure is hidden from the client,
so the it could be changed without breaking any code.

See https://www.reddit.com/r/fsharp/comments/36s0zr/structuring_f_programs_with_abstract_data_types/?
for more on ADTs in F#.

====================================== *)


#load "Common.fsx"

open System
open Common


// ======================================
// Abstract Data Turtle module
// ======================================

module AdtTurtle = 

    (*
    // alternative
    type TurtleState = { something:int }
    type turtle = TurtleState 

    module Turtle =
        let something (t:turtle) = t
    *)

    /// A private structure representing the turtle 
    type Turtle = private {
        position : Position
        angle : float<Degrees>
        color : PenColor
        penState : PenState
    }
    
    /// Functions for manipulating a turtle
    /// "RequireQualifiedAccess" means the module name *must* 
    ///    be used (just like List module)
    /// "ModuleSuffix" is needed so the that module can 
    ///    have the same name as the state type 
    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Turtle =

        /// return a new turtle with the specified color
        let make(initialColor) = {
            position = initialPosition
            angle = 0.0<Degrees>
            color = initialColor
            penState = initialPenState
        }                


        let move log distance state =
            log (sprintf "Move %0.1f" distance)
            // calculate new position 
            let newPosition = calcNewPosition distance state.angle state.position 
            // draw line if needed
            if state.penState = Down then
                dummyDrawLine log state.position newPosition state.color
            // update the state
            {state with position = newPosition}

        let turn log angle state =
            log (sprintf "Turn %0.1f" angle)
            // calculate new angle
            let newAngle = (state.angle + angle) % 360.0<Degrees>
            // update the state
            {state with angle = newAngle}

        let penUp log state =
            log "Pen up" 
            {state with penState = Up}

        let penDown log state =
            log "Pen down" 
            {state with penState = Down}

        let setColor log color state =
            log (sprintf "SetColor %A" color)
            {state with color = color}


// ======================================
// AdtTurtle Client
// ======================================

module AdtTurtleClient = 
    open AdtTurtle

    /// Function to log a message
    let log message =
        printfn "%s" message 

    // versions with log baked in (via partial application)
    let move = Turtle.move log
    let turn = Turtle.turn log
    let penDown = Turtle.penDown log
    let penUp = Turtle.penUp log
    let setColor = Turtle.setColor log

(*
    let initialTurtle = {
        position = initialPosition
        angle = 0.0<Degrees>
        color = initialColor
        penState = initialPenState
    }
    // Compiler error FS1093: 
    //    The union cases or fields of the type 'Turtle'
    //    are not accessible from this code location
*)
(*
    do 
        let turtle = Turtle.make(Red)
        printfn "%A" turtle.position
        // Compiler error FS1093: 
        //    The union cases or fields of the type 'Turtle'
        //    are not accessible from this code location
*)

    let drawTriangle() =
        Turtle.make(Red)
        |> move 100.0 
        |> turn 120.0<Degrees>
        |> move 100.0 
        |> turn 120.0<Degrees>
        |> move 100.0 
        |> turn 120.0<Degrees>
        // back home at (0,0) with angle 0
            
    let drawThreeLines() = 
        Turtle.make(Red)
        // draw black line 
        |> penDown
        |> setColor Black
        |> move 100.0 
        // move without drawing
        |> penUp
        |> turn 90.0<Degrees>
        |> move 100.0 
        |> turn 90.0<Degrees>
        // draw red line 
        |> penDown
        |> setColor Red
        |> move 100.0
        // move without drawing
        |> penUp
        |> turn 90.0<Degrees>
        |> move 100.0 
        |> turn 90.0<Degrees>
        // back home at (0,0) with angle 0
        // draw diagonal blue line 
        |> penDown
        |> setColor Blue
        |> turn 45.0<Degrees>
        |> move 100.0

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n) 
        let angleDegrees = angle * 1.0<Degrees>
        let initialTurtle = Turtle.make(Red)

        // define a function that draws one side
        let oneSide state sideNumber = 
            state
            |> move 100.0 
            |> turn angleDegrees 

        // repeat for all sides
        [1..n] 
        |> List.fold oneSide initialTurtle 

// ======================================
// AdtTurtle tests
// ======================================


AdtTurtleClient.drawTriangle() |> ignore
AdtTurtleClient.drawThreeLines() |> ignore
AdtTurtleClient.drawPolygon 4 |> ignore



