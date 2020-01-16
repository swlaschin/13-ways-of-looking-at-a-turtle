(* ======================================
08-StateMonad.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #8: Batch oriented -- Using a state monad (computation expression)

In this design, the client uses the FP Turtle functions directly.

As before, the client must keep track of the current state and pass it into the next function call,
but this time the state is kept out of sight by using a State monad (called `turtle` computation expression here)

As a result, there are no mutables anywhere. 

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib

// ======================================
// TurtleStateComputation
// ======================================

/// Create a type to wrap a function like:
///    oldState -> (a,newState)
type TurtleStateComputation<'a> = 
    TurtleStateComputation of (Turtle.TurtleState -> 'a * Turtle.TurtleState)

/// Functions that work with TurtleStateComputation 
module TurtleStateComputation = 

    let runT turtle state = 
        // pattern match against the turtle
        // to extract the inner function
        let (TurtleStateComputation innerFn) = turtle 
        // run the inner function with the passed in state
        innerFn state

    let returnT x = 
        let innerFn state =
            (x,state)
        TurtleStateComputation innerFn 

    let bindT f xT = 
        let innerFn state =
            let x,state2 = runT xT state
            runT (f x) state2
        TurtleStateComputation innerFn 

    let mapT f  = 
        bindT (f >> returnT)

    let toComputation f = 
        let innerFn state =
            let (result,newState) = f state
            (result,newState)
        TurtleStateComputation innerFn 

    let toUnitComputation f = 
        let f2 state = 
            (),f state
        toComputation f2

    // define a computation expression builder
    type TurtleBuilder() =
        member this.Return(x) = returnT x
        member this.Bind(x,f) = bindT f x

    // create an instance of the computation expression builder
    let turtle = TurtleBuilder()
     
// ======================================
// TurtleComputationClient
// ======================================

module TurtleComputationClient = 

    open TurtleStateComputation 
    open Result

    /// Function to log a message
    let log message =
        printfn "%s" message 

    let initialTurtleState =
        Turtle.initialTurtleState

    // ----------------------------------------
    // monadic versions of the Turtle functions
    // ----------------------------------------

    let move dist = 
        toUnitComputation (Turtle.move log dist)
    // val move : Distance -> TurtleStateComputation<unit>

    let turn angle = 
        toUnitComputation (Turtle.turn log angle)
    // val turn : Angle -> TurtleStateComputation<unit>

    let penDown = 
        toUnitComputation (Turtle.penDown log)
    // val penDown : TurtleStateComputation<unit>

    let penUp = 
        toUnitComputation (Turtle.penUp log)
    // val penUp : TurtleStateComputation<unit>

    let setColor color = 
        toUnitComputation (Turtle.setColor log color)
    // val setColor : PenColor -> TurtleStateComputation<unit>

    // ----------------------------------------
    // draw various things
    // ----------------------------------------

    let drawTriangle() = 
        // define a set of instructions 
        let t = turtle {
            do! move 100.0 
            do! turn 120.0<Degrees>
            do! move 100.0 
            do! turn 120.0<Degrees>
            do! move 100.0 
            do! turn 120.0<Degrees>
            } 

        // finally, run them using the initial state as input
        runT t initialTurtleState 

    let drawThreeLines() = 
        // define a set of instructions 
        let t = turtle {
            // draw black line 
            do! penDown
            do! setColor Black
            do! move 100.0 
            // move without drawing
            do! penUp
            do! turn 90.0<Degrees>
            do! move 100.0 
            do! turn 90.0<Degrees>
            // draw red line 
            do! penDown
            do! setColor Red
            do! move 100.0
            // move without drawing
            do! penUp
            do! turn 90.0<Degrees>
            do! move 100.0 
            do! turn 90.0<Degrees>
            // back home at (0,0) with angle 0
            // draw diagonal blue line 
            do! penDown
            do! setColor Blue
            do! turn 45.0<Degrees>
            do! move 100.0
            }

        // finally, run them using the initial state
        runT t initialTurtleState 

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n) 
        let angleDegrees = angle * 1.0<Degrees>

        // define a function that draws one side
        let oneSide = turtle {
            do! move 100.0 
            do! turn angleDegrees 
            }

        // chain two turtle operations in sequence
        let chain f g  = turtle {
            do! f
            do! g
            } 

        // create a list of operations, one for each side
        let sides = List.replicate n oneSide

        // chain all the sides into one operation
        let all = sides |> List.reduce chain 

        // finally, run them using the initial state
        runT all initialTurtleState 

// ======================================
// Turtle Computation Tests
// ======================================


TurtleComputationClient.drawTriangle() 
TurtleComputationClient.drawThreeLines() 
TurtleComputationClient.drawPolygon 4 


