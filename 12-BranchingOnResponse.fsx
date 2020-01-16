(* ======================================
12-BranchingOnResponse.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #12: Monadic control flow -- Making decisions in the turtle computation expression

In this design, the turtle can reply to certain commands with errors.

The code demonstrates how the client can make decisions inside the turtle computation expression
while the state is being passed around behind the scenes.

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib2.fsx"

open Common
open FPTurtleLib2

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
        let (TurtleStateComputation innerFn) = turtle 
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
        member this.Zero(x) = returnT ()

    // create an instance of the computation expression builder
    let turtle = TurtleBuilder()
     
// ======================================
// TurtleComputationClient
// ======================================

module TurtleComputationClient = 

    open TurtleStateComputation

    /// Function to log a message
    let log message =
        printfn "%s" message 

    let initialTurtleState =
        Turtle.initialTurtleState

    // ----------------------------------------
    // monadic versions of the Turtle functions
    // ----------------------------------------

    let move dist = 
        toComputation (Turtle.move log dist)
    // val move : Distance -> TurtleStateComputation<MoveResponse>

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
        toComputation (Turtle.setColor log color)
    // val setColor : PenColor -> TurtleStateComputation<SetColorResponse>


    // ----------------------------------------
    // draw various things
    // ----------------------------------------

    let handleMoveResponse moveResponse = turtle {
        match moveResponse with
        | Turtle.MoveOk -> 
            () // do nothing
        | Turtle.HitABarrier ->
            // turn 90 before trying again
            printfn "Oops -- hit a barrier -- turning"
            do! turn 90.0<Degrees>
        }

(*
// it is an error to NOT response to `move` now!

    let drawShape() = 
        // define a set of instructions 
        let t = turtle {
            do! move 60.0   
            // error FS0001: 
            // This expression was expected to have type
            //    Turtle.MoveResponse    
            // but here has type
            //     unit    
            do! move 60.0 
            } 

        // finally, run the monad using the initial state
        runT t initialTurtleState 
*)

    let drawShapeWithoutResponding() = 
        // define a set of instructions 
        let t = turtle {
            let! response = move 60.0 
            let! response = move 60.0 
            let! response = move 60.0 
            return ()
            } 

        // finally, run the monad using the initial state
        runT t initialTurtleState 

    let drawShape() = 
        // define a set of instructions 
        let t = turtle {
            let! response = move 60.0 
            do! handleMoveResponse response 

            let! response = move 60.0 
            do! handleMoveResponse response 

            let! response = move 60.0 
            do! handleMoveResponse response 
            } 

        // finally, run the monad using the initial state
        runT t initialTurtleState 


// ======================================
// Turtle Monad Tests
// ======================================

(*
TurtleComputationClient.drawShapeWithoutResponding()
TurtleComputationClient.drawShape() 
*)

