(* ======================================
13-Interpreter-v1.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #13: The interpreter pattern

In this design, the client builds a data structure (`TurtleProgram`) that represents the instructions.

This Turtle Program can then interpreted later in various ways

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib2.fsx"

open Common
open FPTurtleLib2

// ============================================================================
// Turtle Program V0 -- doesn't work as command and response are not linked
// ============================================================================
module TurtleProgram_v0 = 
    open Turtle

    // we send this to the turtle...
    type TurtleCommand = 
        | Move of Distance 
        | Turn of Angle
        | PenUp
        | PenDown
        | SetColor of PenColor

    // ... and the turtle replies with one of these
    type TurtleResponse = 
        | Moved of MoveResponse
        | Turned 
        | PenWentUp
        | PenWentDown
        | ColorSet of SetColorResponse


// ============================================================================
// need pairs 
// ============================================================================

// Move command => pair of (Move command parameters), (function MoveResponse -> something)
// Turn command => pair of (Turn command parameters), (function unit -> something)


// ============================================================================
// so, steal design from a recursive list structure like "List"
// ============================================================================

module ListExample = 

    type List<'a> = 
        | Empty
        | Cons of ('a * List<'a>)

// ============================================================================
// Turtle Program V1 
//  * needs an explicit Stop case
//  * not generic
// ============================================================================

module TurtleProgram_v1 = 

    open Turtle

    /// Create a union type to represent each instruction
    type TurtleProgram = 
        //         (input params)  (response)
        | Stop
        | Move     of Distance   * (MoveResponse -> TurtleProgram)
        | Turn     of Angle      * (unit -> TurtleProgram)
        | PenUp    of (* none *)   (unit -> TurtleProgram)
        | PenDown  of (* none *)   (unit -> TurtleProgram)
        | SetColor of PenColor   * (SetColorResponse -> TurtleProgram)

// ------------------------
// Example of Turtle Program V1 object
// ------------------------

module TurtleProgram_v1_Example = 
    open TurtleProgram_v1

    let drawTriangle = 
        Move (100.0, fun response -> 
        Turn (120.0<Degrees>, fun () -> 
        Move (100.0, fun response -> 
        Turn (120.0<Degrees>, fun () -> 
        Move (100.0, fun response -> 
        Turn (120.0<Degrees>, fun () -> 
        Stop))))))

    // val drawTriangle : TurtleProgram

// ------------------------
// Interpreters for Turtle Program v1
// ------------------------

module TurtleProgram_v1_Interpreter = 
    open TurtleProgram_v1

    /// Interpret by calling the turtle functions
    let rec interpretAsTurtle state program =
        let log = printfn "%s"

        match program  with
        | Stop -> 
            state
        | Move (dist,next) ->
            let result,newState = Turtle.move log dist state 
            let nextProgram = next result  // compute the next step
            interpretAsTurtle newState nextProgram 
        | Turn (angle,next) ->
            let newState = Turtle.turn log angle state 
            let nextProgram = next()       // compute the next step
            interpretAsTurtle newState nextProgram 
        | PenUp next ->
            let newState = Turtle.penUp log state 
            let nextProgram = next()
            interpretAsTurtle newState nextProgram 
        | PenDown next -> 
            let newState = Turtle.penDown log state 
            let nextProgram = next()
            interpretAsTurtle newState nextProgram 
        | SetColor (color,next) ->
            let result,newState = Turtle.setColor log color state 
            let nextProgram = next result
            interpretAsTurtle newState nextProgram 

    /// Interpret by accumulating distance
    let rec interpretAsDistance distanceSoFar program =
        let recurse = interpretAsDistance 
        let log = printfn "%s"
        
        match program with
        | Stop -> 
            distanceSoFar
        | Move (dist,next) ->
            let newDistanceSoFar = distanceSoFar + dist
            let result = Turtle.MoveOk   // hard-code result
            let nextProgram = next result 
            recurse newDistanceSoFar nextProgram 
        | Turn (angle,next) ->
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | PenUp next ->
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | PenDown next -> 
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | SetColor (color,next) ->
            // no change in distanceSoFar
            let result = Turtle.ColorOk   // hard-code result
            let nextProgram = next result
            recurse distanceSoFar nextProgram 

// ------------------------
// TurtleProgram_v1_Interpreter Tests
// ------------------------
do 
    let program = TurtleProgram_v1_Example.drawTriangle
    let interpret = TurtleProgram_v1_Interpreter.interpretAsTurtle   // choose an interpreter 
    let initialState = Turtle.initialTurtleState
    interpret initialState program |> ignore

    // output
    (*
    Move 100.0
    ...Draw line from (0.0,0.0) to (100.0,0.0) using Black
    Turn 120.0
    Move 100.0
    ...Draw line from (100.0,0.0) to (50.0,86.6) using Black
    Turn 120.0
    Move 100.0
    ...Draw line from (50.0,86.6) to (0.0,0.0) using Black
    Turn 120.0
    *)

do 
    let program = TurtleProgram_v1_Example.drawTriangle               // same program  
    let interpret = TurtleProgram_v1_Interpreter.interpretAsDistance  // choose an interpreter 
    let initialState = 0.0
    interpret initialState program |> printfn "Total distance moved is %0.1f"

    // output
    (*
    Total distance moved is 300.0
    *)

// ============================================================================
// Turtle Program v1 computation expression
//  * changed type to be generic so that bind works properly
// ============================================================================

module TurtleProgram_v1_Workflow =

    open Turtle

    type TurtleProgram<'a> = 
        | Stop     of 'a
        | Move     of Distance * (MoveResponse -> TurtleProgram<'a>)
        | Turn     of Angle    * (unit -> TurtleProgram<'a>)
        | PenUp    of            (unit -> TurtleProgram<'a>)
        | PenDown  of            (unit -> TurtleProgram<'a>)
        | SetColor of PenColor * (SetColorResponse -> TurtleProgram<'a>)

    let returnT x = 
        Stop x  

    let rec bindT f inst  = 
        match inst with
        | Stop x -> 
            f x
        | Move(dist,next) -> 
            (*
            Move(dist,fun response -> (bindT f)(next response)) 
            *)
            // "next >> bindT f" is a shorter version of function response
            Move(dist,next >> bindT f)
        | Turn(angle,next) -> 
            Turn(angle,next >> bindT f)
        | PenUp(next) -> 
            PenUp(next >> bindT f)
        | PenDown(next) -> 
            PenDown(next >> bindT f)
        | SetColor(color,next) -> 
            SetColor(color,next >> bindT f)

    // define a computation expression builder
    type TurtleProgramBuilder() =
        member this.Return(x) = returnT x
        member this.Bind(x,f) = bindT f x
        member this.Zero(x) = returnT ()

    // create an instance of the computation expression builder
    let turtleProgram = TurtleProgramBuilder()


// ------------------------
// Interpreters for Turtle Program v1 workflow
// ------------------------

module TurtleProgram_v1_WorkflowInterpreter = 
    open TurtleProgram_v1_Workflow

    /// Interpret by calling the turtle functions
    let rec interpretAsTurtle state program =
        let log = printfn "%s"

        match program  with
        | Stop x -> 
            state
        | Move (dist,next) ->
            let result,newState = Turtle.move log dist state 
            let nextProgram = next result 
            interpretAsTurtle newState nextProgram 
        | Turn (angle,next) ->
            let newState = Turtle.turn log angle state 
            let nextProgram = next()
            interpretAsTurtle newState nextProgram 
        | PenUp next ->
            let newState = Turtle.penUp log state 
            let nextProgram = next()
            interpretAsTurtle newState nextProgram 
        | PenDown next -> 
            let newState = Turtle.penDown log state 
            let nextProgram = next()
            interpretAsTurtle newState nextProgram 
        | SetColor (color,next) ->
            let result,newState = Turtle.setColor log color state 
            let nextProgram = next result
            interpretAsTurtle newState nextProgram 

    /// Interpret by accumulating distance
    let rec interpretAsDistance distanceSoFar program =
        let recurse = interpretAsDistance 
        let log = printfn "%s"
        
        match program with
        | Stop x -> 
            distanceSoFar
        | Move (dist,next) ->
            let newDistanceSoFar = distanceSoFar + dist
            let result = Turtle.MoveOk   // hard-code result
            let nextProgram = next result 
            recurse newDistanceSoFar nextProgram 
        | Turn (angle,next) ->
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | PenUp next ->
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | PenDown next -> 
            // no change in distanceSoFar
            let nextProgram = next()
            recurse distanceSoFar nextProgram 
        | SetColor (color,next) ->
            // no change in distanceSoFar
            let result = Turtle.ColorOk   // hard-code result
            let nextProgram = next result
            recurse distanceSoFar nextProgram 


// ------------------------
// Example using workflow
//  * because bind is not generic, code fails to compile
// ------------------------

module TurtleProgram_v1_WorkflowExample = 

    open TurtleProgram_v1_Workflow

    // helper functions
    let stop = fun x -> Stop x
    let move dist  = Move (dist, stop)
    let turn angle  = Turn (angle, stop)
    let penUp  = PenUp stop 
    let penDown  = PenDown stop 
    let setColor color = SetColor (color,stop)

    let handleMoveResponse log moveResponse = turtleProgram {
        match moveResponse with
        | Turtle.MoveOk -> 
            ()
        | Turtle.HitABarrier ->
            // turn 90 before trying again
            log "Oops -- hit a barrier -- turning"
            let! x = turn 90.0<Degrees>
            ()
        }

    // example
    let drawTwoLines log = turtleProgram {
        let! response = move 60.0
        do! handleMoveResponse log response 
        let! response = move 60.0
        do! handleMoveResponse log response 
        }
    // val drawTwoLines: TurtleProgram<unit>

// ------------------------
// Interpret a workflow
// ------------------------

// Interpret as turtle
do 
    let log = printfn "%s"
    let program = TurtleProgram_v1_WorkflowExample.drawTwoLines log 
    let interpret = TurtleProgram_v1_WorkflowInterpreter.interpretAsTurtle 
    let initialState = Turtle.initialTurtleState
    interpret initialState program |> ignore

// Interpret as distance
do 
    let log = printfn "%s"
    let program = TurtleProgram_v1_WorkflowExample.drawTwoLines log 
    let interpret = TurtleProgram_v1_WorkflowInterpreter.interpretAsDistance 
    let initialState = 0.0
    interpret initialState program |> printfn "Total distance moved is %0.1f"

