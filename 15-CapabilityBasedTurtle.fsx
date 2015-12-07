(* ======================================
15-CapabilityBasedTurtle.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #15: API with capabilities

In this design, the turtle exposes a list of functions (capabilities) after each action.
These are the ONLY actions available to the client

More on capability-based security at http://fsharpforfunandprofit.com/posts/capability-based-security/
====================================== *)


#load "Common.fsx"

open System
open Common

// ======================================
// Capability-based Turtle module
// ======================================

module CapBasedTurtle = 

    type Log = string -> unit

    /// A private structure representing the turtle 
    type private TurtleState = {
        position : Position
        angle : float<Degrees>
        color : PenColor
        penState : PenState

        canMove : bool                // new!
        availableInk: Set<PenColor>   // new!
        logger : Log                  // new!
    }

    type MoveResponse = 
        | MoveOk 
        | HitABarrier

    type SetColorResponse = 
        | ColorOk
        | OutOfInk

    type TurtleFunctions = {
        move     : MoveFn option
        turn     : TurnFn
        penUp    : PenUpDownFn 
        penDown  : PenUpDownFn 
        setBlack : SetColorFn  option
        setBlue  : SetColorFn  option
        setRed   : SetColorFn  option
        }
    and MoveFn =      Distance -> (MoveResponse * TurtleFunctions)
    and TurnFn =      Angle    -> TurtleFunctions
    and PenUpDownFn = unit     -> TurtleFunctions
    and SetColorFn =  unit     -> (SetColorResponse * TurtleFunctions)


    /// Functions for manipulating a turtle
    /// "RequireQualifiedAccess" means the module name *must* be used (just like List module)
    /// "ModuleSuffix" is needed so the that module can have the same name as type type (not needed with generic types)
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Turtle =

        // if the position is outside the square (0,0,100,100) 
        // then constrain the position and return HitABarrier
        let private checkPosition position =
            let isOutOfBounds p = 
                p > 100.0 || p < 0.0
            let bringInsideBounds p = 
                max (min p 100.0) 0.0

            if isOutOfBounds position.x || isOutOfBounds position.y then
                let newPos = {
                    x = bringInsideBounds position.x 
                    y = bringInsideBounds position.y }
                HitABarrier,newPos
            else
                MoveOk,position

        /// Function is private! Only accessible to the client via the TurtleFunctions record
        let private move log distance state =

            log (sprintf "Move %0.1f" distance)
            // calculate new position 
            let newPosition = calcNewPosition distance state.angle state.position 
            // adjust the new position if out of bounds
            let moveResult, newPosition = checkPosition newPosition 
            // draw line if needed
            if state.penState = Down then
                dummyDrawLine log state.position newPosition state.color

            // return the new state and the Move result
            let newState = {
                state with 
                 position = newPosition
                 canMove = (moveResult <> HitABarrier)  // NEW! 
                }
            (moveResult,newState) 

        /// Function is private! Only accessible to the client via the TurtleFunctions record
        let private turn log angle state =
            log (sprintf "Turn %0.1f" angle)
            // calculate new angle
            let newAngle = (state.angle + angle) % 360.0<Degrees>
            // NEW!! assume you can always move after turning
            let canMove = true
            // update the state
            {state with angle = newAngle; canMove = canMove} 

        /// Function is private! Only accessible to the client via the TurtleFunctions record
        let private penUp log state =
            log "Pen up" 
            {state with penState = Up}

        /// Function is private! Only accessible to the client via the TurtleFunctions record
        let private penDown log state =
            log "Pen down" 
            {state with penState = Down}

        /// Function is private! Only accessible to the client via the TurtleFunctions record
        let private setColor log color state =
            let colorResult = 
                if color = Red then OutOfInk else ColorOk
            log (sprintf "SetColor %A" color)

            // NEW! remove color ink from available inks
            let newAvailableInk = state.availableInk |> Set.remove color

            // return the new state and the SetColor result
            let newState = {state with color = color; availableInk = newAvailableInk}
            (colorResult,newState) 

        /// Create the TurtleFunctions structure associated with a TurtleState
        let rec private createTurtleFunctions state =
            let ctf = createTurtleFunctions  // alias

            // create the move function,
            // if the turtle can't move, return None
            let move = 
                // the inner function
                let f dist = 
                    let resp, newState = move state.logger dist state
                    (resp, ctf newState)

                // return Some of the inner function
                // if the turtle can move, or None
                if state.canMove then
                    Some f
                else
                    None

            // create the turn function
            let turn angle = 
                let newState = turn state.logger angle state
                ctf newState

            // create the pen state functions
            let penDown() = 
                let newState = penDown state.logger state
                ctf newState

            let penUp() = 
                let newState = penUp state.logger state
                ctf newState

            // create the set color functions
            let setColor color = 
                // the inner function
                let f() = 
                    let resp, newState = setColor state.logger color state
                    (resp, ctf newState)

                // return Some of the inner function 
                // if that color is available, or None
                if state.availableInk |> Set.contains color then
                    Some f
                else
                    None

            let setBlack = setColor Black
            let setBlue = setColor Blue
            let setRed = setColor Red
            
            // return the structure
            {
            move     = move
            turn     = turn
            penUp    = penUp 
            penDown  = penDown 
            setBlack = setBlack
            setBlue  = setBlue  
            setRed   = setRed   
            }

        /// Return the initial turtle.
        /// This is the ONLY public function!
        let make(initialColor, log) = 
            let state = {
                position = initialPosition
                angle = 0.0<Degrees>
                color = initialColor
                penState = initialPenState
                canMove = true
                availableInk = [Black; Blue; Red] |> Set.ofList
                logger = log
            }                
            createTurtleFunctions state




// ======================================
// Client
// ======================================

module CapBasedTurtleClient = 

    open CapBasedTurtle 

    /// Function to log a message
    let log message =
        printfn "%s" message 

    // test that the boundary is hit 
    // after second move of 60
    let testBoundary() =
        let turtleFns = Turtle.make(Red,log)
        match turtleFns.move with
        | None -> 
            log "Error: Can't do move 1"
        | Some moveFn -> 
            let (moveResp,turtleFns) = moveFn 60.0 
            match turtleFns.move with
            | None -> 
                log "Error: Can't do move 2"
            | Some moveFn -> 
                let (moveResp,turtleFns) = moveFn 60.0 
                match turtleFns.move with
                | None -> 
                    log "Error: Can't do move 3"
                | Some moveFn -> 
                    log "Success"
    
    // this time, simplify with Option expression
    type MaybeBuilder() =         
        member this.Return(x) = Some x
        member this.Bind(x,f) = Option.bind f x
        member this.Zero() = Some()
    let maybe = MaybeBuilder()

    /// A function that logs and returns Some(),
    /// for use in the "maybe" workflow
    let logO message =
        printfn "%s" message
        Some ()

    // test that ink is used up
    let testInk() =
        maybe {
        // create a turtle
        let turtleFns = Turtle.make(Black,log)
        
        // attempt to get the "setRed" function
        let! setRedFn = turtleFns.setRed 

        // if so, use it
        let (resp,turtleFns) = setRedFn() 

        // attempt to get the "move" function
        let! moveFn = turtleFns.move 

        // if so, move a distance of 60 with the red ink
        let (resp,turtleFns) = moveFn 60.0 

        // check if the "setRed" function is still available
        do! match turtleFns.setRed with
            | None -> 
                logO "Error: Can no longer use Red ink"
            | Some _ -> 
                logO "Success: Can still use Red ink"
        
        // check if the "setBlue" function is still available
        do! match turtleFns.setBlue with
            | None -> 
                logO "Error: Can no longer use Blue ink"
            | Some _ -> 
                logO "Success: Can still use Blue ink"

        } |> ignore
            

// ======================================
// CapBasedTurtleClient Tests
// ======================================

CapBasedTurtleClient.testBoundary() 
(*
Move 60.0
...Draw line from (0.0,0.0) to (60.0,0.0) using Red
Move 60.0
...Draw line from (60.0,0.0) to (100.0,0.0) using Red
Error: Can't do move 3
*)

CapBasedTurtleClient.testInk() 
(*
SetColor Red
Move 60.0
...Draw line from (0.0,0.0) to (60.0,0.0) using Red
Error: Can no longer use Red ink
Success: Can still use Blue ink
*)
