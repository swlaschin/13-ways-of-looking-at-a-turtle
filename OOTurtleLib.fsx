(* ======================================
OOTurtleLib.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Common code for OO-style mutable turtle class

====================================== *)

// requires Common.fsx to be loaded by parent file
// Uncomment to use this file standalone
// #load "Common.fsx"

open System
open Common

// ======================================
// Turtle class
// ======================================

// inject a logging function
type Turtle(log) =

    let mutable currentPosition = initialPosition 
    let mutable currentAngle = 0.0<Degrees>
    let mutable currentColor = initialColor
    let mutable currentPenState = initialPenState
    
    member this.Move(distance) =
        log (sprintf "Move %0.1f" distance)
        // calculate new position 
        let newPosition = calcNewPosition distance currentAngle currentPosition 
        // draw line if needed
        if currentPenState = Down then
            dummyDrawLine log currentPosition newPosition currentColor
        // update the state
        currentPosition <- newPosition

    member this.Turn(angle) =
        log (sprintf "Turn %0.1f" angle)
        // calculate new angle
        let newAngle = (currentAngle + angle) % 360.0<Degrees>
        // update the state
        currentAngle <- newAngle 

    member this.PenUp() =
        log "Pen up" 
        currentPenState <- Up

    member this.PenDown() =
        log "Pen down" 
        currentPenState <- Down

    member this.SetColor(color) =
        log (sprintf "SetColor %A" color)
        currentColor <- color


