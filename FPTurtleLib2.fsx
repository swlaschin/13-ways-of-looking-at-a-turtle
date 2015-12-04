(* ======================================
FPTurtleLib2.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Common code for FP-style immutable turtle functions.

Unlike FPTurtleLib.fsx, the Move and SetColor functions return a response.

====================================== *)

// requires Common.fsx to be loaded by parent file
// Uncomment to use this file standalone
//#load "Common.fsx"

open System
open Common


// ======================================
// Turtle module
// ======================================

module Turtle = 

    type TurtleState = {
        position : Position
        angle : float<Degrees>
        color : PenColor
        penState : PenState
    }
    
    type MoveResponse = 
        | MoveOk 
        | HitABarrier

    type SetColorResponse = 
        | ColorOk
        | OutOfInk

    let initialTurtleState = {
        position = initialPosition
        angle = 0.0<Degrees>
        color = initialColor
        penState = initialPenState
    }                

    // if the position is outside the square (0,0,100,100) 
    // then constrain the position and return HitABarrier
    let checkPosition position =
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

    // note that state is LAST param in all these functions

    let move log distance state =
        log (sprintf "Move %0.1f" distance)
        // calculate new position 
        let newPosition = calcNewPosition distance state.angle state.position 
        // adjust the new position if out of bounds
        let moveResult, newPosition = checkPosition newPosition 
        // draw line if needed
        if state.penState = Down then
            dummyDrawLine log state.position newPosition state.color
        // return the new state and the Move result
        let newState = {state with position = newPosition}
        (moveResult,newState) 

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
        let colorResult = 
            if color = Red then OutOfInk else ColorOk
        log (sprintf "SetColor %A" color)
        // return the new state and the SetColor result
        let newState = {state with color = color}
        (colorResult,newState) 

