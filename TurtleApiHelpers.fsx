(* ======================================
TurtleApiHelpers.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Helper functions for Turtle Api Layer

====================================== *)

// requires Common.fsx to be loaded by parent file
// Uncomment to use this file standalone
//#load "Common.fsx"

open System
open Common


// ======================================
// Helper functions for Turtle Api Layer
// ======================================

open Result
    

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
