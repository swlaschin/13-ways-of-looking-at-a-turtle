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
        Ok (float distanceStr)
    with
    | ex -> 
        Error (InvalidDistance distanceStr)

// convert the angle parameter to a float, or throw an exception
let validateAngle angleStr =
    try
        Ok ((float angleStr) * 1.0<Degrees>)
    with
    | ex -> 
        Error (InvalidAngle angleStr)

// convert the color parameter to a PenColor, or throw an exception
let validateColor colorStr =
    match colorStr with
    | "Black" -> Ok Black
    | "Blue" -> Ok Blue
    | "Red" -> Ok Red
    | _ -> 
        Error (InvalidColor colorStr)
