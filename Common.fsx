(*
Common.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/

*)

open System

// ======================================
// Common types and helper functions
// ======================================

/// An alias for a float
type Distance = float

/// Use a unit of measure to make it clear that the angle is in degrees, not radians
type [<Measure>] Degrees

/// An alias for a float of Degrees
type Angle  = float<Degrees>

/// Enumeration of available pen states
type PenState = Up | Down

/// Enumeration of available pen colors
type PenColor = Black | Red | Blue

/// A structure to store the (x,y) coordinates
type Position = {x:float; y:float}


// ======================================
// Common helper functions
// ======================================

// round a float to two places to make it easier to read
let round2 (flt:float) = Math.Round(flt,2)

/// calculate a new position from the current position given an angle and a distance
let calcNewPosition (distance:Distance) (angle:Angle) currentPos = 
    // Convert degrees to radians with 180.0 degrees = 1 pi radian
    let angleInRads = angle * (Math.PI/180.0) * 1.0<1/Degrees> 
    // current pos
    let x0 = currentPos.x
    let y0 = currentPos.y
    // new pos
    let x1 = x0 + (distance * cos angleInRads)
    let y1 = y0 + (distance * sin angleInRads)
    // return a new Position
    {x=round2 x1; y=round2 y1}
        
/// Default initial state
let initialPosition,initialColor,initialPenState = 
    {x=0.0; y=0.0}, Black, Down

/// Emulating a real implementation for drawing a line
let dummyDrawLine log oldPos newPos color =
    // for now just log it
    log (sprintf "...Draw line from (%0.1f,%0.1f) to (%0.1f,%0.1f) using %A" oldPos.x oldPos.y newPos.x newPos.y color)

/// trim a string 
let trimString (str:string) = str.Trim()

// ======================================
// Result companion module
// ======================================

module Result = 

    let returnR x = 
        Ok x

    // infix version of bind
    let ( >>= ) xR f = 
        Result.bind f xR

    // infix version of map
    let ( <!> ) = Result.map

    let applyR fR xR = 
        fR >>= (fun f ->
        xR >>= (fun x ->
            returnR (f x) ))

    // infix version of apply
    let ( <*> ) = applyR

    // lift a one-parameter function to result world (same as mapR)
    let lift1R f x = f <!> x 

    // lift a two-parameter function to result world
    let lift2R f x y = f <!> x <*> y

    /// Computation Expression
    type ResultBuilder() =
        member this.Bind(m:Result<'a,'error>,f:'a -> Result<'b,'error>) = 
            Result.bind f m
        member this.Return(x) :Result<'a,'error> = 
            returnR x
        member this.ReturnFrom(m) :Result<'a,'error> = 
            m
        member this.Zero() :Result<unit,'error> = 
            this.Return ()
        member this.Combine(m1, f) = 
            this.Bind(m1, f)
        member this.Delay(f) = 
            f
        member this.Run(m) = 
            m()
        member this.TryWith(m:Result<'a,'error>, h: exn -> Result<'a,'error>) =
            try this.ReturnFrom(m)
            with e -> h e
        member this.TryFinally(m:Result<'a,'error>, compensation) =
            try this.ReturnFrom(m)
            finally compensation()
        member this.Using(res:#IDisposable, body) : Result<'b,'error> =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.While(cond, m) =
            if not (cond()) then 
                this.Zero()
            else
                this.Bind(m(), fun _ -> this.While(cond, m))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, fun _ -> body enum.Current)))
        
    let result = ResultBuilder()