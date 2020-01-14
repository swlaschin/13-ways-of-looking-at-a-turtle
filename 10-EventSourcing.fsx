(* ======================================
10-EventSourcing.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #10: Event sourcing -- Building state from a list of past events

In this design, the client sends a `Command` to a `CommandHandler`.
The CommandHandler converts that to a list of events and stores them in an `EventStore`.

In order to know how to process a Command, the CommandHandler builds the current state
from scratch using the past events associated with that particular turtle.

Neither the client nor the command handler needs to track state.  Only the EventStore is mutable.

====================================== *)


#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib

// ======================================
// EventStore
// ======================================

type EventStore() = 
    // private mutable data
    let eventDict = System.Collections.Generic.Dictionary<System.Guid,obj list>()
    
    let saveEvent = new Event<System.Guid * obj>()

    /// Triggered when something is saved
    member this.SaveEvent = 
        saveEvent.Publish 

    /// save an event to storage
    member this.Save(eventId,event) = 
        match eventDict.TryGetValue eventId with
        | true,eventList -> 
            let newList = event :: eventList     // store newest in front
            eventDict.[eventId] <- newList 
        | false, _ -> 
            let newList = [event]
            eventDict.[eventId] <- newList 
        saveEvent.Trigger(eventId,event)

    /// get all events associated with the specified eventId
    member this.Get<'a>(eventId) = 
        match eventDict.TryGetValue eventId with
        | true,eventList -> 
            eventList 
            |> Seq.cast<'a> |> Seq.toList  // convert to typed list
            |> List.rev  // reverse so that oldest events are first
        | false, _ -> 
            []

    /// clear all events associated with the specified eventId
    member this.Clear(eventId) = 
        eventDict.[eventId] <- []

// ====================================
// Common types for event sourcing
// ====================================

type TurtleId = System.Guid

/// A desired action on a turtlr
type TurtleCommandAction = 
    | Move of Distance 
    | Turn of Angle
    | PenUp 
    | PenDown 
    | SetColor of PenColor

/// A command representing a desired action addressed to a specific turtle
type TurtleCommand = {
    turtleId : TurtleId
    action : TurtleCommandAction 
    }

/// An event representing a state change that happened
type StateChangedEvent = 
    | Moved of Distance 
    | Turned of Angle
    | PenWentUp 
    | PenWentDown 
    | ColorChanged of PenColor

/// An event representing a move that happened
/// This can be easily translated into a line-drawing activity on a canvas
type MovedEvent = {
    startPos : Position 
    endPos : Position 
    penColor : PenColor option
    }

/// A union of all possible events
type TurtleEvent = 
    | StateChangedEvent of StateChangedEvent
    | MovedEvent of MovedEvent

// ====================================
// CommandHandler 
// ====================================

module CommandHandler = 

    /// Apply an event to the current state and return the new state of the turtle
    let applyEvent log oldState event =
        match event with
        | Moved distance ->
            Turtle.move log distance oldState 
        | Turned angle ->
            Turtle.turn log angle oldState 
        | PenWentUp ->
            Turtle.penUp log oldState 
        | PenWentDown ->
            Turtle.penDown log oldState 
        | ColorChanged color ->
            Turtle.setColor log color oldState 

    // Determine what events to generate, based on the command and the state.
    let eventsFromCommand log command stateBeforeCommand =

        // --------------------------
        // create the StateChangedEvent from the TurtleCommand
        let stateChangedEvent = 
            match command.action with
            | Move dist -> Moved dist
            | Turn angle -> Turned angle
            | PenUp -> PenWentUp 
            | PenDown -> PenWentDown 
            | SetColor color -> ColorChanged color

        // --------------------------
        // calculate the current state from the new event
        let stateAfterCommand = 
            applyEvent log stateBeforeCommand stateChangedEvent

        // --------------------------
        // create the MovedEvent 
        let startPos = stateBeforeCommand.position 
        let endPos = stateAfterCommand.position 
        let penColor = 
            if stateBeforeCommand.penState=Down then
                Some stateBeforeCommand.color
            else
                None                        

        let movedEvent = {
            startPos = startPos 
            endPos = endPos 
            penColor = penColor
            }

        // --------------------------
        // return the list of events
        if startPos <> endPos then
            // if the turtle has moved, return both the stateChangedEvent and the movedEvent 
            // lifted into the common TurtleEvent type
            [ StateChangedEvent stateChangedEvent; MovedEvent movedEvent]                
        else
            // if the turtle has not moved, return just the stateChangedEvent 
            [ StateChangedEvent stateChangedEvent]                


    /// The type representing a function that gets the StateChangedEvents for a turtle id
    /// The oldest events are first
    type GetStateChangedEventsForId =
         TurtleId -> StateChangedEvent list

    /// The type representing a function that saves a TurtleEvent 
    type SaveTurtleEvent = 
        TurtleId -> TurtleEvent -> unit

    /// main function : process a command
    let commandHandler 
        (log:string -> unit) 
        (getEvents:GetStateChangedEventsForId) 
        (saveEvent:SaveTurtleEvent) 
        (command:TurtleCommand) =

        /// First load all the events from the event store
        let eventHistory = 
            getEvents command.turtleId
        
        /// Then, recreate the state before the command
        let stateBeforeCommand = 
            let nolog = ignore // no logging when recreating state
            eventHistory 
            |> List.fold (applyEvent nolog) Turtle.initialTurtleState
        
        /// Construct the events from the command and the stateBeforeCommand
        /// Do use the supplied logger for this bit
        let events = eventsFromCommand log command stateBeforeCommand 
        
        // store the events in the event store
        events |> List.iter (saveEvent command.turtleId)

// ====================================
// CommandHandlerClient
// ====================================

module CommandHandlerClient = 
    open CommandHandler

    // filter to choose only StateChangedEvent from TurtleEvents
    let stateChangedEventFilter = function 
        | StateChangedEvent ev -> Some ev
        | _ -> None

    /// create a command handler
    let makeCommandHandler() = 
        let logger = printfn "%s"
        let eventStore = EventStore()
        let getStateChangedEvents id =             
            eventStore.Get<TurtleEvent>(id)
            |> List.choose stateChangedEventFilter
        let saveEvent id ev =             
            eventStore.Save(id,ev)
        commandHandler logger getStateChangedEvents saveEvent

    // Command versions of standard actions   
    let turtleId = System.Guid.NewGuid()
    let move dist = {turtleId=turtleId; action=Move dist} 
    let turn angle = {turtleId=turtleId; action=Turn angle} 
    let penDown = {turtleId=turtleId; action=PenDown} 
    let penUp = {turtleId=turtleId; action=PenUp} 
    let setColor color = {turtleId=turtleId; action=SetColor color} 

    let drawTriangle() = 
        let handler = makeCommandHandler()
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
            
    let drawThreeLines() = 
        let handler = makeCommandHandler()
        // draw black line 
        handler penDown
        handler (setColor Black)
        handler (move 100.0) 
        // move without drawing
        handler (penUp)
        handler (turn 90.0<Degrees>)
        handler (move 100.0)
        handler (turn 90.0<Degrees>)
        // draw red line 
        handler penDown
        handler (setColor Red)
        handler (move 100.0)
        // move without drawing
        handler penUp
        handler (turn 90.0<Degrees>)
        handler (move 100.0)
        handler (turn 90.0<Degrees>)
        // back home at (0,0) with angle 0
        // draw diagonal blue line 
        handler penDown
        handler (setColor Blue)
        handler (turn 45.0<Degrees>)
        handler (move 100.0)

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n) 
        let angleDegrees = angle * 1.0<Degrees>
        let handler = makeCommandHandler()

        // define a function that draws one side
        let drawOneSide sideNumber = 
            handler (move 100.0)
            handler (turn angleDegrees)

        // repeat for all sides
        for i in [1..n] do
            drawOneSide i


// ======================================
// Tests
// ======================================

(*
CommandHandlerClient.drawTriangle() 
CommandHandlerClient.drawThreeLines() // Doesn't go back home
CommandHandlerClient.drawPolygon 4 
*)


