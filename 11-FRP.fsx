(* ======================================
11-FRP.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #11: Functional Retroactive Programming -- Business logic is based on reacting to earlier events

In this design, the "write-side" follows the same pattern as the event-sourcing example.
A client sends a Command to a CommandHandler, which converts that to a list of events and stores them in an EventStore.

However in this design, the CommandHandler only updates state and does NOT do any complex business logic.

The domain logic is done on the "read-side", by listening to events emitted from the event store.

====================================== *)

#load "Common.fsx"
#load "FPTurtleLib.fsx"

open System
open Common
open FPTurtleLib

// ======================================
// EventStore
// ======================================

// Same code as event sourcing example
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
// (same code as event sourcing example)
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
// (same code as event sourcing example)
// ====================================

module CommandHandler = 

    /// Handle a command and return the new state of the turtle
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

    /// The type representing a function that gets the StateChangedEvents for a turtle id
    /// The oldest events are first
    type GetStateChangedEventsForId =
         TurtleId -> StateChangedEvent list

    /// The type representing a function that saves a TurtleEvent 
    type SaveTurtleEvent = 
        TurtleId -> TurtleEvent -> unit

    let eventsFromCommand log command stateBeforeCommand =

        // --------------------------
        // create the StateChangedEvent
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


    /// process a command
    let commandHandler 
        (log:string -> unit) 
        (getEvents:GetStateChangedEventsForId) 
        (saveTurtleEvent:SaveTurtleEvent) 
        (command:TurtleCommand) =

        /// First load all the events
        let eventHistory = 
            getEvents command.turtleId
        
        /// Then, recreate the state before the command
        let stateBeforeCommand = 
            let historyLogger = ignore // no logging
            eventHistory |> List.fold (applyEvent historyLogger) Turtle.initialTurtleState
        
        /// Get the newest events from the command and the stateBeforeCommand
        /// use the supplied logger for this bit
        let events = eventsFromCommand log command stateBeforeCommand 
        
        // store the events
        events |> List.iter (saveTurtleEvent command.turtleId)

// ====================================
// EventProcessors 
// ====================================

module EventProcessors = 

    // filter to choose only TurtleEvents
    let turtleFilter ev = 
        match box ev with
        | :? TurtleEvent as tev -> Some tev
        | _ -> None

    // filter to choose only MovedEvents from TurtleEvents
    let moveFilter = function 
        | MovedEvent ev -> Some ev
        | _ -> None

    // filter to choose only StateChangedEvent from TurtleEvents
    let stateChangedEventFilter = function 
        | StateChangedEvent ev -> Some ev
        | _ -> None

    /// Physically move the turtle
    let physicalTurtleProcessor (eventStream:IObservable<Guid*obj>) =

        // the function that handles the input from the observable
        let subscriberFn (ev:MovedEvent) =
            let colorText = 
                match ev.penColor with
                | Some color -> sprintf "line of color %A" color
                | None -> "no line"
            printfn "[turtle  ]: Moved from (%0.2f,%0.2f) to (%0.2f,%0.2f) with %s" 
                ev.startPos.x ev.startPos.y ev.endPos.x ev.endPos.y colorText 

        // start with all events
        eventStream
        // filter the stream on just TurtleEvents
        |> Observable.choose (function (id,ev) -> turtleFilter ev)
        // filter on just MovedEvents
        |> Observable.choose moveFilter
        // handle these
        |> Observable.subscribe subscriberFn 

    /// Draw lines on a graphics device
    let graphicsProcessor (eventStream:IObservable<Guid*obj>) =

        // the function that handles the input from the observable
        let subscriberFn (ev:MovedEvent) =
            match ev.penColor with
            | Some color -> 
                printfn "[graphics]: Draw line from (%0.2f,%0.2f) to (%0.2f,%0.2f) with color %A" 
                    ev.startPos.x ev.startPos.y ev.endPos.x ev.endPos.y color
            | None -> 
                ()  // do nothing

        // start with all events
        eventStream
        // filter the stream on just TurtleEvents
        |> Observable.choose (function (id,ev) -> turtleFilter ev)
        // filter on just MovedEvents
        |> Observable.choose moveFilter
        // handle these
        |> Observable.subscribe subscriberFn 

    /// Listen for "moved" events and aggregate them to keep
    /// track of the total ink used
    let inkUsedProcessor (eventStream:IObservable<Guid*obj>) =

        // Accumulate the total distance moved so far when a new event happens
        let accumulate distanceSoFar (ev:StateChangedEvent) =
            match ev with
            | Moved dist -> 
                distanceSoFar + dist 
            | _ -> 
                distanceSoFar 

        // the function that handles the input from the observable
        let subscriberFn distanceSoFar  =
            printfn "[ink used]: %0.2f" distanceSoFar  

        // start with all events
        eventStream
        // filter the stream on just TurtleEvents
        |> Observable.choose (function (id,ev) -> turtleFilter ev)
        // filter on just StateChangedEvent
        |> Observable.choose stateChangedEventFilter
        // accumulate total distance
        |> Observable.scan accumulate 0.0
        // handle these
        |> Observable.subscribe subscriberFn 

    /// Listen for "moved" events and aggregate them to keep
    /// track of the total distance moved
    /// NEW! No duplicate events! 
    let inkUsedProcessorNoDups (eventStream:IObservable<Guid*obj>) =

        // Accumulate the total distance moved so far when a new event happens
        let accumulate (prevDist,currDist) (ev:StateChangedEvent) =
            let newDist =
                match ev with
                | Moved dist -> 
                    currDist + dist
                | _ -> 
                    currDist
            (currDist, newDist)

        // convert unchanged events to None so they can be filtered out with "choose"
        let changedDistanceOnly (currDist, newDist) =
            if currDist <> newDist then 
                Some newDist 
            else 
                None

        // the function that handles the input from the observable
        let subscriberFn distanceSoFar  =
            printfn "[ink used]: %0.2f" distanceSoFar  

        // start with all events
        eventStream
        // filter the stream on just TurtleEvents
        |> Observable.choose (function (id,ev) -> turtleFilter ev)
        // filter on just StateChangedEvent
        |> Observable.choose stateChangedEventFilter
        // NEW! accumulate total distance as pairs
        |> Observable.scan accumulate (0.0,0.0)   
        // NEW! filter out when distance has not changed
        |> Observable.choose changedDistanceOnly
        // handle these
        |> Observable.subscribe subscriberFn 


// ====================================
// CommandHandlerClient
// ====================================

module CommandHandlerClient = 
    open CommandHandler

    let eventStore = EventStore()

    /// create a command handler
    let makeCommandHandler = 
        let logger = ignore // no logging on write side now, only on read side
        let getEvents id =
            eventStore.Get<TurtleEvent>(id)
        let getStateChangedEvents id =             
            getEvents id 
            |> List.choose (function StateChangedEvent ev -> Some ev | _ -> None)
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
        // clear older events
        eventStore.Clear turtleId   

        // create an event stream from an IEvent
        let eventStream = eventStore.SaveEvent :> IObservable<Guid*obj>

        // register the processors
        use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream 
        use graphicsProcessor = EventProcessors.graphicsProcessor eventStream 
        //use inkUsedProcessor= EventProcessors.inkUsedProcessor eventStream 
        use inkUsedProcessor = EventProcessors.inkUsedProcessorNoDups eventStream 

        let handler = makeCommandHandler
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
            
    let drawThreeLines() = 
        // clear older events
        eventStore.Clear turtleId   

        // create an event stream from an IEvent
        let eventStream = eventStore.SaveEvent :> IObservable<Guid*obj>

        // register the processors
        use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream 
        use graphicsProcessor = EventProcessors.graphicsProcessor eventStream 
        //use inkUsedProcessor= EventProcessors.inkUsedProcessor eventStream 
        use inkUsedProcessor = EventProcessors.inkUsedProcessorNoDups eventStream 

        let handler = makeCommandHandler
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
        // clear older events
        eventStore.Clear turtleId   

        // create an event stream from an IEvent
        let eventStream = eventStore.SaveEvent :> IObservable<Guid*obj>

        // register the processors
        use physicalTurtleProcessor = EventProcessors.physicalTurtleProcessor eventStream 
        use graphicsProcessor = EventProcessors.graphicsProcessor eventStream 
        //use inkUsedProcessor= EventProcessors.inkUsedProcessor eventStream 
        use inkUsedProcessor = EventProcessors.inkUsedProcessorNoDups eventStream 

        let angle = 180.0 - (360.0/float n) 
        let angleDegrees = angle * 1.0<Degrees>
        let handler = makeCommandHandler

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


