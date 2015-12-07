# Thirteen different ways of implementing a LOGO-style turtle in F#!

Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/

## Requirements for a Turtle

A turtle supports four instructions:

* Move some distance in the current direction.
* Turn a certain number of degrees clockwise or anticlockwise.
* Put the pen down or up. When the pen is down, moving the turtle draws a line.
* Set the pen color (one of black, blue or red).

The turtle must convert these instructions to drawing lines on a canvas or other graphics context.
So the implementation will somehow need to keep track of the turtle position and current state.

## The thirteen implementations

Here are the different implementations:


#### 1. Basic OO -- a class with mutable state

In this design, a simple OO class represents the turtle,
and the client talks to the turtle directly.

*Advantages*

* Simple to implement and understand.

*Disadvantages*

* Stateful code is harder to test.
* The client is coupled to a particular implementation. 

#### 2: Basic FP - a module of functions with immutable state

In this design, the turtle state is immutable. A module contains functions that return a new turtle state,
and the client uses these turtle functions directly.

The client must keep track of the current state and pass it into the next function call.

*Advantages*

* Simple to implement and understand.
* Stateless code is easy to test.
* Because there is no global state, the functions are reusable in other contexts (as we'll see in later versions).

*Disadvantages*

* The client is coupled to a particular implementation. 
* The client has to manage the current turtle state.


#### 3: An API with a object-oriented core

In this design, an API layer communicates with a turtle class
and the client talks to the API layer.

The input to the API are strings, and so the API validates the
input and throws an Exception if there are errors.

*Advantages*

* The turtle implementation is hidden from the client.
* An API at a service boundary supports validation and can be extended to support monitoring, sharding, etc.

*Disadvantages*

* The API is coupled to a particular implementation. 
* The system is very stateful. The client is indirectly coupled to the inner core via shared state which can make testing harder.


#### 4: An API with a functional core

In this design, an API layer communicates with pure turtle functions
and the client talks to the API layer.

The API layer manages the state (rather than the client) by storing a mutable turtle state.

*This approach has been named "Functional Core/Imperative Shell" by [Gary Bernhardt](https://www.youtube.com/watch?v=yTkzNHF6rMs)*

The input to the API are strings, and so the API validates the
input and returns a `Result` containing any errors. 


*Advantages*

* The turtle implementation is hidden from the client.
* An API at a service boundary supports validation and can be extended to support monitoring, internal routing, load balancing, etc.
* The only stateful part of the system is at the boundary. The core is stateless which makes testing easier.

*Disadvantages*

* The API is coupled to a particular implementation. 

#### 5: An API in front of an agent

In this design, an API layer communicates with a TurtleAgent via a message queue
and the client talks to the API layer.

Because the `TurtleAgent` has a typed message queue, where all messages are the same type,
we must combine all possible commands into a single discriminated union type (`TurtleCommand`).

There are no mutables anywhere. The Agent manages the turtle state by 
storing the current state as a parameter in the recursive message processing loop.

*Advantages*

* A great way to protect mutable state without using locks.
* The API is decoupled from a particular implementation via the message queue. 
* The turtle agent is naturally asynchronous.
* Agents can easily be scaled horizontally.

*Disadvantages*

* Agents are stateful and have the same problem as stateful objects:
  * It is harder to reason about your code.
  * Testing is harder. 
  * It is all too easy to create a web of complex dependencies between actors.
* A proper implementation can be overly complex, as supervisors, heartbeats, etc., may be needed.


#### 6: Dependency injection using interfaces

In this design, an API layer communicates with a `ITurtle` interface (OO style) or a record of TurtleFunctions (FP style)
rather than directly with a turtle.

The client injects the turtle implementation later, via the API's constructor.

*Advantages*

* The API is decoupled from a particular implementation via the interface.
* For the FP "record of functions" approach (compared to OO interfaces):
  * Records of functions can be cloned more easily than interfaces.

*Disadvantages*

* Interfaces are more monolithic than individual functions and can expand to include too many unrelated methods
  see [Interface segregation principle](https://en.wikipedia.org/wiki/Interface_segregation_principle)) if care is not taken.
* Interfaces are not composable (unlike individual functions).
* For more, see [this Stack Overflow answer by Mark Seemann](https://stackoverflow.com/questions/34011895/f-how-to-pass-equivalent-of-interface/34028711?stw=2#34028711).
* For the OO interface approach:
  * You may have to modify existing classes when refactoring to an interface.
* For the FP "record of functions" approach (compared to OO interfaces):
  * Less tooling support, and poor interop.
 
#### 7: Dependency injection using functions

In this design, an API layer communicates via one or more functions that are passed in as parameters to the API call.
These functions are typically partially applied so that the call site is decoupled from the "injection"

No interface is passed to the constructor as generally there is no constructor!

Two alternatives are shown. 
* In the first approach, each dependency (turtle function) is passed separately.
* In the second approach, only one function is passed in. So to determine which specific turtle function is used, a discriminated union type is defined.

*Advantages*

* The API is decoupled from a particular implementation via parameterization. 
* Because dependencies are passed in at the point of use ("in your face") rather than in a constructor ("out of size"), the tendency for dependencies to multiply is greatly reduced.
* Any function parameter is automatically a "one method interface" so no retrofitting is needed.
* Partial application can be used to for "dependency injection". No special pattern is needed.

*Disadvantages*

* If the number of dependent functions is too great (say more than four) passing them all in as separate parameters can become awkward (hence, the second approach).
* The discriminated union type can be trickier to work with than an interface.

#### 8: Batch processing using a state monad

In this design, the client uses the FP Turtle functions directly.

As before, the client must keep track of the current state and pass it into the next function call,
but this time the state is kept out of sight by using a *state monad* (called a `turtle workflow` computation expression here).

As a result, there are no mutables anywhere. 

*Advantages*

* The client code is similar to imperative code, but preserves immutability.
* The workflows are composable -- you can define two workflows and then combine them to create another workflow.

*Disadvantages*

* Coupled to a particular implementation of the turtle functions.
* More complex than tracking state explicitly.
* Stacks of nested monads are hard to work with.
* Can be inefficient if batches get too large.

#### 9: Batch processing using command objects

In this design, the client creates a list of `Command`s that will be intepreted later.
These commands are then run in sequence using the Turtle library functions.

This approach means that there is no state that needs to be persisted between calls by the client.

*Advantages*

* Simpler to construct and use than workflows or monads.
* Only one function is coupled to a particular implementation. The rest of the client is decoupled.

*Disadvantages*

* Batch oriented only.
* Only suitable when control flow is *not* based on the response from a previous command.


#### 10: Event sourcing

In this design, the client sends a `Command` to a `CommandHandler`.
The CommandHandler converts that to a list of events and stores them in an `EventStore`.

In order to know how to process a Command, the CommandHandler builds the current state
from scratch using the past events associated with that particular turtle.

Neither the client nor the command handler needs to track state.  Only the EventStore is mutable.

*Advantages*

* All code is stateless, hence easy to test.
* Supports replay of events.

*Disadvantages*

* Can be more complex to implement that a CRUD approach.
* If care is not taken, the command handler can get overly complex and evolve into implementing too much business logic.   


#### 11: Functional Retroactive Programming (stream processing)

In the event sourcing approach, all the domain logic (in our case, just tracing the state!) is embedded in the command handler. One drawback of this is that,
as the application evolves, the logic in the command handler can become very complex.

A way to avoid this is to combine ["functional reactive programming"](https://en.wikipedia.org/wiki/Functional_reactive_programming) with event sourcing
to create a design  where the domain logic is performed on the "read-side", by listening to events ("signals") emitted from the event store.

In this approach, the "write-side" follows the same pattern as the event-sourcing example.
A client sends a `Command` to a `commandHandler`, which converts that to a list of events and stores them in an `EventStore`.

However the `commandHandler` only does the *minimal* amount of work, such as updating state, and does NOT do any complex domain logic.
The complex logic is performed by one or more downstream "processors" (also sometimes called "aggregators") that subscribe to the event stream.

You can even think of these events as "commands" to the processors, and of course, the processors can generate new events for another processor to consume,
so this approach can be extended into an architectural style where an application consists of a set of command handlers linked by an event store.

*Advantages*

* Same advantages as event-sourcing.
* Decouples stateful logic from other non-intrinsic logic.
* Easy to add and remove domain logic without affecting the core command handler.

*Disadvantages*

* More complex to implement.


#### 12: Monadic control flow -- Making decisions in the turtle computation expression

In this design, the turtle can reply to certain commands with errors.

The code demonstrates how the client can make decisions inside the turtle computation expression
while the state is being passed around behind the scenes.

*Advantages*

* Computation expressions allow the code to focus on the logic, while taking care of the "plumbing", such as the turtle state.

*Disadvantages*

* Still coupled to a particular implementation of the turtle functions.
* Computation expressions can be complex to implement and not obvious for beginners.


#### 13: The interpreter pattern

In this design, the client builds a data structure, an Abstract Syntax Tree (AST) that represents the instructions paired with handlers for the responses.

This AST can then interpreted later in various ways.

*Advantages*

* An abstract syntax tree completely decouples the program flow from the implementation and allows lots of flexibility.

*Disadvantages*

* Complex to understand.
* Only works well if there are a limited set of operations to perform.
* Can be inefficient if programs get too large.

#### 14: Abstract Data Turtle

In this design, we use the concept of an [abstract data type](https://en.wikipedia.org/wiki/Abstract_data_type) to encapsulate the operations on a turtle.

That is, a "turtle" is defined as an opaque type along with a corresponding set of operations, in the same way that standard F# types such as `List`, `Set` and `Map` are defined.

That is, we have number of functions that work on the type, but we are not allowed to see "inside" the type itself.

*Advantages*

* All code is stateless, hence easy to test.
* The encapsulation of the state means that the focus is always fully on the behavior and properties of the type.
* Clients can never have a dependency on a particular implementation, which means that implementations can be changed safely.
* You can even swap implementations (e.g. by shadowing, or linking to a different assembly) for testing, performance, etc.
  
*Disadvantages*

* The client has to manage the current turtle state.
* The client cannot control the implementation (e.g. by using dependency injection). 

#### 15: Capability-based Turtle 

In the "monadic control flow" approach [(way 12)](http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle-2/#way12) we handled responses from the turtle telling us that it had hit a barrier.

But even though we had hit a barrier, nothing was stopping us from calling the `move` operation over and over again!  

Now imagine that, once we had hit the barrier, the `move` operation was no longer available to us.  We couldn't abuse it because it would be no longer there!

To make this work, we shouldn't provide an API, but instead, after each call, return a list of functions that the client can call to do the next step. The functions would normally include
the usual suspects of `move`, `turn`, `penUp`, etc., but when we hit a barrier, `move` would be dropped from that list.  Simple, but effective.

This technique is closely related to an authorization and security technique called *capability-based security*. If you are interested in learning more,
I have [a whole series of posts devoted to it](http://fsharpforfunandprofit.com/posts/capability-based-security/).

*Advantages*

* Prevents clients from abusing the API.
* Allows APIs to evolve (and devolve) without breaking clients. For example, I could remove the `setColor` part of the API by just returning `None` in the record of functions,
  and no client would break.
* Clients are decoupled from a particular implementation.
  
*Disadvantages*

* Complex to implement.
* The client's logic is much more convoluted as it can never be sure that a function will be available! It has to check every time.
* The API is not easily serializable, unlike some of the data-oriented APIs.
