module Game

open Fable.Import
open Types


let awaitAnimationFrame() =
 Async.FromContinuations(fun (cont,_,_) ->
   Browser.window.requestAnimationFrame(Browser.FrameRequestCallback cont) |> ignore)

let start =

  // our events is simply a ResizeArray we add elements to
  let events = ResizeArray()

  let rec stateMachine (smodel: StateModel) (rmodel: RenderModel option) lastDelta: Async<unit> = async {

   // calculate time difference between frames
   let! current = awaitAnimationFrame()
   let delta = current - lastDelta

   // updating our game state from received events
   // we got from our GameRenderer
   let newState =
      (smodel, events)
      ||> Seq.fold (fun smodel ev ->
        match ev with
        | DonePreparing -> Run
        | _ -> ev
      )

   events.Clear()

   // update our rendering
   // notice the events.Add which will allow us to add events from our GameRenderer
   let newRender = GameRenderer.render newState rmodel events.Add delta

   // calling next frame
   return! stateMachine newState (Some newRender) current
  }

  let state = Init
  stateMachine state None
