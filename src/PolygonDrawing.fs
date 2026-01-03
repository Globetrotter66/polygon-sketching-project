// Example setup for the polygon drawing project.
// The application logics should be implemented in the `updateModel` function.
// The undo-redo-relevant parts should be implemented in `addUndoRedo`.abs
// Please note that, the logics is easiest implemented when always adding elements
// to the beginning of the list e.g. build polylines and polygons in reverse order.
module PolygonDrawing 

open Fable.Core
open Feliz
open Elmish

// we use a record here, a tuple could also serve the purpose though
type Coord = { x : float; y : float }

// "polygon" line. Each list element describes the respective vertex.
// note that we could use a record here, but a type-alias is more lightweight
// and serves its purpose.
// I recommend stroring the coordinates in reverse order, so that each vertex gets prepended
// to the list. This way, adding new vertices is O(1).
type PolyLine = list<Coord>

type Model = {
    // all "finished" polygons, created so far, by convention, new PolyLines can be prepended to this list to
    // make additions efficent and the code more elegant.
    finishedPolygons : list<PolyLine>
    // the polygon, we are currently working on (and extending, vertex-by-vertex). Having the current
    // one explicitly as oposed to already in the finishedPolygons list makes the code a bit more elegant
    // and approachable
    currentPolygon : Option<PolyLine>
    // current positon of the mouse (to draw a preview)
    mousePos : Option<Coord>
    // optionally, the model before this current state (note, that this immutable!), used for redo
    past : Option<Model>
    // used for redo
    future : Option<Model>
}

// and explicit representation of all possible user interactions. This one can be used for 
// automatic testing or storing interaction logs to disk
type Msg =
    | AddPoint of Coord
    | SetCursorPos of Option<Coord>
    | FinishPolygon
    | Undo
    | Redo

// creates the initial model, which is used when creating the interactive application (see Main.fs)
let init () =
    let m = 
        { finishedPolygons = []; currentPolygon = None; // records can be written multiline
          mousePos = None ; past = None; future = None }
    m, Cmd.none // Cmd is optionally to explicitly represent side-effects in a safe manner (here we don't bother)


(*
TODO: implement the core logics of the drawing app, which means:
Depending on the message,
For AddPoint mesages, add the point to the current polygon.
 - if there is no current polygon yet, create a new one with this point as its only vertex.
 - if there is already a polygon, prepend (or append if you like) it to the list of vertices
For FinishPolygon mesages:
 - if there is no current polygon (this means right click was used before even adding a single vertex), ignore the message
 - if there is a current polygon, reset the current polygon to None and add the current polygon as a new elemnet to finishedPolygons.
*)
let updateModel (msg : Msg) (model : Model) =
    match msg with
    | AddPoint pos ->
        match model.currentPolygon with
        | None -> 
            // Create a new list with the first point
            { model with currentPolygon = Some [pos] }
        | Some points -> 
            // Prepend new point to the existing list (Reverse order for efficiency)
            { model with currentPolygon = Some (pos :: points) }

    | FinishPolygon ->
        match model.currentPolygon with
        | None -> 
            model // Ignore if nothing to finish
        | Some points ->
            // Add the current polygon to finished list and clear active
            { model with 
                finishedPolygons = points :: model.finishedPolygons
                currentPolygon = None }
                
    | _ -> model // Undo/Redo/Cursor are handled by the wrapper

let removeFirst lst =
    match lst with
    | h :: t -> t    // If list has a head (h) and a tail (t), return just the tail
    | [] -> []

// wraps an update function with undo/redo.
let addUndoRedo (updateFunction : Msg -> Model -> Model) (msg : Msg) (model : Model) =
    // first let us, handle the cursor position, which is not undoable, and handle undo/redo messages
    // in a next step we actually run the "core" system logics.
    match msg with
    | SetCursorPos p -> 
        // update the mouse position and create a new model.
        { model with mousePos = p }
    | Undo -> 
        // TODO implement undo logics, HINT: restore the model stored in past, and replace the current
        // state with it.
        match model.past with
        | None -> model // Nothing to undo
        | Some previousState -> 
        { previousState with
            future = Some { model with past = None; future = None }
            mousePos = model.mousePos } 
    | Redo -> 
        match model.future with
        | None -> model // Nothing to redo
        | Some nextState ->
            // To redo: Restore 'future', put current into 'past'
            { nextState with 
                past = Some { model with past = None; future = None } 
                mousePos = model.mousePos }
    | _ -> 
        // use the provided update function for all remaining messages
        //{ updateFunction msg model with past = Some model },
        // Run core logic
        let nextModel = updateFunction msg model
        
        // If the model actually changed (excluding mouse), update history
        if nextModel = model then 
            model 
        else
            { nextModel with 
                past = Some { model with past = None; future = None } // Deep snapshot
                future = None } // New actions clear the redo chain


let update (msg : Msg) (model : Model)  =
    let newModel = addUndoRedo updateModel msg model
    newModel, Cmd.none

[<Emit("getSvgCoordinates($0)")>] // wrapper to use the getSvgCoordinates JS function (provided by index.html) from f# here typesafely.
let getSvgCoordinates (o: Browser.Types.MouseEvent): Coord = jsNative

let viewPolygon (color : string) (points : PolyLine) =
    points 
    |> List.pairwise 
    |> List.map (fun (c0,c1) ->
        Svg.line [
            svg.x1 c0.x; svg.y1 c0.y
            svg.x2 c1.x; svg.y2 c1.y
            svg.stroke(color)
            svg.strokeWidth 2.0
            svg.strokeLineJoin "round"
        ]
    )
 

let render (model : Model) (dispatch : Msg -> unit) =
    let border = 
        Svg.rect [ // i used ; to group together attributes semantically.
            svg.x1 0; svg.x2 500
            svg.y1 0; svg.y2 500
            svg.width 500; svg.height 500
            svg.stroke("black"); svg.strokeWidth(2); svg.fill "none"
        ] 

    // collect all svg elements of all finished polygons
    let finisehdPolygons = 
        model.finishedPolygons |> List.collect (viewPolygon "green")
    let currentPolygon =
        match model.currentPolygon with
        | None -> [] // if we have no polygon, create empty svg list
        | Some p -> 
            match model.mousePos with
            | None -> 
                viewPolygon "red" p
            | Some preview -> 
                // if we have a current mouse position, prepend the mouse position to the resulting polygon
                viewPolygon "red" (preview :: p)
 
    let svgElements = List.concat [finisehdPolygons; currentPolygon]

    Html.div [
        prop.style [style.custom("userSelect","none")]
        prop.children [
            Html.h1 "Simplest drawing"
            Html.button [
                prop.style [style.margin 20]; 
                prop.onClick (fun _ -> dispatch Undo)
                prop.children [Html.text "undo"]
            ]
            Html.button [
                prop.style [style.margin 20]
                prop.onClick (fun _ -> dispatch Redo)
                prop.children [Html.text "redo"]
            ]
            Html.br []
            Svg.svg [
                svg.width 500; svg.height 500
                svg.onMouseMove (fun mouseEvent -> 
                    // compute SVG relative coordinates, using javascript function
                    let pos = getSvgCoordinates mouseEvent

                    // fable requires to "send" messages via side-effect. 
                    // Can be moved into UI system, e.g. see  https://elm-lang.org/examples/buttons
                    dispatch (SetCursorPos (Some pos))
                )
                svg.onClick (fun mouseEvent -> 
                    // create messages (purely descriptive)
                    let msgs = 
                        if mouseEvent.detail = 1 then
                            let pos = getSvgCoordinates mouseEvent
                            [AddPoint pos] 
                        elif mouseEvent.detail = 2 then
                            [FinishPolygon]
                        else
                            []

                    // fable requires to "send" messages via side-effect. 
                    // Can be moved into UI system, e.g. see  https://elm-lang.org/examples/buttons
                    msgs |> List.iter dispatch
                )
                svg.children (border :: svgElements) // use : to prepend the border to the other elements
            ]
        ]
    ]