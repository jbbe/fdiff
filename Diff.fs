namespace TripleDiff

module Diff =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    type DiffState = { 
        leftText : string;
        rightText: string;
        diffText: string
    }
    let init = {
        leftText = "Left side Text";
        rightText = "Right side Text";
        diffText = ""
    }

    type Msg = LeftText of text : string | RightText of text : string



    type EditType = Insertion | Deletion | NoChange
    type Edit = {EditType: EditType; Text: string }


    let splitLine = (fun (line : string) -> Seq.toList (line.Split '\n'))

    let rec editCount (a: Edit list) : int =
        match a with
        | head::tail -> if (head.EditType = NoChange) then (editCount tail) else 1 + (editCount tail)
        | [] -> 0

    let shorterList (a: Edit list) (b: Edit list) =
        if (editCount a) < (editCount b) then a else b

    let rec computeDiff (a: string list) (b: string list) : Edit list =
        match a with
        | aHead::aTail ->
            (match b with
                | bHead::bTail ->
                    if aHead.Equals(bHead) then {EditType=NoChange; Text=aHead}::(computeDiff aTail bTail)
                    else (shorterList ({EditType=Insertion; Text=bHead}::(computeDiff a bTail)) ({EditType=Deletion; Text=aHead}::(computeDiff aTail b)))
                | [] -> {EditType=Deletion; Text=aHead}::(computeDiff aTail []))
        | [] -> (match b with
                  | bHead::bTail -> ({EditType=Insertion;  Text=bHead}::(computeDiff [] bTail))
                  | [] -> [])

    let editTypeToString (e: EditType) =
        match e with
        | Insertion -> "+ "
        | Deletion -> "- "
        | NoChange -> ""

    let rec editListToString (edits: Edit list) =
        match edits with
            | edit::tail -> (editTypeToString edit.EditType) + edit.Text + "\n" + (editListToString tail)
            | [] -> ""

    let setDiffText (state: DiffState) =
        let leftSplit = splitLine state.leftText
        let rightSplit = splitLine state.rightText
        let edits = computeDiff leftSplit rightSplit
        { state with  diffText = (editListToString edits) }
        

    let update (msg: Msg) (state: DiffState) =
        match msg with
        | LeftText (text = t) -> (setDiffText { state with leftText = t })
        | RightText (text = t) -> (setDiffText { state with rightText = t })
    
    let view (state: DiffState) (dispatch) =
        DockPanel.create [
            DockPanel.children [
                Grid.create [
                    Grid.dock Dock.Top
                    Grid.columnDefinitions "* * *"
                    Grid.children [
                        TextBox.create [
                            TextBox.fontSize 14.0
                            TextBox.padding (40., 14.)
                            TextBox.background "#4d258d"
                            Grid.row 0
                            Grid.column 0
                            TextBox.acceptsReturn true
                            TextBox.onTextChanged (LeftText >> dispatch)
                        ]
                        TextBlock.create [
                            TextBlock.dock Dock.Bottom
                            TextBlock.fontSize 14.0
                            Grid.row 0
                            Grid.column 1
                                                //TextBlock.verticalAlignment VerticalAlignment.Bottom
                                                //TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text (string state.diffText)
                        ]
                        TextBox.create [
                            TextBox.fontSize 14.0
                            Grid.row 0
                            Grid.column 2
                            TextBox.background "#21133e"
                            TextBox.text (string state.rightText)
                            TextBox.padding (40., 14.)
                            TextBox.acceptsReturn true
                            TextBox.onTextChanged ( RightText >> dispatch) 
                        ]
                   
                    ]
                ]
                
              
            ]
        ]       
