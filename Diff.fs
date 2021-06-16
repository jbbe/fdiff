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



    type EditType = Insertion | Deletion
    type EditSide = Left | Right
    type Edit = {EditType: EditType; EditSide: EditSide; Text: string }


    let splitLine = (fun (line : string) -> Seq.toList (line.Split ','))

    let shorterList (a: Edit list) (b: Edit list) =
        if a.Length < b.Length then a else b

    let rec computeDiff (a: string list) (b: string list) : Edit list =
        match a with
        | aHead::aTail ->
            (match b with
                | bHead::bTail ->
                    if aHead.Equals(bHead) then (computeDiff aTail bTail)
                    else (shorterList ({EditType=Insertion; EditSide=Left; Text=bHead}::(computeDiff a bTail)) ({EditType=Deletion; EditSide=Right; Text=aHead}::(computeDiff aTail b)))
                | [] -> {EditType=Deletion; EditSide=Left; Text=aHead}::(computeDiff aTail []))
        | [] -> (match b with
                  | bHead::bTail -> ({EditType=Insertion; EditSide=Left; Text=bHead}::(computeDiff [] bTail))
                  | [] -> [])

    let editTypeToString (e: EditType) =
        match e with
        | Insertion -> "+ "
        | Deletion -> "- "

    let rec editListToString (edits: Edit list) =
        match edits with
            | edit::tail -> (editTypeToString edit.EditType) + edit.Text + "\n" + (editListToString tail)
            | [] -> ""

    let setDiffText (state: DiffState) =
        //let leftSplit = state.leftText.Split '\n'
        //let rightSplit = state.rightText.Split '\n'
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
                
                TextBox.create [
                    TextBox.dock Dock.Top
                    TextBox.fontSize 14.0
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.horizontalAlignment HorizontalAlignment.Left
                    StackPanel.row 3
                    TextBox.acceptsReturn true
                    TextBox.onTextChanged (LeftText >> dispatch)
                ]

                TextBox.create [
                    TextBox.dock Dock.Top
                    TextBox.fontSize 14.0
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.horizontalAlignment HorizontalAlignment.Right
                    TextBox.text (string state.rightText)
                    StackPanel.row 3
                    TextBox.acceptsReturn true
                    TextBox.onTextChanged ( RightText >> dispatch) 
                ]
                TextBlock.create [
                    TextBlock.dock Dock.Bottom
                    TextBlock.fontSize 14.0
                    TextBlock.verticalAlignment VerticalAlignment.Bottom
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.text (string state.diffText)
                ]
            ]
        ]       
