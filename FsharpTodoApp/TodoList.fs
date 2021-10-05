namespace FsharpTodoApp

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

module TodoList =
    open Avalonia.Layout
    
    type Todo = { text: string; complete: bool }
    
    type State = { todos: ResizeArray<Todo>; workingText: string }
    let init = { todos = ResizeArray<Todo>(); workingText = "" }

    type Msg =
        | Add
        | Remove of int
        | Check of int
        | Uncheck of int
        | Change of string * int
        | Clear
        | SetWorkingText of string

    let update (msg: Msg) (state: State) : State =
        match msg with
        | Add ->
            let todo = { text = state.workingText; complete = false }
            state.todos.Add(todo)
            { state with workingText = "" }
        | Remove(i) ->
            let list = state.todos
            let removed =
                list
                |> Seq.mapi (fun j t -> (j,t))
                |> Seq.filter (fun (j,_) -> j <> i)
                |> Seq.map snd
            { state with todos = ResizeArray<Todo>(removed) }
        | Check(i) ->
            let originalTodo = state.todos.[i]
            let newTodo = { originalTodo with complete = true }
            state.todos.[i] <- newTodo
            state
        | Uncheck(i) ->
            let originalTodo = state.todos.[i]
            let newTodo = { originalTodo with complete = false }
            state.todos.[i] <- newTodo
            state
        | Change(s, i) ->
            let originalTodo = state.todos.[i]
            let newTodo = { originalTodo with text = s }
            state.todos.[i] <- newTodo
            state
        | Clear -> { state with todos = ResizeArray<Todo>() }
        | SetWorkingText(t) -> { state with workingText = t }
    
    let view (state: State) (dispatch) =
        Grid.create [
            Grid.rowDefinitions "*,Auto"
            Grid.columnDefinitions "*,*,*"
            
            Grid.children [
                
                TextBox.create [
                    TextBox.text state.workingText
                    TextBox.onTextChanged (fun t -> dispatch (SetWorkingText(t)))
                    
                    Grid.row 1
                ]
                
                Button.create [
                    Button.content "Add"
                    Button.onClick (fun _ -> dispatch Add)
                    
                    Grid.row 1
                    Grid.column 1
                ]
                
                Button.create [
                    Button.content "Clear"
                    Button.onClick (fun _ -> dispatch Clear)
                    
                    Grid.row 1
                    Grid.column 2
                ]
                
                // todos are shown by this
                StackPanel.create [
                    Grid.columnSpan 3
                    
                    StackPanel.children (state.todos |> Seq.toList |> List.mapi (fun i t ->
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.children [
                                Button.create [
                                    Button.content "x"
                                    Button.onClick (fun _ -> dispatch (Remove(i)))
                                ]
                                CheckBox.create [
                                    CheckBox.onChecked (fun _ -> dispatch (Check(i)))
                                    CheckBox.onUnchecked (fun _ -> dispatch (Uncheck(i)))
                                    CheckBox.isChecked t.complete
                                ]
                                TextBox.create [
                                    TextBox.text t.text
                                    TextBox.onTextChanged (fun t -> dispatch (Change(t, i)))
                                ]
                            ]
                        ]
                        :> IView
                        ))
                ]
                
            ]
        ]