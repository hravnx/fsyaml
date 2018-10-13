open System
open System.Collections.Generic
open System.IO

open YamlDotNet.RepresentationModel

module Yaml =
    /// Convenience function that creates a YamlScaler out of a string
    let ys (s : string) = YamlScalarNode s

    /// Get a string value out of a node
    let toString (n : YamlNode) = n.ToString()

    /// gGet a child yaml node
    let getNode (key : string) (node : YamlNode) = node.[ys key]

    /// Get a string value with a given key, if it exists, None otherwise
    let getOptionalString (key : string) (node : YamlNode) =
        try
            node.[ys key] |> toString |> Some
        with
            | :? KeyNotFoundException -> None

    /// Get a string value with a given key, if it exists, defaultValue otherwise
    let getString (key : string) (defaultValue : string) (node : YamlNode) =
        defaultArg (getOptionalString key node) defaultValue

    /// load a yaml document from a text reader
    let load (reader : TextReader) =
        let yamlStream = YamlStream()
        yamlStream.Load(reader)
        yamlStream.Documents.[0].RootNode :?> YamlMappingNode

    /// load a yaml document from a file
    let loadFile (filePath : string) =
        use reader = new StreamReader(filePath)
        load reader

    /// load a yaml document from a string
    let loadString (content : string) =
        use reader = new StringReader(content)
        load reader

    /// transforms all items in a sequence node according to a function f
    let mapList (f : int -> YamlNode -> 'a) (nodes : YamlSequenceNode) =
        Seq.mapi f nodes

    /// tramsforms all items in a mapping node according a´to a function f
    let mapKeys (f : string -> YamlNode -> 'a) (nodes : YamlMappingNode) = seq {
        for kv in nodes do
            let key = kv.Key |> toString
            yield (f key kv.Value)
    }


// --------------------------------------------------------------------

type MenuItem = {
    Selector : char
    Label    : string
}

let selectors = "0123456789abcdefghijklmnopqrstuvwxyz"

let makeMenuItem (n : int) (label : YamlNode) = {
    Selector = selectors.[n]
    Label = label |> Yaml.toString
}

// --------------------------------------------------------------------

type StartCommand = {
    Name   : string
    Path   : string
    Except : string option
}

let makeStartCommand (name : string) (values : YamlNode) = {
    Name = name
    Path = Yaml.getNode "path" values |> Yaml.toString
    Except = Yaml.getOptionalString "except" values
}


let TEST_YAML = """
version: 3.4

menu:
    - item 1
    - item 2
    - item 3
    - item 4

start_commands:
    start_infra:
        path: /my/wonderful/path/to/a/compose/file.yml
        except: aws-cli

    check_status:
        path: /other/path/that/is/also/good/for/something
"""

/// print a single menu item to the console
let printMenuItemn menuItem =
    printfn "%c. %s" menuItem.Selector menuItem.Label


let printStartCommand startCommand =
    let writeProp n v =
        Console.ForegroundColor <- ConsoleColor.Cyan
        printf "   %-10s" n
        Console.ForegroundColor <- ConsoleColor.White
        printfn "%s" v
        Console.ResetColor()

    Console.ForegroundColor <- ConsoleColor.Yellow
    printfn " %s"startCommand.Name
    Console.ResetColor()

    writeProp "Path" startCommand.Path
    writeProp "Except" (defaultArg startCommand.Except "No exceptions")
    printfn ""

[<EntryPoint>]
let main argv =
    let root = Yaml.loadString TEST_YAML

    let version = match Yaml.getOptionalString "version" root with
                  | None -> eprintfn "Config does not contain version node, can't continue"; exit 1
                  | Some vs -> vs

    printfn "Config is version %s" version
    printfn ""

    // menu example
    root                             // take the root node
    |> Yaml.getNode "menu"           // get the menu child node
    :?> YamlSequenceNode             // cast it to a sequence node (because the menu node is a yaml list)
    |> Yaml.mapList makeMenuItem     // map each sub node of menu to a MenuItem
    |> Seq.iter printMenuItemn       // print all menu items

    printfn "\nCommands"

    // commands example
    root                                // take the rot node
    |> Yaml.getNode "start_commands"    // get the start_commands child node
    :?> YamlMappingNode                 // cast it ti a mapping node (because the start_commands node is a yaml map)
    |> Yaml.mapKeys makeStartCommand    // map each sub node of start_commands to a StartCommand
    |> Seq.iter printStartCommand       // print all start commands

    0 // return an integer exit code
