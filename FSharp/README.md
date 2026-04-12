![Logo](https://raw.githubusercontent.com/goswinr/SplayTree/main/FSharp/Docs/logo128.png)

# SplayTree port to F#


This fork is a careful port of [w8r/splay-tree](https://github.com/w8r/splay-tree) to F#.
See [FSharp/README.md](https://github.com/goswinr/SplayTree/blob/main/FSharp/README.md) for details on the port, tests and benchmarks.


To use this library in F# you may just copy this one file into your project:
[SplayTree.fs](https://github.com/goswinr/SplayTree/blob/main/Fsharp/SplayTree.fs)

or use it via NuGet:

[![SplayTree on nuget.org](https://img.shields.io/nuget/v/SplayTree)](https://www.nuget.org/packages/SplayTree/)

## API

The library lives in the `SplayTree` namespace. The two main types are `Node<'Key,'Value>` and `Tree<'Key,'Value>`.

- `Tree<'Key,'Value>([comparer])`, where `comparer` is an optional `'Key -> 'Key -> int` function
- `tree.Insert(key:'Key, [data:'Value]):Node` - Insert item, allow duplicate keys
- `tree.Add(key:'Key, [data:'Value]):Node` - Insert item if it is not present
- `tree.Remove(key:'Key)` - Remove item
- `tree.Find(key:'Key):Node|null` - Return node by its key
- `tree.FindStatic(key:'Key):Node|null` - Return node by its key (doesn't re-balance the tree)
- `tree.At(index:int):Node|null` - Return node by its index in sorted order of keys
- `tree.Contains(key:'Key):bool` - Whether a node with the given key is in the tree
- `tree.ForEach(visitor:Node -> bool option):Tree` - In-order traversal
- `tree.Keys():'Key array` - Returns the array of keys in order
- `tree.Values():'Value array` - Returns the array of data fields in order
- `tree.Range(low:'Key, high:'Key, fn:Node -> bool option):Tree` - Walks the range of keys in order. Stops if the visitor function returns `Some true`
- `tree.Pop():TreeItem<'Key,'Value> option` - Removes and returns the smallest node
- `tree.Min():'Key option` - Returns min key
- `tree.Max():'Key option` - Returns max key
- `tree.MinNode():Node` - Returns the node with smallest key
- `tree.MaxNode():Node` - Returns the node with highest key
- `tree.Prev(node:Node):Node|null` - Predecessor node
- `tree.Next(node:Node):Node|null` - Successor node
- `tree.Update(key:'Key, newKey:'Key, [newData:'Value])` - Change a key (and optionally its data) in the tree
- `tree.Load(keys:'Key array, [values:'Value array], [presort:bool]):Tree` - Bulk-load items. It expects keys and values to be sorted, but if `presort` is `true`, it will sort them using the comparer (in-place, your arrays will be altered)
- `tree.Clear():Tree` - Remove all nodes
- `tree.IsEmpty():bool` - Whether the tree is empty
- `tree.Split(key:'Key):SplitResult` - Split tree around key into `{ left; right }`
- `tree.ToString([printNode:Node -> string]):string` - ASCII tree diagram
- `tree.Size:int` - Number of nodes in the tree
- `tree.Root:Node` - Root node (nullable)

The tree also implements `IEnumerable<Node<'Key,'Value>>` so you can use `for … in tree` or pipe it into `Seq` functions.

**Comparer**

`'Key -> 'Key -> int` - Comparer function between two keys, it returns

- `0` if the keys are equal
- `< 0` if `a < b`
- `> 0` if `a > b`

The default comparer uses `System.Collections.Generic.Comparer<'Key>.Default`.

**Visitor**

`Node<'Key,'Value> -> bool option` - Used by `ForEach` and `Range`. Return `None` to continue, `Some true` to stop iteration early.

**Duplicate keys**

- `Insert()` method allows duplicate keys. This can be useful in certain applications (example: overlapping points in 2D).
- `Add()` method will not allow duplicate keys - if the key is already present in the tree, no new node is created.

## Example

```fsharp
open SplayTree

let t = Tree<int, string>()
t.Insert(5)  |> ignore
t.Insert(-10) |> ignore
t.Insert(0)  |> ignore
t.Insert(33) |> ignore
t.Insert(2)  |> ignore

printfn "%A" (t.Keys())   // [|-10; 0; 2; 5; 33|]
printfn "%d" t.Size        // 5
printfn "%A" (t.Min())     // Some -10
printfn "%A" (t.Max())     // Some 33

t.Remove(0)
printfn "%d" t.Size        // 4
```

**Custom comparer (reverse sort)**

```fsharp
let t = Tree<int, string>(fun a b -> compare b a)
t.Insert(5)  |> ignore
t.Insert(-10) |> ignore
t.Insert(0)  |> ignore
t.Insert(33) |> ignore
t.Insert(2)  |> ignore

printfn "%A" (t.Keys())   // [|33; 5; 2; 0; -10|]
```

**Bulk insert**

```fsharp
let t = Tree<int, string>()
t.Load([|3; 2; -10; 20|], [|"C"; "B"; "A"; "D"|], true) |> ignore
printfn "%A" (t.Keys())   // [|-10; 2; 3; 20|]
printfn "%A" (t.Values()) // [|"A"; "B"; "C"; "D"|]
```


## Tests

All test are ported too and pass.

see [FSharp/Tests/README.md](https://github.com/goswinr/SplayTree/blob/main/FSharp/Tests/README.md) for details on building the Fable output and running the test suite on .NET and JS.


## AI

ported from https://github.com/w8r/splay-tree on 2026-04-12 with
Codex GPT-5.4 Extra High
... and a lot of manual improvements and fixes.

