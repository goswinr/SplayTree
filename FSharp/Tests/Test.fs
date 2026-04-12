module Test

open System
open System.Collections
open System.Collections.Generic
open Expecto
open SplayTree

type Obj = { value: int; data: int }

let requireNode<'Key, 'Value> (message: string) (node: Node<'Key, 'Value>) : Node<'Key, 'Value> =
    if isNull node then
        failtest message

    node

let requireSome<'T> (message: string) (value: 'T option) : 'T =
    match value with
    | Some result -> result
    | None -> failtest message

let expectReferenceEqual<'T when 'T: not struct> (expected: 'T) (actual: 'T) (message: string) : unit =
    Expect.isTrue (obj.ReferenceEquals(expected, actual)) message

let expectReferenceNotEqual<'T when 'T: not struct> (expected: 'T) (actual: 'T) (message: string) : unit =
    Expect.isFalse (obj.ReferenceEquals(expected, actual)) message

let expectSequenceEqual<'T when 'T: equality> (actual: seq<'T>) (expected: seq<'T>) (message: string) : unit =
    Expect.equal (actual |> Seq.toArray) (expected |> Seq.toArray) message

let shuffle<'T> (items: 'T array) : 'T array =
    let random: Random = Random(12345)
    let copy: 'T array = Array.copy items
    let mutable currentIndex: int = copy.Length

    while currentIndex <> 0 do
        let randomIndex: int = random.Next(currentIndex)
        currentIndex <- currentIndex - 1
        let temporaryValue: 'T = copy.[currentIndex]
        copy.[currentIndex] <- copy.[randomIndex]
        copy.[randomIndex] <- temporaryValue

    copy

let nodeKeys<'Key, 'Value> (tree: Node<'Key, 'Value>) : 'Key array =
    let values: ResizeArray<'Key> = ResizeArray<'Key>()

    let rec loop (node: Node<'Key, 'Value>) : unit =
        if not (isNull node) then
            loop node.Left
            values.Add(node.Key)
            loop node.Right

    loop tree
    values.ToArray()

let createFloatTree (values: float array) : Tree<float, obj> =
    let tree: Tree<float, obj> = Tree<float, obj>()

    values |> Array.iter (fun value -> tree.Insert(value) |> ignore)

    tree

let compareTests: Test =
    testList
        "custom comparator"
        [ testCase "should function correctly given a non-reverse customCompare" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>(fun a b -> b - a)
              tree.Insert(2) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(3) |> ignore
              Expect.equal tree.Size 3 "size should reflect inserted items"
              Expect.equal (requireSome "min should exist" (tree.Min())) 3 "min should follow custom ordering"
              Expect.equal (requireSome "max should exist" (tree.Max())) 1 "max should follow custom ordering"
              tree.Remove(3)
              Expect.equal tree.Size 2 "size should shrink after remove"

              let root: Node<int, obj> = requireNode "root should still exist" tree.Root
              Expect.equal root.Key 2 "root key should match the expected node"
              Expect.isNull root.Left "left child should be null"
              Expect.equal (requireNode "right child should exist" root.Right).Key 1 "right child should match")
          testCase "should support custom keys" (fun _ ->
              let comparator (a: Obj) (b: Obj) : int = a.value - b.value
              let tree: Tree<Obj, obj> = Tree<Obj, obj>(comparator)

              let objects: Obj array =
                  Array.init 10 (fun index -> { value = index; data = index * index }) |> shuffle

              objects |> Array.iter (fun item -> tree.Insert(item) |> ignore)

              let actual: int array = tree.Keys () |> Seq.map (fun item -> item.value) |> Seq.toArray

              let expected: int array =
                  objects |> Array.sortWith comparator |> Array.map (fun item -> item.value)

              Expect.equal actual expected "custom keys should be ordered by the comparator") ]

let containsTests: Test =
    testList
        "contains check"
        [ testCase "should return false if the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isFalse (tree.Contains(1)) "empty tree should not contain any values")
          testCase "should return whether the tree contains a node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isFalse (tree.Contains(1)) "tree should not contain 1 yet"
              Expect.isFalse (tree.Contains(2)) "tree should not contain 2 yet"
              Expect.isFalse (tree.Contains(3)) "tree should not contain 3 yet"
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore
              Expect.isTrue (tree.Contains(1)) "tree should contain 1"
              Expect.isTrue (tree.Contains(2)) "tree should contain 2"
              Expect.isTrue (tree.Contains(3)) "tree should contain 3")
          testCase "should return false when the expected parent has no children" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(2) |> ignore
              Expect.isFalse (tree.Contains(1)) "missing left child should return false"
              Expect.isFalse (tree.Contains(3)) "missing right child should return false") ]

let duplicateTests: Test =
    testList
        "Duplicate keys"
        [ testCase "should allow inserting of duplicate key" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let values: int array = [| 2; 12; 1; -6; 1 |]

              values |> Array.iter (fun value -> tree.Insert(value) |> ignore)

              expectSequenceEqual (tree.Keys ()) [| -6; 1; 1; 2; 12 |] "keys should keep duplicates"
              Expect.equal tree.Size 5 "size should count duplicates")
          testCase "should allow multiple duplicate keys in a row" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let values: int array = [| 2; 12; 1; 1; -6; 2; 1; 1; 13 |]

              values |> Array.iter (fun value -> tree.Insert(value) |> ignore)

              expectSequenceEqual (tree.Keys ()) [| -6; 1; 1; 1; 1; 2; 2; 12; 13 |] "keys should preserve repeated duplicates"
              Expect.equal tree.Size 9 "size should include every inserted duplicate")
          testCase "should remove from a tree with duplicate keys correctly" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let values: int array = [| 2; 12; 1; 1; -6; 1; 1 |]

              values |> Array.iter (fun value -> tree.Insert(value) |> ignore)

              let mutable size: int = tree.Size

              for index = 0 to 3 do
                  tree.Remove(1)

                  if index < 3 then
                      Expect.isTrue (tree.Contains(1)) "duplicate key should still remain before the final removal"

                  size <- size - 1
                  Expect.equal tree.Size size "size should decrement after each duplicate removal"

              Expect.isFalse (tree.Contains(1)) "all duplicate keys should eventually be removed")
          testCase "should remove from a tree with multiple duplicate keys correctly" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let values: int array = [| 2; 12; 1; 1; -6; 1; 1; 2; 0; 2 |]

              values |> Array.iter (fun value -> tree.Insert(value) |> ignore)

              let mutable size: int = tree.Size

              while not (tree.IsEmpty()) do
                  tree.Pop () |> ignore
                  size <- size - 1
                  Expect.equal tree.Size size "size should decrement after each pop"

              Expect.equal tree.Size 0 "tree should be empty after removing every node")
          testCase "should disallow duplicates if noDuplicates is set" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let values: int array = [| 2; 12; 1; -6; 1 |]

              values |> Array.iter (fun value -> tree.Add(value) |> ignore)

              expectSequenceEqual (tree.Keys ()) [| -6; 1; 2; 12 |] "add should reject duplicates"
              Expect.equal tree.Size 4 "size should not count the rejected duplicate")
          testCase "should add only if the key is not there" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore
              tree.Insert(3) |> ignore

              let size: int = tree.Size
              tree.Add(1) |> ignore
              Expect.equal tree.Size size "size should not change when adding an existing key") ]

let emptyTests: Test =
    testList
        "empty check"
        [ testCase "should return whether the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isTrue (tree.IsEmpty()) "new tree should be empty"
              tree.Insert(1) |> ignore
              Expect.isFalse (tree.IsEmpty()) "tree with a value should not be empty"
              tree.Remove(1)
              Expect.isTrue (tree.IsEmpty()) "tree should be empty again after removing its only value") ]

let findTests: Test =
    testList
        "find"
        [ testCase "should return key as the result of search" (fun _ ->
              let tree: Tree<int, int> = Tree<int, int>()
              Expect.isNull (tree.Find(1)) "empty tree should not find 1"
              Expect.isNull (tree.Find(2)) "empty tree should not find 2"
              Expect.isNull (tree.Find(3)) "empty tree should not find 3"
              tree.Insert(1, 4) |> ignore
              tree.Insert(2, 5) |> ignore
              tree.Insert(3, 6) |> ignore

              let mutable root: Node<int, int> = tree.Root
              Expect.equal (requireNode "find 1 should succeed" (tree.Find(1))).Data 4 "find should return data for key 1"
              expectReferenceNotEqual root tree.Root "find should splay the found node to the root"
              root <- tree.Root

              Expect.equal (requireNode "find 2 should succeed" (tree.Find(2))).Data 5 "find should return data for key 2"
              expectReferenceNotEqual root tree.Root "find should change the root when another node is found"
              root <- tree.Root

              Expect.equal (requireNode "find 3 should succeed" (tree.Find(3))).Data 6 "find should return data for key 3"
              expectReferenceNotEqual root tree.Root "find should change the root again when another node is found"
              root <- tree.Root

              Expect.isNull (tree.Find(8)) "missing key should return null"
              expectReferenceEqual root tree.Root "unsuccessful find should leave the same root for this case")
          testCase "should allow finding node without splaying" (fun _ ->
              let tree: Tree<int, int> = Tree<int, int>()
              Expect.isNull (tree.FindStatic(1)) "empty tree should not find 1"
              Expect.isNull (tree.FindStatic(2)) "empty tree should not find 2"
              Expect.isNull (tree.FindStatic(3)) "empty tree should not find 3"
              tree.Insert(-2, 8) |> ignore
              tree.Insert(1, 4) |> ignore
              tree.Insert(2, 5) |> ignore
              tree.Insert(3, 6) |> ignore

              tree.Find(2) |> ignore
              let root: Node<int, int> = tree.Root
              Expect.equal (requireNode "findStatic 1 should succeed" (tree.FindStatic(1))).Data 4 "findStatic should read data for key 1"
              expectReferenceEqual root tree.Root "findStatic should not splay the tree"

              Expect.equal (requireNode "findStatic 2 should succeed" (tree.FindStatic(2))).Data 5 "findStatic should read data for key 2"
              expectReferenceEqual root tree.Root "findStatic should keep the same root"

              Expect.equal (requireNode "findStatic 3 should succeed" (tree.FindStatic(3))).Data 6 "findStatic should read data for key 3"
              expectReferenceEqual root tree.Root "findStatic should still keep the same root"

              Expect.equal (requireNode "findStatic -2 should succeed" (tree.FindStatic(-2))).Data 8 "findStatic should read data for key -2"
              expectReferenceEqual (requireNode "find 2 should return root" (tree.Find(2))) tree.Root "find should still splay to the root") ]

let insertTests: Test =
    testList
        "insert"
        [ testCase "should return the size of the tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore
              tree.Insert(3) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(5) |> ignore
              Expect.equal tree.Size 5 "size should match the number of inserted items")
          testCase "should return the pointer" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let n1: Node<int, obj> = tree.Insert(1)
              let n2: Node<int, obj> = tree.Insert(2)
              let n3: Node<int, obj> = tree.Insert(3)
              Expect.equal n1.Key 1 "first inserted node should report its key"
              Expect.equal n2.Key 2 "second inserted node should report its key"
              Expect.equal n3.Key 3 "third inserted node should report its key") ]

let iterateTests: Test =
    testList
        "iterate check"
        [ testCase "should iterate the tree in order" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(0) |> ignore
              tree.Insert(2) |> ignore

              let mutable index: int = 0

              for node in tree.ToSeq() do
                  Expect.equal node.Key index "iterator should walk in key order"
                  index <- index + 1

              Expect.equal index 4 "iterator should visit each node exactly once")
          testCase "should should support empty tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let mutable index: int = 0

              for node in tree.ToSeq() do
                  Expect.equal node.Key index "empty tree should not yield any values"
                  index <- index + 1

              Expect.equal index 0 "empty tree iterator should not visit any nodes") ]

let ienumerableTests: Test =
    testList
        "sequence conversion"
        [ testCase "ToSeq should iterate nodes in key order" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore

              let keys: int array = tree.ToSeq() |> Seq.map (fun node -> node.Key) |> Seq.toArray

              Expect.equal keys [| 1; 2; 3 |] "ToSeq should yield sorted nodes")
          testCase "ToArray should iterate nodes in key order" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore

              let keys: int array =
                  tree.ToArray()
                  |> Seq.ofArray
                  |> Seq.map (fun node -> node.Key)
                  |> Seq.toArray

              Expect.equal keys [| 1; 2; 3 |] "ToArray should yield sorted nodes")
          testCase "ToSeq enumerator should complete traversal" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(2) |> ignore
              tree.Insert(0) |> ignore
              tree.Insert(1) |> ignore

              let enumerator: IEnumerator = (tree.ToSeq() :> IEnumerable).GetEnumerator()
              let visited: ResizeArray<int> = ResizeArray<int>()

              while enumerator.MoveNext() do
                  let node: Node<int, obj> = enumerator.Current :?> Node<int, obj>
                  visited.Add(node.Key)

              Expect.equal (visited.ToArray()) [| 0; 1; 2 |] "ToSeq enumerator should visit each node in-order"
              Expect.isFalse (enumerator.MoveNext()) "ToSeq enumerator should report completion") ]

let keysValuesTests: Test =
    testList
        "Keys and values"
        [ testCase "should return sorted keys with reverse comparator" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>(fun a b -> b - a)
              tree.Insert(5) |> ignore
              tree.Insert(-10) |> ignore
              tree.Insert(0) |> ignore
              tree.Insert(33) |> ignore
              tree.Insert(2) |> ignore
              expectSequenceEqual (tree.Keys ()) [| 33; 5; 2; 0; -10 |] "keys should follow custom ordering")
          testCase "should return sorted keys" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(5) |> ignore
              tree.Insert(-10) |> ignore
              tree.Insert(0) |> ignore
              tree.Insert(33) |> ignore
              tree.Insert(2) |> ignore
              expectSequenceEqual (tree.Keys ()) [| -10; 0; 2; 5; 33 |] "keys should be sorted")
          testCase "should return sorted values" (fun _ ->
              let tree: Tree<int, string> = Tree<int, string>()
              tree.Insert(5, "D") |> ignore
              tree.Insert(-10, "A") |> ignore
              tree.Insert(0, "B") |> ignore
              tree.Insert(33, "E") |> ignore
              tree.Insert(2, "C") |> ignore
              expectSequenceEqual (tree.Keys ()) [| -10; 0; 2; 5; 33 |] "keys should be sorted"
              expectSequenceEqual (tree.Values()) [| "A"; "B"; "C"; "D"; "E" |] "values should follow key order")
          testCase "should return sorted values with reverse comparator" (fun _ ->
              let tree: Tree<int, string> = Tree<int, string>(fun a b -> b - a)
              tree.Insert(5, "D") |> ignore
              tree.Insert(-10, "A") |> ignore
              tree.Insert(0, "B") |> ignore
              tree.Insert(33, "E") |> ignore
              tree.Insert(2, "C") |> ignore
              expectSequenceEqual (tree.Keys ()) [| 33; 5; 2; 0; -10 |] "keys should follow custom ordering"
              expectSequenceEqual (tree.Values()) [| "E"; "D"; "C"; "B"; "A" |] "values should follow key order")
          testCase "should return sorted values after bulk insert" (fun _ ->
              let tree: Tree<int, string> = Tree<int, string>()

              tree.Load([| 5; -10; 0; 33; 2 |], [| "D"; "A"; "B"; "E"; "C" |], true)
              |> ignore

              expectSequenceEqual (tree.Keys ()) [| -10; 0; 2; 5; 33 |] "keys should be sorted after load"
              expectSequenceEqual (tree.Values()) [| "A"; "B"; "C"; "D"; "E" |] "values should be sorted after load")
          testCase "should be able to bulk-load 10000 items" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let count: int = 10000
              let keys: int array = Array.init count id
              tree.Load(keys) |> ignore
              expectSequenceEqual (tree.Keys () |> Seq.take 20) (keys |> Seq.take 20) "bulk load should preserve the sorted prefix") ]

let minMaxTests: Test =
    testList
        "find min and max"
        [ testCase "should return the maximum key in the tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(3) |> ignore
              tree.Insert(5) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(2) |> ignore
              Expect.equal (requireSome "max should exist" (tree.Max())) 5 "max should be 5")
          testCase "should return null for max if the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isNone (tree.Max()) "empty tree should not have a max")
          testCase "should return the minimum key in the tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(5) |> ignore
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(2) |> ignore
              Expect.equal (requireSome "min should exist" (tree.Min())) 1 "min should be 1")
          testCase "should return the max node" (fun _ ->
              let tree: Tree<int, int> = Tree<int, int>()
              tree.Insert(3) |> ignore
              tree.Insert(5, 10) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(2) |> ignore
              let node: Node<int, int> = requireNode "max node should exist" (tree.MaxNode())
              Expect.equal node.Key 5 "max node key should be 5"
              Expect.equal node.Data 10 "max node data should be preserved")
          testCase "should return null for maxNode if the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isNull (tree.MaxNode()) "empty tree should not have a max node")
          testCase "should return the min node" (fun _ ->
              let tree: Tree<int, int> = Tree<int, int>()
              tree.Insert(5) |> ignore
              tree.Insert(3) |> ignore
              tree.Insert(1, 20) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(2) |> ignore
              let node: Node<int, int> = requireNode "min node should exist" (tree.MinNode())
              Expect.equal node.Key 1 "min node key should be 1"
              Expect.equal node.Data 20 "min node data should be preserved")
          testCase "should return null for min if the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isNone (tree.Min()) "empty tree should not have a min")
          testCase "should support removing min node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(5) |> ignore
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(4) |> ignore
              tree.Insert(2) |> ignore

              let item: TreeItem<int, obj> =
                  requireSome "pop should return the min item" (tree.Pop ())

              Expect.equal item.Key 1 "pop should remove the smallest key")
          testCase "should return null for minNode if the tree is empty" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              Expect.isNull (tree.MinNode()) "empty tree should not have a min node") ]

let printTests: Test =
    testList
        "printing"
        [ testCase "should print the tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 2 do
                  tree.Insert(value) |> ignore

              tree.Find(2) |> ignore

              Expect.equal (tree.ToString ()) "└── 2\n    ├── 1\n    │   ├── 0\n" "string output should match the tree layout")
          testCase "should print the balanced tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 2 do
                  tree.Insert(value) |> ignore

              tree.Find(1) |> ignore

              Expect.equal (tree.ToString ()) "└── 1\n    ├── 0\n    └── 2\n" "string output should match the balanced tree layout") ]

let removeTests: Test =
    testList
        "remove"
        [ testCase "should not change the size of empty tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Remove(1)
              Expect.equal tree.Size 0 "remove on an empty tree should not change size")
          testCase "should remove a single key" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              tree.Remove(1)
              Expect.isTrue (tree.IsEmpty()) "tree should be empty after removing its only key")
          testCase "should ignore a single key which is not there" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              tree.Remove(2)
              Expect.equal tree.Size 1 "size should not change when removing a missing key")
          testCase "should take the right child if the left does not exist" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              tree.Insert(2) |> ignore
              tree.Remove(1)
              Expect.equal (requireNode "root should exist after removal" tree.Root).Key 2 "root should become the right child")
          testCase "should take the left child if the right does not exist" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(2) |> ignore
              tree.Insert(1) |> ignore
              tree.Remove(2)
              Expect.equal (requireNode "root should exist after removal" tree.Root).Key 1 "root should become the left child")
          testCase "should not break the existing pointers to nodes" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(1) |> ignore
              let n2: Node<int, obj> = tree.Insert(2)
              let n3: Node<int, obj> = tree.Insert(3)
              tree.Remove(2)
              Expect.equal n2.Key 2 "existing node references should remain valid"
              Expect.equal n3.Key 3 "existing node references should remain valid")
          testCase "pop()" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(2) |> ignore
              tree.Insert(1) |> ignore
              tree.Remove(2)

              let removed: TreeItem<int, obj> =
                  requireSome "pop should remove the last node" (tree.Pop ())

              Expect.equal removed.Key 1 "pop should return the removed key"
              Expect.isNull removed.Data "pop should return the default data value for missing payloads"
              Expect.isNone (tree.Pop ()) "popping an empty tree should return None")
          testCase "should support clear operation" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(2) |> ignore
              tree.Insert(1) |> ignore
              tree.Remove(2)
              tree.Clear() |> ignore
              Expect.isNull tree.Root "clear should reset the root"
              Expect.equal tree.Size 0 "clear should reset the size") ]

let traversalTests: Test =
    testList
        "traversal check"
        [ testCase "should traverse the tree in order" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(3) |> ignore
              tree.Insert(1) |> ignore
              tree.Insert(0) |> ignore
              tree.Insert(2) |> ignore

              let mutable index: int = 0

              tree.ForEach(fun node ->
                  Expect.equal node.Key index "forEach should visit nodes in order"
                  index <- index + 1
                  false)
              |> ignore)
          testCase "should find predecessor for the node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              for value = 1 to 9 do
                  let predecessor: Node<int, obj> =
                      requireNode "predecessor should exist" (tree.Prev(requireNode "node should exist" (tree.Find(value))))

                  let previousNode: Node<int, obj> =
                      requireNode "previous node should exist" (tree.Find(value - 1))

                  Expect.equal predecessor.Key previousNode.Key "predecessor key should match the previous node")
          testCase "should find successor for a node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              for value = 0 to 8 do
                  let successor: Node<int, obj> =
                      requireNode "successor should exist" (tree.Next(requireNode "node should exist" (tree.Find(value))))

                  let nextNode: Node<int, obj> =
                      requireNode "next node should exist" (tree.Find(value + 1))

                  expectReferenceEqual nextNode successor "successor should be the same node instance")
          testCase "should return null for predecessor of the min node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let mutable minNode: Node<int, obj> =
                  requireNode "min node should exist" (tree.MinNode())

              Expect.isNull (tree.Prev(minNode)) "minimum node should not have a predecessor"
              tree.Remove(minNode.Key)
              minNode <- requireNode "new min node should exist" (tree.MinNode())
              Expect.isNull (tree.Prev(minNode)) "new minimum node should not have a predecessor either")
          testCase "should return null for successor of the max node" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let mutable maxNode: Node<int, obj> =
                  requireNode "max node should exist" (tree.MaxNode())

              Expect.isNull (tree.Next(maxNode)) "maximum node should not have a successor"
              tree.Remove(maxNode.Key)
              maxNode <- requireNode "new max node should exist" (tree.MaxNode())
              Expect.isNull (tree.Next(maxNode)) "new maximum node should not have a successor either")
          testCase "should reach end in walking" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              let keys: int array =
                  [| 49153
                     49154
                     49156
                     49157
                     49158
                     49159
                     49160
                     49161
                     49163
                     49165
                     49191
                     49199
                     49201
                     49202
                     49203
                     49204
                     49206
                     49207
                     49208
                     49209
                     49210
                     49212 |]

              keys |> Array.iter (fun key -> tree.Insert(key) |> ignore)

              let mutable minNode: Node<int, obj> =
                  requireNode "min node should exist" (tree.MinNode())

              keys
              |> Array.iter (fun key ->
                  Expect.equal minNode.Key key "walking the tree should visit nodes in order"
                  minNode <- tree.Next(minNode))

              Expect.isNull minNode "walking past the end should return null")
          testCase "bidirectional stepping" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              let keys: int array =
                  [| 49153
                     49154
                     49156
                     49157
                     49158
                     49159
                     49160
                     49161
                     49163
                     49165
                     49191
                     49199
                     49201
                     49202
                     49203
                     49204
                     49206
                     49207
                     49208
                     49209
                     49210
                     49212 |]

              tree.Load(keys) |> ignore

              let mutable minNode: Node<int, obj> =
                  requireNode "min node should exist" (tree.MinNode())

              keys
              |> Array.iteri (fun index key ->
                  Expect.equal minNode.Key key "walking should stay in order"

                  if index <> 0 then
                      let prevNode: Node<int, obj> =
                          requireNode "previous node should exist" (tree.Prev(minNode))

                      let nextNode: Node<int, obj> =
                          requireNode "next node should exist" (tree.Next(prevNode))

                      Expect.equal nextNode.Key key "next after prev should return the original node"

                  minNode <- tree.Next(minNode))

              Expect.isNull minNode "final successor should be null")
          testCase "should find successor and predecessor for 2-nodes tree" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Insert(5) |> ignore
              tree.Insert(10) |> ignore

              let minNode: Node<int, obj> = requireNode "min node should exist" (tree.MinNode())
              Expect.equal minNode.Key 5 "minimum key should be 5"
              Expect.isNull (tree.Prev(minNode)) "minimum node should have no predecessor"
              Expect.equal (requireNode "successor should exist" (tree.Next(minNode))).Key 10 "successor should be 10"

              let maxNode: Node<int, obj> = requireNode "max node should exist" (tree.MaxNode())
              Expect.equal maxNode.Key 10 "maximum key should be 10"
              Expect.isNull (tree.Next(maxNode)) "maximum node should have no successor"
              Expect.equal (requireNode "predecessor should exist" (tree.Prev(maxNode))).Key 5 "predecessor should be 5")
          testCase "should be able to get a node by its index" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              for value = 0 to 9 do
                  Expect.equal (requireNode "node should exist" (tree.At(value))).Key value "at should return nodes in sorted order"

              Expect.isNull (tree.At(10)) "out of range index should return null"
              Expect.isNull (tree.At(-1)) "negative index should return null")
          testCase "should support range walking" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let items: ResizeArray<int> = ResizeArray<int>()

              tree.Range(
                  3,
                  8,
                  fun node ->
                      items.Add(node.Key)
                      false
              )
              |> ignore

              Expect.equal (items.ToArray()) [| 3; 4; 5; 6; 7; 8 |] "range should walk the inclusive interval")
          testCase "should support range walking with non-existent low key" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let items: ResizeArray<int> = ResizeArray<int>()

              tree.Range(
                  -3,
                  5,
                  fun node ->
                      items.Add(node.Key)
                      false
              )
              |> ignore

              Expect.equal (items.ToArray()) [| 0; 1; 2; 3; 4; 5 |] "range should include values from the smallest matching node")
          testCase "should support range walking with non-existent high key" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let items: ResizeArray<int> = ResizeArray<int>()

              tree.Range(
                  3,
                  15,
                  fun node ->
                      items.Add(node.Key)
                      false
              )
              |> ignore

              Expect.equal (items.ToArray()) [| 3; 4; 5; 6; 7; 8; 9 |] "range should stop at the largest node in range")
          testCase "should support range walking with both keys out of range" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let items: ResizeArray<int> = ResizeArray<int>()

              tree.Range(
                  10,
                  20,
                  fun node ->
                      items.Add(node.Key)
                      false
              )
              |> ignore

              Expect.equal items.Count 0 "range outside the tree should visit no nodes"

              tree.Range(
                  -10,
                  20,
                  fun node ->
                      items.Add(node.Key)
                      false
              )
              |> ignore

              expectSequenceEqual items (tree.Keys ()) "range spanning the whole tree should visit every key")
          testCase "should support range walking with interruption" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()

              for value = 0 to 9 do
                  tree.Insert(value) |> ignore

              let items: ResizeArray<int> = ResizeArray<int>()

              tree.Range(
                  2,
                  8,
                  fun node ->
                      items.Add(node.Key)

                      node.Key = 5
              )
              |> ignore

              Expect.equal (items.ToArray()) [| 2; 3; 4; 5 |] "range should stop when the visitor requests interruption") ]

let updateTests: Test =
    testList
        "update"
        [ testCase "split" (fun _ ->
              let mutable tree: Tree<float, obj> = createFloatTree [| 1.0; 2.0; 3.0 |]
              let mutable split = tree.Split(0.0)
              Expect.isNull split.Left "split below minimum should have no left branch"
              Expect.equal (nodeKeys split.Right) [| 1.0; 2.0; 3.0 |] "right branch should contain all nodes"

              tree <- createFloatTree [| 1.0; 2.0; 3.0 |]
              split <- tree.Split(2.5)
              Expect.equal (nodeKeys split.Left) [| 1.0; 2.0 |] "left branch should contain nodes below the split key"
              Expect.equal (nodeKeys split.Right) [| 3.0 |] "right branch should contain nodes above the split key"

              tree <- createFloatTree [| 1.0; 2.0; 3.0 |]
              split <- tree.Split(2.0)
              Expect.equal (nodeKeys split.Left) [| 1.0 |] "left branch should exclude the split key"
              Expect.equal (nodeKeys split.Right) [| 3.0 |] "right branch should exclude the split key"

              tree <- createFloatTree [| 1.0; 2.0; 3.0 |]
              split <- tree.Split(1.0)
              Expect.equal (nodeKeys split.Left) [||] "left branch should be empty at the minimum key"
              Expect.equal (nodeKeys split.Right) [| 2.0; 3.0 |] "right branch should contain the remaining nodes"

              tree <- createFloatTree [| 1.0; 2.0; 3.0 |]
              split <- tree.Split(3.0)
              Expect.equal (nodeKeys split.Left) [| 1.0; 2.0 |] "left branch should contain nodes below the maximum key"
              Expect.equal (nodeKeys split.Right) [||] "right branch should be empty at the maximum key")
          testCase "merge" (fun _ ->
              let tree: Tree<float, obj> = createFloatTree [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
              tree.Update(3.0, 6.0)
              expectSequenceEqual (tree.Keys ()) [| 1.0; 2.0; 4.0; 5.0; 6.0 |] "update should move a node to the right partition"
              tree.Update(2.0, 0.0)
              expectSequenceEqual (tree.Keys ()) [| 0.0; 1.0; 4.0; 5.0; 6.0 |] "update should move a node to the left partition"
              tree.Update(0.0, 7.0)
              expectSequenceEqual (tree.Keys ()) [| 1.0; 4.0; 5.0; 6.0; 7.0 |] "update should continue to preserve order"
              tree.Update(7.0, -3.0)
              expectSequenceEqual (tree.Keys ()) [| -3.0; 1.0; 4.0; 5.0; 6.0 |] "update should keep the tree ordered after repeated moves") ]

let bulkLoadTests: Test =
    testList
        "bulk-load"
        [ testCase "should allow bulk-insert" (fun _ ->
              let tree: Tree<int, int> = Tree<int, int>()
              let keys: int array = [| 1; 2; 3; 4 |]
              let values: int array = [| 4; 3; 2; 1 |]
              tree.Load(keys, values) |> ignore
              expectSequenceEqual (tree.Keys ()) keys "bulk insert should preserve sorted keys"
              expectSequenceEqual (tree.Values()) values "bulk insert should preserve matching values")
          testCase "should allow bulk-insert without values" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              let keys: int array = [| 1; 2; 3; 4 |]
              tree.Load(keys) |> ignore
              expectSequenceEqual (tree.Keys ()) keys "bulk insert without values should preserve keys"
              expectSequenceEqual (tree.Values()) (Array.zeroCreate<obj> keys.Length) "bulk insert without values should use default values")
          testCase "should be able to load into a tree with contents" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Load([| 22; 56; 0; -10; 12 |], true) |> ignore
              tree.Load([| 100; 500; -400; 20; 10 |], true) |> ignore
              expectSequenceEqual (tree.Keys ()) [| -400; -10; 0; 10; 12; 20; 22; 56; 100; 500 |] "bulk load should merge with existing contents")
          testCase "should be able to load less contents into a tree with contents" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Load([| 100; 500; -400; 20; 10 |], true) |> ignore
              tree.Load([| 22 |], true) |> ignore
              expectSequenceEqual (tree.Keys ()) [| -400; 10; 20; 22; 100; 500 |] "bulk load should merge a smaller batch")
          testCase "should be able to load more contents into a tree with less contents" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Load([| 22 |], true) |> ignore
              tree.Load([| 100; 500; -400; 20; 10 |], true) |> ignore
              expectSequenceEqual (tree.Keys ()) [| -400; 10; 20; 22; 100; 500 |] "bulk load should merge a larger batch")
          testCase "should be able to load into a tree with contents (interleave)" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Load(Array.init 10 (fun index -> index * 10)) |> ignore
              tree.Load(Array.init 10 (fun index -> 5 + (10 * index))) |> ignore
              expectSequenceEqual (tree.Keys ()) (Array.init 20 (fun index -> 5 * index)) "bulk load should interleave sorted lists correctly") ]

let regressionTests: Test =
    testList
        "ported regressions"
        [ testCase "bulk-load overload should support presort without explicit values" (fun _ ->
              let tree: Tree<int, obj> = Tree<int, obj>()
              tree.Load([| 3; 1; 2 |], true) |> ignore
              expectSequenceEqual (tree.Keys ()) [| 1; 2; 3 |] "presort overload should sort and load keys without requiring a values array") ]

let tests: Test =
    testList
        "splay-tree"
        [ compareTests
          containsTests
          duplicateTests
          emptyTests
          findTests
          insertTests
          iterateTests
          ienumerableTests
          keysValuesTests
          minMaxTests
          printTests
          removeTests
          traversalTests
          updateTests
          bulkLoadTests
          regressionTests ]

[<EntryPoint>]
let main (argv: string array) : int = runTestsWithCLIArgs [] argv tests
