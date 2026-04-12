namespace SplayTree

open System


/// Just use a ResizeArray as a stack
type Stack<'T> = ResizeArray<'T>

module Stack =

    let inline push (item: 'T) (stack: Stack<'T>) : unit =
        stack.Add(item)

    let inline pop (stack: Stack<'T>) : 'T =
        // if stack.Count = 0 then  raise (InvalidOperationException("Stack is empty"))
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr stack "$0.pop()"
        #else
            let item: 'T = stack.[stack.Count - 1]
            stack.RemoveAt(stack.Count - 1)
            item
        #endif

module Null =

    let inline isNull' (x: obj) : bool =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr x "$0 === null"
        #else
            Object.ReferenceEquals(x, null)
        #endif


    let inline isNotNull (x: obj) : bool =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr x "$0 !== null"
        #else
            not (Object.ReferenceEquals(x, null))
        #endif

    let inline null'() : 'T =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr () "null"
        #else
            Unchecked.defaultof<'T>
        #endif

    /// DO NOT USE WITH NUMERIC TYPES IN FABLE
    /// They are JS Float64Array
    let inline initArray<'T> (size: int) : 'T[] =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr size "new Array($0)"
        #else
            Array.zeroCreate<'T> size
        #endif

open Null

#if FABLE_COMPILER
type Node<'K, 'V> = { //  In Fable use a record to have direct member access without getters/setters,
    Key: 'K
    Data: 'V
    mutable Left: Node<'K, 'V>
    mutable Right: Node<'K, 'V>
    mutable Next: Node<'K, 'V>
    }

module Node =

    let inline create (key: 'K) (data: 'V)   =
        { Key = key; Data = data; Left = null'(); Right = null'(); Next = null'() }

    let inline empty() : Node<'K, 'V> =
        { Key = null'(); Data = null'(); Left = null'(); Right = null'(); Next = null'() }
#else

[<AllowNullLiteralAttribute>]
type Node<'K, 'V>(key: 'K, data: 'V) =
    member this.Key = key
    member this.Data = data
    member val Left: Node<'K, 'V> = null with get, set
    member val Right: Node<'K, 'V> = null with get, set
    member val Next: Node<'K, 'V> = null with get, set

module Node =

    let inline create (key: 'K) (data: 'V)  = Node(key, data)

    let inline empty() : Node<'K, 'V> = Node(null'(), null'())
#endif



type Comparer<'K> =
    'K -> 'K -> int


type NodePrinter<'K, 'V> =
    Node<'K, 'V> -> string

type StringCollector =
    string -> unit

[<NoComparison>]
type TreeNodeList<'K, 'V> = {
    mutable Head: Node<'K, 'V>
    }

[<NoComparison>]
type SplitResult<'K, 'V> = {
    Left: Node<'K, 'V>
    Right: Node<'K, 'V>
    }

[<AllowNullLiteral>]
type TreeItem<'K, 'V>(key: 'K, data: 'V) =
    member this.Key: 'K = key
    member this.Data: 'V = data

module private TreeHelpers =

    // follows "An implementation of top-down splaying"
    // by D. Sleator <sleator@cs.cmu.edu> March 1992

    let defaultComparer (a: 'K) (b: 'K) : int =
        let cmp: int = Collections.Generic.Comparer<'K>.Default.Compare(a, b)
        if cmp > 0 then 1
        elif cmp < 0 then -1
        else 0

    /// Simple top down splay, not requiring i to be in the tree t.
    let splay (i: 'K) (t: Node<'K, 'V>) (comparer: Comparer<'K>) : Node<'K, 'V> =
        let n: Node<'K, 'V> = Node.empty()

        let mutable l: Node<'K, 'V> = n
        let mutable r: Node<'K, 'V> = n
        let mutable current: Node<'K, 'V> = t
        let mutable loopOn: bool = true

        while loopOn do
            let cmp: int = comparer i current.Key

            //if (i < t.Key) {
            if cmp < 0 then
                if isNull' current.Left then
                    loopOn <- false
                else
                    //if (i < t.Left.Key) {
                    if comparer i current.Left.Key < 0 then
                        let y: Node<'K, 'V> = current.Left // rotate right
                        current.Left <- y.Right
                        y.Right <- current
                        current <- y

                        if isNull' current.Left then
                            loopOn <- false

                    if loopOn then
                        r.Left <- current // link right
                        r <- current
                        current <- current.Left

            //} else if (i > t.Key) {
            elif cmp > 0 then
                if isNull' current.Right then
                    loopOn <- false
                else
                    //if (i > t.Right.Key) {
                    if comparer i current.Right.Key > 0 then
                        let y: Node<'K, 'V> = current.Right // rotate left
                        current.Right <- y.Left
                        y.Left <- current
                        current <- y

                        if isNull' current.Right then
                            loopOn <- false

                    if loopOn then
                        l.Right <- current // link left
                        l <- current
                        current <- current.Right
            else
                loopOn <- false

        // assemble
        l.Right <- current.Left
        r.Left <- current.Right
        current.Left <- n.Right
        current.Right <- n.Left
        current

    let insert (i: 'K) (data: 'V) (t: Node<'K, 'V>) (comparer: Comparer<'K>) : Node<'K, 'V> =
        let node = Node.create i data

        if isNull' t then
            node.Left <- null'()
            node.Right <- null'()
            node
        else
            let mutable root: Node<'K, 'V> = splay i t comparer
            let cmp: int = comparer i root.Key

            if cmp < 0 then
                node.Left <- root.Left
                node.Right <- root
                root.Left <- null'()
            elif cmp >= 0 then
                node.Right <- root.Right
                node.Left <- root
                root.Right <- null'()

            node

    let split (key: 'K) (v: Node<'K, 'V>) (compare: Comparer<'K>) : SplitResult<'K, 'V> =
        let mutable left: Node<'K, 'V> = null'()
        let mutable right: Node<'K, 'V> = null'()

        if isNotNull v then
            let mutable value: Node<'K, 'V> = splay key v compare
            let cmp: int = compare value.Key key

            if cmp = 0 then
                left <- value.Left
                right <- value.Right
            elif cmp < 0 then
                right <- value.Right
                value.Right <- null'()
                left <- value
            else
                left <- value.Left
                value.Left <- null'()
                right <- value

        { Left = left; Right = right }

    let merge (left: Node<'K, 'V>) (right: Node<'K, 'V>) (compare: Comparer<'K>) : Node<'K, 'V> =
        if isNull' right then
            left
        elif isNull' left then
            right
        else
            let mutable rightNode: Node<'K, 'V> = splay left.Key right compare
            rightNode.Left <- left
            rightNode

    ///Prints level of the tree

    let rec printRow (root: Node<'K, 'V>) (prefix: string) (isTail: bool) (out: StringCollector) (printNode: NodePrinter<'K, 'V>) : unit =
        if isNotNull root then
            let branch: string = if isTail then "└── " else "├── "

            out $"{prefix}{branch}{printNode root}\n"

            let indent: string = prefix + (if isTail then "    " else "│   ")

            if isNotNull root.Left then
                printRow root.Left indent false out printNode

            if isNotNull root.Right then
                printRow root.Right indent true out printNode

    let rec loadRecursive (keys: 'K[]) (values: 'V[]) (start: int) (finish: int) : Node<'K, 'V> =
        let size: int = finish - start

        if size > 0 then
            let middle: int = start + (size / 2)
            let key = keys.[middle]
            let data  = values.[middle]
            let node = Node.create key data
            node.Left <- loadRecursive keys values start middle
            node.Right <- loadRecursive keys values (middle + 1) finish
            node
        else
            null'()

    let createList (keys: 'K[]) (values: 'V[]) : Node<'K, 'V> =
        let head = Node.empty()

        let mutable p: Node<'K, 'V> = head

        for i = 0 to keys.Length - 1 do
            let node : Node<'K, 'V> = Node.create keys.[i] values.[i]
            p.Next <- node
            p <- p.Next

        p.Next <- null'()
        head.Next



    // toList performs an in-order traversal of the tree and chains all nodes into a sorted linked list using each node's next pointer.
    // It's the classic iterative in-order walk using an explicit stack (Q).
    // It exists to support bulk-loading in the load() method.
    //  - Convert the existing tree to a sorted linked list (this.toList())
    //  - Create a sorted linked list from the new keys (createList(...))
    //  - Merge the two sorted lists (mergeLists(...)) — like merge-sort's merge step
    //  - Rebuild a balanced BST from the merged sorted list (sortedListToBST(...))
    // This is a standard O(n) technique for bulk-inserting pre-sorted data into a BST —
    // much faster than inserting keys one-by-one (which would be O(n log n) at best,
    // worse for a splay tree due to splaying overhead).
    let toList (root: Node<'K, 'V>) : Node<'K, 'V> =
        let mutable current: Node<'K, 'V> = root
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>()
        let mutable loopOn: bool = true

        let head = Node.empty()

        let mutable p: Node<'K, 'V> = head

        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                let node: Node<'K, 'V> = Stack.pop q
                p.Next <- node
                p <- p.Next
                current <- node.Right
            else
                loopOn <- false

        p.Next <- null'() // that'll work even if the tree was empty
        head.Next

    let rec sortedListToBST (list: TreeNodeList<'K, 'V>) (start: int) (finish: int) : Node<'K, 'V> =
        let size: int = finish - start

        if size > 0 then
            let middle: int = start + (size / 2)
            let left: Node<'K, 'V> = sortedListToBST list start middle
            let root: Node<'K, 'V> = list.Head
            root.Left <- left
            list.Head <- list.Head.Next
            root.Right <- sortedListToBST list (middle + 1) finish
            root
        else
            null'()

    let mergeLists (l1: Node<'K, 'V>) (l2: Node<'K, 'V>) (compare: Comparer<'K>) : Node<'K, 'V> =
        let head = Node.empty() // dummy

        let mutable p: Node<'K, 'V> = head
        let mutable p1: Node<'K, 'V> = l1
        let mutable p2: Node<'K, 'V> = l2

        while isNotNull p1 && isNotNull p2 do
            if compare p1.Key p2.Key < 0 then
                p.Next <- p1
                p1 <- p1.Next
            else
                p.Next <- p2
                p2 <- p2.Next

            p <- p.Next

        if isNotNull p1 then
            p.Next <- p1
        elif isNotNull p2 then
            p.Next <- p2

        head.Next

    let rec sort (keys: 'K []) (values: 'V []) (left: int) (right: int) (compare: Comparer<'K>) : unit =
        if left < right then
            let pivot: 'K = keys.[(left + right) >>> 1]
            let mutable i: int = left - 1
            let mutable j: int = right + 1
            let mutable loopOn: bool = true

            while loopOn do
                i <- i + 1

                while compare keys.[i] pivot < 0 do
                    i <- i + 1

                j <- j - 1

                while compare keys.[j] pivot > 0 do
                    j <- j - 1

                if i >= j then
                    loopOn <- false
                else
                    let keyTmp: 'K = keys.[i]
                    keys.[i] <- keys.[j]
                    keys.[j] <- keyTmp

                    let valueTmp: 'V = values.[i]
                    values.[i] <- values.[j]
                    values.[j] <- valueTmp

            sort keys values left j compare
            sort keys values (j + 1) right compare



type Tree<'K, 'V>(comparer: Comparer<'K>) =
    let _compare: Comparer<'K> = comparer
    let mutable _root: Node<'K, 'V> = null'()
    let mutable _size: int = 0

    new() = Tree<'K, 'V>(TreeHelpers.defaultComparer)

    // Inserts a key, allows duplicates

    member this.Insert(key: 'K, data: 'V) : Node<'K, 'V> =
        _size <- _size + 1
        _root <- TreeHelpers.insert key data _root _compare
        _root

    /// Inserts a key, allows duplicates
    member this.Insert(key: 'K) : Node<'K, 'V> =
        this.Insert(key, null'())

    /// Adds a key, if it is not present in the tree
    member this.Add(key: 'K) : Node<'K, 'V> =
        this.Add(key, null'())

    /// Adds a key, if it is not present in the tree
    member this.Add(key: 'K, data: 'V) : Node<'K, 'V> =
        let node= Node.create key data

        if isNull' _root then
            node.Left <- null'()
            node.Right <- null'()
            _size <- _size + 1
            _root <- node

        let t: Node<'K, 'V> = TreeHelpers.splay key _root _compare
        let cmp: int = _compare key t.Key

        if cmp = 0 then
            _root <- t
        else
            if cmp < 0 then
                node.Left <- t.Left
                node.Right <- t
                t.Left <- null'()
            elif cmp > 0 then
                node.Right <- t.Right
                node.Left <- t
                t.Right <- null'()

            _size <- _size + 1
            _root <- node

        _root

    /// @param  {Key} key
    /// @return {Node|nullLiteral()}
    member this.Remove(key: 'K) : unit =
        _root <- this._Remove (key, _root, _compare)

    /// Deletes i from the tree if it's there
    member private this._Remove(i: 'K, t: Node<'K, 'V>, compare: Comparer<'K>) : Node<'K, 'V> =
        if isNull' t then
            null'()
        else
            let mutable root: Node<'K, 'V> = TreeHelpers.splay i t compare
            let cmp: int = compare i root.Key

            if cmp = 0 then
                // found it
                _size <- _size - 1
                TreeHelpers.merge root.Left root.Right compare
            else
                root // It wasn't there

    /// Removes and returns the node with smallest key
    member this.Pop() : TreeItem<'K, 'V> option =
        let mutable node: Node<'K, 'V> = _root

        if isNotNull node then
            while isNotNull node.Left do
                node <- node.Left

            _root <- TreeHelpers.splay node.Key _root _compare
            _root <- this._Remove (node.Key, _root, _compare)
            Some(TreeItem<'K, 'V>(node.Key, node.Data))
        else
            None

    /// Find without splaying
    member this.FindStatic(key: 'K) : Node<'K, 'V> =
        let mutable current: Node<'K, 'V> = _root

        while isNotNull current && _compare key current.Key <> 0 do
            let cmp: int = _compare key current.Key

            if cmp < 0 then
                current <- current.Left
            else
                current <- current.Right

        current

    member this.Find(key: 'K) : Node<'K, 'V> =
        if isNotNull _root then
            _root <- TreeHelpers.splay key _root _compare

            if _compare key _root.Key <> 0 then
                null'()
            else
                _root
        else
            _root

    member this.Contains(key: 'K) : bool =
        let mutable current: Node<'K, 'V> = _root
        let mutable found: bool = false

        while isNotNull current && not found do
            let cmp: int = _compare key current.Key

            if cmp = 0 then found <- true
            elif cmp < 0 then current <- current.Left
            else current <- current.Right

        found


    member this.ForEach(visitor: Node<'K, 'V> -> bool) : unit = //: Tree<'K, 'V> =
        let mutable current: Node<'K, 'V> = _root
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
        let mutable loopOn: bool = true
        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q
                visitor current |> ignore
                current <- current.Right
            else
                loopOn <- false


    /// Walk key range from `low` to `high`. Stops if `visitor` returns a value.
    /// Walk key range from `low` to `high`. Stops if `visitor` returns a value.
    member this.Range(low: 'K, high: 'K, visitor: Node<'K, 'V> -> bool) : unit = // :Tree<'K, 'V> =
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>()
        let mutable node: Node<'K, 'V> = _root
        let mutable loopOn: bool = true
        while loopOn && (q.Count > 0 || isNotNull node) do
            if isNotNull node then
                Stack.push node q
                node <- node.Left
            else
                node <- Stack.pop q

                let cmp: int = _compare node.Key high

                if cmp > 0 then
                    loopOn <- false
                else
                    if _compare node.Key low >= 0 then
                        if visitor node then
                            loopOn <- false

                node <- node.Right


    member this.Min() : 'K option =
        if isNotNull _root then
            Some(this.MinNode().Key)
        else
            None

    member this.Max() : 'K option =
        if isNotNull _root then
            Some(this.MaxNode().Key)
        else
            None

    member this.MinNode() : Node<'K, 'V> =
        this.MinNode(_root)

    member this.MinNode(t: Node<'K, 'V>) : Node<'K, 'V> =
        let mutable node: Node<'K, 'V> = t
        if isNotNull node then
            while isNotNull node.Left do
                node <- node.Left
        node

    member this.MaxNode() : Node<'K, 'V> =
        this.MaxNode(_root)

    member this.MaxNode(t: Node<'K, 'V>) : Node<'K, 'V> =
        let mutable node: Node<'K, 'V> = t
        if isNotNull node then
            while isNotNull node.Right do
                node <- node.Right
        node

    /// Returns node at given index
    member this.At(index: int) : Node<'K, 'V> =
        let mutable current: Node<'K, 'V> = _root
        let mutable loopOn: bool = true
        let mutable i: int = 0
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>()
        let mutable result: Node<'K, 'V> = null'()
        while loopOn && isNull' result do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q

                if i = index then
                    result <- current
                else
                    i <- i + 1
                    current <- current.Right
            else
                loopOn <- false
        result

    member this.Next(d: Node<'K, 'V>) : Node<'K, 'V> =
        let mutable root: Node<'K, 'V> = _root
        let mutable successor: Node<'K, 'V> = null'()

        if isNotNull d.Right then
            successor <- d.Right

            while isNotNull successor.Left do
                successor <- successor.Left

            successor
        else
            let mutable loopOn: bool = true

            while loopOn && isNotNull root do
                let cmp: int = _compare d.Key root.Key

                if cmp = 0 then
                    loopOn <- false
                elif cmp < 0 then
                    successor <- root
                    root <- root.Left
                else
                    root <- root.Right

            successor

    member this.Prev(d: Node<'K, 'V>) : Node<'K, 'V> =
        let mutable root: Node<'K, 'V> = _root
        let mutable predecessor: Node<'K, 'V> = null'()

        if isNotNull d.Left then
            predecessor <- d.Left

            while isNotNull predecessor.Right do
                predecessor <- predecessor.Right

            predecessor
        else
            let mutable loopOn: bool = true

            while loopOn && isNotNull root do
                let cmp: int = _compare d.Key root.Key

                if cmp = 0 then
                    loopOn <- false
                elif cmp < 0 then
                    root <- root.Left
                else
                    predecessor <- root
                    root <- root.Right

            predecessor

    member this.Clear() : Tree<'K, 'V> =
        _root <- null'()
        _size <- 0
        this

    /// toList performs an in-order traversal of the tree and chains all nodes into a sorted linked list using each node's next pointer.
    /// It's the classic iterative in-order walk using an explicit stack (Q).
    /// It exists to support bulk-loading in the load() method.
    ///  - Convert the existing tree to a sorted linked list (this.toList())
    ///  - Create a sorted linked list from the new keys (createList(...))
    ///  - Merge the two sorted lists (mergeLists(...)) — like merge-sort's merge step
    ///  - Rebuild a balanced BST from the merged sorted list (sortedListToBST(...))
    /// This is a standard O(n) technique for bulk-inserting pre-sorted data into a BST —
    /// much faster than inserting keys one-by-one (which would be O(n log n) at best,
    /// worse for a splay tree due to splaying overhead).
    member this.ToList() : Node<'K, 'V> =
        TreeHelpers.toList _root

    /// Bulk-load items. Both array have to be same size
    member this.Load(keys: 'K array) : Tree<'K, 'V> =
        this.Load(keys, Array.zeroCreate<'V> keys.Length, false)

    /// Bulk-load items. Both array have to be same size
    member this.Load(keys: 'K array, presort: bool) : Tree<'K, 'V> =
        this.Load(keys, Array.zeroCreate<'V> keys.Length, presort)

    /// Bulk-load items. Both array have to be same size
    member this.Load(keys: 'K array, values: 'V array) : Tree<'K, 'V> =
        this.Load(keys, values, false)

    /// Bulk-load items. Both array have to be same size
    member this.Load(keys: 'K array, values: 'V array, presort: bool) : Tree<'K, 'V> =
        let mutable size: int = keys.Length

        // sort if needed
        if presort then
            TreeHelpers.sort keys values 0 (size - 1) _compare

        if isNull' _root then
            // empty tree
            _root <- TreeHelpers.loadRecursive keys values 0 size
            _size <- size
        else
            // that re-builds the whole tree from two in-order traversals
            let mergedList: Node<'K, 'V> =
                TreeHelpers.mergeLists (this.ToList()) (TreeHelpers.createList keys values) _compare

            size <- _size + size
            _root <- TreeHelpers.sortedListToBST { Head = mergedList } 0 size
            _size <- size

        this

    member this.IsEmpty() : bool =
        isNull' _root

    member this.Size: int =
        _size

    member this.Root: Node<'K, 'V> =
        _root

    member this.ToString(printNode: NodePrinter<'K, 'V>) : string =
        let out: ResizeArray<string> = ResizeArray<string>()
        TreeHelpers.printRow _root "" true out.Add printNode
        String.Concat(out)

    override this.ToString() : string =
        this.ToString(fun n -> string n.Key)

    member this.Update(key: 'K, newKey: 'K) : unit =
        this.Update(key, newKey, null'())

    member this.Update(key: 'K, newKey: 'K, newData: 'V) : unit =
        let splitResult: SplitResult<'K, 'V> = TreeHelpers.split key _root _compare
        let mutable left: Node<'K, 'V> = splitResult.Left
        let mutable right: Node<'K, 'V> = splitResult.Right

        if _compare key newKey < 0 then
            right <- TreeHelpers.insert newKey newData right _compare
        else
            left <- TreeHelpers.insert newKey newData left _compare

        _root <- TreeHelpers.merge left right _compare

    member this.Split(key: 'K) : SplitResult<'K, 'V> =
        TreeHelpers.split key _root _compare



    /// Returns a lazy sequence of the nodes in the tree, in order.
    /// An IEnumerable
    member this.ToSeq() : seq<Node<'K, 'V>> =
        seq {
            let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
            let mutable current: Node<'K, 'V> = _root
            let mutable loopOn: bool = true
            while loopOn do
                if isNotNull current then
                    Stack.push current q
                    current <- current.Left
                elif q.Count > 0 then
                    current <- Stack.pop q
                    yield current
                    current <- current.Right
                else
                    loopOn <- false
        }

    /// Returns a ResizeArray of the nodes in the tree, in order.
    member this.ToResizeArray() : ResizeArray<Node<'K, 'V>> =
        let res = ResizeArray<Node<'K, 'V>>(_size)
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
        let mutable current: Node<'K, 'V> = _root
        let mutable loopOn: bool = true
        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q
                res.Add(current)
                current <- current.Right
            else
                loopOn <- false
        res

    /// Returns a ResizeArray of the nodes in the tree, in order.
    member this.ToArray() : Node<'K, 'V>[] =
        let res = initArray<Node<'K, 'V>>(_size)
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
        let mutable current: Node<'K, 'V> = _root
        let mutable loopOn: bool = true
        let mutable i: int = 0
        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q
                res.[ i ] <- current
                i <- i + 1
                current <- current.Right
            else
                loopOn <- false
        res


    /// Returns ResizeArray of keys
    member this.Keys() : 'K ResizeArray =
        let keys: ResizeArray<'K> = ResizeArray<'K>(_size)
        let mutable current: Node<'K, 'V> = _root
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
        let mutable loopOn: bool = true
        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q
                keys.Add current.Key
                current <- current.Right
            else
                loopOn <- false
        keys

    /// Returns ResizeArray of all the data in the nodes
    member this.Values() : 'V ResizeArray =
        let values: ResizeArray<'V> = ResizeArray<'V>()
        let mutable current: Node<'K, 'V> = _root
        let q: Stack<Node<'K, 'V>> = Stack<Node<'K, 'V>>() // Initialize stack s
        let mutable loopOn: bool = true
        while loopOn do
            if isNotNull current then
                Stack.push current q
                current <- current.Left
            elif q.Count > 0 then
                current <- Stack.pop q
                values.Add current.Data
                current <- current.Right
            else
                loopOn <- false
        values


    // interface Collections.Generic.IEnumerable<Node<'K, 'V>> with
    //     member this.GetEnumerator() : Collections.Generic.IEnumerator<Node<'K, 'V>> = (this.asSeq ()).GetEnumerator()

    // interface Collections.IEnumerable with
    //     member this.GetEnumerator() : Collections.IEnumerator =
    //         (this :> Collections.Generic.IEnumerable<Node<'K, 'V>>).GetEnumerator() :> Collections.IEnumerator
