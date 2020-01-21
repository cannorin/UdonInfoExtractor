module Trie

type StringTrie<'a> = internal {
  Value: (string * 'a) option
  Children: Map<char, StringTrie<'a>>
}

module StringTrie =
  let empty = { Value = None; Children = Map.empty }

  let add (key: string) value trie =
    let rec go trie = function
      | [] -> { trie with Value = Some (key, value) }
      | c :: chars ->
        let child = go (Map.tryFind c trie.Children |> Option.defaultValue empty) chars
        let children = trie.Children |> Map.remove c
        { trie with Children = children |> Map.add c child }
    go trie (Seq.toList key)

  [<NoDynamicInvocation>]
  let inline private findLike onNil onConsNone (key: string) trie =
    let rec go trie = function
      | [] -> onNil trie
      | c :: chars ->
        match Map.tryFind c trie.Children with
        | None -> onConsNone trie
        | Some t -> go t chars
    go trie (Seq.toList key)
        
  let contains key trie =
    findLike
      (fun trie -> trie.Value |> Option.isSome)
      (fun _ -> false)
      key trie

  let find (key: string) trie =
    findLike
      (function { Value = Some v } -> snd v | _ -> invalidArg key "key not found")
      (fun _ -> invalidArg key "key not found")
      key trie

  let tryFind (key: string) trie =
    findLike (fun trie -> trie.Value |> Option.map snd) (fun _ -> None) key trie

  let withPrefix (prefix: string) trie =
    findLike id (fun _ -> empty) prefix trie

  let length trie =
    let rec go trie i =
      let i = if trie.Value |> Option.isSome then i + 1 else i
      trie.Children |> Map.fold (fun state _ t -> go t state) i
    go trie 0

  let map (f: string -> 'a -> 'b) (trie: StringTrie<'a>) =
    let rec go trie =
      { Value = trie.Value |> Option.map (fun (k, v) -> k, f k v)
        Children = trie.Children |> Map.map (fun _ v -> go v) }
    go trie

  let filter (p: string -> 'a -> bool) (trie: StringTrie<'a>) =
    let rec go trie =
      { Value = trie.Value |> Option.filter (fun (k, v) -> p k v)
        Children = trie.Children |> Map.map (fun _ v -> go v) }
    go trie

  let fold (f: 'State -> string -> 'a -> 'State) (state: 'State) (trie: StringTrie<'a>) =
    let rec go state trie =
      let state =
        match trie.Value with Some (k, v) -> f state k v | None -> state
      trie.Children |> Map.fold (fun state _ v -> go state v) state
    go state trie

  let ofSeq   (xs: (string * 'a) seq)  = xs |> Seq.fold   (fun state (k, v) -> state |> add k v) empty
  let ofList  (xs: (string * 'a) list) = xs |> List.fold  (fun state (k, v) -> state |> add k v) empty
  let ofArray (xs: (string * 'a)[])    = xs |> Array.fold (fun state (k, v) -> state |> add k v) empty

  let rec toSeq trie =
    seq {
      match trie.Value with Some v -> yield v | None -> ()
      for _, v in trie.Children |> Map.toSeq do
        yield! toSeq v
    }
  let toList trie = toSeq trie |> Seq.toList
  let toArray trie = toSeq trie |> Seq.toArray

type StringTrie<'a> with
  static member Map (x, f) = StringTrie.map f x
  static member ToSeq x = StringTrie.toSeq x
  static member Zero = StringTrie.empty

