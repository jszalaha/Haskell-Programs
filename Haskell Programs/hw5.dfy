/* 
*  hw5.dfy
*  author: Jordan Zalaha
*  p.p. parter: Shrey Valia
*  date: March 14, 2016
*/

// 1.

method Min(a: int, b:int) returns (c: int)
  ensures (c == a || c == b) && (c <= a && c <= b)
{
  if a < b
  { c := a; }
  else 
  { c := b; }
}

method TestMin()
{
  var m := Min(12,5);
  assert m == 5;
  var n := Min(23,42);
  assert n == 23;
}


// 2. 

method Search(arr: array<int>, element: int) returns (idx: int)
  requires arr != null
  ensures 0 <= idx < arr.Length || idx == -1
  ensures (0 <= idx ==> idx < arr.Length && arr[idx] == element)
  ensures (idx < 0 ==> forall k :: 0 <= k < arr.Length ==> arr[k] != element)
{
  var n := 0;
  while (n < arr.Length)
    invariant 0 <= n <= arr.Length
    invariant forall k :: 0 <= k < n ==> arr[k] != element
  {
    if (arr[n] == element) {
      return n;
    }
    n := n + 1;
  }
  return -1;
}

method TestSearch()
{
  var arr := new int[3];
  arr[0] := 23;
  arr[1] := 21;
  arr[2] := 22;
  var s := Search(arr, 21);
  assert s == 1;
  var t := Search(arr, 20);
  assert t == -1;
}

// 3.

predicate sorted(a: array<int>)
  requires a != null
  reads a
{
  forall j, k : int :: 0 <= j < k < a.Length ==> a[j] < a[k]
}

method BinarySearch(arr: array<int>, element: int) returns (idx: int)
  requires arr != null
  requires sorted(arr)
  requires 0 <= arr.Length
  ensures 0 <= idx < arr.Length || idx == -1
  ensures 0 <= idx ==> idx < arr.Length && arr[idx] == element
  ensures idx < 0 ==> forall k :: 0 <= k < arr.Length ==> arr[k] != element
{
  if (arr.Length == 0) {
    return -1;
  }
  var left := 0;
  var right := arr.Length;
  while (left < right)
   invariant 0 <= left <= right <= arr.Length
   invariant forall k :: 0 <= k < arr.Length && !(left <= k < right) ==> arr[k] != element
  {
    var mid := (left + right) / 2;
    if (arr[mid] == element) {
      return mid;
    }
    if (arr[mid] < element) {
      left := mid + 1;
    } else {
      right := mid;
    }
  }
  return -1;
}

method TestBinarySearch()
{
  var arr := new int[3];
  arr[0] := 21;
  arr[1] := 22;
  arr[2] := 23;
  var s := BinarySearch(arr, 22);
  assert s == 1;
  var t := BinarySearch(arr, 24);
  assert t == -1;
}