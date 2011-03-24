unit GraphCore;

interface

uses
  Classes;

const
  INFINITY = 32767;

type
  TNodeStatus = (nsNotInList, nsWasInList, nsNowInList);

  PLink = ^TLink;
  PNode = ^TNode;

  TLink = record
    Node1, Node2: PNode;
    Cost: Integer;
    NextLink: PLink;   // Next link in the node's list of links.
    InTree: Boolean; // Is it in the shortest path tree?
    ExtData: Integer;
  end;

  TNode = record
    Id: Integer;
    X: Integer;
    Y: Integer;
    LinkList: PLink;       // Links out of this node.
    NextNode: PNode;       // Next node in list of all nodes.
    Status: TNodeStatus; // Has it been in the tree?
    Dist: Integer;     // Distance from root.
    InLink: PLink;       // The link into this node.
  end;

  // Cell for candidate linked list.
  TCandidate = class(TObject)
    Node: PNode;
    NextCandidate: TCandidate;
  end;

  TGraph = class(TObject)
  private
  public
    NodeList: PNode;     // List of all nodes.

    constructor Create;
    destructor Destroy; override;

    procedure Clear();
    procedure CreateLink(Node_1, Node_2: PNode; Cost, ExtData1, ExtData2: Integer);
    function  CreateNode(Id, X, Y: Integer): PNode;
    procedure DeleteLink(n1, n2: PNode);
    procedure DeleteNode(target: PNode);
    procedure FindPathTree(root: PNode; Log: TStrings);
    procedure RemoveLinkFromNode(n1, n2: PNode);
    procedure ResetPathTree();
  end;

implementation

uses
  SysUtils;

{ TGraph }

constructor TGraph.Create;
begin
  inherited Create;

  // Start with an empty network.
  NodeList := nil;
end;

destructor TGraph.Destroy;
begin
  Clear();

  inherited Destroy;
end;

// Free the network's dynamically allocated memory.
procedure TGraph.Clear();
var
  node, next_node: PNode;
  link, next_link: PLink;
begin
  // Free all the nodes.
  node := NodeList;
  while (node <> nil) do begin
    // Free the node's links.
    link := node.LinkList;
    while (link <> nil) do begin
      next_link := link.NextLink;
      FreeMem(link);
      link := next_link;
    end;

    // Free the node itself.
    next_node := node.NextNode;
    FreeMem(node);
    node := next_node;
  end;
  NodeList := nil;
end;

// Find a shortest path tree rooted at this node using a
// label correcting algorithm.
procedure TGraph.FindPathTree(root: PNode; Log: TStrings);
var
  top_candidate, new_candidate: TCandidate;
  node_dist, new_dist: Integer;
  node, to_node: PNode;
  link: PLink;
  st: string;
begin
  if (root = nil) then exit;

  // Reset the tree.
  ResetPathTree;

  // Start with the root in the shortest path tree.
  root.Dist := 0;
  root.InLink := nil;
  root.Status := nsNowInList;

  new_candidate := TCandidate.Create();
  top_candidate := new_candidate;
  new_candidate.NextCandidate := nil;
  new_candidate.Node := root;

  // Repeat until the candidate list is empty.
  while (top_candidate <> nil) do begin
    // Add the first candidate to the tree.
    // Remove the entry from the candidate list.
    node := top_candidate.Node;
    new_candidate := top_candidate;
    top_candidate := top_candidate.NextCandidate;
    new_candidate.Destroy;

    node_dist := node.Dist;
    node.Status := nsNotInList;

    // Examine the node's neighbors.
    link := node.LinkList;
    while (link <> nil) do begin
      // See if there's an improved path using this node.
      to_node := link.Node2;
      new_dist := node_dist + link.Cost;
      if (new_dist < to_node.Dist) then begin
        // It's better. Update Dist and InLink.
        to_node.InLink := link;
        to_node.Dist := new_dist;

        // Add to_node to the candidate list if
        // it's not already there.
        if (to_node.Status = nsNotInList) then begin
          new_candidate := TCandidate.Create;
          new_candidate.NextCandidate := top_candidate;
          top_candidate := new_candidate;
          new_candidate.Node := to_node;
          to_node.Status := nsNowInlist;
        end;
      end; // End if (new_dist < to_node.Dist) then

      // Examine the node's next link.
      link := link.NextLink;
    end; // End examining the node's links.
  end; // End while (candidate list is not empty) ...

  // Mark the InLinks so they are easy to draw.
  to_node := NodeList;
  while (to_node <> nil) do begin
    link := to_node.InLink; // The link into this node.
    if (link <> nil) then begin
      link.InTree := True;

      // Mark the reverse link.
      node := link.Node1; // The link's start node.
      link := to_node.LinkList;
      while (link <> nil) do begin
        if (link.Node2 = node) then break;
        link := link.NextLink;
      end;
      if (link <> nil) then link.InTree := True;
    end;
    to_node := to_node.NextNode;
  end;

  // get map
  if (Log <> nil) then begin
    Log.Clear;
    to_node := NodeList;
    while (to_node <> nil) do begin
      st := IntToStr(to_node.Id);

      link := to_node.InLink;
      while (link <> nil) do begin
        node := link.Node1;
        st := st + ' < ' + IntToStr(node.Id);
        link := node.InLink;
      end;

      Log.Add(st);

      to_node := to_node.NextNode;
    end;
  end;
end;

// Remove all the links from the shortest path tree.
procedure TGraph.ResetPathTree();
var
  node: PNode;
  link: PLink;
begin
  node := NodeList;
  while (node <> nil) do begin
    node.Status := nsNotInList;
    node.Dist   := INFINITY;
    node.InLink := nil;

    link := node.LinkList;
    while (link <> nil) do begin
      link.InTree := False;
      link := link.NextLink;
    end;
    node := node.NextNode;
  end;
end;

// Delete the node from the network.
procedure TGraph.DeleteNode(target: PNode);
var
  node, next_node: PNode;
begin
  if (target = nil) then exit;

  // Free the target's links.
  while (target.LinkList <> nil) do begin
    DeleteLink(target, target.LinkList.Node2);
  end;

  // Find the target in the node list.
  node := nil;
  next_node := NodeList;
  while (next_node <> nil) do begin
    if (next_node = target) then break;
    node := next_node;
    next_node := node.NextNode;
  end;
  if (next_node = nil) then exit; // Not found.

  // Remove the node.
  if (node = nil)
  then NodeList := target.NextNode
  else node.NextNode := target.NextNode;

  FreeMem(target);

  // Redraw the network.
  ResetPathTree;
end;

// Delete a link from the network.
procedure TGraph.DeleteLink(n1, n2: PNode);
begin
  if ((n1 = nil) or (n2 = nil)) then exit;

  // Remove the link from the nodes' link lists.
  RemoveLinkFromNode(n1, n2);
  RemoveLinkFromNode(n2, n1);

  // Redraw the network.
  ResetPathTree;
end;

// Remove the link to node n2 from node n1's Links array.
procedure TGraph.RemoveLinkFromNode(n1, n2: PNode);
var
  link, next_link: PLink;
begin
  // Find the link.
  link := nil;
  next_link := n1.LinkList;
  while (next_link <> nil) do begin
    if (next_link.Node2 = n2) then break;
    link := next_link;
    next_link := link.NextLink;
  end;
  if (next_link = nil) then exit;

  // Remove the link.
  if (link = nil)
  then n1.LinkList := next_link.NextLink
  else link.NextLink := next_link.NextLink;
  
  FreeMem(next_link);
end;

// Create a new node.
function TGraph.CreateNode(Id, X, Y: Integer): PNode;
var
  node: PNode;
begin
  // Create the new node.
  GetMem(node, SizeOf(TNode));
  node.Id := Id;
  node.X := X;
  node.Y := Y;
  node.LinkList := nil;

  // Add the node to the node list.
  node.NextNode := NodeList;
  NodeList := node;

  Result := node;

  // Redraw the network.
  ResetPathTree;
end;

// Create a new link between nodes Node_1 and Node_2.
procedure TGraph.CreateLink(Node_1, Node_2: PNode; Cost, ExtData1, ExtData2: Integer);
var
  link1, link2: PLink;
begin
  if ((Node_1 = nil) or (Node_2 = nil)) then exit;

  // Do not allow links from a node to itself.
  if (Node_1 = Node_2) then
    raise Exception.Create('You cannot make a link between a node and itself.');

  // Create the links.
  GetMem(link1, SizeOf(TLink));
  GetMem(link2, SizeOf(TLink));

  link1.Cost := Cost;
  link1.Node1 := Node_1;
  link1.Node2 := Node_2;
  link1.NextLink := nil;
  link1.ExtData := ExtData1; 

  link2.Cost := Cost;
  link2.Node1 := Node_2;
  link2.Node2 := Node_1;
  link2.NextLink := nil;
  link2.ExtData := ExtData2; 

  // Add the links to the nodes' link lists.
  link1.NextLink := Node_1.LinkList;
  Node_1.LinkList := link1;

  link2.NextLink := Node_2.LinkList;
  Node_2.LinkList := link2;

  // Redraw the network.
  ResetPathTree;
end;

end.
