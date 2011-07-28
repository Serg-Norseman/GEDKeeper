unit GraphCore;

interface

uses
  VCLStub;

type
  TGraph = class(TObject)
  public
    const
      INFINITY = 32767;

    type
      TNodeStatus = (nsNotInList, nsWasInList, nsNowInList);

      TGraphNode = class;

      TGraphLink = class(TObject)
      public
        Node1, Node2: TGraphNode;
        Cost: Integer;
        ExtData: Integer;
        InTree: Boolean;    // Is it in the shortest path tree?
        NextLink: TGraphLink;    // Next link in the node's list of links.
      end;

      TGraphNode = class(TObject)
      public
        ExtObj: TObject;
        NextNode: TGraphNode;    // Next node in list of all nodes.
        LinksOut: TGraphLink;    // Links out of this node.
        LinkIn: TGraphLink;      // The link into this node.

        Status: TNodeStatus;// Has it been in the tree?
        Dist: Integer;      // Distance from root.

        constructor Create; virtual;
      end;

      TNodeClass = class of TGraphNode;

      // Cell for candidate linked list.
      TCandidate = class(TObject)
        Node: TGraphNode;
        NextCandidate: TCandidate;
      end;

  private
  public
    NodeList: TGraphNode;     // List of all nodes.

    constructor Create;
    destructor Destroy; override;

    procedure Clear();
    procedure CreateLink(Node_1, Node_2: TGraphNode; Cost: Integer; ExtData1: Integer = 0; ExtData2: Integer = 0);
    function  CreateNode(aNodeClass: TNodeClass; aExtObj: TObject): TGraphNode;
    procedure DeleteLink(n1, n2: TGraphNode);
    procedure DeleteNode(target: TGraphNode);
    procedure FindPathTree(root: TGraphNode);
    procedure RemoveLinkFromNode(n1, n2: TGraphNode);
    procedure ResetPathTree();
  end;

implementation

constructor TGraph.TGraphNode.Create;
begin
  inherited Create;
end;

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
  node, next_node: TGraphNode;
  link, next_link: TGraphLink;
begin
  // Free all the nodes.
  node := NodeList;
  while (node <> nil) do begin
    // Free the node's links.
    link := node.LinksOut;
    while (link <> nil) do begin
      next_link := link.NextLink;
      link.Free;
      link := next_link;
    end;

    // Free the node itself.
    next_node := node.NextNode;
    node.Free;
    node := next_node;
  end;
  NodeList := nil;
end;

// Find a shortest path tree rooted at this node using a
// label correcting algorithm.
procedure TGraph.FindPathTree(root: TGraphNode);
var
  top_candidate, new_candidate: TCandidate;
  node_dist, new_dist: Integer;
  node, to_node: TGraphNode;
  link: TGraphLink;
begin
  if (root = nil) then Exit;

  // Reset the tree.
  ResetPathTree();

  // Start with the root in the shortest path tree.
  root.Dist := 0;
  root.LinkIn := nil;
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
    new_candidate.Free;

    node_dist := node.Dist;
    node.Status := nsNotInList;

    // Examine the node's neighbors.
    link := node.LinksOut;
    while (link <> nil) do begin
      // See if there's an improved path using this node.
      to_node := link.Node2;
      new_dist := node_dist + link.Cost;
      if (new_dist < to_node.Dist) then begin
        // It's better. Update Dist and InLink.
        to_node.LinkIn := link;
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
    link := to_node.LinkIn; // The link into this node.
    if (link <> nil) then begin
      link.InTree := True;

      // Mark the reverse link.
      node := link.Node1; // The link's start node.
      link := to_node.LinksOut;
      while (link <> nil) do begin
        if (link.Node2 = node) then break;
        link := link.NextLink;
      end;
      if (link <> nil) then link.InTree := True;
    end;
    to_node := to_node.NextNode;
  end;
end;

// Remove all the links from the shortest path tree.
procedure TGraph.ResetPathTree();
var
  node: TGraphNode;
  link: TGraphLink;
begin
  node := NodeList;
  while (node <> nil) do begin
    node.Status := nsNotInList;
    node.Dist   := INFINITY;
    node.LinkIn := nil;

    link := node.LinksOut;
    while (link <> nil) do begin
      link.InTree := False;
      link := link.NextLink;
    end;

    node := node.NextNode;
  end;
end;

// Delete the node from the network.
procedure TGraph.DeleteNode(target: TGraphNode);
var
  node, next_node: TGraphNode;
begin
  if (target = nil) then Exit;

  // Free the target's links.
  while (target.LinksOut <> nil) do begin
    DeleteLink(target, target.LinksOut.Node2);
  end;

  // Find the target in the node list.
  node := nil;
  next_node := NodeList;
  while (next_node <> nil) do begin
    if (next_node = target) then break;
    node := next_node;
    next_node := node.NextNode;
  end;
  if (next_node = nil) then Exit; // Not found.

  // Remove the node.
  if (node = nil)
  then NodeList := target.NextNode
  else node.NextNode := target.NextNode;

  target.Free;

  // Redraw the network.
  ResetPathTree();
end;

// Delete a link from the network.
procedure TGraph.DeleteLink(n1, n2: TGraphNode);
begin
  if ((n1 = nil) or (n2 = nil)) then Exit;

  // Remove the link from the nodes' link lists.
  RemoveLinkFromNode(n1, n2);
  RemoveLinkFromNode(n2, n1);

  // Redraw the network.
  ResetPathTree();
end;

// Remove the link to node n2 from node n1's Links array.
procedure TGraph.RemoveLinkFromNode(n1, n2: TGraphNode);
var
  link, next_link: TGraphLink;
begin
  // Find the link.
  link := nil;
  next_link := n1.LinksOut;
  while (next_link <> nil) do begin
    if (next_link.Node2 = n2) then break;
    link := next_link;
    next_link := link.NextLink;
  end;
  if (next_link = nil) then Exit;

  // Remove the link.
  if (link = nil)
  then n1.LinksOut := next_link.NextLink
  else link.NextLink := next_link.NextLink;
  
  next_link.Free;
end;

// Create a new node.
function TGraph.CreateNode(aNodeClass: TNodeClass; aExtObj: TObject): TGraphNode;
var
  node: TGraphNode;
begin
  // Create the new node.
  node := aNodeClass.Create;
  node.LinksOut := nil;
  node.ExtObj := aExtObj;

  // Add the node to the node list.
  node.NextNode := NodeList;
  NodeList := node;

  Result := node;

  // Redraw the network.
  ResetPathTree();
end;

// Create a new link between nodes Node_1 and Node_2.
procedure TGraph.CreateLink(Node_1, Node_2: TGraphNode; Cost: Integer; ExtData1: Integer = 0; ExtData2: Integer = 0);
var
  link1, link2: TGraphLink;
begin
  if ((Node_1 = nil) or (Node_2 = nil)) then Exit;

  // Do not allow links from a node to itself.
  if (Node_1 = Node_2) then
    raise Exception.Create('You cannot make a link between a node and itself.');

  // Create the links.
  link1 := TGraphLink.Create;
  link2 := TGraphLink.Create;

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
  link1.NextLink := Node_1.LinksOut;
  Node_1.LinksOut := link1;

  link2.NextLink := Node_2.LinksOut;
  Node_2.LinksOut := link2;

  // Redraw the network.
  ResetPathTree();
end;

end.
