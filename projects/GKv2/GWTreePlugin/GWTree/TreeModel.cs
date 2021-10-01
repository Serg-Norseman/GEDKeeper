using System;
using System.Collections.Generic;
using System.Drawing;
using BSLib;
using GDModel;
using GKCore.Interfaces;

namespace GWTree
{
    public delegate bool DeskEnum(int i, int k, Node node);

    public interface ITreeView
    {
        void CenterNode(Node node);
        void DrawNode(Graphics gfx, Node node);
        void DrawLine(Graphics gfx, float x1, float y1, float x2, float y2);
    }

    public class TreeModel
    {
        public const float HSpace = 40;
        public const float VSpace = 70;
        public const float PivotOffset = 20.0f;
        public const float VLinkOffset = 4;


        private readonly Dictionary<int, Dictionary<int, Node>> fDesk;
        private List<Family> fFamilies;
        private int fLastUnknownId;
        private readonly List<Node> fNodes;
        private Node fRootPerson;
        private Node fSelectedNode;
        private readonly ITreeView fTreeView;


        public IBaseContext Context { get; set; }

        public Node RootPerson
        {
            get {
                return fRootPerson;
            }
            set {
                if (fRootPerson != value) {
                    fRootPerson = value;
                }
            }
        }

        public Node SelectedNode
        {
            get {
                return fSelectedNode;
            }
            set {
                if (fSelectedNode != value) {
                    if (fSelectedNode != null) {
                        fSelectedNode.Selected = false;
                    }
                    fSelectedNode = value;
                    if (fSelectedNode != null) {
                        fSelectedNode.Selected = true;
                    }
                }
            }
        }


        public TreeModel(ITreeView treeView)
        {
            fTreeView = treeView;
            fDesk = new Dictionary<int, Dictionary<int, Node>>();
            fFamilies = new List<Family>();
            fNodes = new List<Node>();
            fLastUnknownId = 0;
        }

        public void Clear()
        {
            fDesk.Clear();
            fFamilies.Clear();
            fNodes.Clear();

            fSelectedNode = null;
            fRootPerson = null;
            fLastUnknownId = 0;
        }

        public void UpdateGraph()
        {
            UpdateNodesCoord();
            ResolveCollisions();
        }

        private Node GetDeskVal(int floor, int order)
        {
            Dictionary<int, Node> row;
            if (fDesk.TryGetValue(floor, out row)) {
                Node node;
                if (row.TryGetValue(order, out node)) {
                    return node;
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }

        private void SetDeskVal(int floor, int order, Node node)
        {
            Dictionary<int, Node> row;
            if (!fDesk.TryGetValue(floor, out row)) {
                row = new Dictionary<int, Node>();
                fDesk[floor] = row;
            }

            if (node == null && row.ContainsKey(order)) {
                row.Remove(order);
            } else {
                row[order] = node;
            }
        }

        private void EnumDeskValues(DeskEnum enumFunc)
        {
            foreach (var kvRow in fDesk) {
                var i = kvRow.Key;
                foreach (var kvCol in kvRow.Value) {
                    var k = kvCol.Key;
                    var node = kvCol.Value;
                    bool continueRes = enumFunc(i, k, node);
                    if (!continueRes) return;
                }
            }
        }

        private Node GetNodeById(long nodeId)
        {
            foreach (Node node in fNodes) {
                if (node.Id == nodeId) {
                    return node;
                }
            }
            return null;
        }

        private Node PrepareNode(Node node, int order = 0, int floor = 0)
        {
            if (node == null) return null;

            //SetDeskVal(node.Floor, node.Order, null);

            node.Order = order;
            node.Floor = floor;
            node.y = floor * (node.height + VSpace);

            SetDeskVal(floor, order, node);
            UpdateNodesCoord();

            return node;
        }

        private Node NewNode(long nodeId = -1)
        {
            Node node = GetNodeById(nodeId);

            if (node == null) {
                node = new Node(this, nodeId);
                node.Floor = -1;
                node.y = 0;
                fNodes.Add(node);
            }

            return node;
        }

        private Node ProcessRecord(GDMIndividualRecord iRec)
        {
            long id;
            if (iRec != null) {
                id = iRec.GetId();
            } else {
                id = --fLastUnknownId;
            }

            var node = PrepareNode(NewNode(id));
            node.IndiRec = iRec;
            node.LoadPerson();
            return node;
        }

        public void Load(GDMIndividualRecord rootIndiRec)
        {
            fDesk.Clear();
            fFamilies = new List<Family>();
            fNodes.Clear();

            var node = LoadIndividual(rootIndiRec);
            SelectedNode = node;
            RootPerson = node;

            UpdateGraph();
        }

        private List<GDMIndividualRecord> fProcessedRecords = new List<GDMIndividualRecord>();

        private bool IsProcessed(GDMIndividualRecord indiRec)
        {
            return fProcessedRecords.Contains(indiRec);
        }

        private Node LoadIndividual(GDMIndividualRecord indiRec, bool preLoaded = false)
        {
            if (indiRec == null || IsProcessed(indiRec)) return null;

            Node result = (!preLoaded) ? ProcessRecord(indiRec) : NewNode(indiRec.GetId());
            fProcessedRecords.Add(indiRec);

            var list = new List<GDMIndividualRecord>();

            GDMIndividualRecord father, mother;
            Context.Tree.GetParents(indiRec, out father, out mother);
            if (father != null || mother != null) {
                if (!IsProcessed(father) && !IsProcessed(mother)) {
                    Node fathNode = ProcessRecord(father); // can be null
                    Node mothNode = ProcessRecord(mother); // can be null
                    AddParents(result, fathNode, mothNode);
                }
            }

            for (int i = 0; i < indiRec.SpouseToFamilyLinks.Count; i++) {
                var familyRec = Context.Tree.GetPtrValue<GDMFamilyRecord>(indiRec.SpouseToFamilyLinks[i]);

                GDMIndividualRecord spouse = Context.Tree.GetSpouseBy(familyRec, indiRec);

                Family family = null;
                if (!IsProcessed(spouse)) {
                    var spNode = ProcessRecord(spouse); // can be null
                    family = AddSpouse(result, spNode);
                }

                for (int k = 0; k < familyRec.Children.Count; k++) {
                    GDMIndividualRecord child = Context.Tree.GetPtrValue(familyRec.Children[k]);
                    if (child != null && !IsProcessed(child)) {
                        var chNode = ProcessRecord(child);
                        AddChild(result, family, chNode);
                    }
                }
            }

            return result;
        }

        public void UpdateNodesCoord()
        {
            EnumDeskValues((i, k, node) => {
                if (node != null) {
                    //node.Floor = i;
                    //node.Order = k;
                    node.y = node.Floor * (node.height + VSpace);
                }
                return true;
            });
        }

        private void ResolveCollisions()
        {
            int shots = 0;
            bool bContinue = true;
            while (bContinue) {
                if (shots++ > 350) {
                    break;
                }

                bContinue = false;

                foreach (var kvRow in fDesk) {
                    var i = kvRow.Key;
                    int xx = 0;
                    foreach (var kvCol in kvRow.Value) {
                        var k = kvCol.Key;
                        var node = kvCol.Value;

                        if (node != null) {
                            if (xx > node.x) {
                                node.x = xx;
                                //bContinue = true;
                            }
                            xx = (int)(node.x + node.width + HSpace);
                        }
                    }
                }

                foreach (Family family in fFamilies) {
                    float pivotX = 0;
                    if (!family.Pair.IsEmpty && !family.Children.IsEmpty) {
                        Node dum1 = null, dum2 = null;
                        pivotX += family.Pair.GetPivot(ref dum1, ref dum2).X;
                        pivotX -= family.Children.GetPivot(ref dum1, ref dum2).X;

                        if (Math.Abs(pivotX) >= 1) {
                            if (Math.Abs(pivotX) > 0) {
                                family.Children.ShiftPivot((int)pivotX);
                                bContinue = true;
                            } else {
                                family.Pair.ShiftPivot((int)-pivotX);
                                bContinue = true;
                            }
                        }
                    }
                }
            }
        }

        public Pair AddParents(Node person, Node parent1, Node parent2)
        {
            if (person == null || person.InOtherTree || (parent1 == null && parent2 == null)) {
                return null;
            }

            if (person.ParentFamily != null) {
                return person.ParentFamily.Pair;
            }

            Family family = new Family(this, new Pair(this));
            family.Pair.Assign(parent1, parent2);
            family.Children.AddNode(person);
            fFamilies.Add(family);

            parent1.SelfFamily = family;
            parent2.SelfFamily = family;
            person.ParentFamily = family;

            if (person.Partner) {
                parent1.InOtherTree = true;
                parent2.InOtherTree = true;
                return family.Pair;
            }

            if (person.Floor == 0) {
                UpdateNodesCoord();
            }

            int order = person.Order;
            int rightOrder = -1;
            while (--order >= 0) {
                var nd = GetDeskVal(person.Floor, order);
                if (nd != null && nd.ParentFamily != null && !nd.ParentFamily.Pair.IsEmpty) {
                    rightOrder = nd.ParentFamily.Pair.GetRightEdge().Order;
                    break;
                }
            }

            PrepareNode(parent1, rightOrder + 1, person.Floor - 1);
            //LoadIndividual(parent1.IndiRec, true);

            PrepareNode(parent2, rightOrder + 2, person.Floor - 1);
            //LoadIndividual(parent2.IndiRec, true);

            return family.Pair;
        }

        public Family AddSpouse(Node person, Node spouse)
        {
            if (person == null || person.InOtherTree || spouse == null) {
                return null;
            }

            Family family;
            int spOrder;

            if (person.SelfFamily != null) {
                var pair = person.SelfFamily.Pair;
                if (pair.NodeA == spouse || pair.NodeB == spouse) {
                    return person.SelfFamily;
                }

                family = new Family(this, new Partner(this));

                (family.Pair as Partner).Number = person.FamiliesList.Count + 1;
                person.FamiliesList.Add(family);

                spOrder = pair.GetRightEdge().Order + 1;
            } else {
                family = new Family(this, new Pair(this));

                person.SelfFamily = family;

                spOrder = person.Order + 1;
            }

            family.Pair.Assign(person, spouse);
            spouse.Partner = true;
            spouse.SelfFamily = family;
            fFamilies.Add(family);
            PrepareNode(spouse, spOrder, person.Floor);

            //LoadIndividual(spouse.IndiRec, true);

            return family;
        }

        public Node AddChild(Node person, Family family, Node child)
        {
            if (person == null || person.InOtherTree || family == null || child == null) {
                return null;
            }

            var right = family.Children.GetRightEdge();
            int order = (right == null) ? person.Order : right.Order + 1;

            family.Children.AddNode(child);
            child.ParentFamily = family;

            PrepareNode(child, order, person.Floor + 1);

            LoadIndividual(child.IndiRec, true);

            return child;
        }

        public void DrawLine(Graphics gfx, uint color, float x1, float y1, float x2, float y2)
        {
            ((ITreeView)fTreeView).DrawLine(gfx, x1, y1, x2, y2);
        }

        internal void DrawLinks(Graphics gfx)
        {
            foreach (Family family in fFamilies) {
                family.DrawLinks(gfx);
            }
        }

        internal void DrawNodes(Graphics gfx)
        {
            foreach (Node node in fNodes) {
                ((ITreeView)fTreeView).DrawNode(gfx, node);
            }
        }

        public ExtRectF GetImageRect()
        {
            ExtRectF result = ExtRectF.Create(float.MaxValue, float.MaxValue, float.MinValue, float.MinValue);
            foreach (Node node in fNodes) {
                var lx = node.x;
                var ly = node.y;
                var rx = node.x + node.width;
                var ry = node.y + node.height;

                if (result.Left > lx) result.Left = lx;
                if (result.Top > ly) result.Top = ly;
                if (result.Right < rx) result.Right = rx;
                if (result.Bottom < ry) result.Bottom = ry;
            }
            return result;
        }
    }
}
