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

        public void Load(GDMIndividualRecord rootIndiRec)
        {
            fDesk.Clear();
            fFamilies = new List<Family>();
            fNodes.Clear();

            var node = LoadIndividual(null, rootIndiRec, 0, 0, ProcessStage.None);
            SelectedNode = node;
            RootPerson = node;

            UpdateGraph();
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

        private Node GetNode(GDMIndividualRecord iRec)
        {
            long nodeId = (iRec != null) ? iRec.GetId() : --fLastUnknownId;
            Node node = fNodes.Find(nd => (nd.Id == nodeId));

            if (node == null) {
                node = new Node(this, nodeId, iRec);
                fNodes.Add(node);
            }

            return node;
        }

        private Node PrepareNode(Node node, int order = 0, int floor = 0)
        {
            if (node == null) return null;

            //SetDeskVal(node.Floor, node.Order, null);

            node.Order = order;
            node.Floor = floor;

            SetDeskVal(floor, order, node);

            return node;
        }

        private List<GDMIndividualRecord> fProcessedRecords = new List<GDMIndividualRecord>();

        private bool IsProcessed(GDMIndividualRecord indiRec)
        {
            return fProcessedRecords.Contains(indiRec);
        }

        private Node LoadIndividual(Node indiNode, GDMIndividualRecord indiRec, int order, int floor, ProcessStage stage)
        {
            Node result = indiNode ?? GetNode(indiRec);
            PrepareNode(result, order, floor);

            if (indiRec == null || IsProcessed(indiRec)) return null;

            result.LoadPerson();
            fProcessedRecords.Add(indiRec);

            if (stage != ProcessStage.Spouse && stage != ProcessStage.Child) {
                GDMIndividualRecord father, mother;
                Context.Tree.GetParents(indiRec, out father, out mother);
                if (father != null || mother != null) {
                    AddParents(result, father, mother);
                }
            }

            for (int i = 0; i < indiRec.SpouseToFamilyLinks.Count; i++) {
                var familyRec = Context.Tree.GetPtrValue<GDMFamilyRecord>(indiRec.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = Context.Tree.GetSpouseBy(familyRec, indiRec);

                Family family = null;
                if (stage != ProcessStage.Parent) {
                    family = AddSpouse(result, familyRec, spouse);
                }

                if (stage != ProcessStage.Parent && stage != ProcessStage.Spouse) {
                    for (int k = 0; k < familyRec.Children.Count; k++) {
                        GDMIndividualRecord child = Context.Tree.GetPtrValue(familyRec.Children[k]);
                        if (child != null && !IsProcessed(child)) {
                            AddChild(result, family, child);
                        }
                    }
                }
            }

            return result;
        }

        public void UpdateGraph()
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

                            node.y = node.Floor * (node.height + VSpace);
                        }
                    }
                }

                foreach (Family family in fFamilies) {
                    float pivotX = 0;
                    if (!family.Pair.IsEmpty && !family.Children.IsEmpty) {
                        pivotX = family.Pair.GetPivot().X - family.Children.GetPivot().X;

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

        private Pair AddParents(Node person, GDMIndividualRecord father, GDMIndividualRecord mother)
        {
            if (person == null || person.InOtherTree) {
                return null;
            }

            Node fathNode = GetNode(father);
            Node mothNode = GetNode(mother);

            if (person.ParentFamily != null) {
                return person.ParentFamily.Pair;
            }

            Family family = new Family(this, new Pair(this));
            family.Pair.Assign(fathNode, mothNode);
            family.Children.AddNode(person);
            fFamilies.Add(family);

            fathNode.SelfFamily = family;
            mothNode.SelfFamily = family;
            person.ParentFamily = family;

            if (person.Partner) {
                fathNode.InOtherTree = true;
                mothNode.InOtherTree = true;
                return family.Pair;
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

            LoadIndividual(fathNode, fathNode.IndiRec, rightOrder + 1, person.Floor - 1, ProcessStage.Parent);
            LoadIndividual(mothNode, mothNode.IndiRec, rightOrder + 2, person.Floor - 1, ProcessStage.Parent);

            return family.Pair;
        }

        private Family AddSpouse(Node person, GDMFamilyRecord familyRec, GDMIndividualRecord spouse)
        {
            if (person == null || person.InOtherTree) {
                return null;
            }

            var spNode = GetNode(spouse);

            Family family;
            int spOrder;

            if (person.SelfFamily != null) {
                var pair = person.SelfFamily.Pair;
                if (pair.NodeA == spNode || pair.NodeB == spNode) {
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

            family.FamRec = familyRec;
            family.Pair.Assign(person, spNode);
            spNode.Partner = true;
            spNode.SelfFamily = family;
            fFamilies.Add(family);

            LoadIndividual(spNode, spNode.IndiRec, spOrder, person.Floor, ProcessStage.Spouse);

            return family;
        }

        private Node AddChild(Node person, Family family, GDMIndividualRecord child)
        {
            if (person == null || person.InOtherTree || family == null) {
                return null;
            }

            var chNode = PrepareNode(GetNode(child));

            var right = family.Children.GetRightEdge();
            int order = (right == null) ? person.Order : right.Order + 1;

            family.Children.AddNode(chNode);
            chNode.ParentFamily = family;

            LoadIndividual(chNode, chNode.IndiRec, order, person.Floor + 1, ProcessStage.Child);

            return chNode;
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
