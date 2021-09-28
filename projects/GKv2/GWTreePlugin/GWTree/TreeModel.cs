using System;
using System.Collections.Generic;
using System.Drawing;
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
        public const float VLinkOffset = 7;


        private readonly Dictionary<int, Dictionary<int, Node>> fDesk;
        private List<Family> fFamilies;
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
        }

        public void Clear()
        {
            fDesk.Clear();
            fFamilies.Clear();
            fNodes.Clear();

            fSelectedNode = null;
            fRootPerson = null;
        }

        public void UpdateGraph(bool centerSelected = true)
        {
            UpdateNodesCoord();
            ResolveCollisions();

            if (centerSelected) {
                fTreeView.CenterNode(fSelectedNode);
            }
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
            row[order] = node;
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
            var node = PrepareNode(NewNode(iRec.GetId()));
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

        private Node LoadIndividual(GDMIndividualRecord indiRec)
        {
            if (indiRec == null) return null;

            Node result = ProcessRecord(indiRec);

            GDMIndividualRecord father, mother;
            Context.Tree.GetParents(indiRec, out father, out mother);

            Node fathNode = (father == null) ? null : ProcessRecord(father);
            Node mothNode = (mother == null) ? null : ProcessRecord(mother);

            if (fathNode != null || mothNode != null) {
                AddParents(result, fathNode, mothNode);
            }

            for (int i = 0; i < indiRec.SpouseToFamilyLinks.Count; i++) {
                var family = Context.Tree.GetPtrValue<GDMFamilyRecord>(indiRec.SpouseToFamilyLinks[i]);

                GDMIndividualRecord spouse = Context.Tree.GetSpouseBy(family, indiRec);
                if (spouse != null) {
                    var spNode = ProcessRecord(spouse);
                    AddSpouse(result, spNode);
                }

                for (int k = 0; k < family.Children.Count; k++) {
                    GDMIndividualRecord child = Context.Tree.GetPtrValue(family.Children[k]);
                    if (child != null) {
                        var chNode = ProcessRecord(child);
                        AddChild(result, chNode);
                    }
                }
            }

            return result;
        }

        public void UpdateNodesCoord()
        {
            EnumDeskValues((i, k, node) => {
                //node.Floor = i;
                //node.Order = k;
                node.y = node.Floor * (node.height + VSpace);
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

                        if (xx > node.x) {
                            node.x = xx;
                            bContinue = true;
                        }
                        xx = (int)(node.x + node.width + HSpace);
                    }
                }

                foreach (Family family in fFamilies) {
                    float pivotX = 0;
                    if (family.Pair != null) {
                        Node dum1 = null, dum2 = null;
                        pivotX += family.Pair.GetPivot(ref dum1, ref dum2).X;
                    }
                    if (family.Children != null) {
                        Node dum1 = null, dum2 = null;
                        pivotX -= family.Children.GetPivot(ref dum1, ref dum2).X;
                    }

                    if (pivotX > 0 && family.Children != null) {
                        family.Children.ShiftPivot((int)pivotX);
                        bContinue = true;
                    } else if (pivotX < 0 && family.Pair != null) {
                        family.Pair.ShiftPivot((int)-pivotX);
                        bContinue = true;
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

            Family family = new Family(this);
            family.Pair = new Pair(this);
            family.Pair.Assign(parent1, parent2);
            family.Children = new Children(this);
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
            PrepareNode(parent2, rightOrder + 2, person.Floor - 1);
            UpdateNodesCoord();

            return family.Pair;
        }

        public Node AddSpouse(Node person, Node spouse)
        {
            if (person == null || person.InOtherTree || spouse == null) {
                return null;
            }

            if (person.SelfFamily != null) {
                var pair = person.SelfFamily.Pair;
                if (pair.NodeA == spouse) {
                    return pair.NodeA;
                }
                if (pair.NodeB == spouse) {
                    return pair.NodeB;
                }
                return AddPartner(person, spouse);
            }

            spouse.Partner = true;

            Family family = new Family(this);
            family.Pair = new Pair(this);
            family.Pair.Assign(person, spouse);
            fFamilies.Add(family);

            person.SelfFamily = family;
            spouse.SelfFamily = family;

            PrepareNode(spouse, person.Order + 1, person.Floor);
            UpdateNodesCoord();

            return spouse;
        }

        private Node AddPartner(Node person, Node partner)
        {
            if (person == null || person.InOtherTree) {
                return null;
            }

            if (person.FamiliesList == null) {
                person.FamiliesList = new List<Family>();
            }

            partner.Partner = true;

            Family family = new Family(this);
            family.Pair = new Partner(this);
            (family.Pair as Partner).Assign(person, partner);
            (family.Pair as Partner).Number = person.FamiliesList.Count + 1;
            person.FamiliesList.Add(family);
            partner.SelfFamily = family;
            fFamilies.Add(family);

            int order = person.SelfFamily.Pair.GetRightEdge().Order + 1;
            PrepareNode(partner, order, person.Floor);
            UpdateNodesCoord();

            return partner;
        }

        public Node AddChild(Node person, Node child)
        {
            if (person == null || person.InOtherTree || person.SelfFamily == null || child == null) {
                return null;
            }

            Family selfFam = person.SelfFamily;
            int order = 0;
            if (selfFam.Children == null || selfFam.Children.IsEmpty) {
                if (selfFam.Children == null) {
                    selfFam.Children = new Children(this);
                }
                int tmpOrd = person.Order;
                while (--tmpOrd >= 0) {
                    Node nd = GetDeskVal(person.Floor, tmpOrd);
                    if (nd != null && nd.SelfFamily != null && nd.SelfFamily.Children != null && !nd.SelfFamily.Children.IsEmpty) {
                        Node rind = nd.SelfFamily.Children.GetRightEdge();
                        order = rind.Order + 1;
                        if (nd.SelfFamily.Children.GetRightPartner() != null && !nd.SelfFamily.Children.GetRightPartner().Partner) {
                            selfFam.Children = null;
                            return null;
                        }
                        break;
                    }
                }
            } else {
                order = (selfFam.Children.GetLeftEdge().Order);
                if (selfFam.Children.GetLeftPartner() != null) {
                    order = selfFam.Children.GetRightEdge().Order + 1;
                }
            }

            selfFam.Children.AddNode(child);
            child.ParentFamily = person.SelfFamily;

            PrepareNode(child, order, person.Floor + 1);
            UpdateNodesCoord();

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
    }
}
