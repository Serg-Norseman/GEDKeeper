using System.Collections.Generic;
using GDModel;

namespace GWTree
{
    public class Node
    {
        private long fId = -1;
        private int fFloor = -1;
        private TreeModel fModel;
        private int fOrder = -1;
        private bool fSelected;

        public List<Family> FamiliesList;
        public GDMIndividualRecord IndiRec;
        public bool InOtherTree;
        public Family ParentFamily;
        public bool Partner;
        public Family SelfFamily;
        public string StrID;

        public float x;
        public float y;
        public float width;
        public float height;

        public int Floor
        {
            get {
                return fFloor;
            }
            set {
                fFloor = value;
            }
        }

        public long Id
        {
            get {
                return fId;
            }
            set {
                fId = value;
            }
        }

        public int Order
        {
            get {
                return fOrder;
            }
            set {
                fOrder = value;
            }
        }

        public bool Selected
        {
            get {
                return fSelected;
            }
            set {
                if (fSelected != value) {
                    fSelected = value;
                }
            }
        }

        public string Name { get; set; }
        public string Surname { get; set; }


        public Node(TreeModel model, long nodeId = -1)
        {
            fModel = model;
            width = 64;
            height = 32;
            Clear();
            StrID = nodeId.ToString();
            fId = nodeId;
        }

        public void Clear()
        {
            Name = "Unknown";
            Surname = "";
        }

        public void LoadPerson()
        {
            Name = IndiRec.GetPrimaryFullName();
        }
    }
}
