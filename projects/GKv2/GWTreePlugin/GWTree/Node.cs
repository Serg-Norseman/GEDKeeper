using System;
using System.Collections.Generic;
using GDModel;

namespace GWTree
{
    [Flags]
    public enum ProcessStage
    {
        None,
        Parent,
        Spouse,
        Child
    }

    public class Node
    {
        private long fId = -1;
        private int fFloor = -1;
        private TreeModel fModel;
        private int fOrder = -1;
        private bool fSelected;

        public bool InOtherTree;
        public Family ParentFamily;
        public bool Partner;
        public Family SelfFamily;
        public GDMSex Sex;
        public ProcessStage Stage;

        public float x;
        public float y;
        public float width;
        public float height;

        public List<Family> FamiliesList { get; private set; }

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

        public GDMIndividualRecord IndiRec { get; private set; }

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


        public Node(TreeModel model, long nodeId, GDMIndividualRecord iRec)
        {
            FamiliesList = new List<Family>();

            fModel = model;
            fId = nodeId;
            IndiRec = iRec;

            width = 64;
            height = 32;

            Clear();
        }

        public void Clear()
        {
            Name = "Unknown";
            Surname = "";
        }

        public void LoadPerson()
        {
            if (IndiRec != null) {
                Name = IndiRec.GetPrimaryFullName();
                Sex = IndiRec.Sex;
            } else {
                Name = "Unknown";
                Surname = "";
                Sex = GDMSex.svUnknown;
            }
        }
    }
}
