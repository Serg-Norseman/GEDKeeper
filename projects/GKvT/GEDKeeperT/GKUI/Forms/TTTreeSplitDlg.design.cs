#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTTreeSplitDlg
    {
        private Button btnSelectAll;
        private GKListView ListSelected;
        private GKListView ListSkipped;
        private Button btnSelectFamily;
        private Button btnSelectAncestors;
        private Button btnSelectDescendants;
        private Button btnDelete;
        private Button btnSave;
        private Button btnSelectList;

        private void InitializeComponent()
        {
            btnSelectAll = new Button();
            ListSelected = new GKListView();
            ListSkipped = new GKListView();
            btnSelectFamily = new Button();
            btnSelectAncestors = new Button();
            btnSelectDescendants = new Button();
            btnDelete = new Button();
            btnSave = new Button();
            btnSelectList = new Button();

            ListSelected.Location = new Point(1, 1);
            ListSelected.Size = new Size(47, 39);
            ListSelected.TabIndex = 1;

            ListSkipped.Location = new Point(50, 1);
            ListSkipped.Size = new Size(47, 39);
            ListSkipped.TabIndex = 2;

            Add(btnSelectAll);
            Add(ListSelected);
            Add(ListSkipped);
            Add(btnSelectFamily);
            Add(btnSelectAncestors);
            Add(btnSelectDescendants);
            Add(btnDelete);
            Add(btnSave);
            Add(btnSelectList);

            btnSelectAll.Location = new Point(1, 48);
            btnSelectAll.Size = new Size(16, 1);
            btnSelectAll.TabIndex = 0;
            btnSelectAll.Clicked += btnSelectAll_Click;

            btnSelectFamily.Location = new Point(24, 48);
            btnSelectFamily.Size = new Size(16, 1);
            btnSelectFamily.TabIndex = 3;
            btnSelectFamily.Clicked += btnSelectFamily_Click;

            btnSelectAncestors.Location = new Point(1, 50);
            btnSelectAncestors.Size = new Size(16, 1);
            btnSelectAncestors.TabIndex = 4;
            btnSelectAncestors.Clicked += btnSelectAncestors_Click;

            btnSelectDescendants.Location = new Point(24, 50);
            btnSelectDescendants.Size = new Size(16, 1);
            btnSelectDescendants.TabIndex = 5;
            btnSelectDescendants.Clicked += btnSelectDescendants_Click;

            btnSelectList.Location = new Point(50, 48);
            btnSelectList.Size = new Size(16, 1);
            btnSelectList.TabIndex = 5;
            btnSelectList.Clicked += btnSelectList_Click;

            btnDelete.Location = new Point(68, 48);
            btnDelete.Size = new Size(16, 1);
            btnDelete.TabIndex = 6;
            btnDelete.Clicked += btnDelete_Click;

            btnSave.Location = new Point(68, 50);
            btnSave.Size = new Size(16, 1);
            btnSave.TabIndex = 7;
            btnSave.Clicked += btnSave_Click;

            Size = new Size(100, 53);
        }
    }
}
