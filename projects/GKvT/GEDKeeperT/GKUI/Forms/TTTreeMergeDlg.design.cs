#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTTreeMergeDlg
    {
        private Label lblMasterBase;
        private TextField edMasterBase;
        private Label lblOtherBase;
        private TextField edUpdateBase;
        private Button btnTreeMerge;
        private TextView mSyncRes;

        private void InitializeComponent()
        {
            lblMasterBase = new Label();
            lblOtherBase = new Label();
            edMasterBase = new TextField();
            edUpdateBase = new TextField();
            btnTreeMerge = new Button();
            mSyncRes = new TextView();

            Add(lblMasterBase);
            Add(lblOtherBase);
            Add(edMasterBase);
            Add(edUpdateBase);
            Add(btnTreeMerge);
            Add(mSyncRes);

            lblMasterBase.Location = new Point(1, 1);
            lblMasterBase.TabIndex = 0;

            lblOtherBase.Location = new Point(1, 3);
            lblOtherBase.TabIndex = 1;

            edMasterBase.Location = new Point(20, 1);
            edMasterBase.Size = new Size(77, 1);
            edMasterBase.ReadOnly = true;
            edMasterBase.TabIndex = 0;

            edUpdateBase.Location = new Point(26, 3);
            edUpdateBase.Size = new Size(56, 1);
            edUpdateBase.ReadOnly = true;
            edUpdateBase.TabIndex = 1;

            btnTreeMerge.Location = new Point(83, 3);
            btnTreeMerge.Size = new Size(10, 1);
            btnTreeMerge.TabIndex = 2;
            btnTreeMerge.Clicked += btnTreeMerge_Click;

            mSyncRes.Location = new Point(1, 5);
            mSyncRes.Multiline = true;
            mSyncRes.ReadOnly = true;
            mSyncRes.Size = new Size(96, 50);
            mSyncRes.TabIndex = 4;

            Size = new Size(100, 58);
        }
    }
}
