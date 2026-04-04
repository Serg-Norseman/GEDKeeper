#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTFamilyGroupsDlg
    {
        private TreeView tvGroups;
        private LogChart gkLogChart1;
        private Button btnAnalyseGroups;
        private MenuItem miDetails;
        private MenuItem miGoToRecord;
        private ContextMenu menuGT;
        private MenuItem miCopyXRef;

        private void InitializeComponent()
        {
            btnAnalyseGroups = new Button();
            gkLogChart1 = new LogChart();
            tvGroups = new TreeView();
            menuGT = new ContextMenu();
            miDetails = new MenuItem();
            miGoToRecord = new MenuItem();
            miCopyXRef = new MenuItem();

            btnAnalyseGroups.Location = new Point(1, 42);
            btnAnalyseGroups.Size = new Size(16, 1);
            btnAnalyseGroups.TabIndex = 7;
            btnAnalyseGroups.Clicked += btnAnalyseGroups_Click;

            gkLogChart1.Location = new Point(18, 42);
            gkLogChart1.Size = new Size(102, 1);
            gkLogChart1.TabIndex = 1;
            gkLogChart1.TabStop = false;

            tvGroups.ContextMenu = menuGT;
            tvGroups.Location = new Point(0, 0);
            tvGroups.Size = new Size(120, 38);
            tvGroups.TabIndex = 0;
            //tvGroups.DoubleClick += tvGroups_DoubleClick;

            menuGT.Items.AddRange(new[] { miDetails, miGoToRecord, null, miCopyXRef });
            //menuGT.Opening += new System.ComponentModel.CancelEventHandler(contextMenu_Opening);

            miDetails.Action += miDetails_Click;
            miGoToRecord.Action += miGoToRecord_Click;
            miCopyXRef.Action += miCopyXRef_Click;

            Add(btnAnalyseGroups);
            Add(gkLogChart1);
            Add(tvGroups);

            X = Pos.Center();
            Y = Pos.Center();
            Size = new Size(122, 45);
        }
    }
}
