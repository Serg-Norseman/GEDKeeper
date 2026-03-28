#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTFamilyGroupsDlg
    {
        private TreeView tvGroups;
        private Button btnAnalyseGroups;
        private MenuItem miDetails;
        private MenuItem miGoToRecord;
        private ContextMenu menuGT;
        private MenuItem miCopyXRef;
        private MenuItem miDQResetFilter;

        private void InitializeComponent()
        {
            btnAnalyseGroups = new Button();
            tvGroups = new TreeView();
            menuGT = new ContextMenu();
            miDetails = new MenuItem();
            miGoToRecord = new MenuItem();
            miCopyXRef = new MenuItem();
            miDQResetFilter = new MenuItem();

            btnAnalyseGroups.Location = new Point(1, 42);
            btnAnalyseGroups.Size = new Size(16, 1);
            btnAnalyseGroups.TabIndex = 7;
            btnAnalyseGroups.Clicked += btnAnalyseGroups_Click;

            //tvGroups.ContextMenuStrip = menuGT;
            tvGroups.Location = new Point(0, 0);
            tvGroups.Size = new Size(80, 38);
            tvGroups.TabIndex = 0;
            //tvGroups.DoubleClick += tvGroups_DoubleClick;
            tvGroups.MouseClick += (s, args) => {
                if (args.MouseEvent.Flags.HasFlag(MouseFlags.Button3Clicked)) {
                    menuGT.Position = new Point(args.MouseEvent.X, args.MouseEvent.Y);
                    menuGT.Show();
                }
            };

            menuGT.MenuItems = new MenuBarItem("Actions", new MenuItem[] { miDetails, miGoToRecord, miDQResetFilter, null, miCopyXRef });
            //menuGT.Opening += new System.ComponentModel.CancelEventHandler(contextMenu_Opening);

            miDetails.Action += miDetails_Click;
            miGoToRecord.Action += miGoToRecord_Click;
            miCopyXRef.Action += miCopyXRef_Click;
            miDQResetFilter.Action += miResetFilter_Click;

            Add(btnAnalyseGroups);
            Add(tvGroups);

            X = Pos.Center();
            Y = Pos.Center();
            Size = new Size(82, 45);
            Closed += Form_Closed;
        }
    }
}
