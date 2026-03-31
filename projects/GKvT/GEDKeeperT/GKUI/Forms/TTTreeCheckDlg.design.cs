#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTTreeCheckDlg
    {
        private TabView tabsTools;
        private TabPage pageTreeCheck;
        private Button btnBaseRepair;
        private View panProblemsContainer;
        private Button btnAnalyseBase;
        private ContextMenu contextMenu;
        private MenuItem miDetails;
        private MenuItem miGoToRecord;
        private MenuItem miCopyXRef;
        private TabPage pageOptions;
        private CheckBox chkCheckPersonPlaces;
        private CheckBox chkCheckCensuses;
        private CheckBox chkCheckLinks;

        private void InitializeComponent()
        {
            tabsTools = new TabView();
            pageTreeCheck = new TabPage();
            pageOptions = new TabPage();
            btnAnalyseBase = new Button();
            btnBaseRepair = new Button();
            panProblemsContainer = new View();
            contextMenu = new ContextMenu();
            miDetails = new MenuItem();
            miGoToRecord = new MenuItem();
            miCopyXRef = new MenuItem();
            chkCheckPersonPlaces = new CheckBox();
            chkCheckCensuses = new CheckBox();
            chkCheckLinks = new CheckBox();

            contextMenu.Items.AddRange(new[] { miDetails, miGoToRecord, null, miCopyXRef });
            //contextMenu.Opening += new System.ComponentModel.CancelEventHandler(contextMenu_Opening);

            miDetails.Action += miDetails_Click;
            miGoToRecord.Action += miGoToRecord_Click;
            miCopyXRef.Action += miCopyXRef_Click;

            chkCheckPersonPlaces.Location = new Point(1, 1);
            chkCheckPersonPlaces.TabIndex = 0;

            chkCheckCensuses.Location = new Point(1, 3);
            chkCheckCensuses.TabIndex = 0;

            chkCheckLinks.Location = new Point(1, 5);
            chkCheckLinks.TabIndex = 0;

            panProblemsContainer.Location = new Point(0, 0);
            panProblemsContainer.Width = Dim.Fill();
            panProblemsContainer.Height = 44;
            panProblemsContainer.TabIndex = 1;

            btnAnalyseBase.Location = new Point(1, 45);
            //btnAnalyseBase.Size = new Size(20, 1);
            btnAnalyseBase.TabIndex = 0;
            btnAnalyseBase.Clicked += btnAnalyseBase_Click;

            btnBaseRepair.Location = new Point(78, 45);
            //btnBaseRepair.Size = new Size(20, 1);
            btnBaseRepair.TabIndex = 0;
            btnBaseRepair.Clicked += btnBaseRepair_Click;

            pageTreeCheck.View.Add(btnAnalyseBase);
            pageTreeCheck.View.Add(btnBaseRepair);
            pageTreeCheck.View.Add(panProblemsContainer);

            pageOptions.View.Add(chkCheckPersonPlaces);
            pageOptions.View.Add(chkCheckCensuses);
            pageOptions.View.Add(chkCheckLinks);

            tabsTools.AddTab(pageTreeCheck);
            tabsTools.AddTab(pageOptions);
            tabsTools.Location = new Point(0, 0);
            tabsTools.Height = Dim.Fill();
            tabsTools.Width = Dim.Fill();
            tabsTools.TabIndex = 0;

            Size = new Size(100, 52);
            Add(tabsTools);
        }
    }
}
