#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class StatisticsWin
    {
        private FrameView grpSummary;
        private FrameView Panel1;
        private FrameView ToolBar1;
        private ComboBox cbType;
        private GKListView lvSummary;
        private Button tbExcelExport;
        private ZGraphControl fGraph;
        private GKListView fListStats;

        private void InitializeComponent()
        {
            lvSummary = new GKListView();
            lvSummary.Location = new Point(0, 0);
            lvSummary.Height = Dim.Fill();
            lvSummary.Width = Dim.Fill();
            lvSummary.TabIndex = 0;

            grpSummary = new FrameView();
            grpSummary.Location = new Point(0, 0);
            grpSummary.Size = new Size(118, 16);
            grpSummary.TabIndex = 0;
            grpSummary.TabStop = false;
            grpSummary.Add(lvSummary);

            cbType = new ComboBox();
            cbType.Text = "test";
            cbType.Location = new Point(1, 0);
            cbType.Size = new Size(43, 2);
            cbType.SelectedIndexChanged += cbType_SelectedIndexChanged;

            tbExcelExport = new Button();
            tbExcelExport.Text = "Excel";
            tbExcelExport.Location = new Point(45, 0);
            tbExcelExport.Size = new Size(10, 1);
            tbExcelExport.Clicked += tbExcelExport_Click;

            ToolBar1 = new FrameView();
            ToolBar1.Location = new Point(0, 0);
            ToolBar1.Size = new Size(116, 3);
            ToolBar1.TabIndex = 0;
            ToolBar1.Add(cbType, tbExcelExport);

            fListStats = new GKListView();
            fListStats.Location = new Point(0, 3);
            fListStats.Size = new Size(50, 35);

            fGraph = new ZGraphControl();
            fGraph.Location = new Point(50, 3);
            fGraph.Size = new Size(66, 35);

            Panel1 = new FrameView();
            Panel1.Location = new Point(0, 16);
            Panel1.Size = new Size(118, 42);
            Panel1.TabIndex = 2;
            Panel1.Add(fListStats, fGraph, ToolBar1);

            X = Pos.Center();
            Y = Pos.Center();
            Size = new Size(120, 60);
            Add(grpSummary);
            Add(Panel1);
            Loaded += StatisticsWin_Load;
            KeyDown += StatisticsWin_KeyDown;
        }
    }
}
