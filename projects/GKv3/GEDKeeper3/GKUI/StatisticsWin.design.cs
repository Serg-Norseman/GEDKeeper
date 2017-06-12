using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class StatisticsWin
    {
        private GroupBox grpSummary;
        private Panel Panel1;
        private Panel Panel2;
        private ToolBar ToolBar1;
        private ButtonToolItem cbType; // FIXME: GKv3 DevRestriction
        private GKListViewStub lvSummary;
        private ButtonToolItem tbExcelExport;
        private ContextMenu cmStatTypes;

        private void InitializeComponent()
        {
            SuspendLayout();

            lvSummary = new GKListViewStub();
            //lvSummary.FullRowSelect = true;
            lvSummary.Size = new Size(887, 244);
            //lvSummary.View = View.Details;

            grpSummary = new GroupBox();
            grpSummary.Size = new Size(893, 267);
            grpSummary.Text = "grpSummary";
            grpSummary.Content = lvSummary;

            cbType = new ButtonToolItem();
            //cbType.ReadOnly = true;
            //cbType.SelectedIndexChanged += cbType_SelectedIndexChanged;
            cbType.Click += (sender, e) => cmStatTypes.Show(this);

            tbExcelExport = new ButtonToolItem();
            tbExcelExport.Click += tbExcelExport_Click;

            cmStatTypes = new ContextMenu();

            ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        new SeparatorToolItem(),
                                        cbType,
                                        new SeparatorToolItem(),
                                        tbExcelExport});

            Panel1 = new Panel();
            Panel1.Size = new Size(893, 307);

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { grpSummary }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    }
                }
            };
            ToolBar = ToolBar1;

            ClientSize = new Size(893, 574);
            Title = "StatisticsWin";
            Load += StatisticsWin_Load;
            KeyDown += StatisticsWin_KeyDown;
            grpSummary.ResumeLayout();
            Panel1.ResumeLayout();

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
