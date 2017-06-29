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
        private ToolBar ToolBar1;
        private ButtonToolItem cbType;
        private GKListView lvSummary;
        private ButtonToolItem tbExcelExport;
        private ContextMenu cmStatTypes;

        private void InitializeComponent()
        {
            SuspendLayout();

            lvSummary = new GKListView();
            //lvSummary.Size = new Size(887, 244);
            lvSummary.Height = 240;

            grpSummary = new GroupBox();
            //grpSummary.Size = new Size(893, 267);
            grpSummary.Text = "grpSummary";
            grpSummary.Content = lvSummary;

            cbType = new ButtonToolItem();
            cbType.Text = "Stat Types ▼";
            cbType.Click += (sender, e) => cmStatTypes.Show(this);

            tbExcelExport = new ButtonToolItem();
            tbExcelExport.Click += tbExcelExport_Click;
            tbExcelExport.Image = Bitmap.FromResource("Resources.btn_excel.gif");

            cmStatTypes = new ContextMenu();

            ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        new SeparatorToolItem(),
                                        cbType,
                                        new SeparatorToolItem(),
                                        tbExcelExport});

            Panel1 = new Panel();

            Content = new DefTableLayout {
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

            ClientSize = new Size(900, 600);
            Load += StatisticsWin_Load;
            KeyDown += StatisticsWin_KeyDown;
            Title = "StatisticsWin";
            ToolBar = ToolBar1;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
