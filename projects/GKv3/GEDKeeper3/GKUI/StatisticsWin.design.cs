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
        private /*ToolBar*/Panel ToolBar1;
        private /*ButtonToolItem*/ComboBox cbType;
        private GKListView lvSummary;
        private /*ButtonToolItem*/Button tbExcelExport;
        //private ContextMenu cmStatTypes;

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

            /*cbType = new ButtonToolItem();
            cbType.Text = "Stat Types ▼";
            cbType.Click += (sender, e) => cmStatTypes.Show(this);*/
            cbType = new ComboBox();
            cbType.Width = 200;
            cbType.ReadOnly = true;
            cbType.SelectedIndexChanged += cbType_SelectedIndexChanged;

            tbExcelExport = new Button();//ButtonToolItem();
            tbExcelExport.Click += tbExcelExport_Click;
            tbExcelExport.Image = Bitmap.FromResource("Resources.btn_excel.gif");
            tbExcelExport.Size = new Size(26, 26);

            //cmStatTypes = ContextMenu();

            ToolBar1 = new Panel();
            ToolBar1.Content = new DefStackLayout(0, 10, Orientation.Horizontal) {
                Items = { cbType, tbExcelExport }
            };
            /*ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        new SeparatorToolItem(),
                                        cbType,
                                        new SeparatorToolItem(),
                                        tbExcelExport});*/

            Panel1 = new Panel();

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { grpSummary }
                    },
                    new TableRow {
                        Cells = { ToolBar1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    }
                }
            };

            Load += StatisticsWin_Load;
            KeyDown += StatisticsWin_KeyDown;
            Title = "StatisticsWin";
            //ToolBar = ToolBar1;

            UIHelper.SetPredefProperties(this, 900, 600);
            ResumeLayout();
        }
    }
}
