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
        //private ToolStripComboBox cbType; // FIXME: GKv3 DevRestriction
        private GKListViewStub lvSummary;
        //private ColumnHeader ColumnHeader1;
        //private ColumnHeader ColumnHeader2;
        //private ColumnHeader ColumnHeader3;
        //private ColumnHeader ColumnHeader4;
        private SeparatorToolItem TBS1;
        private SeparatorToolItem TBS2;
        private ButtonToolItem tbExcelExport;

        private void InitializeComponent()
        {
            grpSummary = new GroupBox();
            lvSummary = new GKListViewStub();
            ColumnHeader1 = new ColumnHeader();
            ColumnHeader2 = new ColumnHeader();
            ColumnHeader3 = new ColumnHeader();
            ColumnHeader4 = new ColumnHeader();
            Panel1 = new Panel();
            ToolBar1 = new ToolBar();
            TBS1 = new SeparatorToolItem();
            cbType = new ToolStripComboBox();
            TBS2 = new SeparatorToolItem();
            tbExcelExport = new ButtonToolItem();
            grpSummary.SuspendLayout();
            Panel1.SuspendLayout();
            SuspendLayout();

            grpSummary.Controls.Add(lvSummary);
            grpSummary.Dock = DockStyle.Top;
            grpSummary.Location = new Point(0, 0);
            grpSummary.Size = new Size(893, 267);
            grpSummary.Text = "grpSummary";

            lvSummary.Columns.AddRange(new ColumnHeader[] {
                                           ColumnHeader1,
                                           ColumnHeader2,
                                           ColumnHeader3,
                                           ColumnHeader4});
            lvSummary.Dock = DockStyle.Fill;
            lvSummary.FullRowSelect = true;
            lvSummary.Location = new Point(3, 20);
            lvSummary.Size = new Size(887, 244);
            lvSummary.UseCompatibleStateImageBehavior = false;
            lvSummary.View = View.Details;

            ColumnHeader1.Text = "ColumnHeader1";
            ColumnHeader1.Width = 300;

            ColumnHeader2.Text = "ColumnHeader2";
            ColumnHeader2.Width = 100;

            ColumnHeader3.Text = "ColumnHeader3";
            ColumnHeader3.Width = 100;

            ColumnHeader4.Text = "ColumnHeader4";
            ColumnHeader4.Width = 100;

            Panel1.Controls.Add(ToolBar1);
            Panel1.Dock = DockStyle.Fill;
            Panel1.Location = new Point(0, 267);
            Panel1.Size = new Size(893, 307);

            ToolBar1.Items.AddRange(new ToolItem[] {
                                        TBS1,
                                        cbType,
                                        TBS2,
                                        tbExcelExport});
            ToolBar1.Location = new Point(0, 0);

            cbType.ReadOnly = true;
            cbType.SelectedIndexChanged += cbType_SelectedIndexChanged;

            tbExcelExport.Click += tbExcelExport_Click;

            ClientSize = new Size(893, 574);
            Controls.Add(Panel1);
            Controls.Add(grpSummary);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Title = "StatisticsWin";
            Load += StatisticsWin_Load;
            KeyDown += StatisticsWin_KeyDown;
            grpSummary.ResumeLayout();
            Panel1.ResumeLayout();
            ResumeLayout();
        }
    }
}
