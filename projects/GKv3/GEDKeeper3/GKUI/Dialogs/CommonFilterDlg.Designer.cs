using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class CommonFilterDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        protected TabControl tabsFilters;
        private TabPage tsFieldsFilter;
        private Button btnReset;
        private GridView dataGridView1;
        private Panel pnlClient;
        private Panel pnlButtons;

        private void InitializeComponent()
        {
            pnlButtons = new Panel();
            btnReset = new Button();
            btnAccept = new Button();
            btnCancel = new Button();
            pnlClient = new Panel();
            tabsFilters = new TabControl();
            tsFieldsFilter = new TabPage();
            dataGridView1 = new DataGridView();
            pnlButtons.SuspendLayout();
            pnlClient.SuspendLayout();
            tabsFilters.SuspendLayout();
            tsFieldsFilter.SuspendLayout();
            SuspendLayout();

            pnlButtons.Controls.Add(btnReset);
            pnlButtons.Controls.Add(btnAccept);
            pnlButtons.Controls.Add(btnCancel);
            pnlButtons.Dock = DockStyle.Bottom;
            pnlButtons.Location = new Point(0, 526);
            pnlButtons.Size = new Size(859, 62);

            btnReset.Location = new Point(16, 14);
            btnReset.Size = new Size(114, 30);
            btnReset.Text = "btnReset";
            btnReset.Click += btnReset_Click;

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(600, 16);
            btnAccept.Size = new Size(112, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(729, 16);
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";

            pnlClient.Controls.Add(tabsFilters);
            pnlClient.Dock = DockStyle.Fill;
            pnlClient.Location = new Point(0, 0);
            pnlClient.Padding = new Padding(16, 16, 16, 16);
            pnlClient.Size = new Size(859, 526);

            tabsFilters.Controls.Add(tsFieldsFilter);
            tabsFilters.Dock = DockStyle.Fill;
            tabsFilters.Location = new Point(16, 16);
            tabsFilters.SelectedIndex = 0;
            tabsFilters.Size = new Size(827, 494);

            tsFieldsFilter.Controls.Add(dataGridView1);
            tsFieldsFilter.Location = new Point(4, 26);
            tsFieldsFilter.Size = new Size(819, 464);
            tsFieldsFilter.Text = "tsFieldsFilter";

            dataGridView1.AllowUserToResizeRows = false;
            dataGridView1.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            dataGridView1.Dock = DockStyle.Fill;
            dataGridView1.Location = new Point(0, 0);
            dataGridView1.MultiSelect = false;
            dataGridView1.Size = new Size(819, 464);

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(859, 588);
            Controls.Add(pnlClient);
            Controls.Add(pnlButtons);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            Title = "CommonFilterDlg";
            pnlButtons.ResumeLayout();
            pnlClient.ResumeLayout();
            tabsFilters.ResumeLayout();
            tsFieldsFilter.ResumeLayout();
            ResumeLayout();
        }
    }
}
