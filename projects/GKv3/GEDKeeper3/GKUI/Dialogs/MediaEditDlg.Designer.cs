using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class MediaEditDlg
    {
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageSources;
        private Button btnAccept;
        private Button btnCancel;
        private Button btnView;
        private TabPage pageCommon;
        private Label lblName;
        private TextBox txtName;
        private Label lblType;
        private ComboBox cmbMediaType;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Label lblFile;
        private TextBox txtFile;
        private Button btnFileSelect;

        private void InitializeComponent()
        {
            tabsData = new TabControl();
            pageCommon = new TabPage();
            lblName = new Label();
            lblType = new Label();
            lblStoreType = new Label();
            lblFile = new Label();
            txtName = new TextBox();
            cmbMediaType = new ComboBox();
            cmbStoreType = new ComboBox();
            txtFile = new TextBox();
            btnFileSelect = new Button();
            pageNotes = new TabPage();
            pageSources = new TabPage();
            btnAccept = new Button();
            btnCancel = new Button();
            btnView = new Button();
            tabsData.SuspendLayout();
            pageCommon.SuspendLayout();
            SuspendLayout();

            tabsData.Controls.Add(pageCommon);
            tabsData.Controls.Add(pageNotes);
            tabsData.Controls.Add(pageSources);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 0);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(728, 302);

            pageCommon.Controls.Add(lblName);
            pageCommon.Controls.Add(lblType);
            pageCommon.Controls.Add(lblStoreType);
            pageCommon.Controls.Add(lblFile);
            pageCommon.Controls.Add(txtName);
            pageCommon.Controls.Add(cmbMediaType);
            pageCommon.Controls.Add(cmbStoreType);
            pageCommon.Controls.Add(txtFile);
            pageCommon.Controls.Add(btnFileSelect);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Size = new Size(720, 272);
            pageCommon.Text = "pageCommon";

            lblName.Location = new Point(11, 10);
            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblType.Location = new Point(11, 115);
            lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblStoreType.Location = new Point(269, 115);
            lblStoreType.Size = new Size(84, 17);
            lblStoreType.Text = "lblStoreType";

            lblFile.Location = new Point(11, 62);
            lblFile.Size = new Size(38, 17);
            lblFile.Text = "lblFile";

            txtName.Location = new Point(11, 29);
            txtName.Size = new Size(696, 24);
            txtName.TextChanged += edName_TextChanged;

            cmbMediaType.ReadOnly = true;
            cmbMediaType.DropDownWidth = 15;
            cmbMediaType.Location = new Point(11, 135);
            cmbMediaType.Size = new Size(237, 25);

            cmbStoreType.ReadOnly = true;
            cmbStoreType.Location = new Point(269, 135);
            cmbStoreType.Size = new Size(281, 25);

            txtFile.Location = new Point(11, 82);
            txtFile.ReadOnly = true;
            txtFile.Size = new Size(629, 24);

            btnFileSelect.Location = new Point(650, 82);
            btnFileSelect.Size = new Size(60, 25);
            btnFileSelect.Text = "...";
            btnFileSelect.Click += btnFileSelect_Click;

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(720, 272);
            pageNotes.Text = "pageNotes";

            pageSources.Location = new Point(4, 26);
            pageSources.Size = new Size(720, 272);
            pageSources.Text = "pageSources";

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(482, 321);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(605, 321);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            btnView.Location = new Point(11, 321);
            btnView.Size = new Size(114, 30);
            btnView.Text = "btnView";
            btnView.Click += btnView_Click;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(728, 365);
            Controls.Add(tabsData);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(btnView);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "MediaEditDlg";
            tabsData.ResumeLayout();
            pageCommon.ResumeLayout();
            ResumeLayout();
        }
    }
}
