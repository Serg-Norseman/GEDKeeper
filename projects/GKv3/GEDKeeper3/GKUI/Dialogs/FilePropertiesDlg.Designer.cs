using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextBox txtName;
        private TextBox txtTel;
        private TextArea txtAddress;
        private TabPage pageOther;
        private TabControl PageControl1;
        //private ColumnHeader columnHeader2;
        //private ColumnHeader columnHeader1;
        private GKListViewStub lvRecordStats;
        private Button btnLangEdit;
        private TextBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            PageControl1 = new TabControl();
            pageAuthor = new TabPage();
            btnLangEdit = new Button();
            lblName = new Label();
            lblAddress = new Label();
            lblLanguage = new Label();
            lblTelephone = new Label();
            txtName = new TextBox();
            txtLanguage = new TextBox();
            txtTel = new TextBox();
            txtAddress = new TextArea();
            pageOther = new TabPage();
            lvRecordStats = new GKListViewStub();
            columnHeader1 = new ColumnHeader();
            columnHeader2 = new ColumnHeader();
            PageControl1.SuspendLayout();
            pageAuthor.SuspendLayout();
            pageOther.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(381, 359);
            btnAccept.Size = new Size(112, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(504, 359);
            btnCancel.Size = new Size(112, 31);
            btnCancel.Text = "btnCancel";

            PageControl1.Controls.Add(pageAuthor);
            PageControl1.Controls.Add(pageOther);
            PageControl1.Location = new Point(11, 10);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(606, 331);

            pageAuthor.Controls.Add(btnLangEdit);
            pageAuthor.Controls.Add(lblName);
            pageAuthor.Controls.Add(lblAddress);
            pageAuthor.Controls.Add(lblLanguage);
            pageAuthor.Controls.Add(lblTelephone);
            pageAuthor.Controls.Add(txtName);
            pageAuthor.Controls.Add(txtLanguage);
            pageAuthor.Controls.Add(txtTel);
            pageAuthor.Controls.Add(txtAddress);
            pageAuthor.Location = new Point(4, 26);
            pageAuthor.Size = new Size(598, 301);
            pageAuthor.Text = "pageAuthor";

            btnLangEdit.Location = new Point(545, 220);
            btnLangEdit.Size = new Size(39, 34);
            btnLangEdit.Click += btnLangEdit_Click;

            lblName.Location = new Point(11, 14);
            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblAddress.Location = new Point(11, 39);
            lblAddress.Size = new Size(68, 17);
            lblAddress.Text = "lblAddress";

            lblLanguage.Location = new Point(11, 229);
            lblLanguage.Size = new Size(80, 17);
            lblLanguage.Text = "lblLanguage";

            lblTelephone.Location = new Point(11, 189);
            lblTelephone.Size = new Size(83, 17);
            lblTelephone.Text = "lblTelephone";

            txtName.Location = new Point(145, 10);
            txtName.Size = new Size(439, 24);

            txtLanguage.Location = new Point(145, 226);
            txtLanguage.ReadOnly = true;
            txtLanguage.Size = new Size(386, 24);

            txtTel.Location = new Point(145, 185);
            txtTel.Size = new Size(439, 24);

            txtAddress.Location = new Point(145, 39);
            txtAddress.Size = new Size(439, 136);

            pageOther.Controls.Add(lvRecordStats);
            pageOther.Location = new Point(4, 26);
            pageOther.Size = new Size(598, 301);
            pageOther.Text = "pageOther";

            lvRecordStats.Columns.AddRange(new ColumnHeader[] {
                                               columnHeader1,
                                               columnHeader2});
            lvRecordStats.Dock = DockStyle.Fill;
            lvRecordStats.FullRowSelect = true;
            lvRecordStats.Location = new Point(0, 0);
            lvRecordStats.MultiSelect = false;
            lvRecordStats.Size = new Size(598, 301);
            lvRecordStats.UseCompatibleStateImageBehavior = false;
            lvRecordStats.View = View.Details;

            columnHeader1.Text = "Records";
            columnHeader1.Width = 300;

            columnHeader2.Text = "Count";
            columnHeader2.TextAlign = HorizontalAlignment.Right;
            columnHeader2.Width = 100;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(630, 405);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(PageControl1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "FilePropertiesDlg";
            PageControl1.ResumeLayout();
            pageAuthor.ResumeLayout();
            pageOther.ResumeLayout();
            ResumeLayout();
        }
    }
}
