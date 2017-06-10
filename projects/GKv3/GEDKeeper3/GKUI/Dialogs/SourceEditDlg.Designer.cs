using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class SourceEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageRepositories;
        private TabPage pageText;
        private TextArea txtText;
        private TabPage pageCommon;
        private Label lblShortTitle;
        private TextBox txtShortTitle;
        private Label lblAuthor;
        private TextArea txtAuthor;
        private Label lblTitle;
        private TextArea txtTitle;
        private Label lblPublication;
        private TextArea txtPublication;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageCommon = new TabPage();
            lblShortTitle = new Label();
            lblAuthor = new Label();
            lblTitle = new Label();
            lblPublication = new Label();
            txtShortTitle = new TextBox();
            txtAuthor = new TextArea();
            txtTitle = new TextArea();
            txtPublication = new TextArea();
            pageText = new TabPage();
            txtText = new TextArea();
            pageRepositories = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            tabsData.SuspendLayout();
            pageCommon.SuspendLayout();
            pageText.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(504, 505);
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(627, 505);
            btnCancel.Size = new Size(114, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageCommon);
            tabsData.Controls.Add(pageText);
            tabsData.Controls.Add(pageRepositories);
            tabsData.Controls.Add(pageNotes);
            tabsData.Controls.Add(pageMultimedia);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 0);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(752, 487);

            pageCommon.Controls.Add(lblShortTitle);
            pageCommon.Controls.Add(lblAuthor);
            pageCommon.Controls.Add(lblTitle);
            pageCommon.Controls.Add(lblPublication);
            pageCommon.Controls.Add(txtShortTitle);
            pageCommon.Controls.Add(txtAuthor);
            pageCommon.Controls.Add(txtTitle);
            pageCommon.Controls.Add(txtPublication);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Size = new Size(744, 457);
            pageCommon.Text = "pageCommon";

            lblShortTitle.Location = new Point(11, 10);
            lblShortTitle.Size = new Size(78, 17);
            lblShortTitle.Text = "lblShortTitle";

            lblAuthor.Location = new Point(11, 39);
            lblAuthor.Size = new Size(62, 17);
            lblAuthor.Text = "lblAuthor";

            lblTitle.Location = new Point(11, 175);
            lblTitle.Size = new Size(44, 17);
            lblTitle.Text = "lblTitle";

            lblPublication.Location = new Point(11, 311);
            lblPublication.Size = new Size(85, 17);
            lblPublication.Text = "lblPublication";

            txtShortTitle.Location = new Point(157, 10);
            txtShortTitle.Size = new Size(326, 24);
            txtShortTitle.TextChanged += EditShortTitle_TextChanged;

            txtAuthor.Location = new Point(157, 39);
            txtAuthor.Size = new Size(572, 127);

            txtTitle.Location = new Point(157, 175);
            txtTitle.Size = new Size(572, 127);

            txtPublication.Location = new Point(157, 311);
            txtPublication.Size = new Size(572, 127);

            pageText.Controls.Add(txtText);
            pageText.Location = new Point(4, 26);
            pageText.Size = new Size(744, 457);
            pageText.Text = "pageText";

            txtText.Dock = DockStyle.Fill;
            txtText.Location = new Point(0, 0);
            txtText.Size = new Size(744, 457);

            pageRepositories.Location = new Point(4, 26);
            pageRepositories.Size = new Size(744, 457);
            pageRepositories.Text = "pageRepositories";

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(744, 457);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(744, 457);
            pageMultimedia.Text = "pageMultimedia";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(752, 549);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(tabsData);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "SourceEditDlg";
            tabsData.ResumeLayout();
            pageCommon.ResumeLayout();
            pageText.ResumeLayout();
            ResumeLayout();
        }
    }
}
