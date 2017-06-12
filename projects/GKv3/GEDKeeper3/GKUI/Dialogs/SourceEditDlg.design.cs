using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(114, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageText);
            tabsData.Pages.Add(pageRepositories);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(752, 487);

            pageCommon.Size = new Size(744, 457);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblShortTitle, txtShortTitle }
                    },
                    new TableRow {
                        Cells = { lblAuthor, txtAuthor }
                    },
                    new TableRow {
                        Cells = { lblTitle, txtTitle }
                    },
                    new TableRow {
                        Cells = { lblPublication, txtPublication }
                    },
                    null
                }
            };

            lblShortTitle.Size = new Size(78, 17);
            lblShortTitle.Text = "lblShortTitle";

            lblAuthor.Size = new Size(62, 17);
            lblAuthor.Text = "lblAuthor";

            lblTitle.Size = new Size(44, 17);
            lblTitle.Text = "lblTitle";

            lblPublication.Size = new Size(85, 17);
            lblPublication.Text = "lblPublication";

            txtShortTitle.Size = new Size(326, 24);
            txtShortTitle.TextChanged += EditShortTitle_TextChanged;

            txtAuthor.Size = new Size(572, 127);

            txtTitle.Size = new Size(572, 127);

            txtPublication.Size = new Size(572, 127);

            pageText.Content = txtText;
            pageText.Size = new Size(744, 457);
            pageText.Text = "pageText";

            txtText.Size = new Size(744, 457);

            pageRepositories.Size = new Size(744, 457);
            pageRepositories.Text = "pageRepositories";

            pageNotes.Size = new Size(744, 457);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Size = new Size(744, 457);
            pageMultimedia.Text = "pageMultimedia";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(752, 549);
            Title = "SourceEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
