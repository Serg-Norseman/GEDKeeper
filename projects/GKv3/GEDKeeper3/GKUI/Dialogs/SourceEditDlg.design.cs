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
            SuspendLayout();

            lblShortTitle = new Label();
            lblShortTitle.Text = "lblShortTitle";

            lblAuthor = new Label();
            lblAuthor.Text = "lblAuthor";

            lblTitle = new Label();
            lblTitle.Text = "lblTitle";

            lblPublication = new Label();
            lblPublication.Text = "lblPublication";

            txtShortTitle = new TextBox();
            txtShortTitle.TextChanged += EditShortTitle_TextChanged;

            txtAuthor = new TextArea();

            txtTitle = new TextArea();

            txtPublication = new TextArea();

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new DefTableLayout {
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
                    }
                }
            };

            //

            txtText = new TextArea();

            pageText = new TabPage();
            pageText.Content = txtText;
            pageText.Text = "pageText";

            //

            pageRepositories = new TabPage();
            pageRepositories.Text = "pageRepositories";

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageText);
            tabsData.Pages.Add(pageRepositories);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
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
