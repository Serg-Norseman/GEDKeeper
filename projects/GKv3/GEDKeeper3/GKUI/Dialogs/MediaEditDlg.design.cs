using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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

            SuspendLayout();

            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageSources);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(728, 302);

            pageCommon.Size = new Size(720, 272);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblName }
                    },
                    new TableRow {
                        Cells = { txtName }
                    },
                    new TableRow {
                        Cells = { lblFile }
                    },
                    new TableRow {
                        Cells = { txtFile, btnFileSelect }
                    },
                    new TableRow {
                        Cells = { lblType, lblStoreType }
                    },
                    new TableRow {
                        Cells = { cmbMediaType, cmbStoreType }
                    },
                    null
                }
            };

            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblStoreType.Size = new Size(84, 17);
            lblStoreType.Text = "lblStoreType";

            lblFile.Size = new Size(38, 17);
            lblFile.Text = "lblFile";

            txtName.Size = new Size(696, 24);
            txtName.TextChanged += edName_TextChanged;

            cmbMediaType.ReadOnly = true;
            cmbMediaType.Size = new Size(237, 25);

            cmbStoreType.ReadOnly = true;
            cmbStoreType.Size = new Size(281, 25);

            txtFile.ReadOnly = true;
            txtFile.Size = new Size(629, 24);

            btnFileSelect.Size = new Size(60, 25);
            btnFileSelect.Text = "...";
            btnFileSelect.Click += btnFileSelect_Click;

            pageNotes.Size = new Size(720, 272);
            pageNotes.Text = "pageNotes";

            pageSources.Size = new Size(720, 272);
            pageSources.Text = "pageSources";

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            btnView.Size = new Size(114, 30);
            btnView.Text = "btnView";
            btnView.Click += btnView_Click;

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
                        Cells = { btnView, null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(728, 365);
            Title = "MediaEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
