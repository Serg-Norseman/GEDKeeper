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
            SuspendLayout();

            lblName = new Label();
            //lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblType = new Label();
            //lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblStoreType = new Label();
            //lblStoreType.Size = new Size(84, 17);
            lblStoreType.Text = "lblStoreType";

            lblFile = new Label();
            //lblFile.Size = new Size(38, 17);
            lblFile.Text = "lblFile";

            txtName = new TextBox();
            //txtName.Size = new Size(696, 24);
            txtName.TextChanged += edName_TextChanged;

            cmbMediaType = new ComboBox();
            cmbMediaType.ReadOnly = true;
            //cmbMediaType.Size = new Size(237, 25);

            cmbStoreType = new ComboBox();
            cmbStoreType.ReadOnly = true;
            //cmbStoreType.Size = new Size(281, 25);

            txtFile = new TextBox();
            txtFile.ReadOnly = true;
            //txtFile.Size = new Size(629, 24);

            btnFileSelect = new Button();
            //btnFileSelect.Size = new Size(60, 26);
            btnFileSelect.Width = 60;
            btnFileSelect.Text = "...";
            btnFileSelect.Click += btnFileSelect_Click;

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        Cells = { lblFile, TableLayout.Horizontal(10, new TableCell(txtFile, true), btnFileSelect) }
                    },
                    new TableRow {
                        Cells = { lblType, TableLayout.Horizontal(10, cmbMediaType, lblStoreType, cmbStoreType) }
                    },
                    null
                }
            };

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageSources = new TabPage();
            pageSources.Text = "pageSources";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageSources);

            //

            btnView = new Button();
            btnView.Size = new Size(130, 26);
            btnView.Text = "btnView";
            btnView.Click += btnView_Click;

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

            //

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(btnView, null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "MediaEditDlg";

            SetPredefProperties(580, 460);
            ResumeLayout();
        }
    }
}
