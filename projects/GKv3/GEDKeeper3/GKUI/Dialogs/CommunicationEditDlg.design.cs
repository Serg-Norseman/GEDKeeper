using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class CommunicationEditDlg
    {
        private GroupBox GroupBox1;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblTheme;
        private TextBox txtName;
        private Label lblDate;
        private MaskedTextBox txtDate;
        private Label lblType;
        private ComboBox cmbCorrType;
        private ComboBox txtDir;
        private Label lblCorresponder;
        private TextBox txtCorresponder;
        private Button btnPersonAdd;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblTheme = new Label();
            //lblTheme.Size = new Size(62, 17);
            lblTheme.Text = "lblTheme";

            lblDate = new Label();
            //lblDate.Size = new Size(49, 17);
            lblDate.Text = "lblDate";

            lblType = new Label();
            //lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblCorresponder = new Label();
            //lblCorresponder.Size = new Size(104, 17);
            lblCorresponder.Text = "lblCorresponder";

            btnPersonAdd = new Button();
            btnPersonAdd.Size = new Size(26, 26);
            btnPersonAdd.Click += btnPersonAdd_Click;
            btnPersonAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            txtName = new TextBox();
            //txtName.Size = new Size(528, 24);

            txtDate = new MaskedTextBox();
            txtDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtDate.Size = new Size(225, 24);

            cmbCorrType = new ComboBox();
            cmbCorrType.ReadOnly = true;
            //cmbCorrType.Size = new Size(147, 25);

            txtDir = new ComboBox();
            txtDir.ReadOnly = true;
            //txtDir.Size = new Size(91, 25);

            txtCorresponder = new TextBox();
            txtCorresponder.ReadOnly = true;
            //txtCorresponder.Size = new Size(382, 24);

            GroupBox1 = new GroupBox();
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblTheme, txtName }
                    },
                    new TableRow {
                        Cells = { lblCorresponder, txtDir, txtCorresponder, btnPersonAdd }
                    },
                    new TableRow {
                        Cells = { lblType, cmbCorrType, lblDate, txtDate }
                    }
                }
            };

            //

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            tabsData = new TabControl();
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
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(675, 513);
            Title = "CommunicationEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
