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
            GroupBox1 = new GroupBox();
            lblTheme = new Label();
            lblDate = new Label();
            lblType = new Label();
            lblCorresponder = new Label();
            btnPersonAdd = new Button();
            txtName = new TextBox();
            txtDate = new MaskedTextBox();
            cmbCorrType = new ComboBox();
            txtDir = new ComboBox();
            txtCorresponder = new TextBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();

            SuspendLayout();

            GroupBox1.Size = new Size(675, 118);
            GroupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblTheme, txtName }
                    },
                    new TableRow {
                        Cells = { lblCorresponder, txtDir, txtCorresponder, btnPersonAdd }
                    },
                    new TableRow {
                        Cells = { lblType, cmbCorrType, lblDate, txtDate }
                    },
                    null
                }
            };

            lblTheme.Size = new Size(62, 17);
            lblTheme.Text = "lblTheme";

            lblDate.Size = new Size(49, 17);
            lblDate.Text = "lblDate";

            lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            lblCorresponder.Size = new Size(104, 17);
            lblCorresponder.Text = "lblCorresponder";

            btnPersonAdd.Size = new Size(37, 32);
            btnPersonAdd.Click += btnPersonAdd_Click;

            txtName.Size = new Size(528, 24);

            txtDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtDate.Mask = "00/00/0000";
            txtDate.Size = new Size(225, 24);
            //txtDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbCorrType.ReadOnly = true;
            cmbCorrType.Size = new Size(147, 25);

            txtDir.ReadOnly = true;
            txtDir.Size = new Size(91, 25);

            txtCorresponder.TextColor = SystemColors.Control;
            txtCorresponder.ReadOnly = true;
            txtCorresponder.Size = new Size(382, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(675, 330);

            pageNotes.Size = new Size(667, 300);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Size = new Size(667, 300);
            pageMultimedia.Text = "pageMultimedia";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
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
