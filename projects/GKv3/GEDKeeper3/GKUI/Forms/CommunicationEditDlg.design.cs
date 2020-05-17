using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
            lblTheme.Text = "lblTheme";

            lblDate = new Label();
            lblDate.Text = "lblDate";

            lblType = new Label();
            lblType.Text = "lblType";

            lblCorresponder = new Label();
            lblCorresponder.Text = "lblCorresponder";

            btnPersonAdd = new Button();
            btnPersonAdd.Size = new Size(26, 26);
            btnPersonAdd.Click += btnPersonAdd_Click;

            txtName = new TextBox();

            txtDate = new MaskedTextBox();
            txtDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            cmbCorrType = new ComboBox();
            cmbCorrType.ReadOnly = true;

            txtDir = new ComboBox();
            txtDir.ReadOnly = true;
            txtDir.Width = 100;

            txtCorresponder = new TextBox();
            txtCorresponder.ReadOnly = true;

            GroupBox1 = new GroupBox();
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblTheme, txtName }
                    },
                    new TableRow {
                        //Cells = { lblCorresponder, TableLayout.Horizontal(10, new TableCell(txtDir, false), new TableCell(txtCorresponder, true), btnPersonAdd) }
                        Cells = {
                            lblCorresponder,
                            new DefStackLayout(0, 10, Orientation.Horizontal) {
                                Items = { txtDir, new StackLayoutItem(txtCorresponder, true), btnPersonAdd }
                            }
                        }
                    },
                    new TableRow {
                        Cells = {
                            lblType,
                            TableLayout.Horizontal(10, cmbCorrType, lblDate, txtDate)
                        }
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
            tabsData.Size = new Size(600, 260);

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

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
            Title = "CommunicationEditDlg";

            SetPredefProperties(680, 520);
            ResumeLayout();
        }
    }
}
