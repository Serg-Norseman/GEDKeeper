using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class ParentsEditDlg
    {
        private TabControl tabsPersonData;
        private TabPage pageEvents;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private TabPage pageSpouses;
        private TabPage pageAssociations;
        private TabPage pageGroups;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRestriction;
        private ComboBox cmbRestriction;

        private GroupBox GroupBox1;
        private Label lblChildName;
        private TextBox txtChildName;
        private Label lblLinkageType;
        private ComboBox cmbLinkageType;

        private TextBox txtName;
        public ComboBox cmbSex;
        private CheckBox chkPatriarch;
        private TabPage pageUserRefs;
        private Panel panCtlParents;
        private Label lblParents;
        private TextBox txtFather;
        private TextBox txtMother;
        private Button btnParentsAdd;
        private Button btnParentsEdit;
        private Button btnParentsDelete;
        private CheckBox chkBookmark;
        private Label lblSurnamePrefix;
        private TextBox txtSurnamePrefix;
        private Label lblNamePrefix;
        private TextBox txtNamePrefix;
        private Label lblNameSuffix;
        private TextBox txtNameSuffix;
        private Label lblNickname;
        private TextBox txtNickname;
        private GKPortrait imgPortrait;
        private Button btnNameCopy;
        private Button btnPortraitAdd;
        private Button btnPortraitDelete;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnFatherSel;
        private Button btnMotherAdd;
        private Button btnMotherDelete;
        private Button btnMotherSel;
        private TabPage pageNames;
        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;
        private TabPage pageParents;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblChildName = new Label();
            lblChildName.Text = "lblChildName";

            txtChildName = new TextBox();
            txtChildName.Size = new Size(180, 22);
            txtChildName.ReadOnly = true;

            lblLinkageType = new Label();
            lblLinkageType.Text = "lblLinkageType";

            cmbLinkageType = new ComboBox();
            cmbLinkageType.Size = new Size(180, 22);
            cmbLinkageType.ReadOnly = true;

            var personLayout = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblChildName, txtChildName }
                    },
                    new TableRow {
                        Cells = { lblLinkageType, cmbLinkageType }
                    },
                }
            };

            GroupBox1 = new GroupBox();
            GroupBox1.Content = TableLayout.Horizontal(10, new TableCell(personLayout, true));

            //

            lblParents = new Label();
            lblParents.Text = "lblParents";

            txtFather = new TextBox();
            txtFather.Enabled = false;
            txtFather.Size = new Size(220, 22);

            txtMother = new TextBox();
            txtMother.Enabled = false;
            txtMother.Size = new Size(220, 22);

            btnParentsEdit = new Button();
            btnParentsEdit.Size = UIHelper.ShortButtonSize;
            btnParentsEdit.Click += btnParentsEdit_Click;

            btnFatherAdd = new Button();
            btnFatherAdd.Size = UIHelper.ShortButtonSize;
            btnFatherAdd.Click += btnFatherAdd_Click;

            btnFatherDelete = new Button();
            btnFatherDelete.Size = UIHelper.ShortButtonSize;
            btnFatherDelete.Click += btnFatherDelete_Click;

            btnMotherAdd = new Button();
            btnMotherAdd.Size = UIHelper.ShortButtonSize;
            btnMotherAdd.Click += btnMotherAdd_Click;

            btnMotherDelete = new Button();
            btnMotherDelete.Size = UIHelper.ShortButtonSize;
            btnMotherDelete.Click += btnMotherDelete_Click;

            var parentsTab = new DefTableLayout(4, 2);
            parentsTab.SetColumnScale(0, false);
            parentsTab.SetColumnScale(1, true);
            parentsTab.SetColumnScale(2, true);
            parentsTab.SetColumnScale(3, false);
            parentsTab.Add(lblParents, 0, 0);
            parentsTab.Add(txtFather, 1, 0);
            parentsTab.Add(txtMother, 2, 0);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnParentsAdd, btnParentsEdit, btnParentsDelete), 3, 0);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnFatherAdd, btnFatherDelete, btnFatherSel), 1, 1);
            parentsTab.Add(new DefStackLayout(Orientation.Horizontal, 10, btnMotherAdd, btnMotherDelete, btnMotherSel), 2, 1);

            panCtlParents = new GroupBox();
            panCtlParents.Content = parentsTab;

            //

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = UIHelper.LongButtonSize;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = UIHelper.LongButtonSize;
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            //

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { personLayout }
                    },
                    new TableRow {
                        Cells = { panCtlParents }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsPersonData }
                    },
                    UIHelper.MakeDialogFooter(lblRestriction, cmbRestriction, null, btnNameCopy, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "ParentsEditDlg";

            SetPredefProperties(700, 560);
            ResumeLayout();
        }
    }
}
