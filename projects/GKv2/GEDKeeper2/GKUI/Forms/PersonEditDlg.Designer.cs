namespace GKUI.Forms
{
    partial class PersonEditDlg
    {
        private GKUI.Components.GKTabControl tabsData;
        private System.Windows.Forms.TabPage pageEvents;
        private System.Windows.Forms.TabPage pageNotes;
        private System.Windows.Forms.TabPage pageMultimedia;
        private System.Windows.Forms.TabPage pageSources;
        private System.Windows.Forms.TabPage pageSpouses;
        private System.Windows.Forms.TabPage pageAssociations;
        private System.Windows.Forms.TabPage pageGroups;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label lblRestriction;
        private System.Windows.Forms.ComboBox cmbRestriction;
        private System.Windows.Forms.Panel GroupBox1;
        private System.Windows.Forms.Label lblSurname;
        private System.Windows.Forms.Label lblName;
        private System.Windows.Forms.Label lblPatronymic;
        private System.Windows.Forms.Label lblSex;
        private System.Windows.Forms.TextBox txtSurname;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.ComboBox cmbPatronymic;
        public GKUI.Components.GKComboBox cmbSex;
        private System.Windows.Forms.CheckBox chkPatriarch;
        private System.Windows.Forms.TabPage pageUserRefs;
        private System.Windows.Forms.Panel panCtlParents;
        private System.Windows.Forms.Label lblParents;
        private System.Windows.Forms.TextBox txtFather;
        private System.Windows.Forms.TextBox txtMother;
        private System.Windows.Forms.Button btnParentsAdd;
        private System.Windows.Forms.Button btnParentsEdit;
        private System.Windows.Forms.Button btnParentsDelete;
        private System.Windows.Forms.CheckBox chkBookmark;
        private System.Windows.Forms.Label lblNickname;
        private System.Windows.Forms.TextBox txtNickname;
        private GKUI.Components.GKPortrait imgPortrait;
        private System.Windows.Forms.Button btnNameCopy;
        private System.Windows.Forms.Button btnPortraitAdd;
        private System.Windows.Forms.Button btnPortraitDelete;
        private System.Windows.Forms.Button btnFatherAdd;
        private System.Windows.Forms.Button btnFatherDelete;
        private System.Windows.Forms.Button btnFatherSel;
        private System.Windows.Forms.Button btnMotherAdd;
        private System.Windows.Forms.Button btnMotherDelete;
        private System.Windows.Forms.Button btnMotherSel;
        private System.Windows.Forms.TabPage pageNames;
        private System.Windows.Forms.TextBox txtMarriedSurname;
        private System.Windows.Forms.Label lblMarriedSurname;
        private System.Windows.Forms.TabPage pageParents;
        private System.Windows.Forms.TabPage pageChilds;
        private System.Windows.Forms.TabPage pageDNATests;
        private System.Windows.Forms.TabPage pageFamily;
        private GKUI.Components.GKTabControl tabsFamily;
        private System.Windows.Forms.TabPage pageOther;
        private GKUI.Components.GKTabControl tabsOther;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel2;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel3;
        private System.Windows.Forms.Label lblMother;
        private System.Windows.Forms.Label lblFather;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lblRestriction = new System.Windows.Forms.Label();
            this.cmbRestriction = new System.Windows.Forms.ComboBox();
            this.GroupBox1 = new System.Windows.Forms.Panel();
            this.lblMarriedSurname = new System.Windows.Forms.Label();
            this.lblSurname = new System.Windows.Forms.Label();
            this.lblName = new System.Windows.Forms.Label();
            this.lblPatronymic = new System.Windows.Forms.Label();
            this.lblSex = new System.Windows.Forms.Label();
            this.lblNickname = new System.Windows.Forms.Label();
            this.txtMarriedSurname = new System.Windows.Forms.TextBox();
            this.txtSurname = new System.Windows.Forms.TextBox();
            this.txtName = new System.Windows.Forms.TextBox();
            this.cmbPatronymic = new System.Windows.Forms.ComboBox();
            this.cmbSex = new GKUI.Components.GKComboBox();
            this.chkPatriarch = new System.Windows.Forms.CheckBox();
            this.chkBookmark = new System.Windows.Forms.CheckBox();
            this.txtNickname = new System.Windows.Forms.TextBox();
            this.imgPortrait = new GKUI.Components.GKPortrait();
            this.btnPortraitAdd = new System.Windows.Forms.Button();
            this.btnPortraitDelete = new System.Windows.Forms.Button();
            this.panCtlParents = new System.Windows.Forms.Panel();
            this.txtMother = new System.Windows.Forms.TextBox();
            this.lblParents = new System.Windows.Forms.Label();
            this.btnParentsAdd = new System.Windows.Forms.Button();
            this.btnParentsEdit = new System.Windows.Forms.Button();
            this.btnParentsDelete = new System.Windows.Forms.Button();
            this.btnFatherAdd = new System.Windows.Forms.Button();
            this.btnFatherDelete = new System.Windows.Forms.Button();
            this.btnFatherSel = new System.Windows.Forms.Button();
            this.btnMotherAdd = new System.Windows.Forms.Button();
            this.btnMotherDelete = new System.Windows.Forms.Button();
            this.btnMotherSel = new System.Windows.Forms.Button();
            this.txtFather = new System.Windows.Forms.TextBox();
            this.btnNameCopy = new System.Windows.Forms.Button();
            this.tabsData = new GKUI.Components.GKTabControl();
            this.pageEvents = new System.Windows.Forms.TabPage();
            this.pageFamily = new System.Windows.Forms.TabPage();
            this.tabsFamily = new GKUI.Components.GKTabControl();
            this.pageSpouses = new System.Windows.Forms.TabPage();
            this.pageParents = new System.Windows.Forms.TabPage();
            this.pageChilds = new System.Windows.Forms.TabPage();
            this.pageAssociations = new System.Windows.Forms.TabPage();
            this.pageNames = new System.Windows.Forms.TabPage();
            this.pageNotes = new System.Windows.Forms.TabPage();
            this.pageMultimedia = new System.Windows.Forms.TabPage();
            this.pageSources = new System.Windows.Forms.TabPage();
            this.pageOther = new System.Windows.Forms.TabPage();
            this.tabsOther = new GKUI.Components.GKTabControl();
            this.pageUserRefs = new System.Windows.Forms.TabPage();
            this.pageGroups = new System.Windows.Forms.TabPage();
            this.pageDNATests = new System.Windows.Forms.TabPage();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.panel1 = new System.Windows.Forms.Panel();
            this.tableLayoutPanel2 = new System.Windows.Forms.TableLayoutPanel();
            this.tableLayoutPanel3 = new System.Windows.Forms.TableLayoutPanel();
            this.lblFather = new System.Windows.Forms.Label();
            this.lblMother = new System.Windows.Forms.Label();
            this.GroupBox1.SuspendLayout();
            this.panCtlParents.SuspendLayout();
            this.tabsData.SuspendLayout();
            this.pageFamily.SuspendLayout();
            this.tabsFamily.SuspendLayout();
            this.pageOther.SuspendLayout();
            this.tabsOther.SuspendLayout();
            this.tableLayoutPanel1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.tableLayoutPanel2.SuspendLayout();
            this.tableLayoutPanel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(544, 542);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2, 2, 8, 2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(90, 24);
            this.btnAccept.TabIndex = 5;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(644, 542);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(90, 24);
            this.btnCancel.TabIndex = 6;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // lblRestriction
            // 
            this.lblRestriction.AutoSize = true;
            this.lblRestriction.Location = new System.Drawing.Point(9, 548);
            this.lblRestriction.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblRestriction.Name = "lblRestriction";
            this.lblRestriction.Size = new System.Drawing.Size(68, 13);
            this.lblRestriction.TabIndex = 2;
            this.lblRestriction.Text = "lblRestriction";
            // 
            // cmbRestriction
            // 
            this.cmbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRestriction.Location = new System.Drawing.Point(212, 545);
            this.cmbRestriction.Margin = new System.Windows.Forms.Padding(2);
            this.cmbRestriction.Name = "cmbRestriction";
            this.cmbRestriction.Size = new System.Drawing.Size(163, 21);
            this.cmbRestriction.TabIndex = 3;
            this.cmbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
            // 
            // GroupBox1
            // 
            this.GroupBox1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.GroupBox1.Controls.Add(this.lblMarriedSurname);
            this.GroupBox1.Controls.Add(this.lblSurname);
            this.GroupBox1.Controls.Add(this.lblName);
            this.GroupBox1.Controls.Add(this.lblPatronymic);
            this.GroupBox1.Controls.Add(this.lblSex);
            this.GroupBox1.Controls.Add(this.lblNickname);
            this.GroupBox1.Controls.Add(this.txtMarriedSurname);
            this.GroupBox1.Controls.Add(this.txtSurname);
            this.GroupBox1.Controls.Add(this.txtName);
            this.GroupBox1.Controls.Add(this.cmbPatronymic);
            this.GroupBox1.Controls.Add(this.cmbSex);
            this.GroupBox1.Controls.Add(this.chkPatriarch);
            this.GroupBox1.Controls.Add(this.chkBookmark);
            this.GroupBox1.Controls.Add(this.txtNickname);
            this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox1.Location = new System.Drawing.Point(0, 0);
            this.GroupBox1.Margin = new System.Windows.Forms.Padding(0, 0, 0, 4);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Padding = new System.Windows.Forms.Padding(2);
            this.GroupBox1.Size = new System.Drawing.Size(564, 122);
            this.GroupBox1.TabIndex = 0;
            // 
            // lblMarriedSurname
            // 
            this.lblMarriedSurname.AutoSize = true;
            this.lblMarriedSurname.Location = new System.Drawing.Point(6, 46);
            this.lblMarriedSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMarriedSurname.Name = "lblMarriedSurname";
            this.lblMarriedSurname.Size = new System.Drawing.Size(95, 13);
            this.lblMarriedSurname.TabIndex = 2;
            this.lblMarriedSurname.Text = "lblMarriedSurname";
            // 
            // lblSurname
            // 
            this.lblSurname.AutoSize = true;
            this.lblSurname.Location = new System.Drawing.Point(6, 21);
            this.lblSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSurname.Name = "lblSurname";
            this.lblSurname.Size = new System.Drawing.Size(59, 13);
            this.lblSurname.TabIndex = 0;
            this.lblSurname.Text = "lblSurname";
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(6, 71);
            this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(44, 13);
            this.lblName.TabIndex = 4;
            this.lblName.Text = "lblName";
            // 
            // lblPatronymic
            // 
            this.lblPatronymic.AutoSize = true;
            this.lblPatronymic.Location = new System.Drawing.Point(6, 96);
            this.lblPatronymic.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblPatronymic.Name = "lblPatronymic";
            this.lblPatronymic.Size = new System.Drawing.Size(70, 13);
            this.lblPatronymic.TabIndex = 6;
            this.lblPatronymic.Text = "lblPatronymic";
            // 
            // lblSex
            // 
            this.lblSex.AutoSize = true;
            this.lblSex.Location = new System.Drawing.Point(331, 46);
            this.lblSex.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSex.Name = "lblSex";
            this.lblSex.Size = new System.Drawing.Size(35, 13);
            this.lblSex.TabIndex = 16;
            this.lblSex.Text = "lblSex";
            // 
            // lblNickname
            // 
            this.lblNickname.AutoSize = true;
            this.lblNickname.Location = new System.Drawing.Point(331, 21);
            this.lblNickname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNickname.Name = "lblNickname";
            this.lblNickname.Size = new System.Drawing.Size(62, 13);
            this.lblNickname.TabIndex = 14;
            this.lblNickname.Text = "lblNickname";
            // 
            // txtMarriedSurname
            // 
            this.txtMarriedSurname.Location = new System.Drawing.Point(134, 43);
            this.txtMarriedSurname.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.txtMarriedSurname.Name = "txtMarriedSurname";
            this.txtMarriedSurname.Size = new System.Drawing.Size(182, 21);
            this.txtMarriedSurname.TabIndex = 3;
            this.txtMarriedSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.txtXName_KeyDown);
            this.txtMarriedSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            this.txtMarriedSurname.Leave += new System.EventHandler(this.txtXName_Leave);
            // 
            // txtSurname
            // 
            this.txtSurname.Location = new System.Drawing.Point(134, 18);
            this.txtSurname.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.txtSurname.Name = "txtSurname";
            this.txtSurname.Size = new System.Drawing.Size(182, 21);
            this.txtSurname.TabIndex = 1;
            this.txtSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.txtXName_KeyDown);
            this.txtSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            this.txtSurname.Leave += new System.EventHandler(this.txtXName_Leave);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(134, 68);
            this.txtName.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(182, 21);
            this.txtName.TabIndex = 5;
            this.txtName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.txtXName_KeyDown);
            this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            this.txtName.Leave += new System.EventHandler(this.txtXName_Leave);
            // 
            // cmbPatronymic
            // 
            this.cmbPatronymic.Location = new System.Drawing.Point(134, 93);
            this.cmbPatronymic.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.cmbPatronymic.Name = "cmbPatronymic";
            this.cmbPatronymic.Size = new System.Drawing.Size(182, 21);
            this.cmbPatronymic.TabIndex = 7;
            this.cmbPatronymic.KeyDown += new System.Windows.Forms.KeyEventHandler(this.txtXName_KeyDown);
            this.cmbPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            this.cmbPatronymic.Leave += new System.EventHandler(this.txtXName_Leave);
            // 
            // cmbSex
            // 
            this.cmbSex.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.cmbSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSex.Location = new System.Drawing.Point(417, 43);
            this.cmbSex.Margin = new System.Windows.Forms.Padding(2);
            this.cmbSex.Name = "cmbSex";
            this.cmbSex.Size = new System.Drawing.Size(136, 22);
            this.cmbSex.TabIndex = 17;
            this.cmbSex.SelectedIndexChanged += new System.EventHandler(this.cbSex_SelectedIndexChanged);
            // 
            // chkPatriarch
            // 
            this.chkPatriarch.AutoSize = true;
            this.chkPatriarch.Location = new System.Drawing.Point(417, 74);
            this.chkPatriarch.Margin = new System.Windows.Forms.Padding(2);
            this.chkPatriarch.Name = "chkPatriarch";
            this.chkPatriarch.Size = new System.Drawing.Size(85, 17);
            this.chkPatriarch.TabIndex = 18;
            this.chkPatriarch.Text = "chkPatriarch";
            // 
            // chkBookmark
            // 
            this.chkBookmark.AutoSize = true;
            this.chkBookmark.Location = new System.Drawing.Point(417, 95);
            this.chkBookmark.Margin = new System.Windows.Forms.Padding(2);
            this.chkBookmark.Name = "chkBookmark";
            this.chkBookmark.Size = new System.Drawing.Size(88, 17);
            this.chkBookmark.TabIndex = 19;
            this.chkBookmark.Text = "chkBookmark";
            // 
            // txtNickname
            // 
            this.txtNickname.Location = new System.Drawing.Point(417, 18);
            this.txtNickname.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.txtNickname.Name = "txtNickname";
            this.txtNickname.Size = new System.Drawing.Size(136, 21);
            this.txtNickname.TabIndex = 15;
            // 
            // imgPortrait
            // 
            this.imgPortrait.Dock = System.Windows.Forms.DockStyle.Fill;
            this.imgPortrait.Location = new System.Drawing.Point(8, 8);
            this.imgPortrait.Name = "imgPortrait";
            this.imgPortrait.Size = new System.Drawing.Size(142, 199);
            this.imgPortrait.TabIndex = 22;
            // 
            // btnPortraitAdd
            // 
            this.btnPortraitAdd.Location = new System.Drawing.Point(45, 226);
            this.btnPortraitAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnPortraitAdd.Name = "btnPortraitAdd";
            this.btnPortraitAdd.Size = new System.Drawing.Size(29, 29);
            this.btnPortraitAdd.TabIndex = 19;
            this.btnPortraitAdd.TabStop = false;
            this.btnPortraitAdd.Click += new System.EventHandler(this.btnPortraitAdd_Click);
            // 
            // btnPortraitDelete
            // 
            this.btnPortraitDelete.Location = new System.Drawing.Point(82, 226);
            this.btnPortraitDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnPortraitDelete.Name = "btnPortraitDelete";
            this.btnPortraitDelete.Size = new System.Drawing.Size(29, 29);
            this.btnPortraitDelete.TabIndex = 20;
            this.btnPortraitDelete.TabStop = false;
            this.btnPortraitDelete.Click += new System.EventHandler(this.btnPortraitDelete_Click);
            // 
            // panCtlParents
            // 
            this.panCtlParents.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panCtlParents.Controls.Add(this.lblMother);
            this.panCtlParents.Controls.Add(this.lblFather);
            this.panCtlParents.Controls.Add(this.txtMother);
            this.panCtlParents.Controls.Add(this.lblParents);
            this.panCtlParents.Controls.Add(this.btnParentsAdd);
            this.panCtlParents.Controls.Add(this.btnParentsEdit);
            this.panCtlParents.Controls.Add(this.btnParentsDelete);
            this.panCtlParents.Controls.Add(this.btnFatherAdd);
            this.panCtlParents.Controls.Add(this.btnFatherDelete);
            this.panCtlParents.Controls.Add(this.btnFatherSel);
            this.panCtlParents.Controls.Add(this.btnMotherAdd);
            this.panCtlParents.Controls.Add(this.btnMotherDelete);
            this.panCtlParents.Controls.Add(this.btnMotherSel);
            this.panCtlParents.Controls.Add(this.txtFather);
            this.panCtlParents.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panCtlParents.Location = new System.Drawing.Point(0, 126);
            this.panCtlParents.Margin = new System.Windows.Forms.Padding(0);
            this.panCtlParents.Name = "panCtlParents";
            this.panCtlParents.Size = new System.Drawing.Size(564, 127);
            this.panCtlParents.TabIndex = 21;
            // 
            // lblParents
            // 
            this.lblParents.AutoSize = true;
            this.lblParents.Location = new System.Drawing.Point(6, 25);
            this.lblParents.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblParents.Name = "lblParents";
            this.lblParents.Size = new System.Drawing.Size(54, 13);
            this.lblParents.TabIndex = 0;
            this.lblParents.Text = "lblParents";
            // 
            // btnParentsAdd
            // 
            this.btnParentsAdd.Location = new System.Drawing.Point(455, 17);
            this.btnParentsAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsAdd.Name = "btnParentsAdd";
            this.btnParentsAdd.Size = new System.Drawing.Size(29, 29);
            this.btnParentsAdd.TabIndex = 1;
            this.btnParentsAdd.Click += new System.EventHandler(this.btnParentsAdd_Click);
            // 
            // btnParentsEdit
            // 
            this.btnParentsEdit.Location = new System.Drawing.Point(490, 17);
            this.btnParentsEdit.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsEdit.Name = "btnParentsEdit";
            this.btnParentsEdit.Size = new System.Drawing.Size(29, 29);
            this.btnParentsEdit.TabIndex = 2;
            this.btnParentsEdit.Click += new System.EventHandler(this.btnParentsEdit_Click);
            // 
            // btnParentsDelete
            // 
            this.btnParentsDelete.Location = new System.Drawing.Point(525, 17);
            this.btnParentsDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsDelete.Name = "btnParentsDelete";
            this.btnParentsDelete.Size = new System.Drawing.Size(29, 29);
            this.btnParentsDelete.TabIndex = 3;
            this.btnParentsDelete.Click += new System.EventHandler(this.btnParentsDelete_Click);
            // 
            // lblFather
            // 
            this.lblFather.AutoSize = true;
            this.lblFather.Location = new System.Drawing.Point(6, 59);
            this.lblFather.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblFather.Name = "lblFather";
            this.lblFather.Size = new System.Drawing.Size(54, 13);
            this.lblFather.TabIndex = 4;
            this.lblFather.Text = "lblFather";
            // 
            // txtFather
            // 
            this.txtFather.ForeColor = System.Drawing.SystemColors.ControlText;
            this.txtFather.Location = new System.Drawing.Point(70, 55);
            this.txtFather.Margin = new System.Windows.Forms.Padding(2);
            this.txtFather.Name = "txtFather";
            this.txtFather.ReadOnly = true;
            this.txtFather.Size = new System.Drawing.Size(379, 21);
            this.txtFather.TabIndex = 5;
            // 
            // btnFatherAdd
            // 
            this.btnFatherAdd.Location = new System.Drawing.Point(455, 50);
            this.btnFatherAdd.Margin = new System.Windows.Forms.Padding(2, 2, 4, 2);
            this.btnFatherAdd.Name = "btnFatherAdd";
            this.btnFatherAdd.Size = new System.Drawing.Size(29, 29);
            this.btnFatherAdd.TabIndex = 6;
            this.btnFatherAdd.Click += new System.EventHandler(this.btnFatherAdd_Click);
            // 
            // btnFatherDelete
            // 
            this.btnFatherDelete.Location = new System.Drawing.Point(490, 50);
            this.btnFatherDelete.Margin = new System.Windows.Forms.Padding(2, 2, 4, 2);
            this.btnFatherDelete.Name = "btnFatherDelete";
            this.btnFatherDelete.Size = new System.Drawing.Size(29, 29);
            this.btnFatherDelete.TabIndex = 7;
            this.btnFatherDelete.Click += new System.EventHandler(this.btnFatherDelete_Click);
            // 
            // btnFatherSel
            // 
            this.btnFatherSel.Location = new System.Drawing.Point(525, 50);
            this.btnFatherSel.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherSel.Name = "btnFatherSel";
            this.btnFatherSel.Size = new System.Drawing.Size(29, 29);
            this.btnFatherSel.TabIndex = 8;
            this.btnFatherSel.Click += new System.EventHandler(this.btnFatherSel_Click);
            // 
            // lblMother
            // 
            this.lblMother.AutoSize = true;
            this.lblMother.Location = new System.Drawing.Point(6, 92);
            this.lblMother.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMother.Name = "lblMother";
            this.lblMother.Size = new System.Drawing.Size(54, 13);
            this.lblMother.TabIndex = 9;
            this.lblMother.Text = "lblMother";
            // 
            // txtMother
            // 
            this.txtMother.ForeColor = System.Drawing.SystemColors.ControlText;
            this.txtMother.Location = new System.Drawing.Point(70, 88);
            this.txtMother.Margin = new System.Windows.Forms.Padding(2);
            this.txtMother.Name = "txtMother";
            this.txtMother.ReadOnly = true;
            this.txtMother.Size = new System.Drawing.Size(379, 21);
            this.txtMother.TabIndex = 10;
            // 
            // btnMotherAdd
            // 
            this.btnMotherAdd.Location = new System.Drawing.Point(455, 83);
            this.btnMotherAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherAdd.Name = "btnMotherAdd";
            this.btnMotherAdd.Size = new System.Drawing.Size(29, 29);
            this.btnMotherAdd.TabIndex = 11;
            this.btnMotherAdd.Click += new System.EventHandler(this.btnMotherAdd_Click);
            // 
            // btnMotherDelete
            // 
            this.btnMotherDelete.Location = new System.Drawing.Point(490, 83);
            this.btnMotherDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherDelete.Name = "btnMotherDelete";
            this.btnMotherDelete.Size = new System.Drawing.Size(29, 29);
            this.btnMotherDelete.TabIndex = 12;
            this.btnMotherDelete.Click += new System.EventHandler(this.btnMotherDelete_Click);
            // 
            // btnMotherSel
            // 
            this.btnMotherSel.Location = new System.Drawing.Point(525, 83);
            this.btnMotherSel.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherSel.Name = "btnMotherSel";
            this.btnMotherSel.Size = new System.Drawing.Size(29, 29);
            this.btnMotherSel.TabIndex = 13;
            this.btnMotherSel.Click += new System.EventHandler(this.btnMotherSel_Click);
            // 
            // btnNameCopy
            // 
            this.btnNameCopy.Location = new System.Drawing.Point(489, 542);
            this.btnNameCopy.Margin = new System.Windows.Forms.Padding(2, 2, 16, 2);
            this.btnNameCopy.Name = "btnNameCopy";
            this.btnNameCopy.Size = new System.Drawing.Size(37, 24);
            this.btnNameCopy.TabIndex = 4;
            this.btnNameCopy.Click += new System.EventHandler(this.btnNameCopy_Click);
            // 
            // tabsData
            // 
            this.tabsData.CloseableTabs = false;
            this.tabsData.Controls.Add(this.pageEvents);
            this.tabsData.Controls.Add(this.pageFamily);
            this.tabsData.Controls.Add(this.pageNames);
            this.tabsData.Controls.Add(this.pageNotes);
            this.tabsData.Controls.Add(this.pageMultimedia);
            this.tabsData.Controls.Add(this.pageSources);
            this.tabsData.Controls.Add(this.pageOther);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsData.Location = new System.Drawing.Point(2, 261);
            this.tabsData.Margin = new System.Windows.Forms.Padding(2);
            this.tabsData.Name = "tabsData";
            this.tabsData.SelectedIndex = 0;
            this.tabsData.Size = new System.Drawing.Size(724, 256);
            this.tabsData.TabIndex = 1;
            // 
            // pageEvents
            // 
            this.pageEvents.Location = new System.Drawing.Point(4, 22);
            this.pageEvents.Margin = new System.Windows.Forms.Padding(2);
            this.pageEvents.Name = "pageEvents";
            this.pageEvents.Size = new System.Drawing.Size(716, 230);
            this.pageEvents.TabIndex = 0;
            this.pageEvents.Text = "pageEvents";
            // 
            // pageFamily
            // 
            this.pageFamily.BackColor = System.Drawing.SystemColors.Control;
            this.pageFamily.Controls.Add(this.tabsFamily);
            this.pageFamily.Location = new System.Drawing.Point(4, 22);
            this.pageFamily.Name = "pageFamily";
            this.pageFamily.Padding = new System.Windows.Forms.Padding(3);
            this.pageFamily.Size = new System.Drawing.Size(720, 230);
            this.pageFamily.TabIndex = 9;
            this.pageFamily.Text = "pageFamily";
            // 
            // tabsFamily
            // 
            this.tabsFamily.CloseableTabs = false;
            this.tabsFamily.Controls.Add(this.pageSpouses);
            this.tabsFamily.Controls.Add(this.pageParents);
            this.tabsFamily.Controls.Add(this.pageChilds);
            this.tabsFamily.Controls.Add(this.pageAssociations);
            this.tabsFamily.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsFamily.Location = new System.Drawing.Point(3, 3);
            this.tabsFamily.Margin = new System.Windows.Forms.Padding(2);
            this.tabsFamily.Name = "tabsFamily";
            this.tabsFamily.SelectedIndex = 0;
            this.tabsFamily.Size = new System.Drawing.Size(714, 224);
            this.tabsFamily.TabIndex = 1;
            // 
            // pageSpouses
            // 
            this.pageSpouses.Location = new System.Drawing.Point(4, 22);
            this.pageSpouses.Margin = new System.Windows.Forms.Padding(2);
            this.pageSpouses.Name = "pageSpouses";
            this.pageSpouses.Size = new System.Drawing.Size(706, 198);
            this.pageSpouses.TabIndex = 1;
            this.pageSpouses.Text = "pageSpouses";
            // 
            // pageParents
            // 
            this.pageParents.Location = new System.Drawing.Point(4, 22);
            this.pageParents.Margin = new System.Windows.Forms.Padding(2);
            this.pageParents.Name = "pageParents";
            this.pageParents.Size = new System.Drawing.Size(178, 39);
            this.pageParents.TabIndex = 9;
            this.pageParents.Text = "pageParents";
            // 
            // pageChilds
            // 
            this.pageChilds.Location = new System.Drawing.Point(4, 22);
            this.pageChilds.Margin = new System.Windows.Forms.Padding(2);
            this.pageChilds.Name = "pageChilds";
            this.pageChilds.Size = new System.Drawing.Size(178, 39);
            this.pageChilds.TabIndex = 10;
            this.pageChilds.Text = "pageChilds";
            // 
            // pageAssociations
            // 
            this.pageAssociations.Location = new System.Drawing.Point(4, 22);
            this.pageAssociations.Margin = new System.Windows.Forms.Padding(2);
            this.pageAssociations.Name = "pageAssociations";
            this.pageAssociations.Size = new System.Drawing.Size(178, 39);
            this.pageAssociations.TabIndex = 2;
            this.pageAssociations.Text = "pageAssociations";
            // 
            // pageNames
            // 
            this.pageNames.BackColor = System.Drawing.SystemColors.Control;
            this.pageNames.Location = new System.Drawing.Point(4, 22);
            this.pageNames.Margin = new System.Windows.Forms.Padding(2);
            this.pageNames.Name = "pageNames";
            this.pageNames.Size = new System.Drawing.Size(720, 230);
            this.pageNames.TabIndex = 8;
            this.pageNames.Text = "pageNames";
            // 
            // pageNotes
            // 
            this.pageNotes.Location = new System.Drawing.Point(4, 22);
            this.pageNotes.Margin = new System.Windows.Forms.Padding(2);
            this.pageNotes.Name = "pageNotes";
            this.pageNotes.Size = new System.Drawing.Size(720, 230);
            this.pageNotes.TabIndex = 4;
            this.pageNotes.Text = "pageNotes";
            // 
            // pageMultimedia
            // 
            this.pageMultimedia.Location = new System.Drawing.Point(4, 22);
            this.pageMultimedia.Margin = new System.Windows.Forms.Padding(2);
            this.pageMultimedia.Name = "pageMultimedia";
            this.pageMultimedia.Size = new System.Drawing.Size(720, 230);
            this.pageMultimedia.TabIndex = 5;
            this.pageMultimedia.Text = "pageMultimedia";
            // 
            // pageSources
            // 
            this.pageSources.Location = new System.Drawing.Point(4, 22);
            this.pageSources.Margin = new System.Windows.Forms.Padding(2);
            this.pageSources.Name = "pageSources";
            this.pageSources.Size = new System.Drawing.Size(720, 230);
            this.pageSources.TabIndex = 6;
            this.pageSources.Text = "pageSources";
            // 
            // pageOther
            // 
            this.pageOther.BackColor = System.Drawing.SystemColors.Control;
            this.pageOther.Controls.Add(this.tabsOther);
            this.pageOther.Location = new System.Drawing.Point(4, 22);
            this.pageOther.Name = "pageOther";
            this.pageOther.Padding = new System.Windows.Forms.Padding(3);
            this.pageOther.Size = new System.Drawing.Size(720, 230);
            this.pageOther.TabIndex = 9;
            this.pageOther.Text = "pageOther";
            // 
            // tabsOther
            // 
            this.tabsOther.CloseableTabs = false;
            this.tabsOther.Controls.Add(this.pageUserRefs);
            this.tabsOther.Controls.Add(this.pageGroups);
            this.tabsOther.Controls.Add(this.pageDNATests);
            this.tabsOther.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsOther.Location = new System.Drawing.Point(3, 3);
            this.tabsOther.Margin = new System.Windows.Forms.Padding(2);
            this.tabsOther.Name = "tabsOther";
            this.tabsOther.SelectedIndex = 0;
            this.tabsOther.Size = new System.Drawing.Size(714, 224);
            this.tabsOther.TabIndex = 1;
            // 
            // pageUserRefs
            // 
            this.pageUserRefs.Location = new System.Drawing.Point(4, 22);
            this.pageUserRefs.Margin = new System.Windows.Forms.Padding(2);
            this.pageUserRefs.Name = "pageUserRefs";
            this.pageUserRefs.Size = new System.Drawing.Size(706, 198);
            this.pageUserRefs.TabIndex = 7;
            this.pageUserRefs.Text = "pageUserRefs";
            // 
            // pageGroups
            // 
            this.pageGroups.Location = new System.Drawing.Point(4, 22);
            this.pageGroups.Margin = new System.Windows.Forms.Padding(2);
            this.pageGroups.Name = "pageGroups";
            this.pageGroups.Size = new System.Drawing.Size(178, 39);
            this.pageGroups.TabIndex = 3;
            this.pageGroups.Text = "pageGroups";
            // 
            // pageDNATests
            // 
            this.pageDNATests.Location = new System.Drawing.Point(4, 22);
            this.pageDNATests.Margin = new System.Windows.Forms.Padding(2);
            this.pageDNATests.Name = "pageDNATests";
            this.pageDNATests.Size = new System.Drawing.Size(178, 39);
            this.pageDNATests.TabIndex = 11;
            this.pageDNATests.Text = "pageDNATests";
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 2;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 160F));
            this.tableLayoutPanel1.Controls.Add(this.panel1, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.tableLayoutPanel2, 0, 0);
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Margin = new System.Windows.Forms.Padding(0, 0, 0, 6);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 1;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(728, 253);
            this.tableLayoutPanel1.TabIndex = 22;
            // 
            // panel1
            // 
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.imgPortrait);
            this.panel1.Controls.Add(this.btnPortraitAdd);
            this.panel1.Controls.Add(this.btnPortraitDelete);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(568, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(0);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(8);
            this.panel1.Size = new System.Drawing.Size(160, 253);
            this.panel1.TabIndex = 0;
            // 
            // tableLayoutPanel2
            // 
            this.tableLayoutPanel2.ColumnCount = 1;
            this.tableLayoutPanel2.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.Controls.Add(this.panCtlParents, 0, 1);
            this.tableLayoutPanel2.Controls.Add(this.GroupBox1, 0, 0);
            this.tableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel2.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel2.Margin = new System.Windows.Forms.Padding(0, 0, 4, 0);
            this.tableLayoutPanel2.Name = "tableLayoutPanel2";
            this.tableLayoutPanel2.RowCount = 2;
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel2.Size = new System.Drawing.Size(564, 253);
            this.tableLayoutPanel2.TabIndex = 1;
            // 
            // tableLayoutPanel3
            // 
            this.tableLayoutPanel3.ColumnCount = 1;
            this.tableLayoutPanel3.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel3.Controls.Add(this.tableLayoutPanel1, 0, 0);
            this.tableLayoutPanel3.Controls.Add(this.tabsData, 0, 1);
            this.tableLayoutPanel3.Dock = System.Windows.Forms.DockStyle.Top;
            this.tableLayoutPanel3.Location = new System.Drawing.Point(8, 8);
            this.tableLayoutPanel3.Name = "tableLayoutPanel3";
            this.tableLayoutPanel3.RowCount = 2;
            this.tableLayoutPanel3.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel3.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel3.Size = new System.Drawing.Size(728, 519);
            this.tableLayoutPanel3.TabIndex = 23;
            // 
            // PersonEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(744, 577);
            this.Controls.Add(this.tableLayoutPanel3);
            this.Controls.Add(this.lblRestriction);
            this.Controls.Add(this.btnNameCopy);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.cmbRestriction);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "PersonEditDlg";
            this.Padding = new System.Windows.Forms.Padding(8);
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "PersonEditDlg";
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.panCtlParents.ResumeLayout(false);
            this.panCtlParents.PerformLayout();
            this.tabsData.ResumeLayout(false);
            this.pageFamily.ResumeLayout(false);
            this.tabsFamily.ResumeLayout(false);
            this.pageOther.ResumeLayout(false);
            this.tabsOther.ResumeLayout(false);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.tableLayoutPanel2.ResumeLayout(false);
            this.tableLayoutPanel3.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
