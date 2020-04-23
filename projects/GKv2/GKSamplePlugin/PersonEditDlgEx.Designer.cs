namespace GKSamplePlugin
{
    partial class PersonEditDlgEx
    {
        private System.Windows.Forms.TabControl tabsPersonData;
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
        private System.Windows.Forms.GroupBox GroupBox1;
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
        private System.Windows.Forms.Label lblSurnamePrefix;
        private System.Windows.Forms.TextBox txtSurnamePrefix;
        private System.Windows.Forms.Label lblNamePrefix;
        private System.Windows.Forms.TextBox txtNamePrefix;
        private System.Windows.Forms.Label lblNameSuffix;
        private System.Windows.Forms.TextBox txtNameSuffix;
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
        private System.Windows.Forms.TabPage tabSimple;
        
        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lblRestriction = new System.Windows.Forms.Label();
            this.cmbRestriction = new System.Windows.Forms.ComboBox();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.imgPortrait = new GKUI.Components.GKPortrait();
            this.lblMarriedSurname = new System.Windows.Forms.Label();
            this.lblSurname = new System.Windows.Forms.Label();
            this.lblName = new System.Windows.Forms.Label();
            this.lblPatronymic = new System.Windows.Forms.Label();
            this.lblSex = new System.Windows.Forms.Label();
            this.lblSurnamePrefix = new System.Windows.Forms.Label();
            this.lblNamePrefix = new System.Windows.Forms.Label();
            this.lblNameSuffix = new System.Windows.Forms.Label();
            this.lblNickname = new System.Windows.Forms.Label();
            this.btnPortraitAdd = new System.Windows.Forms.Button();
            this.btnPortraitDelete = new System.Windows.Forms.Button();
            this.txtMarriedSurname = new System.Windows.Forms.TextBox();
            this.txtSurname = new System.Windows.Forms.TextBox();
            this.txtName = new System.Windows.Forms.TextBox();
            this.cmbPatronymic = new System.Windows.Forms.ComboBox();
            this.cmbSex = new GKUI.Components.GKComboBox();
            this.chkPatriarch = new System.Windows.Forms.CheckBox();
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
            this.chkBookmark = new System.Windows.Forms.CheckBox();
            this.txtSurnamePrefix = new System.Windows.Forms.TextBox();
            this.txtNamePrefix = new System.Windows.Forms.TextBox();
            this.txtNameSuffix = new System.Windows.Forms.TextBox();
            this.txtNickname = new System.Windows.Forms.TextBox();
            this.btnNameCopy = new System.Windows.Forms.Button();
            this.tabsPersonData = new System.Windows.Forms.TabControl();
            this.pageEvents = new System.Windows.Forms.TabPage();
            this.pageSpouses = new System.Windows.Forms.TabPage();
            this.pageNames = new System.Windows.Forms.TabPage();
            this.pageAssociations = new System.Windows.Forms.TabPage();
            this.pageGroups = new System.Windows.Forms.TabPage();
            this.pageNotes = new System.Windows.Forms.TabPage();
            this.pageMultimedia = new System.Windows.Forms.TabPage();
            this.pageSources = new System.Windows.Forms.TabPage();
            this.pageUserRefs = new System.Windows.Forms.TabPage();
            this.pageParents = new System.Windows.Forms.TabPage();
            this.tabSimple = new System.Windows.Forms.TabPage();
            this.GroupBox1.SuspendLayout();
            this.panCtlParents.SuspendLayout();
            this.tabsPersonData.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(628, 661);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(112, 30);
            this.btnAccept.TabIndex = 5;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(750, 661);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(112, 30);
            this.btnCancel.TabIndex = 6;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            // 
            // lblRestriction
            // 
            this.lblRestriction.AutoSize = true;
            this.lblRestriction.Location = new System.Drawing.Point(11, 664);
            this.lblRestriction.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblRestriction.Name = "lblRestriction";
            this.lblRestriction.Size = new System.Drawing.Size(84, 17);
            this.lblRestriction.TabIndex = 2;
            this.lblRestriction.Text = "lblRestriction";
            // 
            // cmbRestriction
            // 
            this.cmbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRestriction.Location = new System.Drawing.Point(224, 661);
            this.cmbRestriction.Margin = new System.Windows.Forms.Padding(2);
            this.cmbRestriction.Name = "cmbRestriction";
            this.cmbRestriction.Size = new System.Drawing.Size(203, 25);
            this.cmbRestriction.TabIndex = 3;
            this.cmbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.imgPortrait);
            this.GroupBox1.Controls.Add(this.lblMarriedSurname);
            this.GroupBox1.Controls.Add(this.lblSurname);
            this.GroupBox1.Controls.Add(this.lblName);
            this.GroupBox1.Controls.Add(this.lblPatronymic);
            this.GroupBox1.Controls.Add(this.lblSex);
            this.GroupBox1.Controls.Add(this.lblSurnamePrefix);
            this.GroupBox1.Controls.Add(this.lblNamePrefix);
            this.GroupBox1.Controls.Add(this.lblNameSuffix);
            this.GroupBox1.Controls.Add(this.lblNickname);
            this.GroupBox1.Controls.Add(this.btnPortraitAdd);
            this.GroupBox1.Controls.Add(this.btnPortraitDelete);
            this.GroupBox1.Controls.Add(this.txtMarriedSurname);
            this.GroupBox1.Controls.Add(this.txtSurname);
            this.GroupBox1.Controls.Add(this.txtName);
            this.GroupBox1.Controls.Add(this.cmbPatronymic);
            this.GroupBox1.Controls.Add(this.cmbSex);
            this.GroupBox1.Controls.Add(this.chkPatriarch);
            this.GroupBox1.Controls.Add(this.panCtlParents);
            this.GroupBox1.Controls.Add(this.chkBookmark);
            this.GroupBox1.Controls.Add(this.txtSurnamePrefix);
            this.GroupBox1.Controls.Add(this.txtNamePrefix);
            this.GroupBox1.Controls.Add(this.txtNameSuffix);
            this.GroupBox1.Controls.Add(this.txtNickname);
            this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
            this.GroupBox1.Location = new System.Drawing.Point(0, 0);
            this.GroupBox1.Margin = new System.Windows.Forms.Padding(2);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Padding = new System.Windows.Forms.Padding(2);
            this.GroupBox1.Size = new System.Drawing.Size(874, 322);
            this.GroupBox1.TabIndex = 0;
            this.GroupBox1.TabStop = false;
            // 
            // imgPortrait
            // 
            this.imgPortrait.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.imgPortrait.Image = null;
            this.imgPortrait.Location = new System.Drawing.Point(671, 15);
            this.imgPortrait.Margin = new System.Windows.Forms.Padding(5, 5, 5, 5);
            this.imgPortrait.Name = "imgPortrait";
            this.imgPortrait.PixelSpeed = 5;
            this.imgPortrait.Size = new System.Drawing.Size(186, 206);
            this.imgPortrait.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Normal;
            this.imgPortrait.SlidePanelHeight = 38;
            this.imgPortrait.TabIndex = 22;
            // 
            // lblMarriedSurname
            // 
            this.lblMarriedSurname.AutoSize = true;
            this.lblMarriedSurname.Location = new System.Drawing.Point(11, 70);
            this.lblMarriedSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMarriedSurname.Name = "lblMarriedSurname";
            this.lblMarriedSurname.Size = new System.Drawing.Size(119, 17);
            this.lblMarriedSurname.TabIndex = 2;
            this.lblMarriedSurname.Text = "lblMarriedSurname";
            // 
            // lblSurname
            // 
            this.lblSurname.AutoSize = true;
            this.lblSurname.Location = new System.Drawing.Point(11, 19);
            this.lblSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSurname.Name = "lblSurname";
            this.lblSurname.Size = new System.Drawing.Size(75, 17);
            this.lblSurname.TabIndex = 0;
            this.lblSurname.Text = "lblSurname";
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(11, 118);
            this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(55, 17);
            this.lblName.TabIndex = 4;
            this.lblName.Text = "lblName";
            // 
            // lblPatronymic
            // 
            this.lblPatronymic.AutoSize = true;
            this.lblPatronymic.Location = new System.Drawing.Point(11, 165);
            this.lblPatronymic.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblPatronymic.Name = "lblPatronymic";
            this.lblPatronymic.Size = new System.Drawing.Size(90, 17);
            this.lblPatronymic.TabIndex = 6;
            this.lblPatronymic.Text = "lblPatronymic";
            // 
            // lblSex
            // 
            this.lblSex.AutoSize = true;
            this.lblSex.Location = new System.Drawing.Point(450, 118);
            this.lblSex.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSex.Name = "lblSex";
            this.lblSex.Size = new System.Drawing.Size(43, 17);
            this.lblSex.TabIndex = 16;
            this.lblSex.Text = "lblSex";
            // 
            // lblSurnamePrefix
            // 
            this.lblSurnamePrefix.AutoSize = true;
            this.lblSurnamePrefix.Location = new System.Drawing.Point(259, 19);
            this.lblSurnamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSurnamePrefix.Name = "lblSurnamePrefix";
            this.lblSurnamePrefix.Size = new System.Drawing.Size(109, 17);
            this.lblSurnamePrefix.TabIndex = 8;
            this.lblSurnamePrefix.Text = "lblSurnamePrefix";
            // 
            // lblNamePrefix
            // 
            this.lblNamePrefix.AutoSize = true;
            this.lblNamePrefix.Location = new System.Drawing.Point(259, 68);
            this.lblNamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNamePrefix.Name = "lblNamePrefix";
            this.lblNamePrefix.Size = new System.Drawing.Size(89, 17);
            this.lblNamePrefix.TabIndex = 10;
            this.lblNamePrefix.Text = "lblNamePrefix";
            // 
            // lblNameSuffix
            // 
            this.lblNameSuffix.AutoSize = true;
            this.lblNameSuffix.Location = new System.Drawing.Point(259, 118);
            this.lblNameSuffix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNameSuffix.Name = "lblNameSuffix";
            this.lblNameSuffix.Size = new System.Drawing.Size(89, 17);
            this.lblNameSuffix.TabIndex = 12;
            this.lblNameSuffix.Text = "lblNameSuffix";
            // 
            // lblNickname
            // 
            this.lblNickname.AutoSize = true;
            this.lblNickname.Location = new System.Drawing.Point(259, 165);
            this.lblNickname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNickname.Name = "lblNickname";
            this.lblNickname.Size = new System.Drawing.Size(79, 17);
            this.lblNickname.TabIndex = 14;
            this.lblNickname.Text = "lblNickname";
            // 
            // btnPortraitAdd
            // 
            this.btnPortraitAdd.Location = new System.Drawing.Point(772, 185);
            this.btnPortraitAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnPortraitAdd.Name = "btnPortraitAdd";
            this.btnPortraitAdd.Size = new System.Drawing.Size(36, 36);
            this.btnPortraitAdd.TabIndex = 19;
            this.btnPortraitAdd.TabStop = false;
            this.btnPortraitAdd.Click += new System.EventHandler(this.btnPortraitAdd_Click);
            // 
            // btnPortraitDelete
            // 
            this.btnPortraitDelete.Location = new System.Drawing.Point(818, 185);
            this.btnPortraitDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnPortraitDelete.Name = "btnPortraitDelete";
            this.btnPortraitDelete.Size = new System.Drawing.Size(36, 36);
            this.btnPortraitDelete.TabIndex = 20;
            this.btnPortraitDelete.TabStop = false;
            this.btnPortraitDelete.Click += new System.EventHandler(this.btnPortraitDelete_Click);
            // 
            // txtMarriedSurname
            // 
            this.txtMarriedSurname.Location = new System.Drawing.Point(11, 88);
            this.txtMarriedSurname.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtMarriedSurname.Name = "txtMarriedSurname";
            this.txtMarriedSurname.Size = new System.Drawing.Size(226, 24);
            this.txtMarriedSurname.TabIndex = 3;
            this.txtMarriedSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
            this.txtMarriedSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            // 
            // txtSurname
            // 
            this.txtSurname.Location = new System.Drawing.Point(11, 39);
            this.txtSurname.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtSurname.Name = "txtSurname";
            this.txtSurname.Size = new System.Drawing.Size(226, 24);
            this.txtSurname.TabIndex = 1;
            this.txtSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
            this.txtSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(11, 135);
            this.txtName.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(226, 24);
            this.txtName.TabIndex = 5;
            this.txtName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
            this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            // 
            // cmbPatronymic
            // 
            this.cmbPatronymic.Location = new System.Drawing.Point(11, 184);
            this.cmbPatronymic.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.cmbPatronymic.Name = "cmbPatronymic";
            this.cmbPatronymic.Size = new System.Drawing.Size(226, 25);
            this.cmbPatronymic.TabIndex = 7;
            this.cmbPatronymic.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
            this.cmbPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
            // 
            // cmbSex
            // 
            this.cmbSex.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.cmbSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSex.Location = new System.Drawing.Point(450, 135);
            this.cmbSex.Margin = new System.Windows.Forms.Padding(2);
            this.cmbSex.Name = "cmbSex";
            this.cmbSex.Size = new System.Drawing.Size(192, 25);
            this.cmbSex.TabIndex = 17;
            this.cmbSex.SelectedIndexChanged += new System.EventHandler(this.cbSex_SelectedIndexChanged);
            // 
            // chkPatriarch
            // 
            this.chkPatriarch.AutoSize = true;
            this.chkPatriarch.Location = new System.Drawing.Point(448, 175);
            this.chkPatriarch.Margin = new System.Windows.Forms.Padding(2);
            this.chkPatriarch.Name = "chkPatriarch";
            this.chkPatriarch.Size = new System.Drawing.Size(106, 21);
            this.chkPatriarch.TabIndex = 18;
            this.chkPatriarch.Text = "chkPatriarch";
            // 
            // panCtlParents
            // 
            this.panCtlParents.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
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
            this.panCtlParents.Location = new System.Drawing.Point(2, 232);
            this.panCtlParents.Margin = new System.Windows.Forms.Padding(2);
            this.panCtlParents.Name = "panCtlParents";
            this.panCtlParents.Size = new System.Drawing.Size(870, 86);
            this.panCtlParents.TabIndex = 21;
            // 
            // txtMother
            // 
            this.txtMother.ForeColor = System.Drawing.SystemColors.Control;
            this.txtMother.Location = new System.Drawing.Point(414, 10);
            this.txtMother.Margin = new System.Windows.Forms.Padding(2);
            this.txtMother.Name = "txtMother";
            this.txtMother.ReadOnly = true;
            this.txtMother.Size = new System.Drawing.Size(314, 24);
            this.txtMother.TabIndex = 2;
            // 
            // lblParents
            // 
            this.lblParents.AutoSize = true;
            this.lblParents.Location = new System.Drawing.Point(12, 14);
            this.lblParents.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblParents.Name = "lblParents";
            this.lblParents.Size = new System.Drawing.Size(66, 17);
            this.lblParents.TabIndex = 0;
            this.lblParents.Text = "lblParents";
            // 
            // btnParentsAdd
            // 
            this.btnParentsAdd.Location = new System.Drawing.Point(735, 6);
            this.btnParentsAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsAdd.Name = "btnParentsAdd";
            this.btnParentsAdd.Size = new System.Drawing.Size(36, 36);
            this.btnParentsAdd.TabIndex = 3;
            this.btnParentsAdd.Click += new System.EventHandler(this.btnParentsAdd_Click);
            // 
            // btnParentsEdit
            // 
            this.btnParentsEdit.Location = new System.Drawing.Point(776, 6);
            this.btnParentsEdit.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsEdit.Name = "btnParentsEdit";
            this.btnParentsEdit.Size = new System.Drawing.Size(36, 36);
            this.btnParentsEdit.TabIndex = 4;
            this.btnParentsEdit.Click += new System.EventHandler(this.btnParentsEdit_Click);
            // 
            // btnParentsDelete
            // 
            this.btnParentsDelete.Location = new System.Drawing.Point(816, 6);
            this.btnParentsDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsDelete.Name = "btnParentsDelete";
            this.btnParentsDelete.Size = new System.Drawing.Size(38, 36);
            this.btnParentsDelete.TabIndex = 5;
            this.btnParentsDelete.Click += new System.EventHandler(this.btnParentsDelete_Click);
            // 
            // btnFatherAdd
            // 
            this.btnFatherAdd.Location = new System.Drawing.Point(278, 42);
            this.btnFatherAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherAdd.Name = "btnFatherAdd";
            this.btnFatherAdd.Size = new System.Drawing.Size(36, 36);
            this.btnFatherAdd.TabIndex = 6;
            this.btnFatherAdd.Click += new System.EventHandler(this.btnFatherAdd_Click);
            // 
            // btnFatherDelete
            // 
            this.btnFatherDelete.Location = new System.Drawing.Point(322, 42);
            this.btnFatherDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherDelete.Name = "btnFatherDelete";
            this.btnFatherDelete.Size = new System.Drawing.Size(36, 36);
            this.btnFatherDelete.TabIndex = 7;
            this.btnFatherDelete.Click += new System.EventHandler(this.btnFatherDelete_Click);
            // 
            // btnFatherSel
            // 
            this.btnFatherSel.Location = new System.Drawing.Point(368, 42);
            this.btnFatherSel.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherSel.Name = "btnFatherSel";
            this.btnFatherSel.Size = new System.Drawing.Size(36, 36);
            this.btnFatherSel.TabIndex = 8;
            this.btnFatherSel.Click += new System.EventHandler(this.btnFatherSel_Click);
            // 
            // btnMotherAdd
            // 
            this.btnMotherAdd.Location = new System.Drawing.Point(602, 42);
            this.btnMotherAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherAdd.Name = "btnMotherAdd";
            this.btnMotherAdd.Size = new System.Drawing.Size(36, 36);
            this.btnMotherAdd.TabIndex = 9;
            this.btnMotherAdd.Click += new System.EventHandler(this.btnMotherAdd_Click);
            // 
            // btnMotherDelete
            // 
            this.btnMotherDelete.Location = new System.Drawing.Point(648, 42);
            this.btnMotherDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherDelete.Name = "btnMotherDelete";
            this.btnMotherDelete.Size = new System.Drawing.Size(36, 36);
            this.btnMotherDelete.TabIndex = 10;
            this.btnMotherDelete.Click += new System.EventHandler(this.btnMotherDelete_Click);
            // 
            // btnMotherSel
            // 
            this.btnMotherSel.Location = new System.Drawing.Point(692, 42);
            this.btnMotherSel.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherSel.Name = "btnMotherSel";
            this.btnMotherSel.Size = new System.Drawing.Size(36, 36);
            this.btnMotherSel.TabIndex = 11;
            this.btnMotherSel.Click += new System.EventHandler(this.btnMotherSel_Click);
            // 
            // txtFather
            // 
            this.txtFather.ForeColor = System.Drawing.SystemColors.Control;
            this.txtFather.Location = new System.Drawing.Point(90, 10);
            this.txtFather.Margin = new System.Windows.Forms.Padding(2);
            this.txtFather.Name = "txtFather";
            this.txtFather.ReadOnly = true;
            this.txtFather.Size = new System.Drawing.Size(313, 24);
            this.txtFather.TabIndex = 1;
            // 
            // chkBookmark
            // 
            this.chkBookmark.AutoSize = true;
            this.chkBookmark.Location = new System.Drawing.Point(448, 196);
            this.chkBookmark.Margin = new System.Windows.Forms.Padding(2);
            this.chkBookmark.Name = "chkBookmark";
            this.chkBookmark.Size = new System.Drawing.Size(114, 21);
            this.chkBookmark.TabIndex = 19;
            this.chkBookmark.Text = "chkBookmark";
            // 
            // txtSurnamePrefix
            // 
            this.txtSurnamePrefix.Location = new System.Drawing.Point(259, 39);
            this.txtSurnamePrefix.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtSurnamePrefix.Name = "txtSurnamePrefix";
            this.txtSurnamePrefix.Size = new System.Drawing.Size(169, 24);
            this.txtSurnamePrefix.TabIndex = 9;
            // 
            // txtNamePrefix
            // 
            this.txtNamePrefix.Location = new System.Drawing.Point(259, 88);
            this.txtNamePrefix.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtNamePrefix.Name = "txtNamePrefix";
            this.txtNamePrefix.Size = new System.Drawing.Size(169, 24);
            this.txtNamePrefix.TabIndex = 11;
            // 
            // txtNameSuffix
            // 
            this.txtNameSuffix.Location = new System.Drawing.Point(259, 135);
            this.txtNameSuffix.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtNameSuffix.Name = "txtNameSuffix";
            this.txtNameSuffix.Size = new System.Drawing.Size(169, 24);
            this.txtNameSuffix.TabIndex = 13;
            // 
            // txtNickname
            // 
            this.txtNickname.Location = new System.Drawing.Point(259, 184);
            this.txtNickname.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtNickname.Name = "txtNickname";
            this.txtNickname.Size = new System.Drawing.Size(169, 24);
            this.txtNickname.TabIndex = 15;
            // 
            // btnNameCopy
            // 
            this.btnNameCopy.Location = new System.Drawing.Point(538, 661);
            this.btnNameCopy.Margin = new System.Windows.Forms.Padding(2);
            this.btnNameCopy.Name = "btnNameCopy";
            this.btnNameCopy.Size = new System.Drawing.Size(46, 30);
            this.btnNameCopy.TabIndex = 4;
            this.btnNameCopy.Click += new System.EventHandler(this.btnNameCopy_Click);
            // 
            // tabsPersonData
            // 
            this.tabsPersonData.Controls.Add(this.pageEvents);
            this.tabsPersonData.Controls.Add(this.pageSpouses);
            this.tabsPersonData.Controls.Add(this.pageNames);
            this.tabsPersonData.Controls.Add(this.pageAssociations);
            this.tabsPersonData.Controls.Add(this.pageGroups);
            this.tabsPersonData.Controls.Add(this.pageNotes);
            this.tabsPersonData.Controls.Add(this.pageMultimedia);
            this.tabsPersonData.Controls.Add(this.pageSources);
            this.tabsPersonData.Controls.Add(this.pageUserRefs);
            this.tabsPersonData.Controls.Add(this.pageParents);
            this.tabsPersonData.Controls.Add(this.tabSimple);
            this.tabsPersonData.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabsPersonData.Location = new System.Drawing.Point(0, 322);
            this.tabsPersonData.Margin = new System.Windows.Forms.Padding(2);
            this.tabsPersonData.Name = "tabsPersonData";
            this.tabsPersonData.SelectedIndex = 0;
            this.tabsPersonData.Size = new System.Drawing.Size(874, 320);
            this.tabsPersonData.TabIndex = 1;
            // 
            // pageEvents
            // 
            this.pageEvents.Location = new System.Drawing.Point(4, 26);
            this.pageEvents.Margin = new System.Windows.Forms.Padding(2);
            this.pageEvents.Name = "pageEvents";
            this.pageEvents.Size = new System.Drawing.Size(866, 290);
            this.pageEvents.TabIndex = 0;
            this.pageEvents.Text = "pageEvents";
            // 
            // pageSpouses
            // 
            this.pageSpouses.Location = new System.Drawing.Point(4, 26);
            this.pageSpouses.Margin = new System.Windows.Forms.Padding(2);
            this.pageSpouses.Name = "pageSpouses";
            this.pageSpouses.Size = new System.Drawing.Size(866, 290);
            this.pageSpouses.TabIndex = 1;
            this.pageSpouses.Text = "pageSpouses";
            // 
            // pageNames
            // 
            this.pageNames.BackColor = System.Drawing.SystemColors.Control;
            this.pageNames.Location = new System.Drawing.Point(4, 26);
            this.pageNames.Margin = new System.Windows.Forms.Padding(2);
            this.pageNames.Name = "pageNames";
            this.pageNames.Size = new System.Drawing.Size(866, 290);
            this.pageNames.TabIndex = 8;
            this.pageNames.Text = "pageNames";
            // 
            // pageAssociations
            // 
            this.pageAssociations.Location = new System.Drawing.Point(4, 26);
            this.pageAssociations.Margin = new System.Windows.Forms.Padding(2);
            this.pageAssociations.Name = "pageAssociations";
            this.pageAssociations.Size = new System.Drawing.Size(866, 290);
            this.pageAssociations.TabIndex = 2;
            this.pageAssociations.Text = "pageAssociations";
            // 
            // pageGroups
            // 
            this.pageGroups.Location = new System.Drawing.Point(4, 26);
            this.pageGroups.Margin = new System.Windows.Forms.Padding(2);
            this.pageGroups.Name = "pageGroups";
            this.pageGroups.Size = new System.Drawing.Size(866, 290);
            this.pageGroups.TabIndex = 3;
            this.pageGroups.Text = "pageGroups";
            // 
            // pageNotes
            // 
            this.pageNotes.Location = new System.Drawing.Point(4, 26);
            this.pageNotes.Margin = new System.Windows.Forms.Padding(2);
            this.pageNotes.Name = "pageNotes";
            this.pageNotes.Size = new System.Drawing.Size(866, 290);
            this.pageNotes.TabIndex = 4;
            this.pageNotes.Text = "pageNotes";
            // 
            // pageMultimedia
            // 
            this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
            this.pageMultimedia.Margin = new System.Windows.Forms.Padding(2);
            this.pageMultimedia.Name = "pageMultimedia";
            this.pageMultimedia.Size = new System.Drawing.Size(866, 290);
            this.pageMultimedia.TabIndex = 5;
            this.pageMultimedia.Text = "pageMultimedia";
            // 
            // pageSources
            // 
            this.pageSources.Location = new System.Drawing.Point(4, 26);
            this.pageSources.Margin = new System.Windows.Forms.Padding(2);
            this.pageSources.Name = "pageSources";
            this.pageSources.Size = new System.Drawing.Size(866, 290);
            this.pageSources.TabIndex = 6;
            this.pageSources.Text = "pageSources";
            // 
            // pageUserRefs
            // 
            this.pageUserRefs.Location = new System.Drawing.Point(4, 26);
            this.pageUserRefs.Margin = new System.Windows.Forms.Padding(2);
            this.pageUserRefs.Name = "pageUserRefs";
            this.pageUserRefs.Size = new System.Drawing.Size(866, 290);
            this.pageUserRefs.TabIndex = 7;
            this.pageUserRefs.Text = "pageUserRefs";
            // 
            // pageParents
            // 
            this.pageParents.BackColor = System.Drawing.SystemColors.Control;
            this.pageParents.Location = new System.Drawing.Point(4, 26);
            this.pageParents.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.pageParents.Name = "pageParents";
            this.pageParents.Padding = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.pageParents.Size = new System.Drawing.Size(866, 290);
            this.pageParents.TabIndex = 9;
            this.pageParents.Text = "pageParents";
            // 
            // tabSimple
            // 
            this.tabSimple.BackColor = System.Drawing.SystemColors.Control;
            this.tabSimple.Location = new System.Drawing.Point(4, 26);
            this.tabSimple.Name = "tabSimple";
            this.tabSimple.Padding = new System.Windows.Forms.Padding(3);
            this.tabSimple.Size = new System.Drawing.Size(866, 290);
            this.tabSimple.TabIndex = 10;
            this.tabSimple.Text = "tabSimple";
            // 
            // PersonEditDlgEx
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.Title = "PersonEditDlg";
            this.ClientSize = new System.Drawing.Size(874, 704);
            this.Controls.Add(this.tabsPersonData);
            this.Controls.Add(this.lblRestriction);
            this.Controls.Add(this.btnNameCopy);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.cmbRestriction);
            this.Controls.Add(this.GroupBox1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "PersonEditDlgEx";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "PersonEditDlg";
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.panCtlParents.ResumeLayout(false);
            this.panCtlParents.PerformLayout();
            this.tabsPersonData.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }
    }
}
