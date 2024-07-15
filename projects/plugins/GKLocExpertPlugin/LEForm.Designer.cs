namespace GKLocExpertPlugin
{
    partial class LEForm
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.GroupBox grpTestLoc;
        private GKUI.Components.GKDateControl dtlPlaceDate;
        private System.Windows.Forms.Label lblDate;
        private System.Windows.Forms.Button btnAnalysis;
        private System.Windows.Forms.CheckBox chkReverseOrder;
        private System.Windows.Forms.TextBox txtPlace;
        private System.Windows.Forms.Label lblPlace;
        private System.Windows.Forms.Panel panel1;
        private GKUI.Components.GKListView lvEntries;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripLabel lblLocName;
        private System.Windows.Forms.ToolStripComboBox cmbLocationSearch;
        private System.Windows.Forms.ToolStripButton btnLocNameAdd;
        private System.Windows.Forms.ToolStripButton btnLocNameEdit;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripLabel lblTopLink;
        private System.Windows.Forms.ToolStripButton btnTopLinkAdd;
        private System.Windows.Forms.ToolStripButton btnTopLinkEdit;
        private System.Windows.Forms.TextBox txtGeneratedName;
        private System.Windows.Forms.Label lblGeneratedName;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.grpTestLoc = new System.Windows.Forms.GroupBox();
            this.txtGeneratedName = new System.Windows.Forms.TextBox();
            this.lblGeneratedName = new System.Windows.Forms.Label();
            this.btnAnalysis = new System.Windows.Forms.Button();
            this.chkReverseOrder = new System.Windows.Forms.CheckBox();
            this.txtPlace = new System.Windows.Forms.TextBox();
            this.lblPlace = new System.Windows.Forms.Label();
            this.dtlPlaceDate = new GKUI.Components.GKDateControl();
            this.lblDate = new System.Windows.Forms.Label();
            this.panel1 = new System.Windows.Forms.Panel();
            this.lvEntries = new GKUI.Components.GKListView();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.lblLocName = new System.Windows.Forms.ToolStripLabel();
            this.cmbLocationSearch = new System.Windows.Forms.ToolStripComboBox();
            this.btnLocNameAdd = new System.Windows.Forms.ToolStripButton();
            this.btnLocNameEdit = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.lblTopLink = new System.Windows.Forms.ToolStripLabel();
            this.btnTopLinkAdd = new System.Windows.Forms.ToolStripButton();
            this.btnTopLinkEdit = new System.Windows.Forms.ToolStripButton();
            this.grpTestLoc.SuspendLayout();
            this.panel1.SuspendLayout();
            this.toolStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // grpTestLoc
            // 
            this.grpTestLoc.Controls.Add(this.txtGeneratedName);
            this.grpTestLoc.Controls.Add(this.lblGeneratedName);
            this.grpTestLoc.Controls.Add(this.btnAnalysis);
            this.grpTestLoc.Controls.Add(this.chkReverseOrder);
            this.grpTestLoc.Controls.Add(this.txtPlace);
            this.grpTestLoc.Controls.Add(this.lblPlace);
            this.grpTestLoc.Controls.Add(this.dtlPlaceDate);
            this.grpTestLoc.Controls.Add(this.lblDate);
            this.grpTestLoc.Location = new System.Drawing.Point(14, 12);
            this.grpTestLoc.Name = "grpTestLoc";
            this.grpTestLoc.Size = new System.Drawing.Size(804, 209);
            this.grpTestLoc.TabIndex = 2;
            this.grpTestLoc.TabStop = false;
            // 
            // txtGeneratedName
            // 
            this.txtGeneratedName.Location = new System.Drawing.Point(198, 171);
            this.txtGeneratedName.Name = "txtGeneratedName";
            this.txtGeneratedName.ReadOnly = true;
            this.txtGeneratedName.Size = new System.Drawing.Size(600, 23);
            this.txtGeneratedName.TabIndex = 8;
            // 
            // lblGeneratedName
            // 
            this.lblGeneratedName.AutoSize = true;
            this.lblGeneratedName.Location = new System.Drawing.Point(7, 174);
            this.lblGeneratedName.Name = "lblGeneratedName";
            this.lblGeneratedName.Size = new System.Drawing.Size(106, 15);
            this.lblGeneratedName.TabIndex = 7;
            this.lblGeneratedName.Text = "lblGeneratedName";
            // 
            // btnAnalysis
            // 
            this.btnAnalysis.Location = new System.Drawing.Point(596, 22);
            this.btnAnalysis.Name = "btnAnalysis";
            this.btnAnalysis.Size = new System.Drawing.Size(202, 27);
            this.btnAnalysis.TabIndex = 6;
            this.btnAnalysis.Text = "btnAnalysis";
            this.btnAnalysis.UseVisualStyleBackColor = true;
            this.btnAnalysis.Click += new System.EventHandler(this.btnAnalysis_Click);
            // 
            // chkReverseOrder
            // 
            this.chkReverseOrder.AutoSize = true;
            this.chkReverseOrder.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.chkReverseOrder.Location = new System.Drawing.Point(7, 132);
            this.chkReverseOrder.Name = "chkReverseOrder";
            this.chkReverseOrder.Size = new System.Drawing.Size(115, 19);
            this.chkReverseOrder.TabIndex = 5;
            this.chkReverseOrder.Text = "chkReverseOrder";
            this.chkReverseOrder.UseVisualStyleBackColor = true;
            // 
            // txtPlace
            // 
            this.txtPlace.Location = new System.Drawing.Point(82, 102);
            this.txtPlace.Name = "txtPlace";
            this.txtPlace.Size = new System.Drawing.Size(498, 23);
            this.txtPlace.TabIndex = 4;
            // 
            // lblPlace
            // 
            this.lblPlace.AutoSize = true;
            this.lblPlace.Location = new System.Drawing.Point(7, 105);
            this.lblPlace.Name = "lblPlace";
            this.lblPlace.Size = new System.Drawing.Size(48, 15);
            this.lblPlace.TabIndex = 3;
            this.lblPlace.Text = "lblPlace";
            // 
            // dtlPlaceDate
            // 
            this.dtlPlaceDate.FixedDateType = GDModel.GDMDateType.None;
            this.dtlPlaceDate.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.dtlPlaceDate.Location = new System.Drawing.Point(82, 21);
            this.dtlPlaceDate.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.dtlPlaceDate.Name = "dtlPlaceDate";
            this.dtlPlaceDate.Size = new System.Drawing.Size(441, 58);
            this.dtlPlaceDate.TabIndex = 2;
            this.dtlPlaceDate.DateChanged += new System.EventHandler(this.dtlPlaceDate_DateChanged);
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(7, 28);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(44, 15);
            this.lblDate.TabIndex = 0;
            this.lblDate.Text = "lblDate";
            // 
            // panel1
            // 
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panel1.Controls.Add(this.lvEntries);
            this.panel1.Controls.Add(this.toolStrip1);
            this.panel1.Location = new System.Drawing.Point(14, 227);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(804, 281);
            this.panel1.TabIndex = 4;
            // 
            // lvEntries
            // 
            this.lvEntries.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lvEntries.FullRowSelect = true;
            this.lvEntries.HideSelection = false;
            this.lvEntries.ListMan = null;
            this.lvEntries.Location = new System.Drawing.Point(0, 0);
            this.lvEntries.Name = "lvEntries";
            this.lvEntries.OwnerDraw = true;
            this.lvEntries.SelectedIndex = -1;
            this.lvEntries.Size = new System.Drawing.Size(800, 252);
            this.lvEntries.SortColumn = 0;
            this.lvEntries.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            this.lvEntries.TabIndex = 6;
            this.lvEntries.UseCompatibleStateImageBehavior = false;
            this.lvEntries.View = System.Windows.Forms.View.Details;
            this.lvEntries.SelectedIndexChanged += new System.EventHandler(this.lvEntries_SelectedIndexChanged);
            // 
            // toolStrip1
            // 
            this.toolStrip1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lblLocName,
            this.cmbLocationSearch,
            this.btnLocNameAdd,
            this.btnLocNameEdit,
            this.toolStripSeparator1,
            this.toolStripSeparator2,
            this.lblTopLink,
            this.btnTopLinkAdd,
            this.btnTopLinkEdit});
            this.toolStrip1.Location = new System.Drawing.Point(0, 252);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(800, 25);
            this.toolStrip1.TabIndex = 5;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // lblLocName
            // 
            this.lblLocName.Name = "lblLocName";
            this.lblLocName.Size = new System.Drawing.Size(71, 22);
            this.lblLocName.Text = "lblLocName";
            // 
            // cmbLocationSearch
            // 
            this.cmbLocationSearch.Name = "cmbLocationSearch";
            this.cmbLocationSearch.Size = new System.Drawing.Size(260, 25);
            this.cmbLocationSearch.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cmbLocationSearch_KeyUp);
            // 
            // btnLocNameAdd
            // 
            this.btnLocNameAdd.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnLocNameAdd.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnLocNameAdd.Name = "btnLocNameAdd";
            this.btnLocNameAdd.Size = new System.Drawing.Size(102, 22);
            this.btnLocNameAdd.Text = "btnLocNameAdd";
            this.btnLocNameAdd.Click += new System.EventHandler(this.btnLocNameAdd_Click);
            // 
            // btnLocNameEdit
            // 
            this.btnLocNameEdit.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnLocNameEdit.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnLocNameEdit.Name = "btnLocNameEdit";
            this.btnLocNameEdit.Size = new System.Drawing.Size(100, 22);
            this.btnLocNameEdit.Text = "btnLocNameEdit";
            this.btnLocNameEdit.Click += new System.EventHandler(this.btnLocNameEdit_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
            // 
            // lblTopLink
            // 
            this.lblTopLink.Name = "lblTopLink";
            this.lblTopLink.Size = new System.Drawing.Size(61, 22);
            this.lblTopLink.Text = "lblTopLink";
            // 
            // btnTopLinkAdd
            // 
            this.btnTopLinkAdd.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnTopLinkAdd.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnTopLinkAdd.Name = "btnTopLinkAdd";
            this.btnTopLinkAdd.Size = new System.Drawing.Size(92, 22);
            this.btnTopLinkAdd.Text = "btnTopLinkAdd";
            this.btnTopLinkAdd.Click += new System.EventHandler(this.btnTopLinkAdd_Click);
            // 
            // btnTopLinkEdit
            // 
            this.btnTopLinkEdit.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnTopLinkEdit.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnTopLinkEdit.Name = "btnTopLinkEdit";
            this.btnTopLinkEdit.Size = new System.Drawing.Size(90, 22);
            this.btnTopLinkEdit.Text = "btnTopLinkEdit";
            this.btnTopLinkEdit.Click += new System.EventHandler(this.btnTopLinkEdit_Click);
            // 
            // LEForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(829, 521);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.grpTestLoc);
            this.Font = new System.Drawing.Font("Segoe UI", 9F);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "LEForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "LEForm";
            this.grpTestLoc.ResumeLayout(false);
            this.grpTestLoc.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.ResumeLayout(false);

        }
    }
}
