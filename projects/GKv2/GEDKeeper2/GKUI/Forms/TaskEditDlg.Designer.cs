namespace GKUI.Forms
{
	partial class TaskEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private GKUI.Components.GKTabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblPriority;
		private System.Windows.Forms.ComboBox txtPriority;
		private System.Windows.Forms.Label lblStartDate;
		private GKUI.Components.GKDateBox txtStartDate;
		private GKUI.Components.GKDateBox txtStopDate;
		private System.Windows.Forms.Label lblStopDate;
		private System.Windows.Forms.Label lblGoal;
		private System.Windows.Forms.ComboBox cmbGoalType;
		private System.Windows.Forms.TextBox txtGoal;
		private System.Windows.Forms.Button btnGoalSelect;

		private void InitializeComponent()
		{
		    this.GroupBox1 = new System.Windows.Forms.GroupBox();
		    this.lblPriority = new System.Windows.Forms.Label();
		    this.lblStartDate = new System.Windows.Forms.Label();
		    this.lblStopDate = new System.Windows.Forms.Label();
		    this.lblGoal = new System.Windows.Forms.Label();
		    this.btnGoalSelect = new System.Windows.Forms.Button();
		    this.txtPriority = new System.Windows.Forms.ComboBox();
		    this.txtStartDate = new GKUI.Components.GKDateBox();
		    this.txtStopDate = new GKUI.Components.GKDateBox();
		    this.cmbGoalType = new System.Windows.Forms.ComboBox();
		    this.txtGoal = new System.Windows.Forms.TextBox();
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsData = new GKUI.Components.GKTabControl();
		    this.pageNotes = new System.Windows.Forms.TabPage();
		    this.GroupBox1.SuspendLayout();
		    this.tabsData.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // GroupBox1
		    // 
		    this.GroupBox1.Controls.Add(this.lblPriority);
		    this.GroupBox1.Controls.Add(this.lblStartDate);
		    this.GroupBox1.Controls.Add(this.lblStopDate);
		    this.GroupBox1.Controls.Add(this.lblGoal);
		    this.GroupBox1.Controls.Add(this.btnGoalSelect);
		    this.GroupBox1.Controls.Add(this.txtPriority);
		    this.GroupBox1.Controls.Add(this.txtStartDate);
		    this.GroupBox1.Controls.Add(this.txtStopDate);
		    this.GroupBox1.Controls.Add(this.cmbGoalType);
		    this.GroupBox1.Controls.Add(this.txtGoal);
		    this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
		    this.GroupBox1.Location = new System.Drawing.Point(0, 0);
		    this.GroupBox1.Name = "GroupBox1";
		    this.GroupBox1.Size = new System.Drawing.Size(674, 118);
		    this.GroupBox1.TabIndex = 0;
		    this.GroupBox1.TabStop = false;
		    // 
		    // lblPriority
		    // 
		    this.lblPriority.AutoSize = true;
		    this.lblPriority.Location = new System.Drawing.Point(11, 52);
		    this.lblPriority.Name = "lblPriority";
		    this.lblPriority.Size = new System.Drawing.Size(63, 17);
		    this.lblPriority.TabIndex = 4;
		    this.lblPriority.Text = "lblPriority";
		    // 
		    // lblStartDate
		    // 
		    this.lblStartDate.AutoSize = true;
		    this.lblStartDate.Location = new System.Drawing.Point(11, 81);
		    this.lblStartDate.Name = "lblStartDate";
		    this.lblStartDate.Size = new System.Drawing.Size(79, 17);
		    this.lblStartDate.TabIndex = 6;
		    this.lblStartDate.Text = "lblStartDate";
		    // 
		    // lblStopDate
		    // 
		    this.lblStopDate.AutoSize = true;
		    this.lblStopDate.Location = new System.Drawing.Point(343, 81);
		    this.lblStopDate.Name = "lblStopDate";
		    this.lblStopDate.Size = new System.Drawing.Size(78, 17);
		    this.lblStopDate.TabIndex = 8;
		    this.lblStopDate.Text = "lblStopDate";
		    // 
		    // lblGoal
		    // 
		    this.lblGoal.AutoSize = true;
		    this.lblGoal.Location = new System.Drawing.Point(11, 22);
		    this.lblGoal.Name = "lblGoal";
		    this.lblGoal.Size = new System.Drawing.Size(46, 17);
		    this.lblGoal.TabIndex = 0;
		    this.lblGoal.Text = "lblGoal";
		    // 
		    // btnGoalSelect
		    // 
		    this.btnGoalSelect.Location = new System.Drawing.Point(627, 16);
		    this.btnGoalSelect.Name = "btnGoalSelect";
		    this.btnGoalSelect.Size = new System.Drawing.Size(39, 34);
		    this.btnGoalSelect.TabIndex = 3;
		    this.btnGoalSelect.Click += new System.EventHandler(this.btnGoalSelect_Click);
		    // 
		    // txtPriority
		    // 
		    this.txtPriority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.txtPriority.Location = new System.Drawing.Point(101, 49);
		    this.txtPriority.Name = "txtPriority";
		    this.txtPriority.Size = new System.Drawing.Size(225, 25);
		    this.txtPriority.TabIndex = 5;
		    // 
		    // txtStartDate
		    // 
		    this.txtStartDate.Location = new System.Drawing.Point(101, 78);
		    this.txtStartDate.Name = "txtStartDate";
		    this.txtStartDate.Size = new System.Drawing.Size(225, 24);
		    this.txtStartDate.TabIndex = 7;
		    // 
		    // txtStopDate
		    // 
		    this.txtStopDate.Location = new System.Drawing.Point(437, 78);
		    this.txtStopDate.Name = "txtStopDate";
		    this.txtStopDate.Size = new System.Drawing.Size(225, 24);
		    this.txtStopDate.TabIndex = 9;
		    // 
		    // cmbGoalType
		    // 
		    this.cmbGoalType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.cmbGoalType.Location = new System.Drawing.Point(101, 19);
		    this.cmbGoalType.Name = "cmbGoalType";
		    this.cmbGoalType.Size = new System.Drawing.Size(158, 25);
		    this.cmbGoalType.TabIndex = 1;
		    this.cmbGoalType.SelectedIndexChanged += new System.EventHandler(this.cmbGoalType_SelectedIndexChanged);
		    // 
		    // txtGoal
		    // 
		    this.txtGoal.Location = new System.Drawing.Point(269, 19);
		    this.txtGoal.Name = "txtGoal";
		    this.txtGoal.ReadOnly = true;
		    this.txtGoal.Size = new System.Drawing.Size(348, 24);
		    this.txtGoal.TabIndex = 2;
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(426, 447);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(113, 30);
		    this.btnAccept.TabIndex = 2;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(549, 447);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(113, 30);
		    this.btnCancel.TabIndex = 3;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageNotes);
		    this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
		    this.tabsData.Location = new System.Drawing.Point(0, 118);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(674, 311);
		    this.tabsData.TabIndex = 1;
		    // 
		    // pageNotes
		    // 
		    this.pageNotes.Location = new System.Drawing.Point(4, 26);
		    this.pageNotes.Name = "pageNotes";
		    this.pageNotes.Size = new System.Drawing.Size(666, 281);
		    this.pageNotes.TabIndex = 0;
		    this.pageNotes.Text = "pageNotes";
		    // 
		    // TaskEditDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(674, 494);
		    this.Controls.Add(this.tabsData);
		    this.Controls.Add(this.GroupBox1);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TaskEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TaskEditDlg";
		    this.GroupBox1.ResumeLayout(false);
		    this.GroupBox1.PerformLayout();
		    this.tabsData.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}
