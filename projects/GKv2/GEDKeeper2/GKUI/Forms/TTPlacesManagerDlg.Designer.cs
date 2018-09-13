namespace GKUI.Forms
{
	partial class TTPlacesManagerDlg
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabPage pagePlaceManage;
		private System.Windows.Forms.Panel Panel4;
		private System.Windows.Forms.Button btnAnalysePlaces;
		private System.Windows.Forms.Button btnIntoList;

		private void InitializeComponent()
		{
		    this.tabsTools = new System.Windows.Forms.TabControl();
		    this.pagePlaceManage = new System.Windows.Forms.TabPage();
		    this.Panel4 = new System.Windows.Forms.Panel();
		    this.btnIntoList = new System.Windows.Forms.Button();
		    this.btnAnalysePlaces = new System.Windows.Forms.Button();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pagePlaceManage.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pagePlaceManage);
		    this.tabsTools.Location = new System.Drawing.Point(9, 8);
		    this.tabsTools.Margin = new System.Windows.Forms.Padding(2);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(808, 436);
		    this.tabsTools.TabIndex = 0;
		    // 
		    // pagePlaceManage
		    // 
		    this.pagePlaceManage.Controls.Add(this.Panel4);
		    this.pagePlaceManage.Controls.Add(this.btnIntoList);
		    this.pagePlaceManage.Controls.Add(this.btnAnalysePlaces);
		    this.pagePlaceManage.Location = new System.Drawing.Point(4, 22);
		    this.pagePlaceManage.Margin = new System.Windows.Forms.Padding(2);
		    this.pagePlaceManage.Name = "pagePlaceManage";
		    this.pagePlaceManage.Size = new System.Drawing.Size(800, 410);
		    this.pagePlaceManage.TabIndex = 8;
		    this.pagePlaceManage.Text = "pagePlaceManage";
		    // 
		    // Panel4
		    // 
		    this.Panel4.Location = new System.Drawing.Point(0, 0);
		    this.Panel4.Margin = new System.Windows.Forms.Padding(2);
		    this.Panel4.Name = "Panel4";
		    this.Panel4.Size = new System.Drawing.Size(798, 358);
		    this.Panel4.TabIndex = 0;
		    // 
		    // btnIntoList
		    // 
		    this.btnIntoList.Location = new System.Drawing.Point(645, 373);
		    this.btnIntoList.Margin = new System.Windows.Forms.Padding(2);
		    this.btnIntoList.Name = "btnIntoList";
		    this.btnIntoList.Size = new System.Drawing.Size(143, 25);
		    this.btnIntoList.TabIndex = 1;
		    this.btnIntoList.Text = "btnIntoList";
		    this.btnIntoList.Click += new System.EventHandler(this.btnIntoList_Click);
		    // 
		    // btnAnalysePlaces
		    // 
		    this.btnAnalysePlaces.Location = new System.Drawing.Point(9, 373);
		    this.btnAnalysePlaces.Margin = new System.Windows.Forms.Padding(2);
		    this.btnAnalysePlaces.Name = "btnAnalysePlaces";
		    this.btnAnalysePlaces.Size = new System.Drawing.Size(143, 25);
		    this.btnAnalysePlaces.TabIndex = 1;
		    this.btnAnalysePlaces.Text = "btnAnalysePlaces";
		    this.btnAnalysePlaces.Click += new System.EventHandler(this.btnAnalysePlaces_Click);
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(726, 466);
		    this.btnClose.Margin = new System.Windows.Forms.Padding(2);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(91, 24);
		    this.btnClose.TabIndex = 1;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // TTPlacesManagerDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(827, 500);
		    this.Controls.Add(this.tabsTools);
		    this.Controls.Add(this.btnClose);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.Margin = new System.Windows.Forms.Padding(2);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TTPlacesManagerDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pagePlaceManage.ResumeLayout(false);
		    this.ResumeLayout(false);

		}
	}
}