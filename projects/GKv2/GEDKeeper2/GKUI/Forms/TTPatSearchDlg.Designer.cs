namespace GKUI.Forms
{
	partial class TTPatSearchDlg
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabPage pagePatSearch;
		private System.Windows.Forms.Button btnPatSearch;
		private System.Windows.Forms.Panel Panel3;
		private System.Windows.Forms.Label lblMinGenerations;
		private System.Windows.Forms.NumericUpDown edMinGens;
		private System.Windows.Forms.Button btnSetPatriarch;
		private System.Windows.Forms.Button btnPatriarchsDiagram;
		private System.Windows.Forms.CheckBox chkWithoutDates;

		private void InitializeComponent()
		{
		    this.tabsTools = new System.Windows.Forms.TabControl();
		    this.pagePatSearch = new System.Windows.Forms.TabPage();
		    this.btnPatriarchsDiagram = new System.Windows.Forms.Button();
		    this.chkWithoutDates = new System.Windows.Forms.CheckBox();
		    this.lblMinGenerations = new System.Windows.Forms.Label();
		    this.btnPatSearch = new System.Windows.Forms.Button();
		    this.Panel3 = new System.Windows.Forms.Panel();
		    this.edMinGens = new System.Windows.Forms.NumericUpDown();
		    this.btnSetPatriarch = new System.Windows.Forms.Button();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pagePatSearch.SuspendLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edMinGens)).BeginInit();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pagePatSearch);
		    this.tabsTools.Location = new System.Drawing.Point(11, 10);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(1010, 545);
		    this.tabsTools.TabIndex = 0;
		    // 
		    // pagePatSearch
		    // 
		    this.pagePatSearch.Controls.Add(this.btnPatriarchsDiagram);
		    this.pagePatSearch.Controls.Add(this.chkWithoutDates);
		    this.pagePatSearch.Controls.Add(this.lblMinGenerations);
		    this.pagePatSearch.Controls.Add(this.btnPatSearch);
		    this.pagePatSearch.Controls.Add(this.Panel3);
		    this.pagePatSearch.Controls.Add(this.edMinGens);
		    this.pagePatSearch.Controls.Add(this.btnSetPatriarch);
		    this.pagePatSearch.Location = new System.Drawing.Point(4, 26);
		    this.pagePatSearch.Name = "pagePatSearch";
		    this.pagePatSearch.Size = new System.Drawing.Size(1002, 515);
		    this.pagePatSearch.TabIndex = 7;
		    this.pagePatSearch.Text = "pagePatSearch";
		    // 
		    // btnPatriarchsDiagram
		    // 
		    this.btnPatriarchsDiagram.Location = new System.Drawing.Point(871, 464);
		    this.btnPatriarchsDiagram.Name = "btnPatriarchsDiagram";
		    this.btnPatriarchsDiagram.Size = new System.Drawing.Size(105, 30);
		    this.btnPatriarchsDiagram.TabIndex = 6;
		    this.btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
		    this.btnPatriarchsDiagram.Click += new System.EventHandler(this.btnPatriarchsDiagram_Click);
		    // 
		    // chkWithoutDates
		    // 
		    this.chkWithoutDates.AutoSize = true;
		    this.chkWithoutDates.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.chkWithoutDates.Location = new System.Drawing.Point(362, 467);
		    this.chkWithoutDates.Name = "chkWithoutDates";
		    this.chkWithoutDates.Size = new System.Drawing.Size(137, 21);
		    this.chkWithoutDates.TabIndex = 5;
		    this.chkWithoutDates.Text = "chkWithoutDates";
		    this.chkWithoutDates.UseVisualStyleBackColor = true;
		    // 
		    // lblMinGenerations
		    // 
		    this.lblMinGenerations.AutoSize = true;
		    this.lblMinGenerations.Location = new System.Drawing.Point(16, 468);
		    this.lblMinGenerations.Name = "lblMinGenerations";
		    this.lblMinGenerations.Size = new System.Drawing.Size(112, 17);
		    this.lblMinGenerations.TabIndex = 0;
		    this.lblMinGenerations.Text = "lblMinGenerations";
		    // 
		    // btnPatSearch
		    // 
		    this.btnPatSearch.Location = new System.Drawing.Point(757, 464);
		    this.btnPatSearch.Name = "btnPatSearch";
		    this.btnPatSearch.Size = new System.Drawing.Size(105, 30);
		    this.btnPatSearch.TabIndex = 0;
		    this.btnPatSearch.Text = "btnPatSearch";
		    this.btnPatSearch.Click += new System.EventHandler(this.btnPatSearch_Click);
		    // 
		    // Panel3
		    // 
		    this.Panel3.Location = new System.Drawing.Point(0, 0);
		    this.Panel3.Name = "Panel3";
		    this.Panel3.Size = new System.Drawing.Size(998, 448);
		    this.Panel3.TabIndex = 1;
		    // 
		    // edMinGens
		    // 
		    this.edMinGens.Location = new System.Drawing.Point(258, 466);
		    this.edMinGens.Name = "edMinGens";
		    this.edMinGens.Size = new System.Drawing.Size(79, 24);
		    this.edMinGens.TabIndex = 2;
		    this.edMinGens.Value = new decimal(new int[] {
            2,
            0,
            0,
            0});
		    // 
		    // btnSetPatriarch
		    // 
		    this.btnSetPatriarch.Location = new System.Drawing.Point(577, 464);
		    this.btnSetPatriarch.Name = "btnSetPatriarch";
		    this.btnSetPatriarch.Size = new System.Drawing.Size(172, 30);
		    this.btnSetPatriarch.TabIndex = 4;
		    this.btnSetPatriarch.Text = "btnSetPatriarch";
		    this.btnSetPatriarch.Click += new System.EventHandler(this.btnSetPatriarch_Click);
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(907, 583);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(114, 30);
		    this.btnClose.TabIndex = 1;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // TTPatSearchDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(1034, 625);
		    this.Controls.Add(this.tabsTools);
		    this.Controls.Add(this.btnClose);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TTPatSearchDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pagePatSearch.ResumeLayout(false);
		    this.pagePatSearch.PerformLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edMinGens)).EndInit();
		    this.ResumeLayout(false);

		}
	}
}