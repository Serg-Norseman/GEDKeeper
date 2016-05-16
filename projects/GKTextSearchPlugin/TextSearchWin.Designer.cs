namespace GKTextSearchPlugin
{
	partial class TextSearchWin
	{
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.Button btnSearch;
		private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
		private System.Windows.Forms.TextBox txtQuery;

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
			this.txtQuery = new System.Windows.Forms.TextBox();
			this.btnSearch = new System.Windows.Forms.Button();
			this.flowLayoutPanel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// flowLayoutPanel1
			// 
			this.flowLayoutPanel1.AutoSize = true;
			this.flowLayoutPanel1.Controls.Add(this.txtQuery);
			this.flowLayoutPanel1.Controls.Add(this.btnSearch);
			this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
			this.flowLayoutPanel1.Name = "flowLayoutPanel1";
			this.flowLayoutPanel1.Size = new System.Drawing.Size(679, 29);
			this.flowLayoutPanel1.TabIndex = 4;
			// 
			// txtQuery
			// 
			this.txtQuery.Location = new System.Drawing.Point(3, 3);
			this.txtQuery.Name = "txtQuery";
			this.txtQuery.Size = new System.Drawing.Size(396, 21);
			this.txtQuery.TabIndex = 1;
			this.txtQuery.Text = "";
			// 
			// btnSearch
			// 
			this.btnSearch.Location = new System.Drawing.Point(405, 3);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new System.Drawing.Size(75, 23);
			this.btnSearch.TabIndex = 3;
			this.btnSearch.Text = "btnSearch";
			this.btnSearch.UseVisualStyleBackColor = true;
			this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
			// 
			// TextSearchWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(679, 342);
			this.Controls.Add(this.flowLayoutPanel1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.Name = "TextSearchWin";
			this.ShowInTaskbar = true;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "TextSearchWin";
			this.Load += new System.EventHandler(this.TfmTextSearchLoad);
			this.flowLayoutPanel1.ResumeLayout(false);
			this.flowLayoutPanel1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}
