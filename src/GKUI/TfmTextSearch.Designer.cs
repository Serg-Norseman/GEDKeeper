namespace GKUI
{
	partial class TfmTextSearch
	{
		private System.ComponentModel.IContainer components = null;

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
			this.label1 = new System.Windows.Forms.Label();
			this.textBox2 = new System.Windows.Forms.TextBox();
			this.btnSearch = new System.Windows.Forms.Button();
			this.btnReindex = new System.Windows.Forms.Button();
			this.flowLayoutPanel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// flowLayoutPanel1
			// 
			this.flowLayoutPanel1.AutoSize = true;
			this.flowLayoutPanel1.Controls.Add(this.label1);
			this.flowLayoutPanel1.Controls.Add(this.textBox2);
			this.flowLayoutPanel1.Controls.Add(this.btnSearch);
			this.flowLayoutPanel1.Controls.Add(this.btnReindex);
			this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
			this.flowLayoutPanel1.Name = "flowLayoutPanel1";
			this.flowLayoutPanel1.Size = new System.Drawing.Size(679, 29);
			this.flowLayoutPanel1.TabIndex = 4;
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(3, 0);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(59, 23);
			this.label1.TabIndex = 0;
			this.label1.Text = "Искать:";
			this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// textBox2
			// 
			this.textBox2.Location = new System.Drawing.Point(68, 3);
			this.textBox2.Name = "textBox2";
			this.textBox2.Size = new System.Drawing.Size(396, 21);
			this.textBox2.TabIndex = 1;
			this.textBox2.Text = "Сергей";
			// 
			// btnSearch
			// 
			this.btnSearch.Location = new System.Drawing.Point(470, 3);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new System.Drawing.Size(75, 23);
			this.btnSearch.TabIndex = 3;
			this.btnSearch.Text = "Search";
			this.btnSearch.UseVisualStyleBackColor = true;
			this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
			// 
			// btnReindex
			// 
			this.btnReindex.Location = new System.Drawing.Point(551, 3);
			this.btnReindex.Name = "btnReindex";
			this.btnReindex.Size = new System.Drawing.Size(75, 23);
			this.btnReindex.TabIndex = 4;
			this.btnReindex.Text = "Index Init";
			this.btnReindex.UseVisualStyleBackColor = true;
			this.btnReindex.Click += new System.EventHandler(this.btnReindex_Click);
			// 
			// TfmTextSearch
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(679, 342);
			this.Controls.Add(this.flowLayoutPanel1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.Name = "TfmTextSearch";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "TfmTextSearch";
			this.flowLayoutPanel1.ResumeLayout(false);
			this.flowLayoutPanel1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.Button btnReindex;
		private System.Windows.Forms.Button btnSearch;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;

		private System.Windows.Forms.TextBox textBox2;
	}
}
