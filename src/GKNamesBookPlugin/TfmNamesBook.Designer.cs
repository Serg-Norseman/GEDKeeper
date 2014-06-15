using System;

namespace GKNamesBookPlugin
{
	partial class TfmNamesBook
	{
		private System.Windows.Forms.ComboBox cbNames;
		private System.Windows.Forms.TextBox mmDesc;

		private void InitializeComponent()
		{
			this.cbNames = new System.Windows.Forms.ComboBox();
			this.mmDesc = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			this.cbNames.DropDownStyle = System.Windows.Forms.ComboBoxStyle.Simple;
			this.cbNames.Location = new System.Drawing.Point(8, 8);
			this.cbNames.Name = "cbNames";
			this.cbNames.Size = new System.Drawing.Size(257, 168);
			this.cbNames.Sorted = true;
			this.cbNames.TabIndex = 0;
			this.cbNames.SelectedIndexChanged += new System.EventHandler(this.cbNames_SelectedIndexChanged);
			this.mmDesc.BackColor = System.Drawing.SystemColors.Control;
			this.mmDesc.Location = new System.Drawing.Point(8, 184);
			this.mmDesc.Multiline = true;
			this.mmDesc.Name = "mmDesc";
			this.mmDesc.ReadOnly = true;
			this.mmDesc.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.mmDesc.Size = new System.Drawing.Size(257, 161);
			this.mmDesc.TabIndex = 1;
			this.mmDesc.Text = "";
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(274, 353);
			this.Controls.Add(this.cbNames);
			this.Controls.Add(this.mmDesc);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
			this.Load += new System.EventHandler(this.TfmNamesBook_Load);
			this.Name = "TfmNamesBook";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
			this.Text = "TfmNamesBook";
			this.TopMost = true;
			this.Closed += new System.EventHandler(this.TfmNamesBook_Closed);
			this.ResumeLayout(false);
		}
	}
}