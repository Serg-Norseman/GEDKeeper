namespace GKUI
{
	partial class PatriarchsViewerWin
	{
		/// <summary>
		/// Designer variable used to keep track of non-visual components.
		/// </summary>
		private System.ComponentModel.IContainer components = null;
		
		/// <summary>
		/// Disposes resources used by the form.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}
		
		/// <summary>
		/// This method is required for Windows Forms designer support.
		/// Do not change the method contents inside the source code editor. The Forms designer might
		/// not be able to load this method if it was changed manually.
		/// </summary>
		private void InitializeComponent()
		{
            this.arborViewer1 = new ArborGVT.ArborViewer();
			this.SuspendLayout();
			// 
			// arborViewer1
			// 
			this.arborViewer1.BackColor = System.Drawing.Color.White;
			this.arborViewer1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.arborViewer1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.arborViewer1.Location = new System.Drawing.Point(0, 0);
			this.arborViewer1.Name = "arborViewer1";
			this.arborViewer1.Size = new System.Drawing.Size(894, 587);
			this.arborViewer1.TabIndex = 0;
			this.arborViewer1.TabStop = true;
			this.arborViewer1.MouseMove += new System.Windows.Forms.MouseEventHandler(this.ArborViewer1MouseMove);
			// 
			// PatriarchsViewer
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(894, 587);
			this.Controls.Add(this.arborViewer1);
			this.KeyPreview = true;
			this.Name = "PatriarchsViewer";
			this.Text = "PatriarchsViewer";
			this.ResumeLayout(false);
		}
        private ArborGVT.ArborViewer arborViewer1;
	}
}
