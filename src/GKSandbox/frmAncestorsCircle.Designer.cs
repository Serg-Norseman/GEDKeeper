using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKSandbox
{
	partial class frmAncestorsCircle
	{
		private System.Windows.Forms.MenuStrip menuStrip1;
		private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem chooseSubjectToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem optionsToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem colorStylesToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem useFillColorToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem backgroundImageToolStripMenuItem;
		private System.Windows.Forms.OpenFileDialog openFileDialog1;
		private System.Windows.Forms.ToolStripMenuItem layoutToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem imageToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem noneToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem tileToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem centerToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem stretchToolStripMenuItem1;
		private System.Windows.Forms.ToolStripMenuItem zoomToolStripMenuItem1;
		private System.Windows.Forms.ToolStripMenuItem loadToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem clearToolStripMenuItem;
		private System.Windows.Forms.ToolStripMenuItem screenshotToolStripMenuItem;

		protected override void Dispose(bool disposing)
		{
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.menuStrip1 = new System.Windows.Forms.MenuStrip();
			this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.chooseSubjectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.screenshotToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.optionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.colorStylesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.useFillColorToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.backgroundImageToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.imageToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.loadToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.clearToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.layoutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.noneToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.tileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.centerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
			this.stretchToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
			this.zoomToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
			this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
			this.menuStrip1.SuspendLayout();
			this.SuspendLayout();
			// 
			// menuStrip1
			// 
			this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.fileToolStripMenuItem,
									this.optionsToolStripMenuItem});
			this.menuStrip1.Location = new System.Drawing.Point(0, 0);
			this.menuStrip1.Name = "menuStrip1";
			this.menuStrip1.Size = new System.Drawing.Size(809, 24);
			this.menuStrip1.TabIndex = 0;
			this.menuStrip1.Text = "menuStrip1";
			this.menuStrip1.Visible = false;
			// 
			// fileToolStripMenuItem
			// 
			this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.chooseSubjectToolStripMenuItem,
									this.screenshotToolStripMenuItem});
			this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
			this.fileToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
			this.fileToolStripMenuItem.Text = "File";
			// 
			// chooseSubjectToolStripMenuItem
			// 
			this.chooseSubjectToolStripMenuItem.Name = "chooseSubjectToolStripMenuItem";
			this.chooseSubjectToolStripMenuItem.Size = new System.Drawing.Size(159, 22);
			this.chooseSubjectToolStripMenuItem.Text = "Choose subject";
			this.chooseSubjectToolStripMenuItem.Click += new System.EventHandler(this.chooseSubjectToolStripMenuItem_Click);
			// 
			// screenshotToolStripMenuItem
			// 
			this.screenshotToolStripMenuItem.Name = "screenshotToolStripMenuItem";
			this.screenshotToolStripMenuItem.Size = new System.Drawing.Size(159, 22);
			this.screenshotToolStripMenuItem.Text = "Screenshot";
			this.screenshotToolStripMenuItem.Click += new System.EventHandler(this.screenshotToolStripMenuItem_Click);
			// 
			// optionsToolStripMenuItem
			// 
			this.optionsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.colorStylesToolStripMenuItem,
									this.useFillColorToolStripMenuItem,
									this.backgroundImageToolStripMenuItem});
			this.optionsToolStripMenuItem.Name = "optionsToolStripMenuItem";
			this.optionsToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
			this.optionsToolStripMenuItem.Text = "Options";
			// 
			// colorStylesToolStripMenuItem
			// 
			this.colorStylesToolStripMenuItem.Name = "colorStylesToolStripMenuItem";
			this.colorStylesToolStripMenuItem.Size = new System.Drawing.Size(172, 22);
			this.colorStylesToolStripMenuItem.Text = "Color styles...";
			this.colorStylesToolStripMenuItem.Click += new System.EventHandler(this.colorStylesToolStripMenuItem_Click);
			// 
			// useFillColorToolStripMenuItem
			// 
			this.useFillColorToolStripMenuItem.Name = "useFillColorToolStripMenuItem";
			this.useFillColorToolStripMenuItem.Size = new System.Drawing.Size(172, 22);
			this.useFillColorToolStripMenuItem.Text = "Use fill color";
			this.useFillColorToolStripMenuItem.Click += new System.EventHandler(this.useFillColorToolStripMenuItem_Click);
			// 
			// backgroundImageToolStripMenuItem
			// 
			this.backgroundImageToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.imageToolStripMenuItem,
									this.layoutToolStripMenuItem});
			this.backgroundImageToolStripMenuItem.Name = "backgroundImageToolStripMenuItem";
			this.backgroundImageToolStripMenuItem.Size = new System.Drawing.Size(172, 22);
			this.backgroundImageToolStripMenuItem.Text = "Background image";
			// 
			// imageToolStripMenuItem
			// 
			this.imageToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.loadToolStripMenuItem,
									this.clearToolStripMenuItem});
			this.imageToolStripMenuItem.Name = "imageToolStripMenuItem";
			this.imageToolStripMenuItem.Size = new System.Drawing.Size(118, 22);
			this.imageToolStripMenuItem.Text = "Image";
			// 
			// loadToolStripMenuItem
			// 
			this.loadToolStripMenuItem.Name = "loadToolStripMenuItem";
			this.loadToolStripMenuItem.Size = new System.Drawing.Size(110, 22);
			this.loadToolStripMenuItem.Text = "Load";
			this.loadToolStripMenuItem.Click += new System.EventHandler(this.loadToolStripMenuItem_Click);
			// 
			// clearToolStripMenuItem
			// 
			this.clearToolStripMenuItem.Name = "clearToolStripMenuItem";
			this.clearToolStripMenuItem.Size = new System.Drawing.Size(110, 22);
			this.clearToolStripMenuItem.Text = "Clear";
			this.clearToolStripMenuItem.Click += new System.EventHandler(this.clearToolStripMenuItem_Click);
			// 
			// layoutToolStripMenuItem
			// 
			this.layoutToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.noneToolStripMenuItem,
									this.tileToolStripMenuItem,
									this.centerToolStripMenuItem,
									this.stretchToolStripMenuItem1,
									this.zoomToolStripMenuItem1});
			this.layoutToolStripMenuItem.Name = "layoutToolStripMenuItem";
			this.layoutToolStripMenuItem.Size = new System.Drawing.Size(118, 22);
			this.layoutToolStripMenuItem.Text = "Layout";
			// 
			// noneToolStripMenuItem
			// 
			this.noneToolStripMenuItem.Name = "noneToolStripMenuItem";
			this.noneToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
			this.noneToolStripMenuItem.Text = "None";
			this.noneToolStripMenuItem.Click += new System.EventHandler(this.noneToolStripMenuItem_Click);
			// 
			// tileToolStripMenuItem
			// 
			this.tileToolStripMenuItem.Name = "tileToolStripMenuItem";
			this.tileToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
			this.tileToolStripMenuItem.Text = "Tile";
			this.tileToolStripMenuItem.Click += new System.EventHandler(this.tileToolStripMenuItem_Click);
			// 
			// centerToolStripMenuItem
			// 
			this.centerToolStripMenuItem.Name = "centerToolStripMenuItem";
			this.centerToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
			this.centerToolStripMenuItem.Text = "Center";
			this.centerToolStripMenuItem.Click += new System.EventHandler(this.centerToolStripMenuItem_Click);
			// 
			// stretchToolStripMenuItem1
			// 
			this.stretchToolStripMenuItem1.Name = "stretchToolStripMenuItem1";
			this.stretchToolStripMenuItem1.Size = new System.Drawing.Size(120, 22);
			this.stretchToolStripMenuItem1.Text = "Stretch";
			this.stretchToolStripMenuItem1.Click += new System.EventHandler(this.stretchToolStripMenuItem1_Click);
			// 
			// zoomToolStripMenuItem1
			// 
			this.zoomToolStripMenuItem1.Name = "zoomToolStripMenuItem1";
			this.zoomToolStripMenuItem1.Size = new System.Drawing.Size(120, 22);
			this.zoomToolStripMenuItem1.Text = "Zoom";
			this.zoomToolStripMenuItem1.Click += new System.EventHandler(this.zoomToolStripMenuItem1_Click);
			// 
			// openFileDialog1
			// 
			this.openFileDialog1.FileName = "openFileDialog1";
			// 
			// frmAncestorsCircle
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(809, 587);
			this.Controls.Add(this.menuStrip1);
			this.MainMenuStrip = this.menuStrip1;
			this.Name = "frmAncestorsCircle";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Circle of Ancestors";
			this.Load += new System.EventHandler(this.OnLoad);
			this.Paint += new System.Windows.Forms.PaintEventHandler(this.OnPaint);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
			this.menuStrip1.ResumeLayout(false);
			this.menuStrip1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}

	}
}
