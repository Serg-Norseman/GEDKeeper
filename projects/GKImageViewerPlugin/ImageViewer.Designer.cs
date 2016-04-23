using System;

namespace GKImageViewerPlugin
{
	partial class ImageViewer
	{
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ImageViewer));
			this.ToolBar1 = new System.Windows.Forms.ToolBar();
			this.tbFileLoad = new System.Windows.Forms.ToolBarButton();
			this.ImageList_Buttons = new System.Windows.Forms.ImageList(this.components);
			this.OpenDialog1 = new System.Windows.Forms.OpenFileDialog();
			this.SuspendLayout();
			// 
			// ToolBar1
			// 
			this.ToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ToolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
									this.tbFileLoad});
			this.ToolBar1.ButtonSize = new System.Drawing.Size(27, 26);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.ImageList = this.ImageList_Buttons;
			this.ToolBar1.ImeMode = System.Windows.Forms.ImeMode.NoControl;
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new System.Drawing.Size(792, 32);
			this.ToolBar1.TabIndex = 1;
			this.ToolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			// 
			// tbFileLoad
			// 
			this.tbFileLoad.ImageIndex = 0;
			this.tbFileLoad.Name = "tbFileLoad";
			this.tbFileLoad.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
			this.tbFileLoad.ToolTipText = "Открыть файл";
			// 
			// ImageList_Buttons
			// 
			this.ImageList_Buttons.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList_Buttons.ImageStream")));
			this.ImageList_Buttons.TransparentColor = System.Drawing.Color.Transparent;
			this.ImageList_Buttons.Images.SetKeyName(0, "");
			// 
			// OpenDialog1
			// 
			this.OpenDialog1.Filter = "Все файлы (*.*)|*.*";
			// 
			// ImageViewer
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
			this.ClientSize = new System.Drawing.Size(792, 573);
			this.Controls.Add(this.ToolBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "ImageViewer";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Просмотр";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMediaView_KeyDown);
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.ComponentModel.IContainer components;
		public System.Windows.Forms.ImageList ImageList_Buttons;
		private System.Windows.Forms.ToolBarButton tbFileLoad;
		private System.Windows.Forms.ToolBar ToolBar1;
	}
}