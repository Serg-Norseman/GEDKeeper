using System;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GedCom551;
using GKCore.Interfaces;
using GKUI.Controls;

/// <summary>
/// 
/// </summary>

namespace GKImageViewerPlugin
{
	public partial class ImageViewer : Form, ILocalization
	{
		private GKImageControl fImageCtl;

		public ImageViewer(IPlugin plugin)
		{
			this.InitializeComponent();
			//this.fBase = aBase;
		}
		
		void ILocalization.SetLang()
		{
			/*if (this.fImageCtl != null) {
        		this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
				this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
				this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
			}*/
		}

		private void TfmMediaView_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape)
			{
				base.Close();
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (e.Button == this.tbFileLoad) {
				if (OpenDialog1.ShowDialog() == DialogResult.OK) {
					this.SetFileRef(OpenDialog1.FileName);
				}
			}
		}

		private void SetFileRef(string fileName)
		{
			this.SuspendLayout();

			this.Text = fileName;
			Control ctl = null;
			this.fImageCtl = null;

			TGEDCOMMultimediaFormat fmt = TGEDCOMFileReference.RecognizeFormat(fileName);
			
			switch (fmt)
			{
				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
					{
						using (Stream fs = new FileStream(fileName, FileMode.Open))
						{
							this.fImageCtl = new GKImageControl();
							this.fImageCtl.OpenImage(new Bitmap(fs));
							ctl = this.fImageCtl;
						}
						break;
					}

				case TGEDCOMMultimediaFormat.mfWAV:
				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
					break;

				case TGEDCOMMultimediaFormat.mfTXT:
					{
						using (Stream fs = new FileStream(fileName, FileMode.Open))
						{
							using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251)))
							{
								TextBox txtBox = new TextBox();
								txtBox.Multiline = true;
								txtBox.ReadOnly = true;
								txtBox.ScrollBars = ScrollBars.Both;
								txtBox.Text = strd.ReadToEnd().ToString();
								ctl = txtBox;
							}
						}
						break;
					}

				case TGEDCOMMultimediaFormat.mfRTF:
					{
						using (Stream fs = new FileStream(fileName, FileMode.Open))
						{
							using (StreamReader strd = new StreamReader(fs))
							{
								RichTextBox txtBox = new RichTextBox();
								txtBox.ReadOnly = true;
								txtBox.Text = strd.ReadToEnd().ToString();
								ctl = txtBox;
							}
						}
						break;
					}

				case TGEDCOMMultimediaFormat.mfHTM:
					{
						using (Stream fs = new FileStream(fileName, FileMode.Open))
						{
							ctl = new WebBrowser();
							(ctl as WebBrowser).DocumentStream = fs;
						}
						break;
					}
			}

			if (ctl != null) {
				ctl.Dock = DockStyle.Fill;
				ctl.Location = new Point(0, 50);
				ctl.Size = new Size(100, 100);
				base.Controls.Add(ctl);
				base.Controls.SetChildIndex(ctl, 0);
			}

			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}
