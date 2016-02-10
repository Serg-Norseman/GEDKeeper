using System;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKImageViewerPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class ImageViewer : Form, ILocalization
	{
		private ImageView fImageCtl;
    	private readonly ILangMan fLangMan;

    	public ImageViewer(IPlugin plugin)
		{
			this.InitializeComponent();

            this.fLangMan = plugin.LangMan;

            this.SetLang();
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

			GEDCOMMultimediaFormat fmt = GEDCOMFileReference.RecognizeFormat(fileName);
			
			switch (fmt)
			{
				case GEDCOMMultimediaFormat.mfBMP:
				case GEDCOMMultimediaFormat.mfGIF:
				case GEDCOMMultimediaFormat.mfJPG:
				case GEDCOMMultimediaFormat.mfPCX:
				case GEDCOMMultimediaFormat.mfTIF:
				case GEDCOMMultimediaFormat.mfTGA:
				case GEDCOMMultimediaFormat.mfPNG:
					{
						using (Stream fs = new FileStream(fileName, FileMode.Open))
						{
							this.fImageCtl = new ImageView();
							this.fImageCtl.OpenImage(new Bitmap(fs));
							ctl = this.fImageCtl;
						}
						break;
					}

				case GEDCOMMultimediaFormat.mfWAV:
				case GEDCOMMultimediaFormat.mfAVI:
				case GEDCOMMultimediaFormat.mfMPG:
					break;

				case GEDCOMMultimediaFormat.mfTXT:
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

				case GEDCOMMultimediaFormat.mfRTF:
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

				case GEDCOMMultimediaFormat.mfHTM:
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

        #region ILocalization support

        public void SetLang()
        {
        	this.Text = this.fLangMan.LS(IVLS.LSID_ImgViewer);
        	
            /*if (this.fImageCtl != null) {
                this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
                this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
                this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
            }*/
        }

        #endregion
    }
}
