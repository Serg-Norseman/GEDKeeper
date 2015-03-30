using System;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore.Interfaces;
using GKUI.Controls;
using GKCore;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmMediaView : Form, ILocalization
	{
		private readonly IBase fBase;

        private TGEDCOMFileReferenceWithTitle fFileRef;
		private bool fExtern;
		private GKImageControl fImageCtl;

		public bool Extern
		{
			get { return this.fExtern; }
		}

		public TGEDCOMFileReferenceWithTitle FileRef
		{
			get { return this.fFileRef; }
			set { this.SetFileRef(value); }
		}

		private void SetFileRef(TGEDCOMFileReferenceWithTitle value)
		{
			this.fFileRef = value;
			this.fExtern = false;
			this.Text = this.fFileRef.Title;
			Control ctl = null;

			this.SuspendLayout();
			switch (this.fFileRef.MultimediaFormat)
			{
				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
				{
					Image img = this.fBase.BitmapLoad(this.fFileRef, -1, -1, false);

					this.fImageCtl = new GKImageControl();
					this.fImageCtl.OpenImage(img);
					ctl = this.fImageCtl;
					break;
				}

				case TGEDCOMMultimediaFormat.mfWAV:
				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
				{
					this.fExtern = true;
					string targetFile = "";
                    this.fBase.MediaLoad(this.fFileRef, ref targetFile);
                    SysUtils.LoadExtFile(targetFile);
					break;
				}

				case TGEDCOMMultimediaFormat.mfTXT:
				{
					Stream fs;
					this.fBase.MediaLoad(this.fFileRef, out fs, false);
					using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251)))
					{
					    TextBox txtBox = new TextBox();
                        txtBox.Multiline = true;
                        txtBox.ReadOnly = true;
                        txtBox.ScrollBars = ScrollBars.Both;
                        txtBox.Text = strd.ReadToEnd().ToString();
                        ctl = txtBox;
					}
					break;
				}

				case TGEDCOMMultimediaFormat.mfRTF:
				{
					Stream fs;
					this.fBase.MediaLoad(this.fFileRef, out fs, false);
					using (StreamReader strd = new StreamReader(fs))
					{
					    RichTextBox txtBox = new RichTextBox();
                        txtBox.ReadOnly = true;
                        txtBox.Text = strd.ReadToEnd().ToString();
                        ctl = txtBox;
					}
					break;
				}

				case TGEDCOMMultimediaFormat.mfHTM:
				{
					Stream fs;
					this.fBase.MediaLoad(this.fFileRef, out fs, false);

					ctl = new WebBrowser();
					(ctl as WebBrowser).DocumentStream = fs;

					break;
				}
			}

			if (ctl != null) {
				ctl.Dock = DockStyle.Fill;
				ctl.Location = new Point(0, 0);
				ctl.Size = new Size(100, 100);
				base.Controls.Add(ctl);
				base.Controls.SetChildIndex(ctl, 0);
			}

			this.ResumeLayout(false);
		}

		private void TfmMediaView_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape)
			{
				base.Close();
			}
		}

		public TfmMediaView(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;
		}
		
		void ILocalization.SetLang()
		{
			if (this.fImageCtl != null) {
        		this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
				this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
				this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
			}
		}
	}
}
