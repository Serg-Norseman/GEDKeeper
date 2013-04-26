using System;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKSandbox;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmMediaView : Form
	{
		private TfmBase FBase;
		private TGEDCOMFileReferenceWithTitle FFileRef;
		private bool FExtern;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public bool Extern
		{
			get { return this.FExtern; }
		}

		public TGEDCOMFileReferenceWithTitle FileRef
		{
			get { return this.FFileRef; }
			set { this.SetFileRef(value); }
		}

		private void SetFileRef([In] TGEDCOMFileReferenceWithTitle Value)
		{
			this.FFileRef = Value;
			this.FExtern = false;
			this.Text = this.FFileRef.Title;
			Control ctl = null;

			this.SuspendLayout();
			switch (this.FFileRef.MultimediaFormat)
			{
				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
				{
					Image img = this.Base.Engine.BitmapLoad(this.FFileRef.StringValue, -1, -1, false);

					ImageControl imCtl = new ImageControl();
					imCtl.OpenImage(img);
					ctl = imCtl;
					
					//ctl = new PictureBox();
					//(ctl as PictureBox).Image = img;
					//(ctl as PictureBox).SizeMode = PictureBoxSizeMode.CenterImage;
					break;
				}

				case TGEDCOMMultimediaFormat.mfWAV:
				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
				{
					this.FExtern = true;
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);
                    SysUtils.LoadExtFile(target_fn);
					break;
				}

				case TGEDCOMMultimediaFormat.mfTXT:
				{
					Stream fs = null;
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, out fs, false);
					using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251)))
					{
						ctl = new TextBox();
						(ctl as TextBox).Multiline = true;
						(ctl as TextBox).ReadOnly = true;
						(ctl as TextBox).ScrollBars = ScrollBars.Both;
						(ctl as TextBox).Text = strd.ReadToEnd().ToString();
						this.Controls.Add(ctl);
					}
					break;
				}

				case TGEDCOMMultimediaFormat.mfRTF:
				{
					Stream fs = null;
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, out fs, false);
					using (StreamReader strd = new StreamReader(fs))
					{
						ctl = new RichTextBox();
						(ctl as RichTextBox).ReadOnly = true;
						(ctl as RichTextBox).Text = strd.ReadToEnd().ToString();
					}
					break;
				}

				case TGEDCOMMultimediaFormat.mfHTM:
				{
					Stream fs = null;
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, out fs, false);

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

		public TfmMediaView(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
		}
	}
}
