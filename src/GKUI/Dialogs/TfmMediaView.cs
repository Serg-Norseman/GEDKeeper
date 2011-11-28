using System;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using GedCom551;
using GKCore.Sys;

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
			Stream fs = null;
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
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);

					ctl = new PictureBox();
					(ctl as PictureBox).Image = Image.FromFile(target_fn);
					(ctl as PictureBox).SizeMode = PictureBoxSizeMode.CenterImage;
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
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, out fs);
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
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, out fs);
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
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);

					ctl = new WebBrowser();
					(ctl as WebBrowser).Navigate(target_fn);

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

			if (fs != null)
			{
				//fs.Free();
			}
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
