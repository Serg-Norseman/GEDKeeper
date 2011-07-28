using GedCom551;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmMediaView : Form
	{
		private TfmBase FBase;
		private TGEDCOMFileReferenceWithTitle FFileRef;
		private bool FExtern;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
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
			TStream fs = null;
			this.FExtern = false;
			this.Text = this.FFileRef.Title;
			switch (this.FFileRef.MultimediaFormat)
			{
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPNG:
				{
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);
					Control ctl = new PictureBox();
					ctl.Dock = DockStyle.Fill;
					Image.FromFile(target_fn);
					base.Controls.Add(ctl);
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfWAV:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfMPG:
				{
					this.FExtern = true;
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);
					TGKSys.LoadExtFile(target_fn);
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTXT:
				{
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref fs);
					StreamReader strd = new StreamReader(TStreamToCLRStream.GetStream(fs));
					Control ctl = new TextBox();
					ctl.Dock = DockStyle.Fill;
					(ctl as TextBox).ReadOnly = true;
					(ctl as TextBox).ScrollBars = ScrollBars.Both;
					(ctl as TextBox).Text = strd.ReadToEnd().ToString();
					base.Controls.Add(ctl);
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfRTF:
				{
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref fs);
					StreamReader strd = new StreamReader(TStreamToCLRStream.GetStream(fs));
					Control ctl = new RichTextBox();
					ctl.Dock = DockStyle.Fill;
					(ctl as RichTextBox).ReadOnly = true;
					(ctl as RichTextBox).Text = strd.ReadToEnd().ToString();
					base.Controls.Add(ctl);
					break;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfHTM:
				{
					string target_fn = "";
					this.Base.Engine.MediaLoad(this.FFileRef.StringValue, ref target_fn);
					break;
				}
			}
			if (fs != null)
			{
				fs.Free();
			}
		}

		private void TfmMediaView_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape)
			{
				base.Close();
			}
		}

		private void InitializeComponent()
		{
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(792, 573);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.Name = "TfmMediaView";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Просмотр";
			base.KeyDown += new KeyEventHandler(this.TfmMediaView_KeyDown);
		}

		public TfmMediaView(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
		}
	}
}
