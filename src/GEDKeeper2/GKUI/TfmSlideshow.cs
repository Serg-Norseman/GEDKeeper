using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmSlideshow : Form, ILocalization, IWorkWindow
    {
        private readonly IBaseWindow fBase;
        private readonly List<GEDCOMFileReferenceWithTitle> fFileRefs;
        private readonly ImageBox fImageCtl;

        private int fCurrentIndex;
        private string fCurrentText;

        public TfmSlideshow(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.SuspendLayout();
            this.fImageCtl = new ImageBox();
            this.fImageCtl.Dock = DockStyle.Fill;
            this.fImageCtl.Location = new Point(0, 0);
            this.fImageCtl.Size = new Size(100, 100);
            base.Controls.Add(this.fImageCtl);
            base.Controls.SetChildIndex(this.fImageCtl, 0);
            this.ResumeLayout(false);

        	this.fImageCtl.BackColor = System.Drawing.SystemColors.ControlDark;
        	this.fImageCtl.Margin = new System.Windows.Forms.Padding(4);
            this.fImageCtl.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            this.fImageCtl.ImageBorderColor = Color.AliceBlue;
            this.fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;

			this.WindowState = FormWindowState.Maximized;

            this.SetLang();

            this.fBase = aBase;
            this.fFileRefs = new List<GEDCOMFileReferenceWithTitle>();
            this.fCurrentIndex = -1;

            this.LoadList();
        }

		private void TfmSlideshow_Load(object sender, System.EventArgs e)
		{
        	if (this.fFileRefs.Count > 0) {
        		this.fCurrentIndex = 0;
        		this.SetFileRef();
			} else {
				this.UpdateControls();
			}
		}

        private void TfmMediaView_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) {
                base.Close();
            }
        }

        private void LoadList()
        {
        	GEDCOMRecord record;
        	var enumerator = this.fBase.Tree.GetEnumerator(GEDCOMRecordType.rtMultimedia);
        	while (enumerator.MoveNext(out record)) {
                GEDCOMMultimediaRecord mediaRec = (GEDCOMMultimediaRecord)record;
				GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

        		MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
        		if (mmKind == MultimediaKind.mkImage) {
        			this.fFileRefs.Add(fileRef);
        		}
        	}
        }

        public void SetLang()
        {
        	this.Text = LangMan.LS(LSID.LSID_Slideshow);
        	this.tsbPrev.ToolTipText = LangMan.LS(LSID.LSID_PrevRec);
        	this.tsbNext.ToolTipText = LangMan.LS(LSID.LSID_NextRec);

        	//this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
        	//this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
        	//this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
        }

        private void SetFileRef()
        {
        	GEDCOMFileReferenceWithTitle fileRef = this.fFileRefs[this.fCurrentIndex];
        	
        	this.fCurrentText = fileRef.Title;

        	switch (fileRef.MultimediaFormat)
        	{
        		case GEDCOMMultimediaFormat.mfBMP:
        		case GEDCOMMultimediaFormat.mfGIF:
        		case GEDCOMMultimediaFormat.mfJPG:
        		case GEDCOMMultimediaFormat.mfPCX:
        		case GEDCOMMultimediaFormat.mfTIF:
        		case GEDCOMMultimediaFormat.mfTGA:
        		case GEDCOMMultimediaFormat.mfPNG:
        			{
        				Image img = this.fBase.Context.BitmapLoad(fileRef, -1, -1, false);
        				this.fImageCtl.Image = img;
        				this.fImageCtl.ZoomToFit();
        				break;
        			}
        	}

			this.UpdateControls();
        }

		private void tsbStart_Click(object sender, System.EventArgs e)
		{
			if (tsbStart.Text == "Старт") {
				tsbStart.Text = "Стоп";
				tsbStart.Image = global::GKResources.iStop;
				timer1.Enabled = true;
			} else {
				tsbStart.Text = "Старт";
				tsbStart.Image = global::GKResources.iStart;
				timer1.Enabled = false;
			}
		}

		private void tsbPrev_Click(object sender, System.EventArgs e)
		{
			this.fCurrentIndex--;
			this.SetFileRef();
		}

		private void tsbNext_Click(object sender, System.EventArgs e)
		{
			this.fCurrentIndex++;
			this.SetFileRef();
		}

        private void UpdateControls()
        {
        	tsbStart.Enabled = (this.fFileRefs.Count > 0);
			tsbPrev.Enabled = (this.fCurrentIndex > 0);
			tsbNext.Enabled = (this.fCurrentIndex < this.fFileRefs.Count - 1);

			TfmGEDKeeper.Instance.UpdateControls(false);
        }

		private void Timer1Tick(object sender, System.EventArgs e)
		{
			if (this.fCurrentIndex < this.fFileRefs.Count - 1) {
				this.fCurrentIndex++;
			} else {
				this.fCurrentIndex = 0;
			}

			this.SetFileRef();
		}

		#region IWorkWindow implementation

		public string GetStatusString()
		{
			return string.Format("{0} / {1} [{2}]", this.fCurrentIndex + 1, this.fFileRefs.Count, this.fCurrentText);
		}

		public void NavNext()
		{
		}

		public void NavPrev()
		{
		}

		public bool NavCanBackward()
		{
			return false;
		}

		public bool NavCanForward()
		{
			return false;
		}

		public bool AllowQuickFind()
		{
			return true;
		}

		public IList<ISearchResult> FindAll(string searchPattern)
		{
			return null;
		}

		public void SelectByRec(GEDCOMIndividualRecord iRec)
		{
		}

		public void QuickFind()
		{
		}

		public bool AllowFilter()
		{
			return false;
		}

		public void SetFilter()
		{
		}

		#endregion
    }
}
