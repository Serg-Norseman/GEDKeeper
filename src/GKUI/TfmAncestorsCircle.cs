using System;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKUI.Charts;

/// <summary>
/// Localization: dirty
/// CodeTransformation: need
/// </summary>

namespace GKUI
{
	public partial class TfmAncestorsCircle : Form, ILocalization
	{
		private TfmBase FBase;
		private AncestorsCircle FAncestorsCircle;
		private TGEDCOMIndividualRecord FRootPerson;

		public TfmAncestorsCircle(TfmBase aBase, TGEDCOMIndividualRecord p)
		{
			this.InitializeComponent();
			this.MdiParent = TfmGEDKeeper.Instance;

			this.FBase = aBase;
			this.FRootPerson = p;
			this.FAncestorsCircle = new AncestorsCircle(this.FBase.Tree, this.FRootPerson);
			this.FAncestorsCircle.CircleStyle = TfmGEDKeeper.Instance.Options.AncCircleOptions;
			this.BackColor = this.FAncestorsCircle.CircleStyle.BrushColor[9];

			(this as ILocalization).SetLang();
		}

		void OnLoad(object sender, EventArgs e)
		{
			this.Refresh();
		}

		void OnPaint(object sender, PaintEventArgs e)
		{
			this.FAncestorsCircle.Show(e.Graphics, base.Width, base.Height);
		}

		void OnKeyDown(object sender, KeyEventArgs e)
		{
			this.FAncestorsCircle.OnKeyDown(e);
			this.Refresh();
		}

		void noneToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.BackgroundImageLayout = ImageLayout.None;
		}

		void tileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.BackgroundImageLayout = ImageLayout.Tile;
		}

		void centerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.BackgroundImageLayout = ImageLayout.Center;
		}

		void stretchToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			this.BackgroundImageLayout = ImageLayout.Stretch;
		}

		void zoomToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			this.BackgroundImageLayout = ImageLayout.Zoom;
		}

		void loadToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.openFileDialog1.Filter = "Image files (*.jpg, *.gif, *.bmp)|*.jpg;*.gif;*.bmp";
			this.openFileDialog1.FileName = "";

			if (this.openFileDialog1.ShowDialog() == DialogResult.OK)
			{
				this.BackgroundImage = new Bitmap(this.openFileDialog1.FileName);
			}
		}

		void clearToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.BackgroundImage = null;
		}

		void TfmAncestorsCircleResize(object sender, EventArgs e)
		{
			this.Invalidate();
		}

		void ILocalization.SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
		}

	}
}
