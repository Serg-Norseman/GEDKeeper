using System;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKUI;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public partial class frmAncestorsCircle : Form
	{
		private TfmBase FBase;
		private AncestorsCircle FAncestorsCircle;
		private TGEDCOMIndividualRecord FRootPerson;

		public frmAncestorsCircle(TfmBase aBase, TGEDCOMIndividualRecord p)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FRootPerson = p;
		}

		void OnLoad(object sender, EventArgs e)
		{
			this.FAncestorsCircle = new AncestorsCircle(this.FBase.Tree, this.FRootPerson);
			try
			{
				this.BackColor = this.FAncestorsCircle.CircleStyle.BrushColor[9];
			}
			catch (ArgumentException)
			{
			}
			this.useFillColorToolStripMenuItem.Checked = true;
			this.Refresh();
		}

		void chooseSubjectToolStripMenuItem_Click(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)this.FBase.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
			if (iRec != null)
			{
				this.FRootPerson = iRec;
				bool useFillColor = this.FAncestorsCircle.UseFillColor;
				this.FAncestorsCircle = new AncestorsCircle(this.FBase.Tree, this.FRootPerson);
				this.FAncestorsCircle.UseFillColor = useFillColor;
				this.Refresh();
			}
		}

		void OnPaint(object sender, PaintEventArgs e)
		{
			if (this.FAncestorsCircle != null)
			{
				this.FAncestorsCircle.Show(e.Graphics, base.Width, base.Height);
			}
		}

		void OnKeyDown(object sender, KeyEventArgs e)
		{
			this.FAncestorsCircle.OnKeyDown(e);
			this.Refresh();
		}

		void colorStylesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			/*frmAncCircleStyleManager frmAncCircleStyleManager = new frmAncCircleStyleManager();
			frmAncCircleStyleManager.doc = this.doc;
			frmAncCircleStyleManager.FillList();
			frmAncCircleStyleManager.ShowDialog();
			if (frmAncCircleStyleManager.choosedstyle != null)
			{
				this.doc.StdAncCircleStyle = frmAncCircleStyleManager.choosedstyle;
				this.AncestorCircle.UpdateStyle();
				try
				{
					this.BackColor = frmAncCircleStyleManager.choosedstyle.BrushColor[9];
				}
				catch (ArgumentException)
				{
				}
			}
			this.Refresh();*/
		}

		void useFillColorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.useFillColorToolStripMenuItem.Checked = !this.useFillColorToolStripMenuItem.Checked;
			this.FAncestorsCircle.UseFillColor = this.useFillColorToolStripMenuItem.Checked;
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
			this.openFileDialog1.Filter = "jpg files (*.jpg)|*.jpg|gif files (.gif)|*.gif|bmp files (*.bmp)|*.bmp";
			if (this.openFileDialog1.FileName == "openFileDialog1")
			{
				this.openFileDialog1.FileName = "";
			}

			if (this.openFileDialog1.ShowDialog() == DialogResult.OK)
			{
				Bitmap backgroundImage = new Bitmap(this.openFileDialog1.FileName);
				this.BackgroundImage = backgroundImage;
			}
		}

		void clearToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.BackgroundImage = null;
		}

		void screenshotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//
		}
	}
}
