using System;
using System.Drawing;
using System.Windows.Forms;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public class ReportProperties
	{
		private ReportTableProperties tableOptions;

		public Color papercolor;
		public Color headingbackcolor;
		public Color headingforecolor;
		public int headerfontsize;

		public bool Landscape;
		public Padding Margins;

		public ReportTableProperties Table
		{
			get { return this.tableOptions; }
		}

		public ReportProperties()
		{
			this.Margins.Left = 20;
			this.Margins.Top = 20;
			this.Margins.Right = 20;
			this.Margins.Bottom = 20;

			this.papercolor = Color.White;
			this.headingbackcolor = Color.DodgerBlue;
			this.headingforecolor = Color.Black;
			this.headerfontsize = 10;
			this.Landscape = true;

			this.tableOptions = new ReportTableProperties();
		}
	}
}
